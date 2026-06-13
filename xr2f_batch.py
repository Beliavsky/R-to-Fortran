#!/usr/bin/env python3
"""Batch runner for xr2f.py over explicit files, glob patterns, and @list files."""

from __future__ import annotations

import argparse
from concurrent.futures import FIRST_COMPLETED, ThreadPoolExecutor, wait
from dataclasses import dataclass
from datetime import datetime
import glob
import re
import shlex
import subprocess
import sys
import time
from pathlib import Path
from typing import Iterable


@dataclass
class CaseResult:
    index: int
    source: str
    ok: bool
    rc: int
    status: str
    outcome: str
    source_loc: int = 0
    fortran_loc: int = 0
    fortran_source: str = ""
    stage_times: dict[str, float] | None = None
    fail_reason: str | None = None
    output: str = ""


class Tee:
    def __init__(self, path: Path | None):
        self.path = path
        self._fh = path.open("w", encoding="utf-8") if path is not None else None

    def close(self) -> None:
        if self._fh is not None:
            self._fh.close()
            self._fh = None

    def print(self, *args: object, sep: str = " ", end: str = "\n") -> None:
        text = sep.join(str(a) for a in args) + end
        sys.stdout.write(text)
        sys.stdout.flush()
        if self._fh is not None:
            self._fh.write(text)
            self._fh.flush()


_STAGE_RE = re.compile(
    r"^\s*(r run|transpile|compile|fortran run|fortran total)\s+([0-9]+(?:\.[0-9]+)?)\b",
    flags=re.IGNORECASE,
)
_WROTE_F90_RE = re.compile(r"^\s*wrote\s+(.+?\.f90)\s*$", flags=re.IGNORECASE | re.MULTILINE)
_TRANSPILE_FAIL_RE = re.compile(r"^\s*(?:Transpile|Transpile \(R->Python\)):\s*FAIL\s*(?:\((.*)\))?\s*$", flags=re.IGNORECASE)
_PROCESSED_LINE_RE = re.compile(r"^\s*\[\d+/\d+\]\s+(.+?)\s*$")


def _has_glob_meta(s: str) -> bool:
    return any(ch in s for ch in "*?[]")


def _is_r_source(p: Path) -> bool:
    return p.suffix.lower() == ".r"


def _read_input_list(list_path: Path, *, skip_lines: int = 0) -> list[str]:
    text = list_path.read_text(encoding="utf-8", errors="ignore")
    out: list[str] = []
    for iline, ln in enumerate(text.splitlines(), start=1):
        if iline <= skip_lines:
            continue
        raw = ln.strip()
        if not raw or raw.startswith("#"):
            continue
        out.append(raw)
    return out


def _expand_inputs(items: Iterable[str], *, skip_lines: int = 0) -> tuple[list[Path], list[str]]:
    out: list[Path] = []
    errors: list[str] = []
    seen: set[str] = set()
    seen_lists: set[str] = set()

    def add_input(it: str, *, base_dir: Path, nested: bool = False) -> None:
        if it.startswith("@"):
            list_path = Path(it[1:])
            if not list_path.is_absolute():
                list_path = base_dir / list_path
            try:
                list_key = str(list_path.resolve()).lower()
            except OSError:
                list_key = str(list_path).lower()
            if list_key in seen_lists:
                return
            seen_lists.add(list_key)
            if not list_path.exists():
                errors.append(f"@ file not found: {list_path}")
                return
            if not list_path.is_file():
                errors.append(f"@ path is not a file: {list_path}")
                return
            list_skip = skip_lines if not nested else 0
            for nested_item in _read_input_list(list_path, skip_lines=list_skip):
                add_input(nested_item, base_dir=list_path.parent, nested=True)
            return

        p_in = Path(it)
        pattern = str(p_in if p_in.is_absolute() else base_dir / p_in)
        matches = glob.glob(pattern, recursive=True) if _has_glob_meta(pattern) else [pattern]
        for m in matches:
            p = Path(m)
            if p.is_dir():
                for q in sorted(p.rglob("*.r")):
                    add_path(q)
                for q in sorted(p.rglob("*.R")):
                    add_path(q)
                continue
            if p.exists() and _is_r_source(p):
                add_path(p)

    def add_path(p: Path) -> None:
        try:
            key = str(p.resolve()).lower()
        except OSError:
            key = str(p).lower()
        if key in seen:
            return
        seen.add(key)
        out.append(p)

    for item in items:
        add_input(item, base_dir=Path.cwd())
    return sorted(out, key=lambda p: str(p).lower()), errors


def _path_key(p: Path) -> str:
    try:
        return str(p.resolve()).lower()
    except OSError:
        return str(p).lower()


def _processed_from_log(log_path: Path) -> set[str]:
    out: set[str] = set()
    text = log_path.read_text(encoding="utf-8", errors="ignore")
    for ln in text.splitlines():
        m = _PROCESSED_LINE_RE.match(ln)
        if m:
            out.add(_path_key(Path(m.group(1).strip())))
    return out


def _resolve_resume_anchor(r_files: list[Path], anchor_raw: str) -> tuple[Path | None, str | None]:
    anchor_path = Path(anchor_raw)
    keys = {_path_key(p): p for p in r_files}
    exact = keys.get(_path_key(anchor_path))
    if exact is not None:
        return exact, None
    if (not anchor_path.is_absolute()) and anchor_path.parent in {Path("."), Path("")}:
        name = anchor_path.name.lower()
        cands = [p for p in r_files if p.name.lower() == name]
        if len(cands) == 1:
            return cands[0], None
        if len(cands) > 1:
            shown = ", ".join(str(p) for p in cands[:5])
            more = "" if len(cands) <= 5 else f", ... (+{len(cands) - 5} more)"
            return None, f"ambiguous resume anchor '{anchor_raw}' matches {len(cands)} files: {shown}{more}"
    return None, f"resume anchor not found in matched files: {anchor_raw}"


def _parse_stage_times(text: str) -> dict[str, float]:
    out: dict[str, float] = {}
    for ln in text.splitlines():
        m = _STAGE_RE.match(ln)
        if not m:
            continue
        try:
            out[m.group(1).strip().lower()] = float(m.group(2))
        except ValueError:
            pass
    return out


def _extract_written_fortran(stdout: str) -> str:
    m = _WROTE_F90_RE.search(stdout or "")
    return m.group(1).strip() if m else ""


def _extract_transpile_fail_reason(stdout: str, stderr: str) -> str | None:
    for txt in (stdout or "", stderr or ""):
        for ln in txt.splitlines():
            m = _TRANSPILE_FAIL_RE.match(ln)
            if m:
                return (m.group(1) or "transpile failed").strip()
    return None


def _classify_outcome(ok: bool, stdout: str, stderr: str) -> str:
    txt = f"{stdout or ''}\n{stderr or ''}"
    if ok:
        return "full_pass"
    if re.search(r"^\s*Run \(r\):\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "r_fail"
    if re.search(r"^\s*Transpile(?: \(R->Python\))?:\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "transpile_fail"
    if re.search(r"^\s*Build:\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "compile_fail"
    if re.search(r"^\s*Run:\s*FAIL\b", txt, flags=re.IGNORECASE | re.MULTILINE):
        return "run_fail"
    return "other_fail"


def _count_file_lines(path: str) -> int:
    if not path:
        return 0
    p = Path(path)
    if not p.exists() or not p.is_file():
        return 0
    try:
        with p.open("r", encoding="utf-8", errors="ignore") as f:
            return sum(1 for _ in f)
    except OSError:
        return 0


def _command_line(argv: list[str]) -> str:
    return " ".join(shlex.quote(a) for a in argv)


def _default_tee_path(inputs: list[str]) -> Path:
    prefix = "xr2f_results"
    for item in inputs:
        raw = item[1:] if item.startswith("@") else item
        name = Path(raw).name
        if name:
            parts = name.split("_")
            if len(parts) >= 2:
                prefix = "_".join(parts[:2]) + "_results"
            else:
                prefix = Path(name).stem + "_results"
            break
    stamp = datetime.now().strftime("%Y%m%d_%I%M%p").lower()
    return Path(f"{prefix}_{stamp}.txt")


def main() -> int:
    run_started = datetime.now()
    t0 = time.perf_counter()
    ap = argparse.ArgumentParser(description="Run xr2f.py on multiple R files/globs/@list files.")
    ap.add_argument("inputs", nargs="+", help="R files, directories, glob patterns, and/or @list files.")
    ap.add_argument("--helpers", nargs="*", default=[], help="Optional helper .f90 files passed to xr2f.py.")
    ap.add_argument("--compiler", default="gfortran -O3 -march=native -Wfatal-errors", help="Compiler command forwarded to xr2f.py.")
    ap.add_argument("--rscript", default="rscript", help="Rscript command forwarded to xr2f.py.")
    mode = ap.add_mutually_exclusive_group()
    mode.add_argument("--compile", action="store_true", help="Compile transpiled Fortran (default).")
    mode.add_argument("--run", action="store_true", help="Compile and run transpiled Fortran.")
    mode.add_argument("--run-both", action="store_true", help="Run original R and transpiled Fortran.")
    mode.add_argument("--run-diff", action="store_true", help="Run both and compare output.")
    mode.add_argument("--time", action="store_true", help="Time transpile/compile/Fortran run.")
    mode.add_argument("--time-both", action="store_true", help="Time original R and transpiled Fortran.")
    ap.add_argument("--self-contained", action="store_true", help="Forward --self-contained to xr2f.py.")
    ap.add_argument("--via-python", action="store_true", help="Forward --via-python to xr2f.py.")
    ap.add_argument("--via-core-r", action="store_true", help="Forward --via-core-r to xr2f.py.")
    ap.add_argument("--allow-library", action="store_true", help="Forward --allow-library to xr2f.py.")
    ap.add_argument("--disp-real", action="store_true", help="Forward --disp-real to xr2f.py.")
    ap.add_argument("--no-recycle", action="store_true", help="Forward --no-recycle to xr2f.py.")
    ap.add_argument("--recycle-warn", action="store_true", help="Forward --recycle-warn to xr2f.py.")
    ap.add_argument("--recycle-stop", action="store_true", help="Forward --recycle-stop to xr2f.py.")
    ap.add_argument("--normalize-num-output", action="store_true", help="Forward --normalize-num-output to xr2f.py.")
    ap.add_argument("--tee-source", action="store_true", help="Forward --tee to xr2f.py for each file.")
    ap.add_argument("--tee-both-source", action="store_true", help="Forward --tee-both to xr2f.py for each file.")
    ap.add_argument("--real-print-fmt", help="Forward --real-print-fmt to xr2f.py.")
    ap.add_argument("--no-format-print", action="store_true", help="Forward --no-format-print to xr2f.py.")
    ap.add_argument("--if-const-aggressive", action="store_true", help="Forward --if-const-aggressive to xr2f.py.")
    ap.add_argument("--limit", type=int, default=0, help="Process at most this many matched files (0 = no limit).")
    ap.add_argument(
        "--skip-lines",
        type=int,
        default=0,
        help="Skip this many initial entries: lines for top-level @list files, or matched files for globs/direct inputs.",
    )
    ap.add_argument("--max-fail", "--maxfail", dest="max_fail", type=int, default=0, help="Stop after this many failures (0 = no limit).")
    ap.add_argument("--jobs", type=int, default=1, help="Run up to this many independent xr2f.py jobs concurrently.")
    ap.add_argument("--timeout", type=float, default=0.0, help="Per-file timeout in seconds (0 = no timeout).")
    ap.add_argument("--status-interval", type=float, default=60.0, help="Seconds between parallel progress reports while waiting.")
    ap.add_argument("--resume", help="Resume from prior xr2f_batch log by skipping already-processed files.")
    ap.add_argument("--resume-after", help="Start after this R source path in matched ordering.")
    ap.add_argument("--resume-with", help="Start with this R source path in matched ordering.")
    ap.add_argument("--verbose", action="store_true", help="Print full xr2f output for PASS cases too.")
    ap.add_argument("--terse", action="store_true", help="Show only failing cases plus final totals.")
    ap.add_argument(
        "--tee",
        nargs="?",
        const="",
        default=None,
        metavar="FILE",
        help="Also save batch output to FILE; if FILE is omitted, create a timestamped results file.",
    )
    args = ap.parse_args()

    if args.limit < 0:
        print("Invalid options: --limit must be >= 0.")
        return 1
    if args.skip_lines < 0:
        print("Invalid options: --skip-lines must be >= 0.")
        return 1
    if args.jobs < 1:
        print("Invalid options: --jobs must be >= 1.")
        return 1
    if args.timeout < 0:
        print("Invalid options: --timeout must be >= 0.")
        return 1
    if args.status_interval < 0:
        print("Invalid options: --status-interval must be >= 0.")
        return 1
    if args.max_fail < 0:
        print("Invalid options: --max-fail must be >= 0.")
        return 1
    if args.jobs > 1 and args.max_fail > 0:
        print("Invalid options: --jobs > 1 cannot be combined with --max-fail.")
        return 1
    if args.no_recycle and (args.recycle_warn or args.recycle_stop):
        print("Invalid options: --no-recycle cannot be combined with --recycle-warn or --recycle-stop.")
        return 1
    if args.recycle_warn and args.recycle_stop:
        print("Invalid options: --recycle-warn and --recycle-stop are mutually exclusive.")
        return 1
    if args.resume_after and args.resume_with:
        print("Invalid options: --resume-after and --resume-with are mutually exclusive.")
        return 1

    r_files, input_errors = _expand_inputs(args.inputs, skip_lines=args.skip_lines)
    if input_errors:
        for err in input_errors:
            print(err)
        return 1
    if args.skip_lines > 0 and not any(item.startswith("@") for item in args.inputs):
        r_files = r_files[args.skip_lines :]
    if not r_files:
        print("No R files matched the provided inputs.")
        return 1

    if args.resume:
        resume_path = Path(args.resume)
        if not resume_path.exists() or not resume_path.is_file():
            print(f"Invalid options: --resume file not found: {resume_path}")
            return 1
        done_keys = _processed_from_log(resume_path)
        before = len(r_files)
        r_files = [p for p in r_files if _path_key(p) not in done_keys]
        print(f"Resume: skipped {before - len(r_files)} already-processed file(s) from {resume_path}.")
        if not r_files:
            print("Resume: no remaining files to process.")
            return 0

    if args.resume_after or args.resume_with:
        anchor_raw = args.resume_with if args.resume_with else args.resume_after
        anchor_resolved, err = _resolve_resume_anchor(r_files, anchor_raw)
        if anchor_resolved is None:
            print(f"Invalid options: {err}")
            return 1
        idx = [_path_key(p) for p in r_files].index(_path_key(anchor_resolved))
        if args.resume_after:
            r_files = r_files[idx + 1 :]
            print(f"Resume-after: starting after {anchor_resolved}. Remaining: {len(r_files)} file(s).")
        else:
            r_files = r_files[idx:]
            print(f"Resume-with: starting with {anchor_resolved}. Remaining: {len(r_files)} file(s).")
        if not r_files:
            print("Resume: no remaining files to process.")
            return 0

    if args.limit > 0:
        r_files = r_files[: args.limit]

    tee_path: Path | None
    if args.tee is None:
        tee_path = None
    elif args.tee == "":
        tee_path = _default_tee_path(args.inputs)
    else:
        tee_path = Path(args.tee)
    tee = Tee(tee_path)

    xr2f_path = Path(__file__).with_name("xr2f.py")
    if not xr2f_path.exists():
        tee.print(f"Missing script: {xr2f_path}")
        tee.close()
        return 1

    total = len(r_files)
    results: list[CaseResult] = []
    failures = 0
    timing_sum = {"r run": 0.0, "transpile": 0.0, "compile": 0.0, "fortran run": 0.0, "fortran total": 0.0}
    timing_count = {k: 0 for k in timing_sum}

    tee.print("Command:", _command_line([sys.executable, str(Path(__file__).name), *sys.argv[1:]]))
    if tee_path is not None:
        tee.print("Tee file:", tee_path)
    tee.print("Started:", run_started.strftime("%Y-%m-%d %I:%M:%S %p"))
    tee.print("")

    def _run_case(i: int, rf: Path) -> CaseResult:
        rel = str(rf)
        cmd = [sys.executable, str(xr2f_path), rel, *args.helpers]
        mode_flag = "--compile"
        if args.run:
            mode_flag = "--run"
        elif args.run_both:
            mode_flag = "--run-both"
        elif args.run_diff:
            mode_flag = "--run-diff"
        elif args.time:
            mode_flag = "--time"
        elif args.time_both:
            mode_flag = "--time-both"
        cmd.append(mode_flag)
        cmd.extend(["--compiler", args.compiler, "--rscript", args.rscript])
        for flag, enabled in (
            ("--self-contained", args.self_contained),
            ("--via-python", args.via_python),
            ("--via-core-r", args.via_core_r),
            ("--allow-library", args.allow_library),
            ("--disp-real", args.disp_real),
            ("--no-recycle", args.no_recycle),
            ("--recycle-warn", args.recycle_warn),
            ("--recycle-stop", args.recycle_stop),
            ("--normalize-num-output", args.normalize_num_output),
            ("--no-format-print", args.no_format_print),
            ("--if-const-aggressive", args.if_const_aggressive),
        ):
            if enabled:
                cmd.append(flag)
        if args.tee_source:
            cmd.append("--tee")
        if args.tee_both_source:
            cmd.append("--tee-both")
        if args.real_print_fmt:
            cmd.extend(["--real-print-fmt", args.real_print_fmt])

        try:
            cp = subprocess.run(
                cmd,
                text=True,
                capture_output=True,
                encoding="utf-8",
                errors="ignore",
                timeout=(args.timeout if args.timeout > 0 else None),
            )
        except subprocess.TimeoutExpired as e:
            stdout = e.stdout or ""
            stderr = e.stderr or ""
            if isinstance(stdout, bytes):
                stdout = stdout.decode("utf-8", errors="ignore")
            if isinstance(stderr, bytes):
                stderr = stderr.decode("utf-8", errors="ignore")
            stderr = (stderr.rstrip() + "\n" + f"TIMEOUT after {args.timeout:g} seconds: {_command_line(cmd)}").lstrip()
            cp = subprocess.CompletedProcess(cmd, 124, stdout, stderr)

        ok = cp.returncode == 0
        outcome = _classify_outcome(ok, cp.stdout or "", cp.stderr or "")
        fortran_source = _extract_written_fortran(cp.stdout or "")
        out_lines: list[str] = []
        if ok:
            status = "PASS"
            if args.verbose and cp.stdout.strip():
                out_lines.append(cp.stdout.rstrip())
            if args.verbose and cp.stderr.strip():
                out_lines.append(cp.stderr.rstrip())
        else:
            status = "FAIL"
            if args.terse:
                out_lines.append(f"[{i}/{total}] {rel}")
            out_lines.append(f"  FAIL (exit {cp.returncode})")
            out_lines.append(f"command: {_command_line(cmd)}")
            if cp.stdout.strip():
                out_lines.append(cp.stdout.rstrip())
            if cp.stderr.strip():
                out_lines.append(cp.stderr.rstrip())

        return CaseResult(
            index=i,
            source=rel,
            ok=ok,
            rc=cp.returncode,
            status=status,
            outcome=outcome,
            source_loc=_count_file_lines(rel),
            fortran_loc=_count_file_lines(fortran_source),
            fortran_source=fortran_source,
            stage_times=_parse_stage_times(cp.stdout or "") or None,
            fail_reason=_extract_transpile_fail_reason(cp.stdout or "", cp.stderr or ""),
            output="\n".join(out_lines),
        )

    def _record_result(r: CaseResult) -> bool:
        nonlocal failures
        results.append(r)
        if not r.ok:
            failures += 1
        if not args.terse:
            tee.print(f"[{r.index}/{total}] {r.source}")
        if r.output:
            if args.terse and not r.ok:
                tee.print("")
            tee.print(r.output)
        if (not args.terse) and r.index < total:
            tee.print("")
        if args.max_fail > 0 and failures >= args.max_fail:
            tee.print(f"Stopped at max-fail={args.max_fail}.")
            return True
        return False

    try:
        if args.jobs == 1:
            for i, rf in enumerate(r_files, start=1):
                if _record_result(_run_case(i, rf)):
                    break
        else:
            tee.print(f"Jobs: running up to {args.jobs} xr2f.py subprocesses concurrently.")
            pending = {}
            next_idx = 1
            with ThreadPoolExecutor(max_workers=args.jobs) as executor:
                while next_idx <= total and len(pending) < args.jobs:
                    fut = executor.submit(_run_case, next_idx, r_files[next_idx - 1])
                    pending[fut] = next_idx
                    next_idx += 1
                while pending:
                    wait_timeout = args.status_interval if args.status_interval > 0 else None
                    done, _ = wait(pending, timeout=wait_timeout, return_when=FIRST_COMPLETED)
                    if not done:
                        active = ", ".join(f"[{idx}/{total}] {r_files[idx - 1]}" for idx in sorted(pending.values()))
                        tee.print(f"Still running: {active}")
                        continue
                    for fut in sorted(done, key=lambda f: pending[f]):
                        pending.pop(fut)
                        _record_result(fut.result())
                        if next_idx <= total:
                            nfut = executor.submit(_run_case, next_idx, r_files[next_idx - 1])
                            pending[nfut] = next_idx
                            next_idx += 1

        results.sort(key=lambda r: r.index)
        for r in results:
            for k, v in (r.stage_times or {}).items():
                if k in timing_sum:
                    timing_sum[k] += v
                    timing_count[k] += 1

        tee.print("")
        tee.print("Summary:")
        summary_rows = [r for r in results if (not args.terse or not r.ok)]
        if args.terse and not summary_rows:
            tee.print("(no failures)")
        if summary_rows:
            src_w = max(len("source"), *(len(r.source) for r in summary_rows))
            st_w = max(len("status"), *(len(r.status) for r in summary_rows))
            out_w = max(len("outcome"), *(len(r.outcome) for r in summary_rows))
            loc_w = max(len("R_loc"), *(len(str(r.source_loc)) for r in summary_rows))
            f90loc_w = max(len("Fortran_loc"), *(len(str(r.fortran_loc)) for r in summary_rows))
            f90src_w = max(len("Fortran_src"), *(len(r.fortran_source or "") for r in summary_rows))
            tee.print(
                f"{'source':<{src_w}}  {'status':<{st_w}}  {'outcome':<{out_w}}  "
                f"{'R_loc':>{loc_w}}  {'Fortran_loc':>{f90loc_w}}  {'Fortran_src':<{f90src_w}}"
            )
            for r in summary_rows:
                tee.print(
                    f"{r.source:<{src_w}}  {r.status:<{st_w}}  {r.outcome:<{out_w}}  "
                    f"{r.source_loc:>{loc_w}}  {r.fortran_loc:>{f90loc_w}}  {(r.fortran_source or ''):<{f90src_w}}"
                )

        n_pass = sum(1 for r in results if r.ok)
        n_fail = len(results) - n_pass
        tee.print(f"Totals: {len(results)} files, {n_pass} pass, {n_fail} fail")
        outcomes = {
            "full_pass": sum(1 for r in results if r.outcome == "full_pass"),
            "r_fail": sum(1 for r in results if r.outcome == "r_fail"),
            "transpile_fail": sum(1 for r in results if r.outcome == "transpile_fail"),
            "compile_fail": sum(1 for r in results if r.outcome == "compile_fail"),
            "run_fail": sum(1 for r in results if r.outcome == "run_fail"),
            "other_fail": sum(1 for r in results if r.outcome == "other_fail"),
        }
        tee.print(
            "Outcomes: "
            f"full_pass={outcomes['full_pass']}  "
            f"r_fail={outcomes['r_fail']}  "
            f"transpile_fail={outcomes['transpile_fail']}  "
            f"compile_fail={outcomes['compile_fail']}  "
            f"run_fail={outcomes['run_fail']}  "
            f"other_fail={outcomes['other_fail']}"
        )
        if any(timing_count.values()):
            tee.print("")
            tee.print("Timing breakdown:")
            rows = []
            for k in ("r run", "transpile", "compile", "fortran run", "fortran total"):
                cnt = timing_count[k]
                if cnt == 0:
                    continue
                s = timing_sum[k]
                rows.append((k, cnt, s, s / cnt))
            stage_w = max(len("stage"), *(len(r[0]) for r in rows))
            files_w = max(len("files"), *(len(str(r[1])) for r in rows))
            sum_w = max(len("sum_s"), *(len(f"{r[2]:.3f}") for r in rows))
            avg_w = max(len("per_file_s"), *(len(f"{r[3]:.3f}") for r in rows))
            tee.print(f"  {'stage':<{stage_w}}  {'files':>{files_w}}  {'sum_s':>{sum_w}}  {'per_file_s':>{avg_w}}")
            for stage, cnt, s, avg in rows:
                tee.print(f"  {stage:<{stage_w}}  {cnt:>{files_w}}  {s:>{sum_w}.3f}  {avg:>{avg_w}.3f}")
        elapsed = time.perf_counter() - t0
        tee.print(f"Elapsed: {elapsed:.3f} s at {datetime.now().strftime('%Y-%m-%d %I:%M:%S %p')}")
        return 0 if n_fail == 0 else 1
    finally:
        tee.close()


if __name__ == "__main__":
    raise SystemExit(main())
