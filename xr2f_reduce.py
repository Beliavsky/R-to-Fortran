#!/usr/bin/env python3
"""Reduce an R script to a smaller xr2f compile-failure reproducer."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


@dataclass
class Trial:
    ok: bool
    returncode: int
    output: str


def split_top_level_blocks(src: str) -> list[str]:
    lines = src.splitlines(keepends=True)
    blocks: list[str] = []
    cur: list[str] = []
    depth = 0
    in_single = False
    in_double = False
    esc = False

    def scan_line(line: str) -> None:
        nonlocal depth, in_single, in_double, esc
        i = 0
        while i < len(line):
            ch = line[i]
            if esc:
                esc = False
                i += 1
                continue
            if ch == "\\":
                esc = True
                i += 1
                continue
            if ch == "'" and not in_double:
                in_single = not in_single
                i += 1
                continue
            if ch == '"' and not in_single:
                in_double = not in_double
                i += 1
                continue
            if not in_single and not in_double:
                if ch == "#":
                    break
                if ch in "([{":
                    depth += 1
                elif ch in ")]}":
                    depth = max(0, depth - 1)
            i += 1

    for line in lines:
        if not cur and not line.strip():
            blocks.append(line)
            continue
        cur.append(line)
        scan_line(line)
        if depth == 0 and not in_single and not in_double:
            blocks.append("".join(cur))
            cur = []
    if cur:
        blocks.append("".join(cur))
    return blocks


def run_cmd(cmd: list[str], cwd: Path, timeout: float) -> subprocess.CompletedProcess[str]:
    try:
        return subprocess.run(
            cmd,
            cwd=cwd,
            capture_output=True,
            text=True,
            check=False,
            timeout=timeout,
        )
    except subprocess.TimeoutExpired as exc:
        out = (exc.stdout or "") + (exc.stderr or "")
        return subprocess.CompletedProcess(cmd, 124, out, "TIMEOUT\n")


def matches(output: str, patterns: list[str]) -> bool:
    return all(p in output for p in patterns)


def xr2f_compile_cmd(input_r: Path, out_f90: Path, args: argparse.Namespace) -> list[str]:
    cmd = [
        sys.executable,
        str(args.xr2f),
        str(input_r),
        "--out",
        str(out_f90),
        "--compile",
    ]
    if args.no_warn:
        cmd.append("--no-warn")
    return cmd


def infer_match_patterns(output: str) -> list[str]:
    patterns: list[str] = []
    compiler_error = ""
    for line in output.splitlines():
        m_err = re.search(r"\b(?:Error|Fatal Error):\s*(.+)$", line)
        if m_err is not None:
            compiler_error = m_err.group(1).strip()
            break
    if compiler_error:
        patterns.append(compiler_error)

    lines = output.splitlines()
    for i, line in enumerate(lines):
        if re.search(r"\.f90:\d+:\d+:", line):
            for cand in lines[i + 1 : i + 5]:
                m_sym = re.search(r"\b([A-Za-z][A-Za-z0-9_]{3,})\b", cand.strip())
                if m_sym and m_sym.group(1).lower() not in {"error", "warning", "real", "integer"}:
                    sym = m_sym.group(1)
                    if sym not in patterns:
                        patterns.append(sym)
                    return patterns
    return patterns


def infer_matches_from_original(in_path: Path, args: argparse.Namespace) -> tuple[list[str], str, int]:
    with tempfile.TemporaryDirectory(prefix="xr2f_reduce_probe_") as td:
        out_f90 = Path(td) / f"{in_path.stem}.f90"
        cmd = xr2f_compile_cmd(in_path, out_f90, args)
        proc = run_cmd(cmd, args.run_cwd, args.timeout)
        output = (proc.stdout or "") + (proc.stderr or "")
    if proc.returncode == 0:
        return [], output, proc.returncode
    return infer_match_patterns(output), output, proc.returncode


def write_candidate(path: Path, blocks: list[str]) -> None:
    text = "".join(blocks)
    if text and not text.endswith("\n"):
        text += "\n"
    path.write_text(text, encoding="utf-8")


def check_candidate(
    blocks: list[str],
    work_dir: Path,
    candidate_r: Path,
    candidate_f90: Path,
    args: argparse.Namespace,
) -> Trial:
    write_candidate(candidate_r, blocks)
    if args.check_r:
        r_proc = run_cmd([args.rscript, str(candidate_r)], args.run_cwd, args.timeout)
        if r_proc.returncode != 0:
            return Trial(False, r_proc.returncode, (r_proc.stdout or "") + (r_proc.stderr or ""))
    cmd = xr2f_compile_cmd(candidate_r, candidate_f90, args)
    proc = run_cmd(cmd, args.run_cwd, args.timeout)
    output = (proc.stdout or "") + (proc.stderr or "")
    return Trial(proc.returncode != 0 and matches(output, args.match), proc.returncode, output)


def ddmin_blocks(
    blocks: list[str],
    work_dir: Path,
    candidate_r: Path,
    candidate_f90: Path,
    args: argparse.Namespace,
) -> tuple[list[str], Trial]:
    current = list(blocks)
    last_trial = check_candidate(current, work_dir, candidate_r, candidate_f90, args)
    if not last_trial.ok:
        return current, last_trial

    n = 2
    while len(current) >= 2:
        chunk = max(1, len(current) // n)
        reduced = False
        i = 0
        while i < len(current):
            candidate = current[:i] + current[i + chunk :]
            if not candidate:
                i += chunk
                continue
            trial = check_candidate(candidate, work_dir, candidate_r, candidate_f90, args)
            if trial.ok:
                current = candidate
                last_trial = trial
                n = max(2, n - 1)
                reduced = True
                print(f"kept reduction: {len(current)} blocks")
                break
            i += chunk
        if not reduced:
            if n >= len(current):
                break
            n = min(len(current), n * 2)
    return current, last_trial


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Reduce an R script that triggers an xr2f compile failure")
    ap.add_argument("input_r", help="input R script")
    ap.add_argument(
        "--match",
        action="append",
        help=(
            "substring that must appear in xr2f compile output; repeat for multiple "
            "required substrings; default infers a signature from the original failure"
        ),
    )
    ap.add_argument("--out", help="reduced R output path (default: <input>_repro.r)")
    ap.add_argument("--out-f90", help="final generated Fortran path (default: <out stem>.f90)")
    ap.add_argument("--xr2f", default="xr2f.py", help="path to xr2f.py")
    ap.add_argument("--rscript", default="rscript", help="Rscript command used by --check-r")
    ap.add_argument("--check-r", action="store_true", help="require each reduced candidate to run under Rscript")
    ap.add_argument("--run-cwd", default=".", help="working directory for xr2f/Rscript checks (default: current directory)")
    ap.add_argument("--timeout", type=float, default=60.0, help="per-candidate timeout in seconds")
    ap.add_argument("--no-warn", action="store_true", help="pass --no-warn to xr2f.py")
    args = ap.parse_args(argv)

    in_path = Path(args.input_r).resolve()
    args.xr2f = Path(args.xr2f).resolve()
    args.run_cwd = Path(args.run_cwd).resolve()
    if not in_path.exists():
        print(f"Missing input: {in_path}")
        return 2
    if not args.xr2f.exists():
        print(f"Missing xr2f.py: {args.xr2f}")
        return 2
    if not args.match:
        inferred, probe_output, probe_rc = infer_matches_from_original(in_path, args)
        if not inferred:
            print("Could not infer a compile-failure signature from the original input.")
            print(f"Initial xr2f exit code: {probe_rc}")
            print(probe_output)
            return 1
        args.match = inferred
        print("Inferred match substrings:")
        for pat in args.match:
            print(f"  {pat}")

    out_path = Path(args.out).resolve() if args.out else in_path.with_name(f"{in_path.stem}_repro.r")
    out_f90 = Path(args.out_f90).resolve() if args.out_f90 else out_path.with_suffix(".f90")
    src = in_path.read_text(encoding="utf-8-sig")
    blocks = split_top_level_blocks(src)
    if not blocks:
        print("No reducible blocks found.")
        return 1

    with tempfile.TemporaryDirectory(prefix="xr2f_reduce_") as td:
        work_dir = Path(td)
        candidate_r = work_dir / in_path.name
        candidate_f90 = work_dir / f"{in_path.stem}.f90"
        reduced, trial = ddmin_blocks(blocks, work_dir, candidate_r, candidate_f90, args)
        if not trial.ok:
            print("Initial candidate did not reproduce requested failure.")
            print(trial.output)
            return 1
        write_candidate(out_path, reduced)
        final = check_candidate(reduced, work_dir, out_path, out_f90, args)
        print(f"wrote {out_path}")
        print(f"wrote {out_f90}")
        print(f"blocks: {len(blocks)} -> {len(reduced)}")
        if final.ok:
            print("Reduce: PASS")
            return 0
        print("Reduce: final repro check failed unexpectedly.")
        print(final.output)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
