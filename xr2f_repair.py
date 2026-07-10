from __future__ import annotations

import argparse
import json
import re
import shutil
import subprocess
import sys
import time
from dataclasses import asdict, dataclass
from pathlib import Path


DIRECTIVE_RE = re.compile(
    r"#\s*xr2f:\s*force_(?:kind|rank)\([^)]+\)\s*=\s*[A-Za-z0-9_]+"
)


@dataclass
class TrialResult:
    trial: int
    name: str
    directives: list[str]
    source: str
    bundle: str
    command: list[str]
    returncode: int
    status: str
    score: int
    elapsed_sec: float
    stdout_tail: str
    stderr_tail: str


def extract_directive_candidates(text: str) -> list[str]:
    """Return unique xr2f force_* directive comments in first-seen order."""
    seen: set[str] = set()
    out: list[str] = []
    for match in DIRECTIVE_RE.finditer(text):
        directive = re.sub(r"\s+", " ", match.group(0).strip())
        directive = directive.replace("# xr2f: ", "# xr2f: ")
        if directive not in seen:
            seen.add(directive)
            out.append(directive)
    return out


def classify_output(returncode: int, stdout: str, stderr: str, compile_log: str = "") -> tuple[str, int]:
    text = "\n".join([stdout, stderr, compile_log])
    if "Run diff: MATCH" in text or "status: MATCH" in text:
        return "run_diff_match", 50
    if returncode == 0 and ("Run: PASS" in text or "Build: PASS" in text):
        if "Run: PASS" in text:
            return "run_pass", 40
        return "compile_pass", 30
    if returncode == 0:
        return "command_pass", 20
    if "Build: FAIL" in text or "Error:" in text:
        return "compile_fail", 10
    return "fail", 0


def tail_text(text: str, limit: int = 4000) -> str:
    if len(text) <= limit:
        return text
    return text[-limit:]


def prepend_directives(source_text: str, directives: list[str]) -> str:
    if not directives:
        return source_text
    lines = source_text.splitlines(keepends=True)
    insert_at = 0
    if lines and lines[0].startswith("#!"):
        insert_at = 1
    while insert_at < len(lines) and "coding" in lines[insert_at].lower() and lines[insert_at].lstrip().startswith("#"):
        insert_at += 1
    header = ["# xr2f repair trial directives\n"]
    header.extend(f"{directive}\n" for directive in directives)
    header.append("\n")
    return "".join(lines[:insert_at] + header + lines[insert_at:])


def resolve_source_from_bundle(bundle_dir: Path) -> Path:
    candidates = [
        path
        for path in sorted(bundle_dir.glob("*.r")) + sorted(bundle_dir.glob("*.R"))
        if not path.name.endswith("_directed.r") and not path.name.endswith("_directed.R")
    ]
    if candidates:
        return candidates[0]

    report_paths = sorted(bundle_dir.glob("*_report.json"))
    for report_path in report_paths:
        try:
            report = json.loads(report_path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        source_path = report.get("source", {}).get("path")
        if source_path:
            path = Path(source_path)
            if path.exists():
                return path
    raise FileNotFoundError(f"could not find an R source copy in bundle {bundle_dir}")


def read_candidate_text(bundle_dir: Path | None) -> str:
    if bundle_dir is None:
        return ""
    parts: list[str] = []
    for name in ("fix_targets.md",):
        path = bundle_dir / name
        if path.exists():
            parts.append(path.read_text(encoding="utf-8", errors="replace"))
    parts.extend(
        path.read_text(encoding="utf-8", errors="replace")
        for path in sorted(bundle_dir.glob("*_directed.r"))
        if path.exists()
    )
    return "\n".join(parts)


def make_xr2f_command(
    python_exe: str,
    xr2f_path: Path,
    source_path: Path,
    bundle_dir: Path,
    mode: str,
    extra_args: list[str],
) -> list[str]:
    cmd = [python_exe, str(xr2f_path), str(source_path), "--llm-bundle", str(bundle_dir)]
    if mode == "run-both":
        cmd.append("--run-both")
    elif mode == "run":
        cmd.append("--run")
    else:
        cmd.append("--compile")
    cmd.extend(extra_args)
    return cmd


def run_trial(
    *,
    trial: int,
    name: str,
    directives: list[str],
    original_text: str,
    source_name: str,
    trial_dir: Path,
    python_exe: str,
    xr2f_path: Path,
    mode: str,
    extra_args: list[str],
) -> TrialResult:
    trial_dir.mkdir(parents=True, exist_ok=True)
    source_path = trial_dir / source_name
    source_path.write_text(prepend_directives(original_text, directives), encoding="utf-8")
    bundle_dir = trial_dir / "bundle"
    cmd = make_xr2f_command(python_exe, xr2f_path, source_path, bundle_dir, mode, extra_args)
    start = time.perf_counter()
    proc = subprocess.run(
        cmd,
        cwd=trial_dir,
        capture_output=True,
        text=True,
        check=False,
    )
    elapsed = time.perf_counter() - start
    compile_log_path = bundle_dir / "compile_log.txt"
    compile_log = ""
    if compile_log_path.exists():
        compile_log = compile_log_path.read_text(encoding="utf-8", errors="replace")
    status, score = classify_output(proc.returncode, proc.stdout, proc.stderr, compile_log)
    return TrialResult(
        trial=trial,
        name=name,
        directives=directives,
        source=str(source_path),
        bundle=str(bundle_dir),
        command=cmd,
        returncode=proc.returncode,
        status=status,
        score=score,
        elapsed_sec=round(elapsed, 3),
        stdout_tail=tail_text(proc.stdout),
        stderr_tail=tail_text(proc.stderr),
    )


def write_repair_logs(work_dir: Path, results: list[TrialResult], best: TrialResult | None) -> None:
    payload = {
        "schema_version": 1,
        "best_trial": asdict(best) if best is not None else None,
        "trials": [asdict(result) for result in results],
    }
    (work_dir / "repair_log.json").write_text(json.dumps(payload, indent=2), encoding="utf-8")
    lines = ["# xr2f repair log", ""]
    if best is not None:
        lines.extend(
            [
                f"Best trial: {best.trial} ({best.status})",
                f"Source: `{best.source}`",
                f"Bundle: `{best.bundle}`",
                "",
            ]
        )
    lines.append("| trial | status | score | directives |")
    lines.append("| --- | --- | ---: | --- |")
    for result in results:
        directives = "<br>".join(f"`{directive}`" for directive in result.directives) or "(none)"
        lines.append(f"| {result.trial} | {result.status} | {result.score} | {directives} |")
    lines.append("")
    (work_dir / "repair_log.md").write_text("\n".join(lines), encoding="utf-8")


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Try conservative xr2f directive repairs on copies of an R source. "
            "Original sources and bundles are not modified."
        )
    )
    parser.add_argument("input_r", nargs="?", help="R source to repair")
    parser.add_argument("--bundle", type=Path, help="existing llm bundle to read source/candidates from")
    parser.add_argument("--work-dir", type=Path, help="directory for trial sources, bundles, and logs")
    parser.add_argument("--xr2f", type=Path, default=Path(__file__).with_name("xr2f.py"))
    parser.add_argument("--python", default=sys.executable, help="Python executable for invoking xr2f.py")
    parser.add_argument("--max-trials", type=int, default=8, help="maximum candidate directive trials")
    parser.add_argument(
        "--mode",
        choices=("compile", "run", "run-both"),
        default="compile",
        help="xr2f validation mode for each trial",
    )
    parser.add_argument(
        "--extra-xr2f-arg",
        action="append",
        default=[],
        help="additional argument passed through to every xr2f.py invocation",
    )
    parser.add_argument(
        "--keep-going",
        action="store_true",
        help="try candidates even if the baseline already satisfies the selected mode",
    )
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    bundle_dir = args.bundle.resolve() if args.bundle is not None else None
    if args.input_r is None and bundle_dir is None:
        raise SystemExit("provide an R source or --bundle")

    source_path = Path(args.input_r).resolve() if args.input_r is not None else resolve_source_from_bundle(bundle_dir)  # type: ignore[arg-type]
    if not source_path.exists():
        raise SystemExit(f"source not found: {source_path}")
    original_text = source_path.read_text(encoding="utf-8-sig")
    work_dir = args.work_dir
    if work_dir is None:
        base_name = bundle_dir.name if bundle_dir is not None else source_path.stem
        work_dir = source_path.parent / f"{base_name}_repair"
    work_dir = work_dir.resolve()
    work_dir.mkdir(parents=True, exist_ok=True)

    candidate_text = read_candidate_text(bundle_dir)
    candidates = extract_directive_candidates(candidate_text)

    results: list[TrialResult] = []
    source_name = source_path.name
    baseline = run_trial(
        trial=0,
        name="baseline",
        directives=[],
        original_text=original_text,
        source_name=source_name,
        trial_dir=work_dir / "trial_00_baseline",
        python_exe=args.python,
        xr2f_path=args.xr2f.resolve(),
        mode=args.mode,
        extra_args=args.extra_xr2f_arg,
    )
    results.append(baseline)
    if not candidates:
        candidates = extract_directive_candidates(read_candidate_text(Path(baseline.bundle)))

    target_score = 30 if args.mode == "compile" else 40
    if baseline.score >= target_score and not args.keep_going:
        best = baseline
        write_repair_logs(work_dir, results, best)
        print(f"baseline already satisfies {args.mode}: {baseline.status}")
        print(f"repair log: {work_dir / 'repair_log.md'}")
        return 0

    for offset, directive in enumerate(candidates[: max(0, args.max_trials)], start=1):
        result = run_trial(
            trial=offset,
            name=f"directive_{offset:02d}",
            directives=[directive],
            original_text=original_text,
            source_name=source_name,
            trial_dir=work_dir / f"trial_{offset:02d}",
            python_exe=args.python,
            xr2f_path=args.xr2f.resolve(),
            mode=args.mode,
            extra_args=args.extra_xr2f_arg,
        )
        results.append(result)
        if result.score >= target_score and not args.keep_going:
            break

    best = max(results, key=lambda item: (item.score, -item.trial), default=None)
    if best is not None and best.trial != 0 and best.score > baseline.score:
        shutil.copyfile(best.source, work_dir / "best.r")
    write_repair_logs(work_dir, results, best)

    if best is None:
        print("no trials were run")
        return 1
    print(f"best trial: {best.trial} ({best.status})")
    print(f"source: {best.source}")
    print(f"bundle: {best.bundle}")
    print(f"repair log: {work_dir / 'repair_log.md'}")
    return 0 if best.score >= target_score else 1


if __name__ == "__main__":
    raise SystemExit(main())
