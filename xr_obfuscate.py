#!/usr/bin/env python3
"""Obfuscate R source files using xr2f's R-name obfuscator."""

from __future__ import annotations

import argparse
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

from xr2f import obfuscate_r_source


@dataclass
class WorkItem:
    src: Path
    rel: Path


@dataclass
class Result:
    src: Path
    out: Path | None
    ok: bool
    stage: str
    message: str = ""
    skipped: bool = False


def _is_r_file(path: Path) -> bool:
    return path.is_file() and path.suffix.lower() == ".r"


def _discover_one(path: Path, recursive: bool) -> list[WorkItem]:
    path = path.resolve()
    if path.is_file():
        if not _is_r_file(path):
            raise ValueError(f"not an R file: {path}")
        return [WorkItem(path, Path(path.name))]
    if not path.is_dir():
        raise ValueError(f"missing input: {path}")
    pattern = "**/*" if recursive else "*"
    items: list[WorkItem] = []
    for cand in sorted(path.glob(pattern)):
        if _is_r_file(cand):
            items.append(WorkItem(cand.resolve(), Path(path.name) / cand.relative_to(path)))
    return items


def discover_inputs(paths: list[str], recursive: bool) -> list[WorkItem]:
    items: list[WorkItem] = []
    seen: set[Path] = set()
    for raw in paths:
        for item in _discover_one(Path(raw), recursive):
            if item.src not in seen:
                seen.add(item.src)
                items.append(item)
    return items


def output_path_for(item: WorkItem, args: argparse.Namespace, n_items: int) -> Path:
    if args.out:
        if n_items != 1:
            raise ValueError("--out can only be used with one input file")
        return Path(args.out).resolve()
    rel = item.rel.with_name(f"{item.rel.stem}{args.suffix}{item.rel.suffix}")
    if args.out_dir:
        return (Path(args.out_dir).resolve() / rel).resolve()
    return item.src.with_name(f"{item.src.stem}{args.suffix}{item.src.suffix}").resolve()


def print_captured(proc: subprocess.CompletedProcess[str]) -> None:
    if proc.stdout:
        print(proc.stdout, end="" if proc.stdout.endswith("\n") else "\n")
    if proc.stderr:
        print(proc.stderr, end="" if proc.stderr.endswith("\n") else "\n")


def check_cwd_for(item: WorkItem, out_path: Path, args: argparse.Namespace) -> Path:
    mode = args.check_cwd
    if mode == "current":
        return Path.cwd().resolve()
    if mode == "source":
        return item.src.parent.resolve()
    if mode == "out":
        return out_path.parent.resolve()
    return Path(mode).resolve()


def process_one(item: WorkItem, out_path: Path, args: argparse.Namespace) -> Result:
    try:
        src = item.src.read_text(encoding="utf-8-sig")
        obfuscated = obfuscate_r_source(src)
    except Exception as exc:
        return Result(item.src, None, False, "obfuscate", str(exc))

    try:
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(obfuscated, encoding="utf-8")
    except Exception as exc:
        return Result(item.src, out_path, False, "write", str(exc))

    if args.check:
        cmd = [args.rscript, str(out_path)]
        check_cwd = check_cwd_for(item, out_path, args)
        proc = subprocess.run(
            cmd,
            cwd=check_cwd,
            capture_output=True,
            text=True,
            check=False,
        )
        if proc.returncode != 0:
            orig_cmd = [args.rscript, str(item.src)]
            orig_proc = subprocess.run(
                orig_cmd,
                cwd=check_cwd,
                capture_output=True,
                text=True,
                check=False,
            )
            if orig_proc.returncode != 0:
                return Result(
                    item.src,
                    out_path,
                    True,
                    "original-fail",
                    f"original exit {orig_proc.returncode}",
                    skipped=True,
                )
            print("Run (obfuscated r):", " ".join(cmd))
            print(f"Run (obfuscated r): FAIL (exit {proc.returncode})")
            print_captured(proc)
            return Result(item.src, out_path, False, "check", f"exit {proc.returncode}")
        if args.verbose:
            print("Run (obfuscated r):", " ".join(cmd))
            print("Run (obfuscated r): PASS")
            print_captured(proc)

    return Result(item.src, out_path, True, "ok")


def print_summary(results: list[Result], failures_only: bool = False) -> None:
    print("")
    print("Summary:")
    for result in results:
        if failures_only and result.ok:
            continue
        status = "SKIP" if result.skipped else ("PASS" if result.ok else "FAIL")
        if result.out is not None:
            line = f"{status} {result.src} -> {result.out}"
        else:
            line = f"{status} {result.src}"
        if not result.ok or result.skipped:
            line += f" [{result.stage}: {result.message}]"
        print(line)
    n = len(results)
    n_skip = sum(1 for r in results if r.skipped)
    n_pass = sum(1 for r in results if r.ok and not r.skipped)
    n_fail = sum(1 for r in results if not r.ok)
    print(f"Totals: {n} files, {n_pass} pass, {n_skip} skip, {n_fail} fail")


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Obfuscate R files using xr2f's R-name obfuscator")
    ap.add_argument("inputs", nargs="+", help="R files or directories containing .r/.R files")
    ap.add_argument("--out", help="output path for a single input file")
    ap.add_argument("--out-dir", help="directory for obfuscated outputs")
    ap.add_argument("--suffix", default="_obfuscated", help="suffix before .r/.R (default: _obfuscated)")
    ap.add_argument("--recursive", action="store_true", help="recursively discover .r/.R files in input directories")
    ap.add_argument("--check", action="store_true", help="run each generated obfuscated R file with Rscript")
    ap.add_argument("--rscript", default="rscript", help="command used by --check (default: rscript)")
    ap.add_argument(
        "--check-cwd",
        default="current",
        help=(
            "working directory for --check: current, source, out, or an explicit path "
            "(default: current)"
        ),
    )
    ap.add_argument("--keep-going", action="store_true", help="continue after failures")
    ap.add_argument("--summary", action="store_true", help="print a compact pass/fail summary")
    ap.add_argument("--quiet", action="store_true", help="suppress per-file PASS lines; failures and summaries are still printed")
    ap.add_argument("--verbose", action="store_true", help="print Rscript output for passing --check runs")
    args = ap.parse_args(argv)

    try:
        items = discover_inputs(args.inputs, args.recursive)
        if not items:
            print("No R files found.")
            return 1
        out_paths = [output_path_for(item, args, len(items)) for item in items]
    except ValueError as exc:
        print(f"Error: {exc}")
        return 2

    results: list[Result] = []
    for item, out_path in zip(items, out_paths):
        result = process_one(item, out_path, args)
        results.append(result)
        if not args.quiet or not result.ok:
            status = "SKIP" if result.skipped else ("PASS" if result.ok else "FAIL")
            if result.out is not None:
                print(f"{status} {result.src} -> {result.out}")
            else:
                print(f"{status} {result.src}")
            if not result.ok or result.skipped:
                print(f"  {result.stage}: {result.message}")
        if not result.ok and not args.keep_going:
            break

    if args.summary or len(results) != 1:
        print_summary(results, failures_only=args.quiet)
    return 0 if all(r.ok for r in results) else 1


if __name__ == "__main__":
    raise SystemExit(main())
