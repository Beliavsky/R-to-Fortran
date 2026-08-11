#!/usr/bin/env python3
"""Interactive R-to-Fortran runner backed by xr2f.py."""

from __future__ import annotations

import argparse
import re
import shlex
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parent
DEFAULT_XR2F = ROOT / "xr2f.py"
DEFAULT_R_HELPER = ROOT / "src" / "r_mod.f90"
DEFAULT_OFORT = Path(r"c:\c\ofort\ofort.exe")
DEFAULT_SESSION_R = "xr2f_repl_session.R"
DEFAULT_SESSION_FORTRAN = "xr2f_repl_session.f90"
COMPILER_NAMES = {"gfortran", "ifx", "ofort"}


@dataclass
class SessionResult:
    ok: bool
    stdout: str = ""
    stderr: str = ""
    fortran: str = ""
    message: str = ""
    seconds: float = 0.0


def clean_input_line(line: str) -> str:
    return line.lstrip("\ufeff").removeprefix("Ã¯Â»Â¿")


def is_setup_or_control_line(line: str) -> bool:
    stripped = clean_input_line(line).strip()
    if not stripped or stripped.startswith("#"):
        return True
    if "<-" in stripped:
        return True
    if re.match(r"^[A-Za-z.][\w.]*\s*=", stripped):
        return True
    if re.match(r"^(if|for|while|repeat|function|else|return|break|next)\b", stripped):
        return True
    if stripped in {"}", "{"}:
        return True
    if stripped.endswith("{"):
        return True
    if re.match(r"^(print|cat|message|warning|stop)\s*\(", stripped):
        return True
    return False


def repl_source(lines: list[str]) -> str:
    out: list[str] = []
    for line in lines:
        if is_setup_or_control_line(line):
            out.append(line)
        else:
            leading = line[: len(line) - len(line.lstrip())]
            out.append(f"{leading}print({line.strip()})")
    return "\n".join(out) + ("\n" if out else "")


def _xr2f_run_output(stdout: str) -> str:
    marker = "Run: PASS"
    pos = stdout.find(marker)
    if pos < 0:
        return stdout
    rest = stdout[pos + len(marker) :]
    return rest.lstrip("\r\n")


def default_compiler_command(name: str) -> str:
    if name == "gfortran":
        return "gfortran -O3 -march=native"
    if name == "ifx":
        return "ifx /O2" if sys.platform.startswith("win") else "ifx -O2"
    if name == "ofort":
        exe = str(DEFAULT_OFORT).replace("\\", "/") if DEFAULT_OFORT.exists() else "ofort"
        return f"{exe} --fast"
    return name


def parse_compiler_words(words: list[str], *, allow: bool = True) -> tuple[list[tuple[str, str]] | None, str]:
    if not words:
        return [], ""
    if not allow:
        return None, "compiler selectors are not valid for R-only commands"
    specs: list[tuple[str, list[str]]] = []
    current_name = ""
    current_options: list[str] = []
    for word in words:
        word_l = word.lower()
        if word_l in COMPILER_NAMES:
            if current_name:
                specs.append((current_name, current_options))
            current_name = word_l
            current_options = []
            continue
        if not current_name:
            return None, f"compiler options must follow a compiler name: {word}"
        current_options.append(word)
    if current_name:
        specs.append((current_name, current_options))
    out: list[tuple[str, str]] = []
    for name, options in specs:
        if options:
            command = " ".join([name, *options])
        else:
            command = default_compiler_command(name)
        out.append((command, command))
    return out, ""


def parse_repeat_verbose_and_compiler_words(words: list[str]) -> tuple[int | None, bool, list[str], str]:
    verbose = False
    if not words:
        return 1, verbose, [], ""
    if re.fullmatch(r"\d+", words[0]):
        repeat = int(words[0])
        if repeat < 1:
            return None, verbose, [], "repeat count must be positive"
        words = words[1:]
    else:
        repeat = 1
    kept: list[str] = []
    for word in words:
        if word.lower() == "verbose":
            verbose = True
        else:
            kept.append(word)
    return repeat, verbose, kept, ""


def build_xr2f_command(
    args: argparse.Namespace,
    source_path: Path,
    fortran_path: Path,
    mode: str,
    compiler_command: str | None = None,
    repeat: int = 1,
    verbose_runs: bool = False,
) -> list[str]:
    if mode == "time-both":
        run_flag = "--time-both"
    elif mode == "time":
        run_flag = "--time"
    elif mode == "run-both":
        run_flag = "--run-both"
    else:
        run_flag = "--run"
    cmd = [
        sys.executable,
        str(Path(args.xr2f)),
        str(source_path),
        "--out",
        str(fortran_path),
        run_flag,
    ]
    if repeat != 1:
        cmd.extend(["--run-repeat", str(repeat)])
    if verbose_runs:
        cmd.append("--verbose-runs")
    if compiler_command:
        cmd.extend(["--compiler", compiler_command])
    elif args.compiler:
        cmd.extend(["--compiler", args.compiler])
    if args.rscript != "rscript":
        cmd.extend(["--rscript", args.rscript])
    if args.pretty:
        cmd.append("--pretty")
    if args.round is not None:
        cmd.extend(["--round-both", str(args.round)])
    if args.wrap_out is not None:
        cmd.extend(["--wrap-out", str(args.wrap_out)])
    if args.trim_zero_decimals:
        cmd.append("--trim-zero-decimals")
    if args.r_rng:
        cmd.append("--r-rng")
    if args.no_fortran_comments:
        cmd.append("--no-fortran-comments")
    if not compiler_command:
        if args.ifx:
            cmd.append("--ifx")
        if args.gfortran:
            cmd.append("--gfortran")
    return cmd


def build_rscript_command(args: argparse.Namespace, source_path: Path) -> list[str]:
    return [args.rscript, str(source_path)]


def is_ofort_command(command: str) -> bool:
    try:
        parts = shlex.split(command)
    except ValueError:
        return False
    if not parts:
        return False
    return Path(parts[0]).name.lower() in {"ofort", "ofort.exe"}


def build_translate_command(args: argparse.Namespace, source_path: Path, fortran_path: Path) -> list[str]:
    cmd = [sys.executable, str(Path(args.xr2f)), str(source_path), "--out", str(fortran_path)]
    if args.no_fortran_comments:
        cmd.append("--no-fortran-comments")
    return cmd


def build_ofort_command(compiler_command: str, fortran_path: Path) -> list[str]:
    parts = shlex.split(compiler_command)
    sources: list[str] = []
    text = fortran_path.read_text(encoding="utf-8", errors="replace") if fortran_path.exists() else ""
    if re.search(r"(?im)^\s*use\s+r_mod\b", text) and DEFAULT_R_HELPER.exists():
        sources.append(str(DEFAULT_R_HELPER))
    sources.append(str(fortran_path))
    return [*parts, *sources]


def run_ofort_spec(
    source_path: Path,
    fortran_path: Path,
    args: argparse.Namespace,
    mode: str,
    label: str,
    compiler_command: str,
    repeat: int,
    verbose_runs: bool,
) -> SessionResult:
    t0 = time.perf_counter()
    stdout_parts: list[str] = []
    stderr_parts: list[str] = []
    r_run_times: list[float] = []
    if mode in {"run-both", "time-both"}:
        r_cmd = build_rscript_command(args, source_path)
        stdout_parts.append("Run (r): " + " ".join(r_cmd) + "\n")
        r_runs: list[subprocess.CompletedProcess[str]] = []
        try:
            for _ in range(repeat):
                tr = time.perf_counter()
                r_runs.append(subprocess.run(r_cmd, text=True, capture_output=True, timeout=args.timeout))
                r_run_times.append(time.perf_counter() - tr)
        except subprocess.TimeoutExpired as exc:
            return SessionResult(False, message=f"{r_cmd[0]} timed out after {exc.timeout} seconds")
        failed_r = next((rr for rr in r_runs if rr.returncode != 0), None)
        r_show = failed_r or r_runs[0]
        if failed_r is not None:
            stdout_parts.append(f"Run (r): FAIL (exit {failed_r.returncode})\n")
            return SessionResult(False, stdout="".join(stdout_parts), stderr=failed_r.stderr or "", message=(failed_r.stdout or "") + (failed_r.stderr or ""), seconds=time.perf_counter() - t0)
        stdout_parts.append("Run (r): PASS\n")
        if repeat != 1:
            stdout_parts.append(f"Run (r) repeat: {repeat} ({'showing all runs' if verbose_runs else 'showing first run output'})\n")
        r_print_runs = r_runs if verbose_runs else [r_show]
        for i_rep, rr in enumerate(r_print_runs, 1):
            if verbose_runs and repeat != 1:
                stdout_parts.append(f"Run (r) output {i_rep}/{repeat}:\n")
            stdout_parts.append(rr.stdout or "")
            if rr.stdout and not rr.stdout.endswith("\n"):
                stdout_parts.append("\n")
            if rr.stderr:
                stderr_parts.append(rr.stderr)
                if not rr.stderr.endswith("\n"):
                    stderr_parts.append("\n")
        stdout_parts.append("\n")

    translate_cmd = build_translate_command(args, source_path, fortran_path)
    try:
        translate = subprocess.run(translate_cmd, text=True, capture_output=True, timeout=args.timeout)
    except subprocess.TimeoutExpired as exc:
        return SessionResult(False, message=f"{translate_cmd[0]} timed out after {exc.timeout} seconds")
    translate_seconds = time.perf_counter() - t0
    fortran = fortran_path.read_text(encoding="utf-8", errors="replace") if fortran_path.exists() else ""
    if translate.returncode != 0:
        message = "\n".join(part.rstrip() for part in (translate.stdout, translate.stderr) if part and part.strip())
        return SessionResult(False, stdout=translate.stdout, stderr=translate.stderr, fortran=fortran, message=message, seconds=translate_seconds)

    cmd = build_ofort_command(compiler_command, fortran_path)
    runs: list[subprocess.CompletedProcess[str]] = []
    run_times: list[float] = []
    try:
        for _ in range(repeat):
            tr = time.perf_counter()
            runs.append(subprocess.run(cmd, text=True, capture_output=True, timeout=args.timeout))
            run_times.append(time.perf_counter() - tr)
    except FileNotFoundError:
        return SessionResult(False, fortran=fortran, message=f"{cmd[0]} was not found", seconds=time.perf_counter() - t0)
    except subprocess.TimeoutExpired as exc:
        return SessionResult(False, fortran=fortran, message=f"{cmd[0]} timed out after {exc.timeout} seconds", seconds=time.perf_counter() - t0)
    failed = next((rr for rr in runs if rr.returncode != 0), None)
    representative = failed or runs[0]
    stdout_parts.append("Run: PASS\n" if failed is None else f"Run: FAIL (exit {failed.returncode})\n")
    if repeat != 1:
        stdout_parts.append(f"Run repeat: {repeat} ({'showing all runs' if verbose_runs else 'showing first run output'})\n")
    shown = runs if verbose_runs else [representative]
    for i_rep, rr in enumerate(shown, 1):
        if verbose_runs and repeat != 1:
            stdout_parts.append(f"Run output {i_rep}/{repeat}:\n")
        stdout_parts.append(rr.stdout or "")
        if rr.stdout and not rr.stdout.endswith("\n"):
            stdout_parts.append("\n")
    run_total = sum(run_times)
    run_mean = run_total / float(len(run_times)) if run_times else 0.0
    if len(run_times) > 1:
        run_sd = (sum((x - run_mean) ** 2 for x in run_times) / float(len(run_times) - 1)) ** 0.5
    else:
        run_sd = 0.0
    if mode in {"time", "time-both"} or args.time:
        stdout_parts.append("\nTiming summary (seconds):\n")
        stdout_parts.append("  stage             seconds\n")
        if r_run_times:
            r_total = sum(r_run_times)
            r_mean = r_total / float(len(r_run_times))
            if len(r_run_times) > 1:
                r_sd = (sum((x - r_mean) ** 2 for x in r_run_times) / float(len(r_run_times) - 1)) ** 0.5
                stdout_parts.append(f"  r run mean        {r_mean:.4f}\n")
                stdout_parts.append(f"  r run sd          {r_sd:.4f}\n")
            else:
                stdout_parts.append(f"  r run             {r_total:.4f}\n")
        stdout_parts.append(f"  transpile          {translate_seconds:.4f}\n")
        if repeat == 1:
            stdout_parts.append(f"  ofort run          {run_total:.4f}\n")
        else:
            stdout_parts.append(f"  ofort run mean     {run_mean:.4f}\n")
            stdout_parts.append(f"  ofort run sd       {run_sd:.4f}\n")
        stdout_parts.append(f"  ofort total        {(translate_seconds + run_total):.4f}\n")
    stderr = representative.stderr or ""
    ok = failed is None
    message = "\n".join(part.rstrip() for part in ("".join(stdout_parts), stderr) if part and part.strip()) if not ok else ""
    return SessionResult(ok, stdout="".join(stdout_parts), stderr=stderr, fortran=fortran, message=message, seconds=time.perf_counter() - t0)


def run_session(
    lines: list[str],
    args: argparse.Namespace,
    *,
    source_name: str = DEFAULT_SESSION_R,
    mode: str = "run",
    compilers: list[tuple[str, str]] | None = None,
    repeat: int = 1,
    verbose_runs: bool = False,
) -> SessionResult:
    source = repl_source(lines)
    with tempfile.TemporaryDirectory(prefix="xr2f_repl_") as tmp:
        tmpdir = Path(tmp)
        source_path = tmpdir / source_name
        fortran_path = tmpdir / DEFAULT_SESSION_FORTRAN
        source_path.write_text(source, encoding="utf-8")
        if mode not in {"run-r", "time-r"} and getattr(args, "ofort", False) and not compilers:
            command = default_compiler_command("ofort")
            return run_ofort_spec(source_path, fortran_path, args, mode, command, command, repeat, verbose_runs)
        if mode not in {"run-r", "time-r"} and compilers:
            if len(compilers) == 1 and is_ofort_command(compilers[0][1]):
                return run_ofort_spec(source_path, fortran_path, args, mode, compilers[0][0], compilers[0][1], repeat, verbose_runs)
            return run_compiler_specs(source_path, fortran_path, args, mode, compilers, repeat, verbose_runs)
        cmd = (
            build_rscript_command(args, source_path)
            if mode in {"run-r", "time-r"}
            else build_xr2f_command(args, source_path, fortran_path, mode, repeat=repeat, verbose_runs=verbose_runs)
        )
        t0 = time.perf_counter()
        try:
            runs = [subprocess.run(cmd, text=True, capture_output=True, timeout=args.timeout) for _ in range(repeat if mode in {"run-r", "time-r"} else 1)]
        except subprocess.TimeoutExpired as exc:
            return SessionResult(False, message=f"{cmd[0]} timed out after {exc.timeout} seconds")
        seconds = time.perf_counter() - t0
        cp = runs[0]
        failed = next((rr for rr in runs if rr.returncode != 0), None)
        if failed is not None:
            cp = failed
        fortran = fortran_path.read_text(encoding="utf-8", errors="replace") if fortran_path.exists() else ""
        if failed is None and cp.returncode == 0:
            stdout = cp.stdout
            if mode in {"run-r", "time-r"} and repeat != 1:
                if verbose_runs:
                    stdout_parts: list[str] = [f"Run (r) repeat: {repeat} (showing all runs)\n"]
                    for i_rep, rr in enumerate(runs, 1):
                        stdout_parts.append(f"Run (r) output {i_rep}/{repeat}:\n")
                        stdout_parts.append(rr.stdout)
                        if rr.stdout and not rr.stdout.endswith("\n"):
                            stdout_parts.append("\n")
                    stdout = "".join(stdout_parts)
                else:
                    stdout = f"Run (r) repeat: {repeat} (showing first run output)\n" + stdout
            if mode in {"run", "time"}:
                stdout = _xr2f_run_output(stdout)
            return SessionResult(True, stdout=stdout, stderr=cp.stderr, fortran=fortran, seconds=seconds)
        message = "\n".join(part.rstrip() for part in (cp.stdout, cp.stderr) if part and part.strip())
        return SessionResult(False, stdout=cp.stdout, stderr=cp.stderr, fortran=fortran, message=message, seconds=seconds)


def run_compiler_specs(
    source_path: Path,
    fortran_path: Path,
    args: argparse.Namespace,
    mode: str,
    compilers: list[tuple[str, str]],
    repeat: int = 1,
    verbose_runs: bool = False,
) -> SessionResult:
    t0 = time.perf_counter()
    stdout_parts: list[str] = []
    stderr_parts: list[str] = []
    timings: list[tuple[str, dict[str, float]]] = []
    ok = True
    last_fortran = ""
    for label, compiler_command in compilers:
        stdout_parts.append(f"=== Compiler: {label} ===\n")
        if is_ofort_command(compiler_command):
            result = run_ofort_spec(source_path, fortran_path, args, mode, label, compiler_command, repeat, verbose_runs)
            stdout_parts.append(result.stdout)
            if result.stdout and not result.stdout.endswith("\n"):
                stdout_parts.append("\n")
            if result.stderr:
                stderr_parts.append(result.stderr)
                if not result.stderr.endswith("\n"):
                    stderr_parts.append("\n")
            parsed = parse_timing_summary(result.stdout + "\n" + result.stderr)
            if parsed:
                timings.append((label, parsed))
            if result.fortran:
                last_fortran = result.fortran
            if not result.ok:
                ok = False
            continue
        cmd = build_xr2f_command(args, source_path, fortran_path, mode, compiler_command, repeat, verbose_runs)
        try:
            cp = subprocess.run(cmd, text=True, capture_output=True, timeout=args.timeout)
        except subprocess.TimeoutExpired as exc:
            ok = False
            stderr_parts.append(f"{cmd[0]} timed out after {exc.timeout} seconds\n")
            continue
        stdout_parts.append(cp.stdout)
        if cp.stdout and not cp.stdout.endswith("\n"):
            stdout_parts.append("\n")
        if cp.stderr:
            stderr_parts.append(cp.stderr)
            if not cp.stderr.endswith("\n"):
                stderr_parts.append("\n")
        parsed = parse_timing_summary(cp.stdout + "\n" + cp.stderr)
        if parsed:
            timings.append((label, parsed))
        if fortran_path.exists():
            last_fortran = fortran_path.read_text(encoding="utf-8", errors="replace")
        if cp.returncode != 0:
            ok = False
    combined = combined_timing_summary(timings)
    if combined:
        stdout_parts.append(combined)
    seconds = time.perf_counter() - t0
    message = "".join(stdout_parts + stderr_parts) if not ok else ""
    return SessionResult(ok, stdout="".join(stdout_parts), stderr="".join(stderr_parts), fortran=last_fortran, message=message, seconds=seconds)


def parse_timing_summary(text: str) -> dict[str, float]:
    rows: dict[str, float] = {}
    in_table = False
    for line in text.splitlines():
        stripped = line.strip()
        if stripped == "Timing summary (seconds):":
            in_table = True
            continue
        if not in_table:
            continue
        if not stripped:
            if rows:
                break
            continue
        if stripped.startswith("stage "):
            continue
        m = re.match(r"^(.+?)\s+([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)(?:\s+\S+)?\s*$", stripped)
        if m:
            rows[m.group(1).strip()] = float(m.group(2))
    return rows


def combined_timing_summary(timings: list[tuple[str, dict[str, float]]]) -> str:
    if not timings:
        return ""
    preferred = ["r run", "transpile", "compile", "fortran run", "fortran total"]
    stages = [stage for stage in preferred if any(stage in vals for _, vals in timings)]
    for _, vals in timings:
        for stage in vals:
            if stage not in stages:
                stages.append(stage)
    headers = ["compiler", *stages]
    body = [
        [label, *[f"{vals[stage]:.4f}" if stage in vals else "" for stage in stages]]
        for label, vals in timings
    ]
    widths = [max(len(str(row[i])) for row in [headers, *body]) for i in range(len(headers))]
    lines = ["", "Combined timing summary (seconds):"]
    lines.append("  " + "  ".join(str(headers[i]).ljust(widths[i]) for i in range(len(headers))))
    for row in body:
        lines.append("  " + "  ".join(str(row[i]).ljust(widths[i]) for i in range(len(row))))
    return "\n".join(lines) + "\n"


def print_timing(result: SessionResult) -> None:
    print(f"xr2f_repl timing: {result.seconds:.4f} s ({'ok' if result.ok else 'failed'})")


def run_file(args: argparse.Namespace) -> int:
    source_path = Path(args.source)
    source = source_path.read_text(encoding="utf-8-sig")
    lines = source.splitlines()
    result = run_session(
        lines,
        args,
        source_name=source_path.name,
        mode=args.mode,
        repeat=args.repeat,
        verbose_runs=args.verbose_runs,
    )
    if args.fortran:
        print(result.fortran, end="" if result.fortran.endswith("\n") else "\n")
    if args.time:
        print_timing(result)
    if result.ok:
        if result.stdout:
            print(result.stdout, end="" if result.stdout.endswith("\n") else "\n")
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="" if result.stderr.endswith("\n") else "\n")
        return 0
    if result.message:
        print(result.message, file=sys.stderr)
    return 1


def run_repl(args: argparse.Namespace, initial_source: Path | None = None) -> int:
    print("xr2f interactive mode")
    print("Commands: run, time, run-r, time-r, run-both, time-both, fortran, list, clear, quit")
    print("Run commands accept: [N] [verbose] [gfortran|ifx|ofort options...]")
    lines: list[str] = []
    if initial_source is not None:
        lines = initial_source.read_text(encoding="utf-8-sig").splitlines()
        print(f"loaded {initial_source} ({len(lines)} lines)")
    last_fortran = ""
    while True:
        try:
            line = input("xr2f> ")
        except EOFError:
            print()
            break
        line = clean_input_line(line)
        raw_cmd = line.strip()
        cmd = raw_cmd.lower()
        if cmd in {"quit", "exit"}:
            break
        if cmd == "clear":
            lines.clear()
            last_fortran = ""
            continue
        if cmd == "list":
            for i, saved in enumerate(lines, 1):
                print(f"{i}: {saved}")
            continue
        if cmd == "fortran":
            if last_fortran:
                print(last_fortran, end="" if last_fortran.endswith("\n") else "\n")
            continue
        words = raw_cmd.split()
        if words and words[0].lower() in {"run", "time", "run-r", "time-r", "run-both", "time-both"}:
            run_cmd = words[0].lower()
            repeat, verbose_runs, compiler_words, error = parse_repeat_verbose_and_compiler_words(words[1:])
            if repeat is None:
                print(f"xr2f_repl: {error}", file=sys.stderr)
                continue
            compilers, error = parse_compiler_words(compiler_words, allow=(run_cmd not in {"run-r", "time-r"}))
            if compilers is None:
                print(f"xr2f_repl: {error}", file=sys.stderr)
                continue
            result = run_session(lines, args, mode=run_cmd, compilers=compilers, repeat=repeat, verbose_runs=verbose_runs)
            if args.time or run_cmd in {"time", "time-r"}:
                print_timing(result)
            if result.ok:
                if result.fortran:
                    last_fortran = result.fortran
                if result.stdout:
                    print(result.stdout, end="" if result.stdout.endswith("\n") else "\n")
                if result.stderr:
                    print(result.stderr, file=sys.stderr, end="" if result.stderr.endswith("\n") else "\n")
            else:
                print("xr2f_repl: session failed", file=sys.stderr)
                if result.message:
                    print(result.message, file=sys.stderr)
            continue
        if is_setup_or_control_line(line):
            lines.append(line)
            continue
        candidate = lines + [line]
        result = run_session(candidate, args)
        if args.time:
            print_timing(result)
        if result.ok:
            last_fortran = result.fortran
            if result.stdout:
                print(result.stdout, end="" if result.stdout.endswith("\n") else "\n")
            if result.stderr:
                print(result.stderr, file=sys.stderr, end="" if result.stderr.endswith("\n") else "\n")
        else:
            print("xr2f_repl: line was not saved", file=sys.stderr)
            if result.message:
                print(result.message, file=sys.stderr)
    if lines and not args.no_save:
        source = repl_source(lines)
        Path(args.save_r).write_text(source, encoding="utf-8")
        result = run_session(lines, args)
        if result.ok:
            Path(args.save_fortran).write_text(result.fortran, encoding="utf-8")
            print(f"saved {args.save_r} and {args.save_fortran}")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="interactive R-to-Fortran runner using xr2f.py")
    parser.add_argument("source", nargs="?", help="optional R source file; omitted starts the REPL")
    parser.add_argument("--xr2f", default=str(DEFAULT_XR2F), help="path to xr2f.py")
    parser.add_argument(
        "--mode",
        choices=["run", "time", "run-r", "time-r", "run-both", "time-both"],
        default="run",
        help="batch execution backend",
    )
    parser.add_argument("--batch", action="store_true", help="with a source file, run once and exit instead of loading the REPL")
    parser.add_argument("--compiler", default="", help="compiler command passed to xr2f.py")
    parser.add_argument("--rscript", default="rscript", help="Rscript executable")
    parser.add_argument("--timeout", type=float, default=60.0, help="xr2f.py timeout in seconds")
    parser.add_argument("--repeat", type=int, default=1, help="run program this many times after one transpile/build")
    parser.add_argument("--verbose-runs", action="store_true", help="with --repeat, print output from every repeated run")
    parser.add_argument("--fortran", action="store_true", help="print generated Fortran before output in file mode")
    parser.add_argument("--time", action="store_true", help="print elapsed time")
    parser.add_argument("--pretty", action="store_true", help="pass --pretty to xr2f.py")
    parser.add_argument("--round", type=int, help="pass --round-both N to xr2f.py")
    parser.add_argument("--wrap-out", type=int, help="pass --wrap-out N to xr2f.py")
    parser.add_argument("--trim-zero-decimals", "--trim-zd", action="store_true", help="trim trailing .0 output")
    parser.add_argument("--r-rng", action="store_true", help="use R RNG shim")
    parser.add_argument("--no-fortran-comments", action="store_true", help="treat #f and #fortran comments as ordinary comments")
    parser.add_argument("--ofort", action="store_true", help="run generated Fortran directly with ofort")
    parser.add_argument("--ifx", action="store_true", help="compile with ifx")
    parser.add_argument("--gfortran", action="store_true", help="compile with gfortran")
    parser.add_argument("--save-r", default=DEFAULT_SESSION_R, help="REPL session R file written on exit")
    parser.add_argument("--save-fortran", default=DEFAULT_SESSION_FORTRAN, help="REPL session Fortran file written on exit")
    parser.add_argument("--no-save", action="store_true", help="in REPL mode, do not save session files on exit")
    args = parser.parse_args(argv)
    if args.repeat < 1:
        print("xr2f_repl: --repeat must be positive", file=sys.stderr)
        return 2
    if args.ofort and (args.gfortran or args.ifx or args.compiler):
        print("xr2f_repl: --ofort cannot be combined with --gfortran, --ifx, or --compiler", file=sys.stderr)
        return 2

    xr2f = Path(args.xr2f)
    if not xr2f.exists():
        print(f"xr2f_repl: xr2f.py not found: {xr2f}", file=sys.stderr)
        return 2
    if args.source:
        source = Path(args.source)
        if not source.exists():
            print(f"xr2f_repl: source file not found: {source}", file=sys.stderr)
            return 2
        if args.batch:
            return run_file(args)
        return run_repl(args, initial_source=source)
    return run_repl(args)


if __name__ == "__main__":
    raise SystemExit(main())
