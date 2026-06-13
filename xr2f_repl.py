#!/usr/bin/env python3
"""Interactive R-to-Fortran runner backed by xr2f.py."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from pathlib import Path


ROOT = Path(__file__).resolve().parent
DEFAULT_XR2F = ROOT / "xr2f.py"
DEFAULT_SESSION_R = "xr2f_repl_session.R"
DEFAULT_SESSION_FORTRAN = "xr2f_repl_session.f90"
COMPILER_NAMES = {"gfortran", "ifx"}


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
    return name


def parse_compiler_words(words: list[str], *, allow: bool = True) -> tuple[list[tuple[str, str]] | None, str]:
    if not words:
        return [], ""
    if not allow:
        return None, "compiler selectors are not valid for run-r"
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


def build_xr2f_command(
    args: argparse.Namespace,
    source_path: Path,
    fortran_path: Path,
    mode: str,
    compiler_command: str | None = None,
) -> list[str]:
    if mode == "time-both":
        run_flag = "--time-both"
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
    if not compiler_command:
        if args.ifx:
            cmd.append("--ifx")
        if args.gfortran:
            cmd.append("--gfortran")
    return cmd


def build_rscript_command(args: argparse.Namespace, source_path: Path) -> list[str]:
    return [args.rscript, str(source_path)]


def run_session(
    lines: list[str],
    args: argparse.Namespace,
    *,
    source_name: str = DEFAULT_SESSION_R,
    mode: str = "run",
    compilers: list[tuple[str, str]] | None = None,
) -> SessionResult:
    source = repl_source(lines)
    with tempfile.TemporaryDirectory(prefix="xr2f_repl_") as tmp:
        tmpdir = Path(tmp)
        source_path = tmpdir / source_name
        fortran_path = tmpdir / DEFAULT_SESSION_FORTRAN
        source_path.write_text(source, encoding="utf-8")
        if mode != "run-r" and compilers:
            return run_compiler_specs(source_path, fortran_path, args, mode, compilers)
        cmd = (
            build_rscript_command(args, source_path)
            if mode == "run-r"
            else build_xr2f_command(args, source_path, fortran_path, mode)
        )
        t0 = time.perf_counter()
        try:
            cp = subprocess.run(cmd, text=True, capture_output=True, timeout=args.timeout)
        except subprocess.TimeoutExpired as exc:
            return SessionResult(False, message=f"{cmd[0]} timed out after {exc.timeout} seconds")
        seconds = time.perf_counter() - t0
        fortran = fortran_path.read_text(encoding="utf-8", errors="replace") if fortran_path.exists() else ""
        if cp.returncode == 0:
            stdout = cp.stdout
            if mode == "run":
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
) -> SessionResult:
    t0 = time.perf_counter()
    stdout_parts: list[str] = []
    stderr_parts: list[str] = []
    timings: list[tuple[str, dict[str, float]]] = []
    ok = True
    last_fortran = ""
    for label, compiler_command in compilers:
        stdout_parts.append(f"=== Compiler: {label} ===\n")
        cmd = build_xr2f_command(args, source_path, fortran_path, mode, compiler_command)
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
        m = re.match(r"^(.+?)\s+([+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?)\s+(?:\S+)\s*$", stripped)
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
        [label, *[f"{vals[stage]:.6f}" if stage in vals else "" for stage in stages]]
        for label, vals in timings
    ]
    widths = [max(len(str(row[i])) for row in [headers, *body]) for i in range(len(headers))]
    lines = ["", "Combined timing summary (seconds):"]
    lines.append("  " + "  ".join(str(headers[i]).ljust(widths[i]) for i in range(len(headers))))
    for row in body:
        lines.append("  " + "  ".join(str(row[i]).ljust(widths[i]) for i in range(len(row))))
    return "\n".join(lines) + "\n"


def print_timing(result: SessionResult) -> None:
    print(f"xr2f_repl timing: {result.seconds:.6f} s ({'ok' if result.ok else 'failed'})")


def run_file(args: argparse.Namespace) -> int:
    source_path = Path(args.source)
    source = source_path.read_text(encoding="utf-8-sig")
    lines = source.splitlines()
    result = run_session(lines, args, source_name=source_path.name, mode=args.mode)
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


def run_repl(args: argparse.Namespace) -> int:
    print("xr2f interactive mode")
    print("Commands: run, run-r, run-both, time-both, fortran, list, clear, quit")
    lines: list[str] = []
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
        if words and words[0].lower() in {"run", "run-r", "run-both", "time-both"}:
            run_cmd = words[0].lower()
            compilers, error = parse_compiler_words(words[1:], allow=(run_cmd != "run-r"))
            if compilers is None:
                print(f"xr2f_repl: {error}", file=sys.stderr)
                continue
            result = run_session(lines, args, mode=run_cmd, compilers=compilers)
            if args.time:
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
        choices=["run", "run-r", "run-both", "time-both"],
        default="run",
        help="file mode execution backend",
    )
    parser.add_argument("--compiler", default="", help="compiler command passed to xr2f.py")
    parser.add_argument("--rscript", default="rscript", help="Rscript executable")
    parser.add_argument("--timeout", type=float, default=60.0, help="xr2f.py timeout in seconds")
    parser.add_argument("--fortran", action="store_true", help="print generated Fortran before output in file mode")
    parser.add_argument("--time", action="store_true", help="print elapsed time")
    parser.add_argument("--pretty", action="store_true", help="pass --pretty to xr2f.py")
    parser.add_argument("--round", type=int, help="pass --round-both N to xr2f.py")
    parser.add_argument("--wrap-out", type=int, help="pass --wrap-out N to xr2f.py")
    parser.add_argument("--trim-zero-decimals", "--trim-zd", action="store_true", help="trim trailing .0 output")
    parser.add_argument("--r-rng", action="store_true", help="use R RNG shim")
    parser.add_argument("--ifx", action="store_true", help="compile with ifx")
    parser.add_argument("--gfortran", action="store_true", help="compile with gfortran")
    parser.add_argument("--save-r", default=DEFAULT_SESSION_R, help="REPL session R file written on exit")
    parser.add_argument("--save-fortran", default=DEFAULT_SESSION_FORTRAN, help="REPL session Fortran file written on exit")
    parser.add_argument("--no-save", action="store_true", help="in REPL mode, do not save session files on exit")
    args = parser.parse_args(argv)

    xr2f = Path(args.xr2f)
    if not xr2f.exists():
        print(f"xr2f_repl: xr2f.py not found: {xr2f}", file=sys.stderr)
        return 2
    if args.source:
        source = Path(args.source)
        if not source.exists():
            print(f"xr2f_repl: source file not found: {source}", file=sys.stderr)
            return 2
        return run_file(args)
    return run_repl(args)


if __name__ == "__main__":
    raise SystemExit(main())
