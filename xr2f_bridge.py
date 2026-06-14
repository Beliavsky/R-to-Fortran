#!/usr/bin/env python3
"""Generate R-to-Fortran bridge code for selected R functions.

This mode leaves top-level R code in R and translates selected R functions to
Fortran routines callable from R through .Fortran().
"""

from __future__ import annotations

import argparse
import os
import re
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path

import xr2f


DEFAULT_COMPILER = xr2f.DEFAULT_COMPILER


@dataclass
class BridgeArg:
    name: str
    rank: int
    kind: str


@dataclass
class BridgeFunction:
    name: str
    args: list[BridgeArg]
    ret_rank: int
    ret_kind: str
    out_len_expr: str | None = None


def _strip_r_comment(line: str) -> str:
    out: list[str] = []
    in_single = False
    in_double = False
    esc = False
    for ch in line:
        if esc:
            out.append(ch)
            esc = False
            continue
        if ch == "\\" and (in_single or in_double):
            out.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            out.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            out.append(ch)
            continue
        if ch == "#" and not in_single and not in_double:
            break
        out.append(ch)
    return "".join(out)


def _brace_delta(line: str) -> int:
    s = _strip_r_comment(line)
    in_single = False
    in_double = False
    esc = False
    depth = 0
    for ch in s:
        if esc:
            esc = False
            continue
        if ch == "\\" and (in_single or in_double):
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            continue
        if in_single or in_double:
            continue
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
    return depth


def extract_top_level_function_sources(src: str) -> dict[str, str]:
    """Extract simple top-level `name <- function(...) { ... }` definitions."""
    lines = src.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    out: dict[str, str] = {}
    i = 0
    while i < len(lines):
        m = re.match(r"^\s*([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*<-\s*function\s*\(", lines[i])
        if m is None:
            i += 1
            continue
        name = m.group(1)
        start = i
        depth = 0
        seen_brace = False
        while i < len(lines):
            delta = _brace_delta(lines[i])
            if "{" in _strip_r_comment(lines[i]):
                seen_brace = True
            depth += delta
            i += 1
            if seen_brace and depth <= 0:
                break
            if not seen_brace and i > start:
                break
        out[name] = "\n".join(lines[start:i]).rstrip() + "\n"
    return out


def remove_top_level_functions(src: str, names: set[str]) -> str:
    """Return R source with selected top-level function definitions removed."""
    lines = src.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    out: list[str] = []
    i = 0
    names_l = {n.lower() for n in names}
    while i < len(lines):
        m = re.match(r"^\s*([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*<-\s*function\s*\(", lines[i])
        if m is None or m.group(1).lower() not in names_l:
            out.append(lines[i])
            i += 1
            continue
        depth = 0
        seen_brace = False
        while i < len(lines):
            delta = _brace_delta(lines[i])
            if "{" in _strip_r_comment(lines[i]):
                seen_brace = True
            depth += delta
            i += 1
            if seen_brace and depth <= 0:
                break
            if not seen_brace:
                break
    return "\n".join(out).rstrip() + "\n"


def _parse_functions(function_src: str) -> list[xr2f.FuncDef]:
    comment_lookup = xr2f.build_r_comment_lookup(function_src)
    lines = xr2f.preprocess_r_lines(function_src)
    stmts, i = xr2f.parse_block(lines, 0, comment_lookup=comment_lookup)
    if i != len(lines):
        raise NotImplementedError("could not parse function-only source")
    stmts = xr2f._lower_dim_assignments(stmts)
    stmts = xr2f.attach_function_adjacent_comments(stmts)
    stmts = xr2f._rename_duplicate_function_defs(stmts)
    stmts = xr2f.rename_conflicting_loop_vars(stmts)
    stmts = xr2f.rename_conflicting_reused_vars(stmts)
    stmts = xr2f.rename_case_conflicting_names(stmts)
    return [s for s in stmts if isinstance(s, xr2f.FuncDef)]


def _kind_for_arg(fn: xr2f.FuncDef, arg: str) -> str:
    if arg in xr2f._function_declared_double_args(fn):
        return "real"
    kinds = xr2f._annotation_kind_maps_for_function(fn)
    k = kinds.get(arg, "double")
    if k == "double":
        return "real"
    if k == "integer":
        return "integer"
    if k == "logical":
        return "logical"
    raise NotImplementedError(f"bridge does not support character argument `{arg}` in `{fn.name}`")


def _parse_result_decl(module_text: str, fn_name: str) -> tuple[str, int]:
    result_name = f"{fn_name}_result"
    pat = rf"(?im)^\s*(real\(kind=dp\)|integer|logical)(?:\s*,\s*allocatable)?\s*::\s*{re.escape(result_name)}\s*(?:\(([^)]*)\))?\s*$"
    m = re.search(pat, module_text)
    if m is None:
        raise NotImplementedError(f"could not infer bridge return type for `{fn_name}`")
    typ = m.group(1).lower()
    dims = m.group(2)
    kind = "real" if typ.startswith("real") else typ
    rank = 0 if dims is None else max(1, dims.count(",") + 1)
    return kind, rank


def _return_expr(fn: xr2f.FuncDef) -> str | None:
    if not fn.body:
        return None
    last = fn.body[-1]
    if not isinstance(last, xr2f.ExprStmt):
        return None
    expr = last.expr.strip()
    ret_arg = xr2f._return_call_arg(expr)
    return ret_arg.strip() if ret_arg is not None else expr


def _r_length_expr(src: str) -> str:
    expr = src.strip()
    expr = re.sub(r"\blength\s*\(", "length(", expr)
    expr = re.sub(r"\bsize\s*\(", "length(", expr)
    expr = expr.replace("**", "^")
    return expr


def infer_return_length_expr(fn: xr2f.FuncDef) -> str | None:
    ret = _return_expr(fn)
    if ret is None or not re.fullmatch(r"[A-Za-z]\w*", ret):
        return None
    target = ret
    for st in fn.body:
        if not isinstance(st, xr2f.Assign) or st.name != target:
            continue
        c = xr2f.parse_call_text(st.expr.strip())
        if c is None:
            continue
        if c[0].lower() in {"double", "numeric", "integer", "logical"}:
            len_src = c[1][0].strip() if c[1] else c[2].get("length", c[2].get("length.out", "")).strip()
            if len_src:
                return _r_length_expr(len_src)
        if c[0].lower() == "rep":
            len_src = c[2].get("length.out", c[2].get("len", "")).strip()
            if len_src:
                return _r_length_expr(len_src)
    return None


def _fortran_type(kind: str) -> str:
    if kind == "real":
        return "real(kind=dp)"
    if kind == "integer":
        return "integer"
    if kind == "logical":
        return "logical"
    raise NotImplementedError(f"unsupported bridge kind `{kind}`")


def _r_mode(kind: str) -> str:
    if kind == "real":
        return "double"
    if kind == "integer":
        return "integer"
    if kind == "logical":
        return "logical"
    raise NotImplementedError(f"unsupported R bridge kind `{kind}`")


def infer_bridge_functions(function_src: str, selected: list[str], module_text: str) -> list[BridgeFunction]:
    funcs = {f.name.lower(): f for f in _parse_functions(function_src)}
    out: list[BridgeFunction] = []
    for name in selected:
        fn = funcs.get(name.lower())
        if fn is None:
            raise ValueError(f"function `{name}` was not found")
        args: list[BridgeArg] = []
        for arg in fn.args:
            rank = int(xr2f.infer_arg_rank(fn, arg))
            if rank > 2:
                raise NotImplementedError(f"bridge supports rank <= 2 arguments; `{fn.name}` argument `{arg}` has rank {rank}")
            args.append(BridgeArg(arg, rank, _kind_for_arg(fn, arg)))
        ret_kind, ret_rank = _parse_result_decl(module_text, fn.name)
        if ret_rank > 2:
            raise NotImplementedError(f"bridge supports rank <= 2 return values; `{fn.name}` has rank {ret_rank}")
        out.append(BridgeFunction(fn.name, args, ret_rank, ret_kind, infer_return_length_expr(fn)))
    return out


def _dims_for_result(bf: BridgeFunction) -> tuple[str, str | None]:
    if bf.ret_rank == 0:
        return "1", None
    if bf.ret_rank == 1 and bf.out_len_expr:
        return bf.out_len_expr, None
    match = next((a for a in bf.args if a.rank == bf.ret_rank), None)
    if match is None:
        raise NotImplementedError(
            f"`{bf.name}` returns rank {bf.ret_rank}; first bridge version needs a same-rank argument to size the output"
        )
    if bf.ret_rank == 1:
        return f"n_{match.name}", match.name
    return f"n_{match.name}_1 * n_{match.name}_2", match.name


def emit_fortran_wrappers(module_name: str, funcs: list[BridgeFunction]) -> str:
    lines: list[str] = ["", "! R .Fortran() bridge wrappers"]
    for bf in funcs:
        bridge_name = f"{bf.name}_bridge"
        parts: list[str] = []
        for a in bf.args:
            parts.append(a.name)
            if a.rank == 1:
                parts.append(f"n_{a.name}")
            elif a.rank == 2:
                parts.extend([f"n_{a.name}_1", f"n_{a.name}_2"])
        parts.append("out")
        if bf.ret_rank >= 1:
            parts.append("n_out")
        lines.append(f"subroutine {bridge_name}({', '.join(parts)})")
        lines.append("use, intrinsic :: iso_fortran_env, only: dp => real64")
        lines.append(f"use {module_name}, only: {bf.name}")
        lines.append("implicit none")
        for a in bf.args:
            if a.rank == 1:
                lines.append(f"integer, intent(in) :: n_{a.name}")
                lines.append(f"{_fortran_type(a.kind)}, intent(in) :: {a.name}(n_{a.name})")
            elif a.rank == 2:
                lines.append(f"integer, intent(in) :: n_{a.name}_1")
                lines.append(f"integer, intent(in) :: n_{a.name}_2")
                lines.append(f"{_fortran_type(a.kind)}, intent(in) :: {a.name}(n_{a.name}_1, n_{a.name}_2)")
            else:
                lines.append(f"{_fortran_type(a.kind)}, intent(in) :: {a.name}")
        if bf.ret_rank == 0:
            lines.append(f"{_fortran_type(bf.ret_kind)}, intent(out) :: out")
        elif bf.ret_rank == 1:
            lines.append("integer, intent(in) :: n_out")
            lines.append(f"{_fortran_type(bf.ret_kind)}, intent(out) :: out(n_out)")
        else:
            lines.append("integer, intent(in) :: n_out")
            lines.append(f"{_fortran_type(bf.ret_kind)}, intent(out) :: out(n_out)")
            lines.append(f"{_fortran_type(bf.ret_kind)}, allocatable :: tmp(:, :)")
        call_args = ", ".join(a.name for a in bf.args)
        if bf.ret_rank == 2:
            lines.append(f"tmp = {bf.name}({call_args})")
            lines.append("out = reshape(tmp, [n_out])")
        else:
            lines.append(f"out = {bf.name}({call_args})")
        lines.append(f"end subroutine {bridge_name}")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def emit_r_wrappers(dll_name: str, funcs: list[BridgeFunction]) -> str:
    lines: list[str] = [
        "# generated by xr2f_bridge.py",
        f'dyn.load("{dll_name}")',
        "",
    ]
    for bf in funcs:
        lines.append(f"{bf.name} <- function({', '.join(a.name for a in bf.args)}) {{")
        for a in bf.args:
            mode = _r_mode(a.kind)
            if a.rank == 2:
                lines.append(f"  {a.name} <- as.matrix({a.name})")
                lines.append(f'  storage.mode({a.name}) <- "{mode}"')
                lines.append(f"  n_{a.name}_1 <- as.integer(nrow({a.name}))")
                lines.append(f"  n_{a.name}_2 <- as.integer(ncol({a.name}))")
            elif a.rank == 1:
                lines.append(f"  {a.name} <- as.{mode}({a.name})")
                lines.append(f"  n_{a.name} <- as.integer(length({a.name}))")
            else:
                lines.append(f"  {a.name} <- as.{mode}({a.name})")
        out_len, shape_from = _dims_for_result(bf)
        lines.append(f"  out <- {_r_mode(bf.ret_kind)}({out_len})")
        dot_args: list[str] = [f'"{bf.name}_bridge"']
        for a in bf.args:
            dot_args.append(f"{a.name}={a.name}")
            if a.rank == 1:
                dot_args.append(f"n_{a.name}=n_{a.name}")
            elif a.rank == 2:
                dot_args.append(f"n_{a.name}_1=n_{a.name}_1")
                dot_args.append(f"n_{a.name}_2=n_{a.name}_2")
        dot_args.append("out=out")
        if bf.ret_rank >= 1:
            dot_args.append("n_out=as.integer(length(out))")
        lines.append(f"  res <- .Fortran({', '.join(dot_args)})")
        if bf.ret_rank == 2 and shape_from is not None:
            lines.append(f"  matrix(res$out, nrow = nrow({shape_from}), ncol = ncol({shape_from}))")
        else:
            lines.append("  res$out")
        lines.append("}")
        lines.append("")
    return "\n".join(lines).rstrip() + "\n"


def compile_shared(f90_path: Path, dll_path: Path, compiler: str) -> subprocess.CompletedProcess[str]:
    parts = shlex.split(compiler, posix=not sys.platform.startswith("win"))
    if parts and "gfortran" in Path(parts[0]).name.lower() and not any(p.lower() == "-cpp" for p in parts):
        parts.append("-cpp")
    root = Path(__file__).resolve().parent
    r_runtime = root / "r.f90"
    f90_abs = f90_path.resolve()
    dll_abs = dll_path.resolve()
    f90_text = f90_path.read_text(encoding="utf-8", errors="replace")
    uses_r_mod = re.search(r"(?im)^\s*use\s+r_mod\b", f90_text) is not None
    extra_objs: list[str] = []
    include_flags: list[str] = []
    if uses_r_mod:
        obj, mod_dir, cp_cache = xr2f._cached_runtime_object(r_runtime, parts)
        if cp_cache is not None and cp_cache.returncode != 0:
            return cp_cache
        extra_objs.append(str(obj))
        compiler_name = Path(parts[0]).name.lower() if parts else ""
        if "ifx" in compiler_name:
            include_flags.extend([f"/module:{mod_dir}"] if sys.platform.startswith("win") else ["-module", str(mod_dir)])
        else:
            include_flags.extend(["-I", str(mod_dir)])
    cmd = parts + ["-shared", "-fPIC", *include_flags, *extra_objs, str(f90_abs), "-o", str(dll_abs)]
    if sys.platform.startswith("win"):
        cmd = parts + ["-shared", *include_flags, *extra_objs, str(f90_abs), "-o", str(dll_abs)]
    return subprocess.run(cmd, cwd=root, capture_output=True, text=True, check=False)


def emit_runner_r(wrapper_path: Path, original_src_without_functions: str) -> str:
    wrapper_ref = str(wrapper_path.resolve()).replace("\\", "/")
    return f'source("{wrapper_ref}")\n\n' + original_src_without_functions


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Generate Fortran shared-library bridge wrappers for selected R functions.")
    ap.add_argument("input_r", help="input R source")
    ap.add_argument("--functions", "-f", help="comma-separated function names to expose to R (default: all top-level functions)")
    ap.add_argument("--out", help="Fortran output path (default: <input>_bridge.f90)")
    ap.add_argument("--r-out", help="R wrapper output path (default: <input>_bridge.R)")
    ap.add_argument("--dll", help="shared library path (default: <input>_bridge.dll/.so)")
    ap.add_argument("--compile", action="store_true", help="compile generated Fortran to a shared library")
    ap.add_argument("--run", action="store_true", help="compile bridge and run the original R script with Fortran-backed wrappers")
    ap.add_argument("--time", action="store_true", help="time bridge generation, compile, and run; implies --run")
    ap.add_argument("--time-both", action="store_true", help="time original R and bridged R; implies --run")
    ap.add_argument("--runner-out", help="R runner path for --run (default: <input>_bridge_run.R)")
    ap.add_argument("--rscript", default="rscript", help="command used to run R scripts")
    ap.add_argument("--compiler", default=DEFAULT_COMPILER, help='compiler command, e.g. "gfortran -O3"')
    ap.add_argument("--only-selected", action="store_true", help="only compile selected function definitions, not all top-level functions")
    args = ap.parse_args(argv)
    if args.time or args.time_both:
        args.run = True

    timings: dict[str, float] = {}
    input_path = Path(args.input_r)
    src = input_path.read_text(encoding="utf-8-sig")
    if args.time_both:
        cmd_r = [args.rscript, str(input_path.resolve())]
        print("Run (r):", " ".join(cmd_r))
        t_r = time.perf_counter()
        cp_r = subprocess.run(cmd_r, cwd=input_path.parent, capture_output=True, text=True, check=False)
        timings["original_r_run"] = time.perf_counter() - t_r
        if cp_r.stdout:
            print(cp_r.stdout, end="")
        if cp_r.stdout and cp_r.stderr and not cp_r.stdout.endswith(("\n", "\r")):
            print("")
        if cp_r.stderr:
            print(cp_r.stderr, end="")
        if (cp_r.stdout or cp_r.stderr) and not ((cp_r.stderr or cp_r.stdout).endswith(("\n", "\r"))):
            print("")
        if cp_r.returncode != 0:
            print(f"Run (r): FAIL (exit {cp_r.returncode})")
            return cp_r.returncode
        print("Run (r): PASS")

    t_gen = time.perf_counter()
    functions = extract_top_level_function_sources(src)
    selected = [x.strip() for x in (args.functions or "").split(",") if x.strip()]
    if not selected:
        selected = list(functions.keys())
    if not selected:
        raise SystemExit("no top-level R functions found")
    missing = [x for x in selected if x not in functions]
    if missing:
        raise SystemExit("missing function(s): " + ", ".join(missing))
    if args.only_selected:
        function_src = "\n".join(functions[x] for x in selected)
    else:
        function_src = "\n".join(functions.values())

    stem = xr2f._fortran_ident(input_path.stem + "_bridge")
    module_name = xr2f._module_name_from_stem(stem)
    module_text = xr2f.transpile_r_functions_to_fortran_module(
        function_src,
        stem=stem,
        helper_modules={"r_mod"},
    )
    bridge_funcs = infer_bridge_functions(function_src, selected, module_text)
    wrappers_f90 = emit_fortran_wrappers(module_name, bridge_funcs)

    f90_path = Path(args.out) if args.out else input_path.with_name(input_path.stem + "_bridge.f90")
    r_path = Path(args.r_out) if args.r_out else input_path.with_name(input_path.stem + "_bridge.R")
    dll_suffix = ".dll" if sys.platform.startswith("win") else ".so"
    dll_path = Path(args.dll) if args.dll else input_path.with_name(input_path.stem + "_bridge" + dll_suffix)

    f90_path.write_text(module_text + "\n" + wrappers_f90, encoding="utf-8")
    dll_ref = str(dll_path.resolve()).replace("\\", "/")
    r_path.write_text(emit_r_wrappers(dll_ref, bridge_funcs), encoding="utf-8")
    timings["bridge_transpile"] = time.perf_counter() - t_gen
    print(f"wrote {f90_path}")
    print(f"wrote {r_path}")

    if args.run:
        args.compile = True

    if args.compile:
        t_compile = time.perf_counter()
        cp = compile_shared(f90_path, dll_path, args.compiler)
        timings["bridge_compile"] = time.perf_counter() - t_compile
        print("Build shared library")
        if cp.returncode != 0:
            print(f"Build: FAIL (exit {cp.returncode})")
            if cp.stdout:
                print(cp.stdout, end="")
            if cp.stderr:
                print(cp.stderr, end="")
            return cp.returncode
        print("Build: PASS")
        print(f"wrote {dll_path}")
    if args.run:
        runner_path = Path(args.runner_out) if args.runner_out else input_path.with_name(input_path.stem + "_bridge_run.R")
        runner_src = emit_runner_r(r_path, remove_top_level_functions(src, set(selected)))
        runner_path.write_text(runner_src, encoding="utf-8")
        print(f"wrote {runner_path}")
        cmd = [args.rscript, str(runner_path.resolve())]
        print("Run:", " ".join(cmd))
        t_run = time.perf_counter()
        cp_run = subprocess.run(cmd, cwd=input_path.parent, capture_output=True, text=True, check=False)
        timings["bridge_run"] = time.perf_counter() - t_run
        if cp_run.stdout:
            print(cp_run.stdout, end="")
        if cp_run.stdout and cp_run.stderr and not cp_run.stdout.endswith(("\n", "\r")):
            print("")
        if cp_run.stderr:
            print(cp_run.stderr, end="")
        if (cp_run.stdout or cp_run.stderr) and not ((cp_run.stderr or cp_run.stdout).endswith(("\n", "\r"))):
            print("")
        if cp_run.returncode != 0:
            print(f"Run: FAIL (exit {cp_run.returncode})")
            return cp_run.returncode
        print("Run: PASS")
    if args.time or args.time_both:
        rows: list[tuple[str, float]] = []
        if "original_r_run" in timings:
            rows.append(("original R run", timings["original_r_run"]))
        rows.append(("bridge transpile", timings.get("bridge_transpile", 0.0)))
        if "bridge_compile" in timings:
            rows.append(("bridge compile", timings["bridge_compile"]))
        if "bridge_run" in timings:
            rows.append(("bridge run", timings["bridge_run"]))
        bridge_total = timings.get("bridge_transpile", 0.0) + timings.get("bridge_compile", 0.0) + timings.get("bridge_run", 0.0)
        rows.append(("bridge total", bridge_total))
        if "original_r_run" in timings and timings.get("bridge_run", 0.0) > 0:
            rows.append(("speedup vs R run", timings["original_r_run"] / timings["bridge_run"]))
        name_w = max(len("stage"), max(len(n) for n, _ in rows))
        val_w = max(len("seconds"), max(len(f"{v:.4f}") for _, v in rows))
        print("")
        print("Timing summary (seconds):")
        print(f"  {'stage':<{name_w}}  {'seconds':>{val_w}}")
        for n, v in rows:
            print(f"  {n:<{name_w}}  {v:>{val_w}.4f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
