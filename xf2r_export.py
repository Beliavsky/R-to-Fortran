#!/usr/bin/env python3
"""Generate initial R interfaces for public Fortran module procedures.

This is intentionally conservative.  It parses a useful subset of Fortran:

- one or more `module ... contains ... end module` blocks
- `public :: name, ...` exports
- `function ... result(...)` and `subroutine ...` procedures
- simple declarations with `intent(...)`, `allocatable`, and ranks via `(:)`

The first output target is an R `.Call` wrapper file plus optional JSON
metadata.  The C/Fortran shim that implements those `.Call` symbols is a
separate generation layer.
"""

from __future__ import annotations

import argparse
import json
import re
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable


@dataclass
class Argument:
    name: str
    fortran_type: str
    kind: str | None
    rank: int
    intent: str | None
    allocatable: bool


@dataclass
class Procedure:
    module: str
    name: str
    kind: str
    args: list[Argument]
    result_name: str | None
    result: Argument | None
    public: bool


@dataclass
class Module:
    name: str
    public_names: list[str]
    default_public: bool
    procedures: list[Procedure]


def strip_comment(line: str) -> str:
    quote: str | None = None
    i = 0
    while i < len(line):
        ch = line[i]
        if quote is not None:
            if ch == quote:
                quote = None
            i += 1
            continue
        if ch in {"'", '"'}:
            quote = ch
            i += 1
            continue
        if ch == "!":
            return line[:i]
        i += 1
    return line


def logical_lines(text: str) -> list[str]:
    out: list[str] = []
    cur = ""
    for raw in text.splitlines():
        line = strip_comment(raw).rstrip()
        if not line.strip():
            continue
        continued = line.rstrip().endswith("&")
        if continued:
            line = line.rstrip()[:-1].rstrip()
        if cur:
            cur += " " + line.lstrip("&").strip()
        else:
            cur = line.strip()
        if not continued:
            out.append(cur)
            cur = ""
    if cur:
        out.append(cur)
    return out


def split_top_level_commas(text: str) -> list[str]:
    parts: list[str] = []
    cur: list[str] = []
    depth = 0
    quote: str | None = None
    for ch in text:
        if quote is not None:
            cur.append(ch)
            if ch == quote:
                quote = None
            continue
        if ch in {"'", '"'}:
            quote = ch
            cur.append(ch)
            continue
        if ch == "(":
            depth += 1
        elif ch == ")" and depth > 0:
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
    if cur:
        parts.append("".join(cur).strip())
    return [p for p in parts if p]


def parse_decl(line: str) -> list[Argument]:
    m = re.match(r"\s*(?P<lhs>.+?)::\s*(?P<rhs>.+)\s*$", line, re.IGNORECASE)
    if m is None:
        return []

    lhs = m.group("lhs").strip()
    rhs = m.group("rhs").strip()
    attrs = [p.strip() for p in split_top_level_commas(lhs)]
    type_spec = attrs[0].lower()
    attr_text = ",".join(attrs[1:]).lower()

    type_m = re.match(r"(real|integer|logical|character|complex)\b(?:\s*\((.*?)\))?", type_spec)
    if type_m is None:
        return []
    ftype = type_m.group(1).lower()
    kind_text = type_m.group(2)
    kind = None
    if kind_text:
        kind_m = re.search(r"(?:kind\s*=\s*)?([A-Za-z_][A-Za-z0-9_]*)", kind_text)
        if kind_m:
            kind = kind_m.group(1)

    intent = None
    intent_m = re.search(r"\bintent\s*\(\s*(inout|in|out)\s*\)", attr_text)
    if intent_m:
        intent = intent_m.group(1).lower()
    allocatable = bool(re.search(r"\ballocatable\b", attr_text))

    args: list[Argument] = []
    for entity in split_top_level_commas(rhs):
        name_m = re.match(r"\s*([A-Za-z_][A-Za-z0-9_]*)\s*(?:\((.*?)\))?", entity)
        if name_m is None:
            continue
        name = name_m.group(1)
        dims = name_m.group(2)
        rank = 0
        if dims is not None:
            rank = len(split_top_level_commas(dims))
        args.append(
            Argument(
                name=name,
                fortran_type=ftype,
                kind=kind,
                rank=rank,
                intent=intent,
                allocatable=allocatable,
            )
        )
    return args


def parse_public_names(lines: Iterable[str]) -> tuple[list[str], bool]:
    public: list[str] = []
    default_public = True
    saw_access_stmt = False
    for line in lines:
        low = line.lower().strip()
        if low == "private":
            saw_access_stmt = True
            default_public = False
        elif low == "public":
            saw_access_stmt = True
            default_public = True
        m = re.match(r"\s*public\s*::\s*(.+)$", line, re.IGNORECASE)
        if m:
            saw_access_stmt = True
            public.extend(p.strip().lower() for p in split_top_level_commas(m.group(1)))
    if not saw_access_stmt:
        default_public = True
    return public, default_public


def parse_procedure(module_name: str, lines: list[str], start: int, public_names: set[str], default_public: bool) -> tuple[Procedure, int] | None:
    header = lines[start]
    m_fun = re.match(
        r"\s*(?:(?:pure|elemental|impure|recursive)\s+)*function\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*?)\)\s*(?:result\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*\))?",
        header,
        re.IGNORECASE,
    )
    m_sub = re.match(
        r"\s*(?:(?:pure|elemental|impure|recursive)\s+)*subroutine\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*?)\)",
        header,
        re.IGNORECASE,
    )
    if m_fun is None and m_sub is None:
        return None

    proc_kind = "function" if m_fun is not None else "subroutine"
    m = m_fun if m_fun is not None else m_sub
    assert m is not None
    name = m.group(1)
    dummy_names = [p.strip() for p in split_top_level_commas(m.group(2)) if p.strip()]
    result_name = m.group(3) if m_fun is not None and m.lastindex and m.lastindex >= 3 else None
    if proc_kind == "function" and result_name is None:
        result_name = name

    end_re = re.compile(rf"\s*end\s+{proc_kind}\b(?:\s+{re.escape(name)})?\s*$", re.IGNORECASE)
    j = start + 1
    body: list[str] = []
    while j < len(lines):
        if end_re.match(lines[j]):
            break
        body.append(lines[j])
        j += 1
    if j >= len(lines):
        return None

    decls: dict[str, Argument] = {}
    for line in body:
        for arg in parse_decl(line):
            decls[arg.name.lower()] = arg

    args: list[Argument] = []
    for dummy in dummy_names:
        arg = decls.get(dummy.lower())
        if arg is None:
            arg = Argument(dummy, "unknown", None, 0, None, False)
        args.append(arg)

    result = decls.get(result_name.lower()) if result_name else None
    public = default_public or name.lower() in public_names
    return Procedure(module_name, name, proc_kind, args, result_name, result, public), j + 1


def parse_modules(path: Path) -> list[Module]:
    lines = logical_lines(path.read_text(encoding="utf-8-sig"))
    modules: list[Module] = []
    i = 0
    while i < len(lines):
        m = re.match(r"\s*module\s+([A-Za-z_][A-Za-z0-9_]*)\s*$", lines[i], re.IGNORECASE)
        if m is None or re.match(r"\s*module\s+procedure\b", lines[i], re.IGNORECASE):
            i += 1
            continue
        module_name = m.group(1)
        j = i + 1
        mod_lines: list[str] = []
        while j < len(lines):
            if re.match(r"\s*end\s+module\b", lines[j], re.IGNORECASE):
                break
            mod_lines.append(lines[j])
            j += 1
        contains_idx = next((k for k, line in enumerate(mod_lines) if line.lower().strip() == "contains"), None)
        spec_lines = mod_lines if contains_idx is None else mod_lines[:contains_idx]
        body_lines = [] if contains_idx is None else mod_lines[contains_idx + 1 :]
        public_names, default_public = parse_public_names(spec_lines)
        public_set = set(public_names)

        procedures: list[Procedure] = []
        k = 0
        while k < len(body_lines):
            parsed = parse_procedure(module_name, body_lines, k, public_set, default_public)
            if parsed is None:
                k += 1
                continue
            proc, k = parsed
            if proc.public:
                procedures.append(proc)
        modules.append(Module(module_name, public_names, default_public, procedures))
        i = j + 1
    return modules


def r_coercion(arg: Argument) -> str:
    if arg.fortran_type == "real":
        if arg.rank == 0:
            return f"{arg.name} <- as.numeric({arg.name})"
        if arg.rank == 1:
            return f"{arg.name} <- as.numeric({arg.name})"
        if arg.rank == 2:
            return f"{arg.name} <- matrix(as.numeric({arg.name}), nrow = nrow({arg.name}), dimnames = dimnames({arg.name}))"
    if arg.fortran_type == "integer":
        return f"{arg.name} <- as.integer({arg.name})"
    if arg.fortran_type == "logical":
        return f"{arg.name} <- as.logical({arg.name})"
    return f"# TODO: add coercion for {arg.name}"


def call_symbol(proc: Procedure, prefix: str | None) -> str:
    if prefix:
        return f"{prefix}{proc.name}"
    return f"{proc.module}_{proc.name}"


def render_r_wrapper(proc: Procedure, prefix: str | None) -> str:
    in_args = [arg for arg in proc.args if arg.intent in {None, "in", "inout"}]
    out_args = [arg for arg in proc.args if arg.intent == "out"]
    r_args = ", ".join(arg.name for arg in in_args)
    lines: list[str] = [f"{proc.name} <- function({r_args}) {{"]
    for arg in in_args:
        lines.append(f"  {r_coercion(arg)}")
    call_args = ", ".join(arg.name for arg in in_args)
    symbol = call_symbol(proc, prefix)
    if call_args:
        lines.append(f'  .Call("{symbol}", {call_args})')
    else:
        lines.append(f'  .Call("{symbol}")')
    if proc.kind == "subroutine" and len(out_args) != 1:
        lines.insert(-1, f"  # Expected Fortran outputs: {', '.join(a.name for a in out_args) or 'none'}")
    lines.append("}")
    return "\n".join(lines)


def render_r_file(modules: list[Module], prefix: str | None) -> str:
    parts = [
        "# Generated by xf2r_export.py.",
        "# These wrappers expect matching native .Call symbols.",
        "",
    ]
    for mod in modules:
        if not mod.procedures:
            continue
        parts.append(f"# Module: {mod.name}")
        for proc in mod.procedures:
            parts.append(render_r_wrapper(proc, prefix))
            parts.append("")
    return "\n".join(parts).rstrip() + "\n"


def supported_real_arg(arg: Argument) -> bool:
    return arg.fortran_type == "real" and (arg.kind in {None, "dp", "c_double"}) and arg.rank in {0, 1, 2}


def proc_supported(proc: Procedure) -> bool:
    if not all(supported_real_arg(arg) for arg in proc.args):
        return False
    if proc.kind == "function":
        return proc.result is not None and supported_real_arg(proc.result) and proc.result.rank == 0
    out_args = [arg for arg in proc.args if arg.intent == "out"]
    return len(out_args) == 1 and supported_real_arg(out_args[0])


def input_args(proc: Procedure) -> list[Argument]:
    return [arg for arg in proc.args if arg.intent in {None, "in", "inout"}]


def output_args(proc: Procedure) -> list[Argument]:
    return [arg for arg in proc.args if arg.intent == "out"]


def f_symbol(proc: Procedure, prefix: str | None) -> str:
    return f"{call_symbol(proc, prefix)}_f"


def render_fortran_shim_proc(proc: Procedure, prefix: str | None) -> str:
    if not proc_supported(proc):
        return f"! Skipped unsupported procedure: {proc.name}"

    ins = input_args(proc)
    outs = output_args(proc)
    fsym = f_symbol(proc, prefix)
    arg_names: list[str] = []
    for arg in ins:
        arg_names.append(arg.name)
        if arg.rank == 1:
            arg_names.append(f"n_{arg.name}")
        elif arg.rank == 2:
            arg_names.extend([f"nr_{arg.name}", f"nc_{arg.name}"])
    if proc.kind == "function":
        arg_names.append("result_value")
    else:
        arg_names.append(outs[0].name)

    lines = [
        f"subroutine {fsym}({', '.join(arg_names)}) bind(C, name=\"{fsym}\")",
        "use, intrinsic :: iso_c_binding, only: c_double, c_int",
        f"use {proc.module}, only: {proc.name}",
        "implicit none",
    ]
    for arg in ins:
        if arg.rank == 0:
            lines.append(f"real(c_double), value :: {arg.name}")
        elif arg.rank == 1:
            lines.append(f"integer(c_int), value :: n_{arg.name}")
            lines.append(f"real(c_double), intent(in) :: {arg.name}(n_{arg.name})")
        elif arg.rank == 2:
            lines.append(f"integer(c_int), value :: nr_{arg.name}, nc_{arg.name}")
            lines.append(f"real(c_double), intent(in) :: {arg.name}(nr_{arg.name}, nc_{arg.name})")
    if proc.kind == "function":
        lines.append("real(c_double), intent(out) :: result_value")
        lines.append(f"result_value = {proc.name}({', '.join(arg.name for arg in ins)})")
    else:
        out = outs[0]
        if out.rank == 1:
            matrix_in = next((arg for arg in ins if arg.rank == 2), None)
            vector_in = next((arg for arg in ins if arg.rank == 1), None)
            if matrix_in is not None:
                out_len = f"nc_{matrix_in.name}"
            elif vector_in is not None:
                out_len = f"n_{vector_in.name}"
            else:
                out_len = f"size({out.name})"
            lines.append(f"real(c_double), intent(out) :: {out.name}({out_len})")
            lines.append(f"real(c_double), allocatable :: {out.name}_tmp(:)")
            call_args = [arg.name for arg in proc.args]
            lines.append(f"call {proc.name}({', '.join(call_args[:-1] + [out.name + '_tmp'])})")
            lines.append(f"{out.name} = {out.name}_tmp")
        elif out.rank == 2:
            matrix_in = next((arg for arg in ins if arg.rank == 2), None)
            if matrix_in is None:
                out_shape = f"size({out.name}, 1), size({out.name}, 2)"
            else:
                out_shape = f"nr_{matrix_in.name}, nc_{matrix_in.name}"
            lines.append(f"real(c_double), intent(out) :: {out.name}({out_shape})")
            lines.append(f"real(c_double), allocatable :: {out.name}_tmp(:,:)")
            call_args = [arg.name for arg in proc.args]
            lines.append(f"call {proc.name}({', '.join(call_args[:-1] + [out.name + '_tmp'])})")
            lines.append(f"{out.name} = {out.name}_tmp")
        else:
            lines.append(f"real(c_double), intent(out) :: {out.name}")
            call_args = [arg.name for arg in proc.args]
            lines.append(f"call {proc.name}({', '.join(call_args)})")
    lines.append(f"end subroutine {fsym}")
    return "\n".join(lines)


def render_fortran_shim(modules: list[Module], prefix: str | None) -> str:
    parts = [
        "! Generated by xf2r_export.py.",
        "! C-interoperable wrappers used by generated .Call C shims.",
        "",
    ]
    for mod in modules:
        for proc in mod.procedures:
            parts.append(render_fortran_shim_proc(proc, prefix))
            parts.append("")
    return "\n".join(parts).rstrip() + "\n"


def c_type_decl_for_input(arg: Argument) -> list[str]:
    if arg.rank == 0:
        return [f"  double c_{arg.name} = REAL({arg.name})[0];"]
    if arg.rank == 1:
        return [
            f"  R_xlen_t n_{arg.name}_xl = XLENGTH({arg.name});",
            f"  if (n_{arg.name}_xl > INT_MAX) Rf_error(\"{arg.name} is too long\");",
            f"  int n_{arg.name} = (int)n_{arg.name}_xl;",
            f"  double *p_{arg.name} = REAL({arg.name});",
        ]
    return [
        f"  if (!Rf_isMatrix({arg.name})) Rf_error(\"{arg.name} must be a matrix\");",
        f"  SEXP dim_{arg.name} = PROTECT(Rf_getAttrib({arg.name}, R_DimSymbol));",
        f"  int nr_{arg.name} = INTEGER(dim_{arg.name})[0];",
        f"  int nc_{arg.name} = INTEGER(dim_{arg.name})[1];",
        f"  double *p_{arg.name} = REAL({arg.name});",
    ]


def c_call_actuals(proc: Procedure) -> list[str]:
    actuals: list[str] = []
    for arg in input_args(proc):
        if arg.rank == 0:
            actuals.append(f"c_{arg.name}")
        elif arg.rank == 1:
            actuals.extend([f"p_{arg.name}", f"n_{arg.name}"])
        elif arg.rank == 2:
            actuals.extend([f"p_{arg.name}", f"nr_{arg.name}", f"nc_{arg.name}"])
    return actuals


def render_c_proc(proc: Procedure, prefix: str | None) -> str:
    if not proc_supported(proc):
        return f"/* Skipped unsupported procedure: {proc.name} */"

    ins = input_args(proc)
    outs = output_args(proc)
    csym = call_symbol(proc, prefix)
    fsym = f_symbol(proc, prefix)
    r_formals = ", ".join(f"SEXP {arg.name}" for arg in ins)
    lines = [f"SEXP {csym}({r_formals}) {{"]
    protect_count = 0
    for arg in ins:
        lines.append(f"  if (!Rf_isReal({arg.name})) Rf_error(\"{arg.name} must be numeric\");")
        decls = c_type_decl_for_input(arg)
        lines.extend(decls)
        if arg.rank == 2:
            protect_count += 1
    vector_inputs = [arg for arg in ins if arg.rank == 1]
    if len(vector_inputs) > 1:
        first = vector_inputs[0]
        for arg in vector_inputs[1:]:
            lines.append(
                f"  if (n_{arg.name} != n_{first.name}) Rf_error(\"vector lengths must match\");"
            )
    matrix_inputs = [arg for arg in ins if arg.rank == 2]
    if len(matrix_inputs) > 1:
        first = matrix_inputs[0]
        for arg in matrix_inputs[1:]:
            lines.append(
                f"  if (nr_{arg.name} != nr_{first.name} || nc_{arg.name} != nc_{first.name}) "
                "Rf_error(\"matrix dimensions must match\");"
            )

    actuals = c_call_actuals(proc)
    if proc.kind == "function":
        lines.append("  double result_value = 0.0;")
        lines.append(f"  {fsym}({', '.join(actuals + ['&result_value'])});")
        lines.append("  SEXP ans = PROTECT(Rf_allocVector(REALSXP, 1));")
        protect_count += 1
        lines.append("  REAL(ans)[0] = result_value;")
    else:
        out = outs[0]
        if out.rank == 1:
            matrix_in = next((arg for arg in ins if arg.rank == 2), None)
            vector_in = next((arg for arg in ins if arg.rank == 1), None)
            if matrix_in is not None:
                out_len = f"nc_{matrix_in.name}"
            elif vector_in is not None:
                out_len = f"n_{vector_in.name}"
            else:
                out_len = "1"
            lines.append(f"  SEXP ans = PROTECT(Rf_allocVector(REALSXP, {out_len}));")
            protect_count += 1
            lines.append(f"  {fsym}({', '.join(actuals + ['REAL(ans)'])});")
        elif out.rank == 2:
            matrix_in = next((arg for arg in ins if arg.rank == 2), None)
            if matrix_in is None:
                lines.append('  Rf_error("matrix output requires a matrix input in this initial generator");')
                lines.append("  return R_NilValue;")
                lines.append("}")
                return "\n".join(lines)
            lines.append(f"  SEXP ans = PROTECT(Rf_allocMatrix(REALSXP, nr_{matrix_in.name}, nc_{matrix_in.name}));")
            protect_count += 1
            lines.append(f"  {fsym}({', '.join(actuals + ['REAL(ans)'])});")
        else:
            lines.append("  SEXP ans = PROTECT(Rf_allocVector(REALSXP, 1));")
            protect_count += 1
            lines.append(f"  {fsym}({', '.join(actuals + ['REAL(ans)'])});")
    if protect_count:
        lines.append(f"  UNPROTECT({protect_count});")
    lines.append("  return ans;")
    lines.append("}")
    return "\n".join(lines)


def render_c_file(modules: list[Module], prefix: str | None) -> str:
    procs = [proc for mod in modules for proc in mod.procedures]
    parts = [
        "/* Generated by xf2r_export.py. */",
        "#include <R.h>",
        "#include <Rinternals.h>",
        "#include <R_ext/Rdynload.h>",
        "#include <limits.h>",
        "",
    ]
    for proc in procs:
        if not proc_supported(proc):
            continue
        actuals: list[str] = []
        for arg in input_args(proc):
            if arg.rank == 0:
                actuals.append("double")
            elif arg.rank == 1:
                actuals.extend(["double *", "int"])
            elif arg.rank == 2:
                actuals.extend(["double *", "int", "int"])
        actuals.append("double *")
        parts.append(f"extern void {f_symbol(proc, prefix)}({', '.join(actuals)});")
    parts.append("")
    for proc in procs:
        parts.append(render_c_proc(proc, prefix))
        parts.append("")
    parts.append("static const R_CallMethodDef CallEntries[] = {")
    for proc in procs:
        if not proc_supported(proc):
            continue
        nargs = len(input_args(proc))
        csym = call_symbol(proc, prefix)
        parts.append(f'  {{"{csym}", (DL_FUNC) &{csym}, {nargs}}},')
    parts.append("  {NULL, NULL, 0}")
    parts.append("};")
    parts.append("")
    parts.append("void R_init_xf2r_export(DllInfo *dll) {")
    parts.append("  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);")
    parts.append("  R_useDynamicSymbols(dll, FALSE);")
    parts.append("}")
    return "\n".join(parts).rstrip() + "\n"


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("source", type=Path, help="Fortran source file")
    ap.add_argument("--r-out", type=Path, help="write generated R wrappers")
    ap.add_argument("--c-out", type=Path, help="write generated C .Call shims")
    ap.add_argument("--f90-out", type=Path, help="write generated Fortran bind(C) shims")
    ap.add_argument("--json-out", type=Path, help="write parsed interface metadata")
    ap.add_argument("--symbol-prefix", help="override .Call symbol prefix, e.g. pkg_")
    ap.add_argument("--list", action="store_true", help="print public procedure summary")
    ns = ap.parse_args(argv)

    modules = parse_modules(ns.source)
    if ns.json_out:
        ns.json_out.write_text(
            json.dumps([asdict(mod) for mod in modules], indent=2) + "\n",
            encoding="utf-8",
        )
    if ns.r_out:
        ns.r_out.write_text(render_r_file(modules, ns.symbol_prefix), encoding="utf-8")
    if ns.c_out:
        ns.c_out.write_text(render_c_file(modules, ns.symbol_prefix), encoding="utf-8")
    if ns.f90_out:
        ns.f90_out.write_text(render_fortran_shim(modules, ns.symbol_prefix), encoding="utf-8")
    if ns.list or not any([ns.json_out, ns.r_out, ns.c_out, ns.f90_out]):
        for mod in modules:
            print(f"module {mod.name}")
            for proc in mod.procedures:
                args = ", ".join(f"{a.name}:{a.fortran_type}[rank={a.rank}, intent={a.intent}]" for a in proc.args)
                result = f" -> {proc.result.fortran_type}[rank={proc.result.rank}]" if proc.result else ""
                print(f"  {proc.kind} {proc.name}({args}){result}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
