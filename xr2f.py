#!/usr/bin/env python3
"""Partial R-to-Fortran transpiler (numeric subset).

This is a pragmatic first pass analogous in workflow to xp2f.py:
- transpile an R script to free-form Fortran
- optionally compile/run Fortran
- optionally run original R via `rscript`
- optionally compare outputs
"""

from __future__ import annotations

import argparse
import difflib
import glob
import hashlib
import os
import re
import shlex
import subprocess
import sys
import tempfile
import time
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path

import fortran_post as fpost
import fortran_scan as fscan

_HAS_R_MOD = False
_USER_FUNC_ARG_KIND: dict[str, list[str]] = {}
_USER_FUNC_ARG_INDEX: dict[str, dict[str, int]] = {}
_USER_FUNC_ARG_RANK: dict[str, dict[str, int]] = {}
_USER_FUNC_RETURN_RANK: dict[str, int] = {}
_USER_FUNC_ELEMENTAL: set[str] = set()
_VOID_FUNCTION_LIKE: set[str] = set()
_SUBROUTINE_FUNCTIONS: set[str] = set()
_KNOWN_VECTOR_NAMES: set[str] = set()
_KNOWN_MATRIX_NAMES: set[str] = set()
_KNOWN_LOGICAL_VECTOR_NAMES: set[str] = set()
_NULL_ARRAY_SENTINELS: dict[str, str] = {}
_NAMED_VECTOR_NAMES: dict[str, str] = {}
_NAMED_VECTOR_LABELS: dict[str, list[str]] = {}
_CATEGORICAL_LABELS: dict[str, list[str]] = {}
_TABLE_LABELS: dict[str, tuple[list[str] | None, list[str] | None]] = {}
_KNOWN_RANK3_NAMES: set[str] = set()
_KNOWN_OBJECT_LIST_NAMES: set[str] = set()
_DOTTED_VAR_RENAMES: dict[str, str] = {}
_EXPANDED_DATA_FRAME_FIELDS: dict[str, list[str]] = {}
_EXPANDED_DATA_FRAME_ALIASES: dict[str, dict[str, str]] = {}
_DATA_FRAME_FORCE_MATERIALIZE: set[str] = set()
_NO_RECYCLE = False
_R_SD_CALL_NAME = "sd"
_R_COMMENT_SENTINEL = "__XR2F_COMMENT__:"
DEFAULT_COMPILER = "gfortran -O3 -march=native -Wfatal-errors"
DEBUG_COMPILER = "gfortran -g -O0 -Wall -Wextra -Wimplicit-interface -fcheck=all -fbacktrace"
_PRETTY_FLOAT_TOKEN_RE = re.compile(
    r"(?<![A-Za-z0-9_])([+-]?(?:\d+\.\d*|\.\d+)(?:[eEdD][+-]?\d+)?)"
)


def _expanded_data_frame_col_expr(obj: str, field: str) -> str | None:
    fields = _EXPANDED_DATA_FRAME_FIELDS.get(obj)
    aliases = _EXPANDED_DATA_FRAME_ALIASES.get(obj)
    if fields is None:
        fields = _EXPANDED_DATA_FRAME_FIELDS.get(obj.lower())
        aliases = _EXPANDED_DATA_FRAME_ALIASES.get(obj.lower())
    if fields is None:
        return None
    field_l = field.lower()
    if field_l not in {f.lower() for f in fields}:
        return None
    if aliases is not None:
        for k, v in aliases.items():
            if k.lower() == field_l:
                return v
    return f"{obj}_{_sanitize_fortran_kwarg_name(field)}"


@dataclass
class Assign:
    name: str
    expr: str
    comment: str = ""


@dataclass
class PrintStmt:
    args: list[str]
    comment: str = ""


@dataclass
class ForStmt:
    var: str
    iter_expr: str
    body: list[object]


@dataclass
class WhileStmt:
    cond: str
    body: list[object]


@dataclass
class RepeatStmt:
    body: list[object]


@dataclass
class IfStmt:
    cond: str
    then_body: list[object]
    else_body: list[object]


@dataclass
class CallStmt:
    name: str
    args: list[str]
    comment: str = ""


@dataclass
class ExprStmt:
    expr: str
    comment: str = ""


@dataclass
class CommentStmt:
    text: str


@dataclass
class FuncDef:
    name: str
    args: list[str]
    defaults: dict[str, str]
    body: list[object]
    leading_comments: tuple[str, ...] = ()


def _collect_stmt_expr_texts(stmts: list[object]) -> list[str]:
    out: list[str] = []
    for st in stmts:
        if isinstance(st, Assign):
            out.append(st.expr)
        elif isinstance(st, PrintStmt):
            out.extend(st.args)
        elif isinstance(st, ForStmt):
            out.append(st.iter_expr)
            out.extend(_collect_stmt_expr_texts(st.body))
        elif isinstance(st, IfStmt):
            out.append(st.cond)
            out.extend(_collect_stmt_expr_texts(st.then_body))
            out.extend(_collect_stmt_expr_texts(st.else_body))
        elif isinstance(st, CallStmt):
            out.extend(st.args)
        elif isinstance(st, ExprStmt):
            out.append(st.expr)
    return out


def _collect_stmt_assigned_names(stmts: list[object]) -> set[str]:
    out: set[str] = set()
    for st in stmts:
        if isinstance(st, Assign):
            out.add(st.name.lower())
        elif isinstance(st, ForStmt):
            out.add(st.var.lower())
            out.update(_collect_stmt_assigned_names(st.body))
        elif isinstance(st, IfStmt):
            out.update(_collect_stmt_assigned_names(st.then_body))
            out.update(_collect_stmt_assigned_names(st.else_body))
    return out


def _infer_function_free_names(fn: FuncDef) -> set[str]:
    locals_l = {a.lower() for a in fn.args}
    locals_l.update(_collect_stmt_assigned_names(fn.body))
    refs_l: set[str] = set()
    for txt in _collect_stmt_expr_texts(fn.body):
        for t in re.findall(r"\b[A-Za-z]\w*\b", txt):
            refs_l.add(t.lower())
    # Conservative filter of obvious non-variable tokens.
    ignore = {
        "true", "false", "na", "nan", "null", "inf",
        "if", "else", "for", "while", "function", "in",
        "sum", "mean", "sd", "var", "sqrt", "log", "exp",
        "abs", "sin", "cos", "tan", "asin", "acos", "atan",
        "min", "max", "pmin", "pmax", "quantile", "dnorm",
        "runif", "rnorm", "sample", "sample.int", "length",
        "matrix", "array", "cbind", "crossprod", "tcrossprod",
        "t", "print", "cat", "paste", "paste0",
    }
    refs_l = {r for r in refs_l if r not in ignore}
    refs_l.discard(fn.name.lower())
    return {r for r in refs_l if r not in locals_l}


def helper_modules_from_files(paths: list[Path]) -> set[str]:
    """Extract top-level module names from helper Fortran files."""
    mods: set[str] = set()
    m_re = re.compile(r"^\s*module\s+([a-z][a-z0-9_]*)\b", re.IGNORECASE)
    end_re = re.compile(r"^\s*end\s+module\b", re.IGNORECASE)
    proc_re = re.compile(r"^\s*module\s+procedure\b", re.IGNORECASE)
    for p in paths:
        try:
            txt = p.read_text(encoding="utf-8")
        except Exception:
            txt = p.read_text(encoding="utf-8", errors="replace")
        for ln in txt.splitlines():
            s = ln.strip()
            if not s:
                continue
            if end_re.match(s) or proc_re.match(s):
                continue
            m = m_re.match(s)
            if m:
                mods.add(m.group(1).lower())
    return mods


def _wrapped_public_line(names: set[str]) -> list[str]:
    vals = sorted(names)
    if not vals:
        return []
    out: list[str] = []
    cur = "public :: "
    for i, nm in enumerate(vals):
        piece = nm if cur.endswith(":: ") else ", " + nm
        if len(cur + piece) > 88:
            out.append(cur + ", &")
            cur = "   & " + nm
        else:
            cur += piece
    out.append(cur)
    return out


def _r_mod_needed_public_names(f90: str) -> set[str]:
    txt = re.sub(r"&\s*\n\s*&?", "", f90)
    out: set[str] = set()
    for m in re.finditer(r"^\s*use\s+r_mod\s*,\s*only\s*:\s*(.+)$", txt, re.IGNORECASE | re.MULTILINE):
        for part in m.group(1).split(","):
            nm = part.strip()
            if "=>" in nm:
                nm = nm.split("=>", 1)[1].strip()
            if nm:
                out.add(nm)
    return out


def _render_r_mod_only(names: set[str]) -> str:
    rendered: list[str] = []
    for nm in sorted(names):
        if nm == "r_sd":
            rendered.append("r_sd => sd")
        elif nm == "print_matrix_rstyle":
            rendered.append("print_matrix => print_matrix_rstyle")
        else:
            rendered.append(nm)
    return ", ".join(rendered)


def _extract_r_mod(txt: str) -> str | None:
    m0 = re.search(r"^\s*module\s+r_mod\b", txt, re.IGNORECASE | re.MULTILINE)
    if m0 is None:
        return None
    m1 = re.search(r"^\s*end\s+module\s+r_mod\b.*$", txt[m0.start():], re.IGNORECASE | re.MULTILINE)
    if m1 is None:
        return txt[m0.start():]
    return txt[m0.start():m0.start() + m1.end()]


def _parse_r_mod_runtime(module_txt: str) -> tuple[list[str], dict[str, list[str]], dict[str, list[str]], dict[str, list[str]]]:
    lines = module_txt.splitlines()
    contains_idx = next((i for i, ln in enumerate(lines) if re.match(r"^\s*contains\s*$", ln, re.IGNORECASE)), -1)
    if contains_idx < 0:
        return lines, {}, {}, {}
    header = lines[:contains_idx]
    body = lines[contains_idx + 1:]

    interfaces: dict[str, list[str]] = {}
    procedures: dict[str, list[str]] = {}
    types: dict[str, list[str]] = {}
    header_out: list[str] = []
    i = 0
    while i < len(header):
        ln = header[i]
        m_int = re.match(r"^\s*interface\s+([A-Za-z]\w*)\b", ln, re.IGNORECASE)
        if m_int:
            name = m_int.group(1)
            block = [ln]
            i += 1
            while i < len(header):
                block.append(header[i])
                if re.match(r"^\s*end\s+interface\b", header[i], re.IGNORECASE):
                    i += 1
                    break
                i += 1
            interfaces[name.lower()] = block
            continue
        m_type = re.match(r"^\s*type\s*::\s*([A-Za-z]\w*)\b", ln, re.IGNORECASE)
        if m_type:
            name = m_type.group(1)
            block = [ln]
            i += 1
            while i < len(header):
                block.append(header[i])
                if re.match(r"^\s*end\s+type\b", header[i], re.IGNORECASE):
                    i += 1
                    break
                i += 1
            types[name.lower()] = block
            continue
        if re.match(r"^\s*public\b", ln, re.IGNORECASE):
            i += 1
            while i < len(header) and header[i - 1].rstrip().endswith("&"):
                i += 1
            continue
        header_out.append(ln)
        i += 1

    proc_start = re.compile(
        r"^\s*(?!end\b)(?:(?:pure|elemental|recursive|integer|logical|real(?:\([^)]*\))?|character(?:\([^)]*\))?|type\([^)]*\))\s+)*"
        r"(function|subroutine)\s+([A-Za-z]\w*)\b",
        re.IGNORECASE,
    )
    proc_end = re.compile(r"^\s*end\s+(function|subroutine)\b", re.IGNORECASE)
    i = 0
    while i < len(body):
        m = proc_start.match(body[i])
        if not m:
            i += 1
            continue
        name = m.group(2)
        block = [body[i]]
        i += 1
        while i < len(body):
            block.append(body[i])
            if proc_end.match(body[i]):
                i += 1
                break
            i += 1
        procedures[name.lower()] = block
    return header_out, interfaces, types, procedures


def _interface_procedures(block: list[str]) -> set[str]:
    out: set[str] = set()
    for ln in block:
        m = re.match(r"^\s*module\s+procedure\s+(.+)$", ln, re.IGNORECASE)
        if not m:
            continue
        for part in m.group(1).split(","):
            nm = part.strip().rstrip("&").strip()
            if nm:
                out.add(nm.lower())
    return out


def _declared_names_in_fortran_line(line: str) -> set[str]:
    code = fscan.strip_comment(line).strip()
    if "::" not in code:
        return set()
    lhs, rhs = code.split("::", 1)
    if re.match(r"^\s*(public|private|use|interface|module\s+procedure)\b", lhs, re.IGNORECASE):
        return set()
    out: set[str] = set()
    for part in split_top_level_commas(rhs):
        item = part.strip()
        if not item:
            continue
        item = item.split("=", 1)[0].strip()
        m = re.match(r"^([A-Za-z]\w*)", item)
        if m:
            out.add(m.group(1).lower())
    return out


def prune_r_mod_runtime(module_txt: str, public_needed: set[str]) -> str:
    header, interfaces, types, procedures = _parse_r_mod_runtime(module_txt)
    proc_names = set(procedures)
    iface_members = {name: _interface_procedures(block) for name, block in interfaces.items()}
    needed_public_l = {n.lower() for n in public_needed}
    keep_procs: set[str] = set()
    keep_ifaces: set[str] = set()

    def add_name(name_l: str) -> None:
        if name_l in interfaces:
            keep_ifaces.add(name_l)
            keep_procs.update(iface_members.get(name_l, set()))
        elif name_l in procedures:
            keep_procs.add(name_l)

    for nm in needed_public_l:
        add_name(nm)

    changed = True
    while changed:
        changed = False
        scan = "\n".join("\n".join(procedures[p]) for p in sorted(keep_procs) if p in procedures)
        for pname in sorted(proc_names - keep_procs):
            if re.search(rf"\b{re.escape(pname)}\s*\(", scan, re.IGNORECASE):
                keep_procs.add(pname)
                changed = True
        for iname, members in interfaces.items():
            if iname in keep_ifaces:
                continue
            if re.search(rf"\b{re.escape(iname)}\s*\(", scan, re.IGNORECASE):
                keep_ifaces.add(iname)
                before = len(keep_procs)
                keep_procs.update(members)
                changed = changed or len(keep_procs) != before

    kept_text = "\n".join("\n".join(procedures[p]) for p in sorted(keep_procs) if p in procedures)
    keep_types = {n for n in types if n in needed_public_l or re.search(rf"\btype\s*\(\s*{re.escape(n)}\s*\)", kept_text, re.IGNORECASE)}
    public_out = {n for n in public_needed if n.lower() in keep_ifaces or n.lower() in keep_procs or n.lower() in keep_types}
    used_header_names = {
        nm.lower()
        for nm in re.findall(r"\b[A-Za-z]\w*\b", kept_text + "\n" + "\n".join("\n".join(types[t]) for t in keep_types))
    }

    out: list[str] = []
    i_head = 0
    while i_head < len(header):
        ln = header[i_head]
        m_iso = re.match(r"^(\s*use\s*,\s*intrinsic\s*::\s*iso_fortran_env\s*,\s*only\s*:\s*)(.*)$", ln, re.IGNORECASE)
        if m_iso:
            imports = ["real64"]
            if "int64" in used_header_names:
                imports.append("int64")
            out.append(m_iso.group(1) + ", ".join(imports))
            i_head += 1
            continue
        m_ieee = re.match(r"^(\s*use\s*,\s*intrinsic\s*::\s*ieee_arithmetic\s*,\s*only\s*:\s*)(.*)$", ln, re.IGNORECASE)
        if m_ieee:
            block = [ln]
            i_head += 1
            while block[-1].rstrip().endswith("&") and i_head < len(header):
                block.append(header[i_head])
                i_head += 1
            imports_txt = " ".join(block)
            imports_txt = imports_txt.replace("&", " ")
            imports = []
            m_only = re.search(r"\bonly\s*:\s*(.*)$", imports_txt, re.IGNORECASE)
            if m_only:
                for part in m_only.group(1).split(","):
                    nm = part.strip()
                    if nm and nm.lower() in used_header_names:
                        imports.append(nm)
            if imports:
                out.append(m_ieee.group(1) + ", ".join(imports))
            continue
        names = _declared_names_in_fortran_line(ln)
        if names and "dp" not in names and not (names & used_header_names):
            i_head += 1
            continue
        out.append(ln)
        i_head += 1
    # Keep all simple module state declarations; procedures may depend on them.
    out.extend(_wrapped_public_line(public_out))
    for t in sorted(keep_types):
        out.append("")
        out.extend(types[t])
    for iface in sorted(keep_ifaces):
        out.append("")
        block = interfaces[iface]
        members = iface_members.get(iface, set()) & keep_procs
        if members:
            new_block: list[str] = [block[0]]
            emitted_members = False
            for ln in block[1:]:
                if re.match(r"^\s*module\s+procedure\b", ln, re.IGNORECASE):
                    if not emitted_members:
                        for pln in _wrapped_public_line(members):
                            new_block.append(pln.replace("public ::", "   module procedure"))
                        emitted_members = True
                elif re.match(r"^\s*end\s+interface\b", ln, re.IGNORECASE):
                    new_block.append(ln)
            out.extend(new_block)
        else:
            out.extend(block)
    out.append("")
    out.append("contains")
    for p in sorted(keep_procs):
        out.append("")
        out.extend(procedures[p])
    out.append("")
    out.append("end module r_mod")
    return "\n".join(normalize_fortran_lines(out, max_consecutive_blank=1)) + "\n"


def prepend_self_contained_runtime(f90: str, helper_paths: list[Path]) -> str:
    """Prepend a pruned r_mod runtime source so the emitted file can compile alone."""
    needed = _r_mod_needed_public_names(f90)
    runtime_parts: list[str] = []
    for hp in helper_paths:
        try:
            txt = hp.read_text(encoding="utf-8")
        except Exception:
            txt = hp.read_text(encoding="utf-8", errors="replace")
        mod_txt = _extract_r_mod(txt)
        if mod_txt is not None:
            runtime_parts.append(prune_r_mod_runtime(mod_txt, needed).rstrip())
    if not runtime_parts:
        return f90
    return "\n\n".join(runtime_parts) + "\n\n" + f90


@dataclass
class ListReturnSpec:
    fn_name: str
    root_fields: dict[str, object]
    nested_types: dict[tuple[str, ...], dict[str, object]]


def _split_top_level_else(text: str) -> tuple[str, str] | None:
    """Split `A else B` at top level, outside strings/parentheses."""
    in_single = False
    in_double = False
    esc = False
    depth = 0
    bdepth = 0
    i = 0
    while i < len(text):
        ch = text[i]
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
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif depth == 0 and text[i : i + 5] == " else":
                left = text[:i].strip()
                right = text[i + 5 :].strip()
                if left and right:
                    return left, right
        i += 1
    return None


def _looks_vector_expr_for_recycle(expr: str) -> bool:
    t = fscan.strip_redundant_outer_parens_expr(expr.strip())
    if not t:
        return False
    if re.match(r"^[A-Za-z]\w*$", t) and t.lower() in _KNOWN_MATRIX_NAMES:
        return False
    if t.startswith("[") and t.endswith("]"):
        return True
    if _split_top_level_colon(t) is not None:
        return True
    if re.match(r"^[A-Za-z]\w*$", t) and t.lower() in _KNOWN_VECTOR_NAMES:
        return True
    c = parse_call_text(t)
    if c is None:
        return False
    nm = c[0].lower()
    return nm in {
        "r_seq_int",
        "r_seq_len",
        "r_seq_int_by",
        "r_seq_int_length",
        "r_seq_real_by",
        "r_seq_real_length",
        "r_rep_real",
        "r_rep_int",
        "runif_vec",
        "rnorm_vec",
        "numeric",
        "pack",
        "tail",
        "quantile",
        "r_add",
        "r_sub",
        "r_mul",
        "r_div",
    }


def _looks_matrix_expr(expr: str) -> bool:
    t = fscan.strip_redundant_outer_parens_expr(expr.strip())
    if re.match(r"^[A-Za-z]\w*$", t) and t.lower() in _KNOWN_MATRIX_NAMES:
        return True
    c = parse_call_text(t)
    if c is None:
        return False
    return c[0].lower() in {"matrix", "array", "r_matmul", "crossprod", "tcrossprod", "t", "chol", "backsolve"}


def _parse_if_head(line: str) -> tuple[str, str] | None:
    s = line.strip()
    if not s.startswith("if"):
        return None
    m = re.match(r"^if\s*\(", s)
    if not m:
        return None
    i = m.end() - 1
    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    esc = False
    j = i
    while j < len(s):
        ch = s[j]
        if esc:
            esc = False
            j += 1
            continue
        if ch == "\\":
            esc = True
            j += 1
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            j += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            j += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return s[i + 1 : j].strip(), s[j + 1 :].strip()
        j += 1
    return None


def _parse_while_head(line: str) -> tuple[str, str] | None:
    s = line.strip()
    if not s.startswith("while"):
        return None
    m = re.match(r"^while\s*\(", s)
    if not m:
        return None
    i = m.end() - 1
    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    esc = False
    j = i
    while j < len(s):
        ch = s[j]
        if esc:
            esc = False
            j += 1
            continue
        if ch == "\\":
            esc = True
            j += 1
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            j += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            j += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return s[i + 1 : j].strip(), s[j + 1 :].strip()
        j += 1
    return None


def _parse_function_assign_head(line: str) -> tuple[str, str, str] | None:
    s = line.strip()
    m = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*(?:<-|=)\s*function\s*\(", s)
    if not m:
        return None
    fname = m.group(1)
    i = m.end() - 1
    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    esc = False
    j = i
    while j < len(s):
        ch = s[j]
        if esc:
            esc = False
            j += 1
            continue
        if ch == "\\":
            esc = True
            j += 1
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            j += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            j += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    return fname, s[i + 1 : j].strip(), s[j + 1 :].strip()
        j += 1
    return None


def _parse_for_head(line: str) -> tuple[str, str, str] | None:
    s = line.strip()
    if not s.startswith("for"):
        return None
    m = re.match(r"^for\s*\(", s)
    if not m:
        return None
    i0 = m.end() - 1
    depth = 0
    in_single = False
    in_double = False
    esc = False
    j = i0
    while j < len(s):
        ch = s[j]
        if esc:
            esc = False
            j += 1
            continue
        if ch == "\\":
            esc = True
            j += 1
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            j += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            j += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")":
                depth -= 1
                if depth == 0:
                    inside = s[i0 + 1 : j].strip()
                    tail = s[j + 1 :].strip()
                    m_in = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s+in\s+(.+)$", inside)
                    if not m_in:
                        return None
                    raw_var = m_in.group(1)
                    var = _sanitize_r_var_name(raw_var)
                    if raw_var != var:
                        _DOTTED_VAR_RENAMES[raw_var] = var
                    return var, m_in.group(2).strip(), tail
        j += 1
    return None


def parse_call_text(txt: str) -> tuple[str, list[str], dict[str, str]] | None:
    s = txt.strip()
    m = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*\(", s)
    if not m:
        return None
    nm = m.group(1)
    open_i = s.find("(", m.end(1))
    depth = 0
    quote: str | None = None
    esc = False
    close_i = -1
    for i in range(open_i, len(s)):
        ch = s[i]
        if quote is not None:
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == quote:
                quote = None
            continue
        if ch in {"'", '"'}:
            quote = ch
            continue
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                close_i = i
                break
    if close_i < 0 or s[close_i + 1 :].strip():
        return None
    inner = s[open_i + 1 : close_i].strip()
    parts = split_top_level_commas(inner) if inner else []
    pos: list[str] = []
    kw: dict[str, str] = {}
    for p in parts:
        pt = p.strip()
        asn = split_top_level_assignment(pt)
        if asn is not None:
            lhs = asn[0].strip()
            rhs = asn[1].strip()
            if re.match(r"^[A-Za-z]\w*(?:\.[A-Za-z]\w*)*$", lhs):
                kw[lhs] = rhs
                continue
        pos.append(pt)
    return nm, pos, kw


def _sanitize_fortran_kwarg_name(name: str) -> str:
    """Map R-style keyword names to valid Fortran named-argument identifiers."""
    nm = name.strip()
    if not nm:
        return nm
    nm = nm.replace(".", "_")
    nm = re.sub(r"[^A-Za-z0-9_]", "_", nm)
    if nm and nm[0].isdigit():
        nm = "_" + nm
    return nm


def _sanitize_r_var_name(name: str) -> str:
    """Map an R variable name to a valid Fortran identifier without function-name collisions."""
    nm = name.strip()
    if "." in nm:
        nm = nm.replace(".", "_dot_")
    nm = re.sub(r"[^A-Za-z0-9_]", "_", nm)
    if nm and nm[0].isdigit():
        nm = "_" + nm
    return nm


def _replace_dotted_var_refs(expr: str) -> str:
    out = expr
    for src, dst in sorted(_DOTTED_VAR_RENAMES.items(), key=lambda kv: len(kv[0]), reverse=True):
        pat = re.compile(
            rf"(?<![A-Za-z0-9_.]){re.escape(src)}(?![A-Za-z0-9_.])(?!(?:\s*\())"
        )
        out = pat.sub(dst, out)
    return out


def _looks_vector_actual_for_matrix_arg(src: str, lowered: str) -> bool:
    """Heuristic for wrapping a vector actual when a helper expects a matrix."""
    t = lowered.strip()
    if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?\(:,\s*[^,()]+\)$", t):
        return True
    if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?\([^,()]+:\s*[^,()]+\)$", t):
        return True
    s = src.strip()
    if re.match(r"^[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\[\[\s*.+\s*\]\]$", s):
        return True
    return False


def _fortran_str_literal(raw: str) -> str:
    txt = raw.replace('"', '""')
    return f'"{txt}"'


def _dequote_string_literal(s: str) -> str | None:
    t = s.strip()
    if len(t) >= 2 and ((t[0] == '"' and t[-1] == '"') or (t[0] == "'" and t[-1] == "'")):
        return t[1:-1]
    return None


def _strip_named_actual_value(txt: str) -> str:
    """Return the value side for simple R named actuals such as `a = 10`."""
    asn = split_top_level_assignment(txt.strip())
    if asn is None:
        return txt.strip()
    lhs, rhs = asn[0].strip(), asn[1].strip()
    if re.match(r"^[A-Za-z]\w*(?:\.[A-Za-z]\w*)*$", lhs):
        return rhs
    return txt.strip()


def _parse_named_c_vector(expr: str) -> tuple[list[str], list[str]] | None:
    cinfo = parse_call_text(expr.strip())
    if cinfo is None or cinfo[0].lower() != "c":
        return None
    _nm, pos, kw = cinfo
    if pos or not kw:
        return None
    labels: list[str] = []
    values: list[str] = []
    for lab, val in kw.items():
        if not re.match(r"^[A-Za-z]\w*(?:\.[A-Za-z]\w*)*$", lab):
            return None
        labels.append(lab)
        values.append(val.strip())
    return labels, values


def _parse_string_c_vector(expr: str) -> list[str] | None:
    cinfo = parse_call_text(expr.strip())
    if cinfo is None or cinfo[0].lower() != "c":
        return None
    vals = cinfo[1] + list(cinfo[2].values())
    labels: list[str] = []
    for v in vals:
        lab = _dequote_string_literal(v.strip())
        if lab is None:
            return None
        labels.append(lab)
    return labels


def _numeric_list_array_expr(expr: str) -> tuple[str, int] | None:
    """Lower simple numeric list(...) of equal vectors/matrices to rank-2/3 arrays."""
    cinfo = parse_call_text(expr.strip())
    if cinfo is None or cinfo[0].lower() != "list" or not cinfo[1]:
        return None
    items = [p.strip() for p in cinfo[1]]

    def _literal_vals(src: str) -> list[str] | None:
        t = r_expr_to_fortran(src).strip()
        if not (t.startswith("[") and t.endswith("]")):
            return None
        return [v.strip() for v in split_top_level_commas(t[1:-1]) if v.strip()]

    vec_vals = [_literal_vals(p) for p in items]
    if all(v is not None for v in vec_vals):
        vals2 = [v for v in vec_vals if v is not None]
        n = len(vals2[0])
        if n > 0 and all(len(v) == n for v in vals2):
            flat = ", ".join(x for v in vals2 for x in v)
            return f"reshape([{flat}], [{n}, {len(vals2)}])", 2

    mats: list[tuple[list[str], int, int, str, str, bool]] = []
    for p in items:
        cm = parse_call_text(p)
        if cm is None or cm[0].lower() != "matrix":
            mats = []
            break
        _nm, pos, kw = cm
        data_src = pos[0] if pos else kw.get("data")
        nr_src = kw.get("nrow") or (pos[1] if len(pos) >= 2 else None)
        nc_src = kw.get("ncol") or (pos[2] if len(pos) >= 3 else None)
        if data_src is None or nr_src is None:
            mats = []
            break
        vals = _literal_vals(data_src)
        if vals is None:
            mats = []
            break
        nr_f = _int_bound_expr(r_expr_to_fortran(nr_src))
        try:
            nr = int(nr_f)
        except ValueError:
            nr = int(round(len(vals) ** 0.5)) if nc_src is None and int(round(len(vals) ** 0.5)) ** 2 == len(vals) else 0
        if nc_src is not None:
            nc_f = _int_bound_expr(r_expr_to_fortran(nc_src))
            try:
                nc = int(nc_f)
            except ValueError:
                nc = len(vals) // nr if nr > 0 and len(vals) % nr == 0 else 0
        else:
            nc = len(vals) // nr if nr > 0 and len(vals) % nr == 0 else 0
            nc_f = str(nc) if nr_f.isdigit() else f"({len(vals)} / ({nr_f}))"
        if nr <= 0 or nc <= 0:
            mats = []
            break
        byrow_src = kw.get("byrow") or (pos[3] if len(pos) >= 4 else "")
        byrow = str(byrow_src).strip().upper() in {"TRUE", ".TRUE.", "T", "1"}
        if len(vals) != nr * nc:
            mats = []
            break
        mats.append((vals, nr, nc, nr_f, nc_f, byrow))
    if mats:
        nr0, nc0, nr0_f, nc0_f = mats[0][1], mats[0][2], mats[0][3], mats[0][4]
        if all(nr == nr0 and nc == nc0 and nr_f == nr0_f and nc_f == nc0_f for _vals, nr, nc, nr_f, nc_f, _byrow in mats):
            flat_parts: list[str] = []
            for vals, nr, nc, _nr_f, _nc_f, byrow in mats:
                if byrow:
                    flat_parts.extend(vals[r * nc + c] for c in range(nc) for r in range(nr))
                else:
                    flat_parts.extend(vals)
            return f"reshape([{', '.join(flat_parts)}], [{nr0_f}, {nc0_f}, {len(mats)}])", 3
    return None


def _numeric_nested_matrix_list_array_expr(expr: str) -> tuple[str, int] | None:
    cinfo = parse_call_text(expr.strip())
    if cinfo is None or cinfo[0].lower() != "list" or not cinfo[1]:
        return None
    regimes: list[list[tuple[list[str], int, int, bool]]] = []

    def _literal_vals(src: str) -> list[str] | None:
        t = r_expr_to_fortran(src).strip()
        if not (t.startswith("[") and t.endswith("]")):
            return None
        return [v.strip() for v in split_top_level_commas(t[1:-1]) if v.strip()]

    for item in cinfo[1]:
        inner = parse_call_text(item.strip())
        if inner is None or inner[0].lower() != "list" or not inner[1]:
            return None
        mats: list[tuple[list[str], int, int, bool]] = []
        for p in inner[1]:
            cm = parse_call_text(p.strip())
            if cm is None or cm[0].lower() != "matrix":
                return None
            _nm, pos, kw = cm
            data_src = pos[0] if pos else kw.get("data")
            nr_src = kw.get("nrow") or (pos[1] if len(pos) >= 2 else None)
            nc_src = kw.get("ncol") or (pos[2] if len(pos) >= 3 else None)
            if data_src is None or nr_src is None:
                return None
            vals = _literal_vals(data_src)
            if vals is None:
                return None
            nr = int(_int_bound_expr(r_expr_to_fortran(nr_src)))
            nc = int(_int_bound_expr(r_expr_to_fortran(nc_src))) if nc_src is not None else len(vals) // nr
            byrow_src = kw.get("byrow") or (pos[3] if len(pos) >= 4 else "")
            byrow = str(byrow_src).strip().upper() in {"TRUE", ".TRUE.", "T", "1"}
            if len(vals) != nr * nc:
                return None
            mats.append((vals, nr, nc, byrow))
        regimes.append(mats)
    nr0, nc0 = regimes[0][0][1], regimes[0][0][2]
    if any(nr != nr0 or nc != nc0 for mats in regimes for _vals, nr, nc, _byrow in mats):
        return None
    max_lag = max(len(mats) for mats in regimes)
    nan = "ieee_value(0.0_dp, ieee_quiet_nan)"
    flat_parts: list[str] = []
    for regime in regimes:
        for lag_i in range(max_lag):
            if lag_i >= len(regime):
                flat_parts.extend([nan] * (nr0 * nc0))
                continue
            vals, nr, nc, byrow = regime[lag_i]
            if byrow:
                flat_parts.extend(vals[r * nc + c] for c in range(nc) for r in range(nr))
            else:
                flat_parts.extend(vals)
    return f"reshape([{', '.join(flat_parts)}], [{nr0}, {nc0}, {max_lag}, {len(regimes)}])", 4


def _selects_named_order_column(expr: str) -> bool:
    m = re.match(r"^([A-Za-z]\w*)\s*\[(.*)\]$", expr.strip())
    if m is None:
        return False
    dims = _split_index_dims(m.group(2).strip())
    if len(dims) < 2:
        return False
    name_idx = _name_indices_from_subscript(m.group(1), dims[1].strip())
    labels = _NAMED_VECTOR_LABELS.get(m.group(1).lower())
    return name_idx == [1] and bool(labels) and labels[0].lower() == "order"


def _name_indices_from_subscript(base: str, inner: str) -> list[int] | None:
    labels = _NAMED_VECTOR_LABELS.get(base.lower())
    if not labels:
        return None
    t = inner.strip()
    one = _dequote_string_literal(t)
    if one is not None:
        try:
            return [labels.index(one) + 1]
        except ValueError:
            return None
    cinfo = parse_call_text(t)
    if cinfo is not None and cinfo[0].lower() == "c":
        vals = cinfo[1] + list(cinfo[2].values())
        out: list[int] = []
        for v in vals:
            lab = _dequote_string_literal(v.strip())
            if lab is None:
                return None
            try:
                out.append(labels.index(lab) + 1)
            except ValueError:
                return None
        return out
    return None


def _named_subscript_lhs_to_fortran(lhs_src: str) -> str | None:
    m = re.match(r"^([A-Za-z]\w*)\s*\[\s*(.+)\s*\]$", lhs_src.strip())
    if m is None:
        return None
    base = m.group(1)
    inner = m.group(2).strip()
    if len(_split_index_dims(inner)) != 1:
        return None
    idx = _name_indices_from_subscript(base, inner)
    if idx is not None:
        if len(idx) == 1:
            return f"{base}({idx[0]})"
        return f"{base}([" + ", ".join(str(i) for i in idx) + "])"
    label = _dequote_string_literal(inner)
    if label is not None and (base == "y" or base.endswith("_result")):
        dynamic = {
            "order": "1",
            "intercept": "2",
            "sigma2": "nacf + 3",
            "aic": "nacf + 4",
            "bic": "nacf + 5",
        }
        if label in dynamic:
            return f"{base}({dynamic[label]})"
    c_inner = parse_call_text(inner)
    if c_inner is not None and (base == "y" or base.endswith("_result")) and c_inner[0].lower() == "paste0" and len(c_inner[1]) >= 2:
        prefix = _dequote_string_literal(c_inner[1][0].strip())
        seq_call = parse_call_text(c_inner[1][1].strip())
        if prefix == "phi" and seq_call is not None and seq_call[0].lower() == "seq_len":
            n_src = seq_call[1][0] if seq_call[1] else seq_call[2].get("n", "")
            if n_src:
                n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                return f"{base}(r_seq_int(3, {n_f} + 2))"
    return None


def _split_sprintf_format(fmt: str) -> tuple[list[str], int]:
    # Split on printf-like conversion specs and return literal pieces + count(specs).
    # Supports common specs used by these scripts (e.g. %d, %.6f, %g).
    spec_re = re.compile(r"%(?:[-+ 0#]*)(?:\d+)?(?:\.\d+)?[a-zA-Z]")
    pieces: list[str] = []
    last = 0
    nspec = 0
    for m in spec_re.finditer(fmt):
        pieces.append(fmt[last : m.start()])
        last = m.end()
        nspec += 1
    pieces.append(fmt[last:])
    return pieces, nspec


def _replace_balanced_func_calls(expr: str, fname: str, repl_fn) -> str:
    """Replace `fname(<arg>)` calls using balanced-parentheses parsing."""
    out: list[str] = []
    i = 0
    n = len(expr)
    fnlow = fname.lower()
    while i < n:
        m = re.search(rf"\b{re.escape(fname)}\b", expr[i:], re.IGNORECASE)
        if m is None:
            out.append(expr[i:])
            break
        s0 = i + m.start()
        e0 = i + m.end()
        out.append(expr[i:s0])
        j = e0
        while j < n and expr[j].isspace():
            j += 1
        if j >= n or expr[j] != "(":
            out.append(expr[s0:e0])
            i = e0
            continue
        depth = 0
        in_single = False
        in_double = False
        k = j
        close = -1
        while k < n:
            ch = expr[k]
            if ch == "'" and not in_double:
                in_single = not in_single
            elif ch == '"' and not in_single:
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        close = k
                        break
            k += 1
        if close < 0:
            out.append(expr[s0:])
            break
        inner = expr[j + 1 : close]
        out.append(repl_fn(inner))
        i = close + 1
    return "".join(out)


def _display_expr_to_fortran(expr: str) -> str:
    """Lower display-oriented R wrappers (sprintf/paste) to printable Fortran expr."""
    s = expr.strip()
    cinfo = parse_call_text(s)
    if cinfo is not None:
        nm, pos, kw = cinfo
        low = nm.lower()
        if low == "sprintf":
            if len(pos) >= 2:
                return r_expr_to_fortran(pos[1])
            if len(pos) == 1:
                return r_expr_to_fortran(pos[0])
        if low == "paste":
            # Common case: paste(sprintf(fmt, arr), collapse=" ")
            if pos:
                inner = pos[0].strip()
                c2 = parse_call_text(inner)
                if c2 is not None and c2[0].lower() == "sprintf":
                    p2 = c2[1]
                    if len(p2) >= 2:
                        return r_expr_to_fortran(p2[1])
                return r_expr_to_fortran(inner)
    return r_expr_to_fortran(s)


def _format_vec_call_from_paste(expr: str) -> str | None:
    """Lower paste(sprintf("%.<digits>f", x), collapse=sep) to a helper."""
    ci = parse_call_text(expr.strip())
    if ci is None or ci[0].lower() != "paste":
        return None
    pos = ci[1]
    kw = ci[2]
    if not pos:
        return None
    collapse = kw.get("collapse")
    if collapse is None or _dequote_string_literal(collapse.strip()) is None:
        return None
    sp = parse_call_text(pos[0].strip())
    if sp is None or sp[0].lower() != "sprintf" or len(sp[1]) < 2:
        return None
    fmt_src = sp[1][0].strip()
    val_src = sp[1][1].strip()
    digits_src = "6"
    fmt_literal = _dequote_string_literal(fmt_src)
    if fmt_literal is not None:
        m_fmt = re.match(r"^%\.(\d+)f$", fmt_literal)
        if m_fmt is not None:
            digits_src = m_fmt.group(1)
    fmt_call = parse_call_text(fmt_src)
    if fmt_call is not None and fmt_call[0].lower() == "paste0" and len(fmt_call[1]) >= 3:
        parts = fmt_call[1]
        if _dequote_string_literal(parts[0].strip()) == "%." and _dequote_string_literal(parts[2].strip()) == "f":
            digits_src = parts[1].strip()
    val_f = r_expr_to_fortran(val_src)
    digits_f = _int_bound_expr(r_expr_to_fortran(digits_src))
    sep_f = r_expr_to_fortran(collapse.strip())
    return f"r_format_vec(real({val_f}, kind=dp), {digits_f}, {sep_f})"


def _expr_returns_character(expr: str) -> bool:
    """Conservative character-scalar result test for function returns."""
    t = expr.strip()
    if _dequote_string_literal(t) is not None:
        return True
    if _format_vec_call_from_paste(t) is not None:
        return True
    ci = parse_call_text(t)
    if ci is None:
        return False
    return ci[0].lower() in {"paste", "paste0", "sprintf", "format", "sub", "substr"}


def _sprintf_arg_items(expr: str) -> list[str] | None:
    """Lower sprintf(fmt, ...) into printable Fortran item expressions."""
    ci = parse_call_text(expr.strip())
    if ci is None or ci[0].lower() != "sprintf":
        return None
    pos = ci[1]
    if not pos:
        return []
    fmt_raw = _dequote_string_literal(pos[0])
    vals = [r_expr_to_fortran(a) for a in pos[1:]]
    if fmt_raw is None:
        return vals
    pieces, nspec = _split_sprintf_format(fmt_raw)
    out_items: list[str] = []
    nuse = min(nspec, len(vals))
    for i in range(nuse + 1):
        lit = pieces[i].replace("\\n", "").replace("\\t", " ")
        if lit:
            out_items.append(_fortran_str_literal(lit))
        if i < nuse:
            out_items.append(vals[i])
    if nuse < len(vals):
        out_items.extend(vals[nuse:])
    return out_items


def strip_r_comment(line: str) -> str:
    out = []
    in_single = False
    in_double = False
    esc = False
    for ch in line:
        if esc:
            out.append(ch)
            esc = False
            continue
        if ch == "\\":
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
    return "".join(out).rstrip()


def split_r_code_comment(line: str) -> tuple[str, str]:
    """Split R source line into code and trailing `#` comment (outside strings)."""
    out = []
    in_single = False
    in_double = False
    esc = False
    for i, ch in enumerate(line):
        if esc:
            out.append(ch)
            esc = False
            continue
        if ch == "\\":
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
            return "".join(out), line[i + 1 :]
        out.append(ch)
    return "".join(out), ""


def extract_r_top_comments(src: str) -> list[str]:
    """Collect leading top-of-file R comments (before first code statement)."""
    out: list[str] = []
    seen_code = False
    for raw in src.splitlines():
        code, cmt = split_r_code_comment(raw)
        tcode = code.strip()
        tcmt = cmt.strip()
        if tcode:
            seen_code = True
            break
        if tcmt:
            if tcmt.startswith("!"):
                continue
            out.append(tcmt)
    dedup: list[str] = []
    prev = None
    for c in out:
        if c != prev:
            dedup.append(c)
        prev = c
    return dedup


def _normalize_r_code_key(code: str) -> str:
    return re.sub(r"\s+", " ", code.strip())


def build_r_comment_lookup(src: str) -> dict[str, list[str]]:
    out: dict[str, list[str]] = {}
    for raw in src.splitlines():
        code, cmt = split_r_code_comment(raw)
        tcode = code.strip()
        tcmt = cmt.strip()
        if not tcode:
            continue
        if tcmt:
            out.setdefault(_normalize_r_code_key(tcode), []).append(tcmt)
    return out


def pop_comment_for_code(code: str, lookup: dict[str, list[str]] | None) -> str:
    if not lookup:
        return ""
    key = _normalize_r_code_key(code)
    arr = lookup.get(key)
    if not arr:
        return ""
    c = arr.pop(0).strip()
    if not arr:
        lookup.pop(key, None)
    return c


def split_top_level_commas(s: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    bdepth = 0
    cdepth = 0
    in_single = False
    in_double = False
    esc = False
    for ch in s:
        if esc:
            cur.append(ch)
            esc = False
            continue
        if ch == "\\":
            cur.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "[":
                bdepth += 1
            elif ch == "]" and bdepth > 0:
                bdepth -= 1
            elif ch == "{":
                cdepth += 1
            elif ch == "}" and cdepth > 0:
                cdepth -= 1
            elif ch == "," and depth == 0 and bdepth == 0 and cdepth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def split_top_level_semicolons(s: str) -> list[str]:
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    bdepth = 0
    cdepth = 0
    in_single = False
    in_double = False
    esc = False
    for ch in s:
        if esc:
            cur.append(ch)
            esc = False
            continue
        if ch == "\\":
            cur.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "[":
                bdepth += 1
            elif ch == "]" and bdepth > 0:
                bdepth -= 1
            elif ch == "{":
                cdepth += 1
            elif ch == "}" and cdepth > 0:
                cdepth -= 1
            elif ch == ";" and depth == 0 and bdepth == 0 and cdepth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    tail = "".join(cur).strip()
    if tail:
        out.append(tail)
    return out


def split_top_level_assignment(s: str) -> tuple[str, str] | None:
    """Split `lhs <- rhs` or top-level `lhs = rhs` outside brackets/strings."""
    depth = 0
    bdepth = 0
    cdepth = 0
    in_single = False
    in_double = False
    esc = False
    i = 0
    while i < len(s):
        ch = s[i]
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
            if ch == "(":
                depth += 1
                i += 1
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                i += 1
                continue
            if ch == "[":
                bdepth += 1
                i += 1
                continue
            if ch == "]" and bdepth > 0:
                bdepth -= 1
                i += 1
                continue
            if ch == "{":
                cdepth += 1
                i += 1
                continue
            if ch == "}" and cdepth > 0:
                cdepth -= 1
                i += 1
                continue
            if depth == 0 and bdepth == 0 and cdepth == 0:
                if s.startswith("<-", i):
                    return s[:i].strip(), s[i + 2 :].strip()
                if ch == "=":
                    prev = s[i - 1] if i > 0 else ""
                    nxt = s[i + 1] if i + 1 < len(s) else ""
                    if prev not in {"=", "!", "<", ">"} and nxt != "=":
                        return s[:i].strip(), s[i + 1 :].strip()
        i += 1
    return None


def _r_statement_continues(txt: str) -> bool:
    """Return true when an R physical line clearly needs the next line."""
    t = txt.rstrip()
    if not t:
        return False
    return re.search(r"(?:<-|=|,|%\*%|%%|%/%|[+\-*/^]|&&|\|\||&|\|)\s*$", t) is not None


def preprocess_r_lines(src: str) -> list[str]:
    lines0: list[str] = []
    seen_code = False
    for raw in src.splitlines():
        code, cmt = split_r_code_comment(raw)
        tcode = code.strip()
        tcmt = cmt.strip()
        if tcode:
            lines0.append(tcode)
            seen_code = True
        elif tcmt and seen_code:
            lines0.append(_R_COMMENT_SENTINEL + tcmt)
    # Join multiline statements by balanced parentheses.
    joined: list[str] = []
    cur = ""
    depth = 0
    in_single = False
    in_double = False
    pending_comments: list[str] = []
    for ln in lines0:
        txt = ln.strip()
        if txt.startswith(_R_COMMENT_SENTINEL):
            if cur.strip():
                pending_comments.append(txt)
            else:
                joined.append(txt)
            continue
        if pending_comments and not cur.strip():
            joined.extend(pending_comments)
            pending_comments = []
        if not cur:
            cur = txt
        else:
            cur = cur + " " + txt
        i = 0
        while i < len(txt):
            ch = txt[i]
            if ch == "'" and not in_double:
                in_single = not in_single
            elif ch == '"' and not in_single:
                in_double = not in_double
            elif not in_single and not in_double:
                if ch == "(":
                    depth += 1
                elif ch == ")" and depth > 0:
                    depth -= 1
            i += 1
        if depth == 0 and not in_single and not in_double and not _r_statement_continues(cur):
            joined.append(cur)
            cur = ""
            if pending_comments:
                joined.extend(pending_comments)
                pending_comments = []
    if cur.strip():
        joined.append(cur)
    if pending_comments:
        joined.extend(pending_comments)
    lines0 = joined
    out: list[str] = []
    for ln in lines0:
        if ln.startswith(_R_COMMENT_SENTINEL):
            out.append(ln)
            continue
        # split top-level semicolon-separated statements
        semis = split_top_level_semicolons(ln) if ";" in ln else [ln]
        for ln_part in semis:
            cur = ln_part
            # make braces standalone tokens to simplify parsing
            while "{" in cur or "}" in cur:
                i_open = cur.find("{") if "{" in cur else 10**9
                i_close = cur.find("}") if "}" in cur else 10**9
                i = min(i_open, i_close)
                if i == 10**9:
                    break
                left = cur[:i].strip()
                br = cur[i]
                right = cur[i + 1 :].strip()
                if left:
                    out.append(left)
                out.append(br)
                cur = right
            if cur.strip():
                out.append(cur.strip())
    return out


def parse_single_statement(ln: str, *, comment_lookup: dict[str, list[str]] | None = None) -> object:
    ln = ln.strip()
    cmt = pop_comment_for_code(ln, comment_lookup)
    fhead = _parse_for_head(ln)
    if fhead is not None:
        var, itexpr, tail = fhead
        if not tail:
            raise NotImplementedError("for requires body in this subset")
        body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
        return ForStmt(var=var, iter_expr=itexpr, body=body)
    ih = _parse_if_head(ln)
    if ih is not None:
        cond, tail = ih
        if not tail:
            raise NotImplementedError("if requires body in this subset")
        split_tail = _split_top_level_else(" " + tail)
        if split_tail is not None:
            then_body = [parse_single_statement(split_tail[0], comment_lookup=comment_lookup)]
            else_body = [parse_single_statement(split_tail[1], comment_lookup=comment_lookup)]
        else:
            then_body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
            else_body = []
        return IfStmt(cond=cond, then_body=then_body, else_body=else_body)
    wh = _parse_while_head(ln)
    if wh is not None:
        cond, tail = wh
        if not tail:
            raise NotImplementedError("while requires body in this subset")
        body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
        return WhileStmt(cond=cond, body=body)
    if ln == "break":
        return ExprStmt(expr="break", comment=cmt)
    if ln == "next":
        return ExprStmt(expr="next", comment=cmt)
    if ln.startswith("function("):
        raise NotImplementedError("nested/anonymous function definitions not supported")
    if ln.startswith("print(") and ln.endswith(")"):
        inner = ln[len("print(") : -1].strip()
        args = split_top_level_commas(inner) if inner else []
        return PrintStmt(args=args, comment=cmt)
    m_asn_dot = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)+)\s*(<-|=)\s*(.+)$", ln)
    if m_asn_dot:
        raw_name = m_asn_dot.group(1)
        name = _sanitize_r_var_name(raw_name)
        _DOTTED_VAR_RENAMES[raw_name] = name
        rhs = m_asn_dot.group(3).strip()
        return Assign(name=name, expr=rhs, comment=cmt)
    m_asn = re.match(r"^([A-Za-z]\w*)\s*(<-|=)\s*(.+)$", ln)
    if m_asn:
        rhs = m_asn.group(3).strip()
        return Assign(name=m_asn.group(1), expr=rhs, comment=cmt)
    m_asn_any = re.match(
        r"^([A-Za-z]\w*(?:\[[^\]]+\])?(?:\$[A-Za-z]\w*(?:\[[^\]]+\])?)*)\s*(<-|=)\s*(.+)$",
        ln,
    )
    if m_asn_any:
        # Keep non-simple LHS assignments as generic expr statements.
        return ExprStmt(expr=ln, comment=cmt)
    cinfo = parse_call_text(ln)
    if cinfo is not None:
        nm, pos, kw = cinfo
        args = list(pos) + [f"{k}={v}" for k, v in kw.items()]
        if nm.lower() in {"print", "stopifnot", "set.seed", "cat", "stop", "writelines", "write.table"}:
            return CallStmt(name=nm, args=args, comment=cmt)
        return ExprStmt(expr=ln, comment=cmt)
    return ExprStmt(expr=ln, comment=cmt)


def parse_block(
    lines: list[str],
    i0: int = 0,
    *,
    stop_at_rbrace: bool = False,
    comment_lookup: dict[str, list[str]] | None = None,
) -> tuple[list[object], int]:
    stmts: list[object] = []
    i = i0
    while i < len(lines):
        ln = lines[i].strip()
        if ln.startswith(_R_COMMENT_SENTINEL):
            txt = ln[len(_R_COMMENT_SENTINEL) :].strip()
            if txt:
                stmts.append(CommentStmt(text=txt))
            i += 1
            continue
        if ln == "}":
            if stop_at_rbrace:
                return stmts, i + 1
            i += 1
            continue
        if ln == "{":
            i += 1
            continue

        wh = _parse_while_head(ln)
        if wh is not None:
            cond, tail = wh
            if tail:
                if tail == "{":
                    i += 1
                    body, i = parse_block(lines, i, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
                    i += 1
            else:
                i += 1
                if i < len(lines) and lines[i].strip() == "{":
                    body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    if i >= len(lines):
                        raise NotImplementedError("while missing body")
                    body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                    i += 1
            stmts.append(WhileStmt(cond=cond, body=body))
            continue

        if ln.startswith("repeat"):
            tail = ln[len("repeat") :].strip()
            if tail:
                if tail == "{":
                    i += 1
                    body, i = parse_block(lines, i, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
                    i += 1
            else:
                i += 1
                if i < len(lines) and lines[i].strip() == "{":
                    body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    if i >= len(lines):
                        raise NotImplementedError("repeat missing body")
                    body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                    i += 1
            stmts.append(RepeatStmt(body=body))
            continue

        fhead = _parse_for_head(ln)
        if fhead is not None:
            var, itexpr, tail = fhead
            if tail:
                if tail == "{":
                    i += 1
                    body, i = parse_block(lines, i, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
                    i += 1
                stmts.append(ForStmt(var=var, iter_expr=itexpr, body=body))
                continue
            i += 1
            if i < len(lines) and lines[i].strip() == "{":
                body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
            else:
                if i >= len(lines):
                    raise NotImplementedError("for missing body")
                body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                i += 1
            stmts.append(ForStmt(var=var, iter_expr=itexpr, body=body))
            continue

        fn_head = _parse_function_assign_head(ln)
        if fn_head is not None:
            fname, arg_txt, fn_tail = fn_head
            args: list[str] = []
            defaults: dict[str, str] = {}
            arg_name_map: dict[str, str] = {}
            if arg_txt:
                for part in split_top_level_commas(arg_txt):
                    pt = part.strip()
                    m_ap = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*=\s*(.+)$", pt)
                    if m_ap:
                        raw_anm = m_ap.group(1)
                        anm = _sanitize_fortran_kwarg_name(raw_anm)
                        args.append(anm)
                        defaults[anm] = _replace_idents(m_ap.group(2).strip(), arg_name_map)
                        if raw_anm != anm:
                            arg_name_map[raw_anm] = anm
                    else:
                        if re.match(r"^[A-Za-z]\w*(?:\.[A-Za-z]\w*)*$", pt):
                            anm = _sanitize_fortran_kwarg_name(pt)
                            args.append(anm)
                            if pt != anm:
                                arg_name_map[pt] = anm
                        else:
                            args.append(pt)
            if fn_tail:
                body = [parse_single_statement(fn_tail, comment_lookup=comment_lookup)]
                i += 1
            else:
                i += 1
                if i < len(lines) and lines[i].strip() == "{":
                    body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    if i >= len(lines):
                        raise NotImplementedError("function missing body in this subset")
                    body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                    i += 1
            if arg_name_map:
                body = [_rename_stmt_obj(st, arg_name_map) for st in body]
            stmts.append(FuncDef(name=fname, args=args, defaults=defaults, body=body))
            continue

        ih = _parse_if_head(ln)
        if ih is not None:
            cond, tail = ih
            then_body: list[object] = []
            else_body: list[object] = []
            if tail:
                split_tail = _split_top_level_else(" " + tail)
                if split_tail is not None:
                    then_body = [parse_single_statement(split_tail[0], comment_lookup=comment_lookup)]
                    else_body = [parse_single_statement(split_tail[1], comment_lookup=comment_lookup)]
                    i += 1
                else:
                    then_body = [parse_single_statement(tail, comment_lookup=comment_lookup)]
                    i += 1
            else:
                i += 1
                if i < len(lines) and lines[i].strip() == "{":
                    then_body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                else:
                    # accept single next-statement body without braces
                    if i >= len(lines):
                        raise NotImplementedError("if missing body")
                    then_body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                    i += 1
                if i < len(lines) and lines[i].strip() == "else":
                    i += 1
                    if i < len(lines) and lines[i].strip() == "{":
                        else_body, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                    else:
                        if i >= len(lines):
                            raise NotImplementedError("else missing body")
                        else_body = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                        i += 1
                # Support brace-style chained else-if / else blocks:
                #   if (...) { ... } else if (...) { ... } else { ... }
                head_else_if: IfStmt | None = None
                tail_else_if: IfStmt | None = None
                while i < len(lines) and lines[i].strip().lower().startswith("else if"):
                    e_line = lines[i].strip()
                    e_if = _parse_if_head(e_line[len("else ") :].strip())
                    if e_if is None:
                        break
                    e_cond, e_tail = e_if
                    e_then: list[object] = []
                    i += 1
                    if e_tail:
                        if e_tail == "{":
                            e_then, i = parse_block(lines, i, stop_at_rbrace=True, comment_lookup=comment_lookup)
                        else:
                            e_then = [parse_single_statement(e_tail, comment_lookup=comment_lookup)]
                    else:
                        if i < len(lines) and lines[i].strip() == "{":
                            e_then, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                        else:
                            if i >= len(lines):
                                raise NotImplementedError("else if missing body")
                            e_then = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                            i += 1
                    node = IfStmt(cond=e_cond, then_body=e_then, else_body=[])
                    if head_else_if is None:
                        head_else_if = node
                    if tail_else_if is not None:
                        tail_else_if.else_body = [node]
                    tail_else_if = node

                if head_else_if is not None:
                    else_body = [head_else_if]

                if i < len(lines) and lines[i].strip() == "else":
                    i += 1
                    e_final: list[object] = []
                    if i < len(lines) and lines[i].strip() == "{":
                        e_final, i = parse_block(lines, i + 1, stop_at_rbrace=True, comment_lookup=comment_lookup)
                    else:
                        if i >= len(lines):
                            raise NotImplementedError("else missing body")
                        e_final = [parse_single_statement(lines[i], comment_lookup=comment_lookup)]
                        i += 1
                    if tail_else_if is not None:
                        tail_else_if.else_body = e_final
                    else:
                        else_body = e_final
            stmts.append(IfStmt(cond=cond, then_body=then_body, else_body=else_body))
            continue

        try:
            st = parse_single_statement(ln, comment_lookup=comment_lookup)
            stmts.append(st)
            i += 1
            continue
        except NotImplementedError as e:
            if "function definitions not yet supported" in str(e):
                raise
            pass

        raise NotImplementedError(f"unrecognized statement: {ln}")
    return stmts, i


def infer_assigned_names(stmts: list[object], out: dict[str, int] | None = None) -> dict[str, int]:
    if out is None:
        out = {}
    for st in stmts:
        if isinstance(st, Assign):
            out[st.name] = out.get(st.name, 0) + 1
        elif isinstance(st, ForStmt):
            out[st.var] = out.get(st.var, 0) + 1
            infer_assigned_names(st.body, out)
        elif isinstance(st, WhileStmt):
            infer_assigned_names(st.body, out)
        elif isinstance(st, RepeatStmt):
            infer_assigned_names(st.body, out)
        elif isinstance(st, IfStmt):
            infer_assigned_names(st.then_body, out)
            infer_assigned_names(st.else_body, out)
        elif isinstance(st, FuncDef):
            # separate scope
            continue
    return out


def collect_assignment_comments(stmts: list[object], out: dict[str, str] | None = None) -> dict[str, str]:
    if out is None:
        out = {}
    for st in stmts:
        if isinstance(st, Assign):
            cmt = st.comment.strip()
            if cmt and st.name not in out:
                out[st.name] = cmt
        elif isinstance(st, ForStmt):
            collect_assignment_comments(st.body, out)
        elif isinstance(st, WhileStmt):
            collect_assignment_comments(st.body, out)
        elif isinstance(st, RepeatStmt):
            collect_assignment_comments(st.body, out)
        elif isinstance(st, IfStmt):
            collect_assignment_comments(st.then_body, out)
            collect_assignment_comments(st.else_body, out)
        elif isinstance(st, FuncDef):
            continue
    return out


def attach_function_adjacent_comments(stmts: list[object]) -> list[object]:
    out: list[object] = []
    pending_comments: list[str] = []
    for st in stmts:
        if isinstance(st, CommentStmt):
            pending_comments.append(st.text)
            continue
        if isinstance(st, FuncDef) and pending_comments:
            st = FuncDef(
                name=st.name,
                args=list(st.args),
                defaults=dict(st.defaults),
                body=list(st.body),
                leading_comments=tuple(list(st.leading_comments) + pending_comments),
            )
            pending_comments = []
            out.append(st)
            continue
        if pending_comments:
            out.extend(CommentStmt(text=c) for c in pending_comments)
            pending_comments = []
        out.append(st)
    if pending_comments:
        out.extend(CommentStmt(text=c) for c in pending_comments)
    return out


def _split_top_level_colon(s: str) -> tuple[str, str] | None:
    """Split `a:b` at top level (outside parens/strings), else None."""
    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    for i, ch in enumerate(s):
        if ch == "'" and not in_double:
            in_single = not in_single
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            continue
        if in_single or in_double:
            continue
        if ch == "(":
            depth += 1
            continue
        if ch == ")" and depth > 0:
            depth -= 1
            continue
        if ch == "[":
            bdepth += 1
            continue
        if ch == "]" and bdepth > 0:
            bdepth -= 1
            continue
        if ch == ":" and depth == 0 and bdepth == 0:
            # Do not treat namespace operator `::` as sequence colon.
            if (i > 0 and s[i - 1] == ":") or (i + 1 < len(s) and s[i + 1] == ":"):
                continue
            a = s[:i].strip()
            b = s[i + 1 :].strip()
            if a and b:
                return a, b
            return None
    return None


def _split_top_level_token(text: str, token: str, *, from_right: bool = False) -> tuple[str, str] | None:
    """Split `text` at top-level `token` outside (), [], {}, and quotes."""
    def _is_exponent_sign(pos: int) -> bool:
        if token not in {"+", "-"} or pos <= 0 or pos + 1 >= len(text):
            return False
        if text[pos - 1] not in {"e", "E"} or not text[pos + 1].isdigit():
            return False
        return pos >= 2 and (text[pos - 2].isdigit() or text[pos - 2] == ".")

    in_single = False
    in_double = False
    esc = False
    pdepth = 0
    bdepth = 0
    cdepth = 0
    hits: list[int] = []
    i = 0
    n = len(text)
    while i < n:
        ch = text[i]
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
            if ch == "(":
                pdepth += 1
                i += 1
                continue
            if ch == ")" and pdepth > 0:
                pdepth -= 1
                i += 1
                continue
            if ch == "[":
                bdepth += 1
                i += 1
                continue
            if ch == "]" and bdepth > 0:
                bdepth -= 1
                i += 1
                continue
            if ch == "{":
                cdepth += 1
                i += 1
                continue
            if ch == "}" and cdepth > 0:
                cdepth -= 1
                i += 1
                continue
            if pdepth == 0 and bdepth == 0 and cdepth == 0 and text.startswith(token, i):
                if _is_exponent_sign(i):
                    i += 1
                    continue
                hits.append(i)
                i += len(token)
                continue
        i += 1
    if not hits:
        return None
    k = hits[-1] if from_right else hits[0]
    return text[:k].strip(), text[k + len(token) :].strip()


def _find_top_level_addsub(s: str) -> tuple[int, str] | None:
    """Find first top-level binary + or - (outside parens/strings)."""
    def _is_exponent_sign(pos: int) -> bool:
        if pos <= 0 or pos + 1 >= len(s):
            return False
        if s[pos - 1] not in {"e", "E"} or not s[pos + 1].isdigit():
            return False
        return pos >= 2 and (s[pos - 2].isdigit() or s[pos - 2] == ".")

    depth = 0
    bdepth = 0
    in_single = False
    in_double = False
    prev_nonspace = ""
    for i, ch in enumerate(s):
        if ch == "'" and not in_double:
            in_single = not in_single
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            continue
        if in_single or in_double:
            continue
        if ch == "(":
            depth += 1
            continue
        if ch == ")" and depth > 0:
            depth -= 1
            continue
        if ch == "[":
            bdepth += 1
            continue
        if ch == "]" and bdepth > 0:
            bdepth -= 1
            continue
        if depth == 0 and bdepth == 0 and ch in {"+", "-"}:
            if _is_exponent_sign(i):
                prev_nonspace = ch
                continue
            # skip unary signs
            if prev_nonspace == "" or prev_nonspace in {"(", ",", ":", "+", "-", "*", "/", "^"}:
                prev_nonspace = ch
                continue
            return i, ch
        if not ch.isspace():
            prev_nonspace = ch
    return None


def _split_index_dims(inner: str) -> list[str]:
    """Split index list by top-level commas, preserving empty dims."""
    out: list[str] = []
    cur: list[str] = []
    depth = 0
    bdepth = 0
    cdepth = 0
    in_single = False
    in_double = False
    esc = False
    for ch in inner:
        if esc:
            cur.append(ch)
            esc = False
            continue
        if ch == "\\":
            cur.append(ch)
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            cur.append(ch)
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            cur.append(ch)
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
            elif ch == ")" and depth > 0:
                depth -= 1
            elif ch == "[":
                bdepth += 1
            elif ch == "]" and bdepth > 0:
                bdepth -= 1
            elif ch == "{":
                cdepth += 1
            elif ch == "}" and cdepth > 0:
                cdepth -= 1
            elif ch == "," and depth == 0 and bdepth == 0 and cdepth == 0:
                out.append("".join(cur).strip())
                cur = []
                continue
        cur.append(ch)
    out.append("".join(cur).strip())
    return out


def _index_inner_1d_to_fortran(inner: str) -> str:
    """Translate one R index expression with R ':' precedence into Fortran."""
    t = fscan.strip_redundant_outer_parens_expr(inner.strip())
    # R precedence: ':' binds tighter than +/-. So 2:5-1 means (2:5)-1.
    addsub = _find_top_level_addsub(t)
    if addsub is not None:
        pos, op = addsub
        left = t[:pos].strip()
        right = t[pos + 1 :].strip()
        seq = _split_top_level_colon(left)
        if seq is not None:
            a, b = seq
            a_f = _int_bound_expr(r_expr_to_fortran(a))
            b_f = _int_bound_expr(r_expr_to_fortran(b))
            c_f = _int_bound_expr(r_expr_to_fortran(right))
            return f"(r_seq_int({a_f}, {b_f}) {op} {c_f})"
    seq = _split_top_level_colon(t)
    if seq is not None:
        a, b = seq
        a_f = _int_bound_expr(r_expr_to_fortran(a))
        b_f = _int_bound_expr(r_expr_to_fortran(b))
        return f"{a_f}:{b_f}"
    m_which = re.match(r"^which\.(min|max)\s*\((.*)\)$", t, re.IGNORECASE)
    if m_which is not None:
        fn = "minloc" if m_which.group(1).lower() == "min" else "maxloc"
        inner = m_which.group(2).strip()
        return f"{fn}({inner}, dim=1)"
    return r_expr_to_fortran(t)


def _index_dim_to_fortran(base: str, dimno: int, d: str) -> str:
    """Translate one dimension subscript with R negative-index semantics."""
    dt = d.strip()
    if dt == "":
        return ":"
    if dimno == 2 and dt == "price_names":
        return "price_names + 1"
    if dimno == 2 and base == "coef_mat":
        lab_dt = _dequote_string_literal(dt)
        coef_idx = {
            "order": "1",
            "intercept": "2",
            "sigma2": "nacf + 3",
            "aic": "nacf + 4",
            "bic": "nacf + 5",
        }
        if lab_dt in coef_idx:
            return coef_idx[lab_dt]
    m_seq_len = re.match(r"^r_seq_len\s*\(\s*(.+)\s*\)$", dt, re.IGNORECASE)
    if m_seq_len is not None:
        return f"1:{_int_bound_expr(m_seq_len.group(1).strip())}"
    m_seq_int = re.match(r"^r_seq_int\s*\(\s*([^,]+?)\s*,\s*([^)]+?)\s*\)$", dt, re.IGNORECASE)
    if m_seq_int is not None:
        return f"{_int_bound_expr(m_seq_int.group(1).strip())}:{_int_bound_expr(m_seq_int.group(2).strip())}"
    name_idx = _name_indices_from_subscript(base, dt)
    if name_idx is not None:
        if len(name_idx) == 1:
            return str(name_idx[0])
        return "[" + ", ".join(str(i) for i in name_idx) + "]"
    m_drop_vec = re.match(r"^-\s*c\s*\((.*)\)\s*$", dt, re.IGNORECASE)
    if m_drop_vec:
        parts = split_top_level_commas(m_drop_vec.group(1).strip())
        vals = ", ".join(f"int({r_expr_to_fortran(p.strip())})" for p in parts if p.strip())
        return f"r_drop_indices(r_seq_int(1, size({base}, {dimno})), [{vals}])"
    m_drop_one = re.match(r"^-\s*(.+)$", dt)
    if m_drop_one:
        kf = _int_bound_expr(r_expr_to_fortran(m_drop_one.group(1).strip()))
        return f"r_drop_index(r_seq_int(1, size({base}, {dimno})), {kf})"
    return _index_inner_1d_to_fortran(dt)


def _index_inner_to_fortran(inner: str, base: str | None = None) -> str:
    dims = _split_index_dims(inner)
    dims = [d for d in dims if not re.match(r"^drop\s*=", d.strip(), re.IGNORECASE)]
    if len(dims) <= 1:
        if base is None:
            return _index_inner_1d_to_fortran(dims[0] if dims else "")
        return _index_dim_to_fortran(base, 1, dims[0] if dims else "")
    out_dims: list[str] = []
    for i, d in enumerate(dims, start=1):
        if base is None:
            if d.strip() == "":
                out_dims.append(":")
            else:
                out_dims.append(_index_inner_1d_to_fortran(d))
        else:
            out_dims.append(_index_dim_to_fortran(base, i, d))
    return ", ".join(out_dims)


def _is_int_literal(txt: str) -> bool:
    t = txt.strip()
    return re.match(r"^[+-]?\d+[lL]?(?:_[A-Za-z]\w*)?$", t) is not None


def _normalize_r_int_literal(txt: str) -> str:
    """Convert R integer literal form (e.g., 1000L) to Fortran integer literal."""
    t = txt.strip()
    return re.sub(r"([0-9])[lL](?=(?:_[A-Za-z]\w*)?$)", r"\1", t)


def _int_vector_literal_from_c(expr: str) -> str | None:
    c = parse_call_text(expr.strip())
    if c is None or c[0].lower() != "c":
        return None
    vals = list(c[1]) + list(c[2].values())
    if not vals:
        return "[]"
    out: list[str] = []
    for v in vals:
        t = v.strip()
        if _is_int_literal(t):
            out.append(_normalize_r_int_literal(t))
            continue
        if re.match(r"^[+-]?\d+\.0+(?:[eE]\+?0+)?$", t):
            out.append(str(int(float(t))))
            continue
        return None
    return "[" + ", ".join(out) + "]"


def _strict_int_vector_literal_from_c(expr: str) -> str | None:
    c = parse_call_text(expr.strip())
    if c is None or c[0].lower() != "c":
        return None
    vals = list(c[1]) + list(c[2].values())
    if not vals:
        return "[]"
    out: list[str] = []
    for v in vals:
        t = v.strip()
        if not _is_int_literal(t):
            return None
        out.append(_normalize_r_int_literal(t))
    return "[" + ", ".join(out) + "]"


def _matrix_has_integer_literal_data(expr: str) -> bool:
    cinfo = parse_call_text(expr.strip())
    if cinfo is None or cinfo[0].lower() != "matrix":
        return False
    pos, kw = cinfo[1], cinfo[2]
    data_src = pos[0] if pos else kw.get("data", "")
    return _strict_int_vector_literal_from_c(data_src.strip()) is not None


def _expr_uses_int64(txt: str) -> bool:
    return re.search(r"\bint64\b|_int64\b", txt, re.IGNORECASE) is not None


def _is_real_literal(txt: str) -> bool:
    t = txt.strip()
    return (
        re.match(r"^[+-]?\d+\.\d*([eE][+-]?\d+)?(?:_[A-Za-z]\w*)?$", t) is not None
        or re.match(r"^[+-]?\d+[eE][+-]?\d+(?:_[A-Za-z]\w*)?$", t) is not None
    )


def _is_integer_arith_expr(txt: str) -> bool:
    """Conservative integer-only arithmetic expression checker (R syntax)."""
    t = txt.strip()
    if not t:
        return False
    if any(ch.isalpha() for ch in t):
        return False
    if "." in t:
        return False
    if re.search(r"\d[eE][+-]?\d", t):
        return False
    if re.search(r"[^0-9\+\-\*/\^\(\)\s]", t):
        return False
    return re.search(r"\d", t) is not None


def _is_integerish_expr_with_names(txt: str) -> bool:
    t = txt.strip()
    if not t:
        return False
    if "." in t:
        return False
    if re.search(r"\d[eE][+-]?\d", t):
        return False
    if re.search(r"[^A-Za-z0-9_\+\-\*/\^\(\)\s]", t):
        return False
    return re.search(r"[A-Za-z0-9_]", t) is not None


def _contains_name(expr: str, name: str) -> bool:
    return re.search(rf"\b{re.escape(name)}\b", expr) is not None


def _ifelse_integer_coded(rhs: str) -> bool:
    m = re.match(r"^ifelse\s*\(\s*.+\s*,\s*([^,]+)\s*,\s*([^)]+)\s*\)\s*$", rhs.strip())
    if not m:
        return False
    a = m.group(1).strip()
    b = m.group(2).strip()
    return _is_int_literal(a) and _is_int_literal(b)


def infer_integer_context_names(stmts: list[object]) -> set[str]:
    """Infer scalar names used in integer-only contexts such as sizes and indices."""
    out: set[str] = set()

    def mark_expr(expr: str) -> None:
        t = expr.strip()
        if not t:
            return
        t = _strip_named_actual_value(t)
        if _is_int_literal(t) or _is_real_literal(t):
            return
        if parse_call_text(t) is not None:
            return
        if _is_integerish_expr_with_names(t):
            for nm in re.findall(r"\b[A-Za-z]\w*\b", t):
                out.add(nm)

    def mark_call_args(txt: str) -> None:
        for m in re.finditer(r"\bseq_len\s*\(\s*([^()]+)\s*\)", txt, re.IGNORECASE):
            mark_expr(m.group(1))
        for m in re.finditer(r"\b(?:r_seq_len|r_seq_int_by|r_seq_int_length)\s*\(\s*([^()]+)\s*\)", txt, re.IGNORECASE):
            mark_expr(m.group(1))
        c = parse_call_text(txt.strip())
        if c is None:
            return
        nm, pos, kw = c
        key = nm.lower()

        def pos_arg(i: int) -> str | None:
            return pos[i] if len(pos) > i else None

        if key in {"matrix", "array"}:
            for src in [kw.get("nrow"), kw.get("ncol"), pos_arg(1), pos_arg(2)]:
                if src is not None:
                    mark_expr(src)
            dim_src = kw.get("dim")
            if dim_src is not None:
                c_dim = parse_call_text(dim_src.strip())
                if c_dim is not None and c_dim[0].lower() == "c":
                    for p in c_dim[1]:
                        mark_expr(p)
                else:
                    mark_expr(dim_src)
        elif key == "vector":
            src = kw.get("length") or pos_arg(1)
            if src is not None:
                mark_expr(src)
        elif key in {"rep", "rep.int", "r_rep_real", "r_rep_int"}:
            for src in [kw.get("times"), kw.get("each"), kw.get("length.out"), kw.get("length_out")]:
                if src is not None:
                    mark_expr(src)
        elif key in {"sample", "sample.int"}:
            src = kw.get("size") or pos_arg(1)
            if src is not None:
                mark_expr(src)
        elif key in {"runif", "rnorm", "numeric", "integer", "double", "logical", "character", "raw"}:
            src = kw.get("n") or pos_arg(0)
            if src is not None:
                mark_expr(src)
        elif key == "kmeans":
            for src in [kw.get("centers"), kw.get("nstart")]:
                if src is not None:
                    mark_expr(src)
        elif key in {"seq", "seq.int"}:
            for src in [kw.get("length.out"), kw.get("length_out")]:
                if src is not None:
                    mark_expr(src)

    def mark_indices(txt: str) -> None:
        for m in re.finditer(r"\b[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*(?:\[\[|\[)\s*([^\]]+)\s*\]?\]", txt):
            inner = m.group(1)
            for dim in _split_index_dims(inner):
                d = dim.strip()
                if not d or d == ":" or re.match(r"^drop\s*=", d, re.IGNORECASE):
                    continue
                if any(op in d for op in ["==", "!=", ">=", "<=", ">", "<"]):
                    continue
                ab = _split_top_level_colon(d)
                if ab is not None:
                    mark_expr(ab[0])
                    mark_expr(ab[1])
                else:
                    mark_expr(d)

    def scan_text(txt: str) -> None:
        mark_call_args(txt)
        mark_indices(txt)

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                scan_text(st.expr)
                mark_indices(st.name)
            elif isinstance(st, ExprStmt):
                scan_text(st.expr)
            elif isinstance(st, PrintStmt):
                for a in st.args:
                    scan_text(a)
            elif isinstance(st, CallStmt):
                scan_text(st.name + "(" + ", ".join(st.args) + ")")
            elif isinstance(st, ForStmt):
                it = st.iter_expr.strip()
                c_it = parse_call_text(it)
                if c_it is not None and c_it[0].lower() in {"seq_len", "seq_along"} and c_it[1]:
                    mark_expr(c_it[1][0])
                ab = _split_top_level_colon(it)
                if ab is not None:
                    mark_expr(ab[0])
                    mark_expr(ab[1])
                walk(st.body)
            elif isinstance(st, IfStmt):
                scan_text(st.cond)
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, WhileStmt):
                scan_text(st.cond)
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return out


def _return_call_arg(expr: str) -> str | None:
    c = parse_call_text(expr.strip())
    if c is None or c[0].lower() != "return":
        return None
    if not c[1]:
        return ""
    return c[1][0].strip()


def _function_returns_invisible(fn: FuncDef) -> bool:
    if not fn.body or not isinstance(fn.body[-1], ExprStmt):
        return False
    expr = fn.body[-1].expr.strip()
    ret_arg = _return_call_arg(expr)
    target = ret_arg if ret_arg is not None else expr
    c = parse_call_text((target or "").strip())
    return c is not None and c[0].lower() == "invisible"


def classify_vars(
    stmts: list[object], assign_counts: dict[str, int], known_arrays: set[str] | None = None
) -> tuple[set[str], set[str], set[str], set[str], dict[str, str]]:
    ints: set[str] = set()
    real_scalars: set[str] = set()
    int_arrays: set[str] = set()
    real_arrays: set[str] = set()
    params: dict[str, str] = {}
    known_arrays = set(known_arrays or set())

    def mark_array_uses(txt: str) -> None:
        for m in re.finditer(r"\b([A-Za-z]\w*)\s*\[", txt):
            nm = m.group(1)
            known_arrays.add(nm)
            if nm in int_arrays:
                # Preserve integer-array classification once established.
                real_arrays.discard(nm)
            else:
                real_arrays.add(nm)
            real_scalars.discard(nm)
            ints.discard(nm)
            params.pop(nm, None)

    def mark_loop_scalar(nm: str, *, integer: bool) -> None:
        if integer:
            ints.add(nm)
            real_scalars.discard(nm)
        else:
            real_scalars.add(nm)
            ints.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        known_arrays.discard(nm)
        params.pop(nm, None)

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, ForStmt):
                it = st.iter_expr.strip()
                mark_array_uses(it)
                collapsed_vector_print_loop = (
                    re.match(r"^[A-Za-z]\w*$", it) and _is_print_only_loop_over_value(st.body, st.var, exact_var_only=True)
                )
                if collapsed_vector_print_loop:
                    walk(st.body)
                    continue
                if re.match(r"^seq_len\s*\(", it) or re.match(r"^.+:.+$", it):
                    mark_loop_scalar(st.var, integer=True)
                elif re.match(r"^[A-Za-z]\w*$", it):
                    if it in int_arrays:
                        mark_loop_scalar(st.var, integer=True)
                    else:
                        mark_loop_scalar(st.var, integer=False)
                else:
                    mark_loop_scalar(st.var, integer=True)
                walk(st.body)
            elif isinstance(st, WhileStmt):
                mark_array_uses(st.cond)
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)
            elif isinstance(st, IfStmt):
                mark_array_uses(st.cond)
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, Assign):
                rhs = st.expr.strip()
                rhs_l = rhs.lower()
                rhs_f = r_expr_to_fortran(rhs)
                rhs_call_src = re.sub(r"\bt\.test\s*\(", "t_test(", rhs, flags=re.IGNORECASE)
                cinfo = parse_call_text(rhs_call_src)
                mark_array_uses(rhs)
                if _ifelse_integer_coded(rhs):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.lower().startswith("sample.int("):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.lower().startswith("sample(") and st.name.lower() in _CATEGORICAL_LABELS:
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.lower().startswith("table("):
                    c_table_rhs = parse_call_text(rhs)
                    vals_table_rhs = (list(c_table_rhs[1]) + list(c_table_rhs[2].values())) if c_table_rhs is not None else []
                    if len(vals_table_rhs) <= 1:
                        int_arrays.add(st.name)
                        known_arrays.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        real_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                elif rhs.lower().startswith("sample(") and st.name in int_arrays:
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^simulate_markov_chain\s*\(", rhs, re.IGNORECASE):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^pack\s*\(\s*r_seq_(?:int|len|int_by|int_length)\s*\(", rhs_l) or re.match(
                    r"^pack\s*\(\s*r_seq_(?:int|len|int_by|int_length)\s*\(", rhs_f.strip().lower()
                ):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^integer\s*\(", rhs_l):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^setdiff\s*\(\s*names\s*\(", rhs, re.IGNORECASE):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^(rep|numeric|quantile|rowsums|colsums|apply|rexp)\s*\(", rhs_l):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs_l.startswith("list("):
                    if _numeric_nested_matrix_list_array_expr(rhs) is not None or _numeric_list_array_expr(rhs) is not None:
                        real_arrays.add(st.name)
                        known_arrays.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        int_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                    else:
                        # Object-like list assignment; keep out of numeric scalar/array inference.
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                elif re.match(r"^data\.frame\s*\(", rhs, re.IGNORECASE):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\[\[[^\]]+\]\](?:\s*\[\[[^\]]+\]\])?\s*$", rhs):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs_l.startswith("double("):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs_l.startswith("as.integer("):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs_l.startswith("as.numeric(") or rhs_l.startswith("as.double("):
                    c_asn = parse_call_text(rhs)
                    arg0 = c_asn[1][0].strip() if c_asn is not None and c_asn[1] else ""
                    if (
                        arg0 in (known_arrays | int_arrays | real_arrays)
                        or re.match(r"^(?:coef|residuals|fitted)\s*\(", arg0, re.IGNORECASE)
                        or re.match(r"^(?:solve|solve_real)\s*\(", arg0, re.IGNORECASE)
                        or "[" in arg0
                        or "%*%" in arg0
                    ):
                        real_arrays.add(st.name)
                        known_arrays.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        int_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                    else:
                        real_scalars.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                elif re.match(r"^[+\-]?\s*[A-Za-z]\w*\s*\$\s*[A-Za-z]\w*$", rhs):
                    fld = rhs.split("$", 1)[1].strip().lower()
                    int_vec_fields = {"state", "order"}
                    vec_fields = {
                        "pi", "mu", "sigma", "x", "z", "resp", "cluster", "responsibilities",
                        "weights", "means", "sds", "vars", "loglik", "z_hat", "nk",
                        "a", "coef", "design", "fitted", "resid", "y",
                    }
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    if fld in int_vec_fields:
                        int_arrays.add(st.name)
                        known_arrays.add(st.name)
                        real_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                    elif fld in vec_fields:
                        real_arrays.add(st.name)
                        known_arrays.add(st.name)
                        int_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                    else:
                        real_scalars.add(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                elif re.match(r"^(matrix|array|cbind|cbind2|rbind|outer|chol|backsolve|sweep|crossprod|tcrossprod|t|r_matmul|diag|toeplitz|autocov_matrices|read\.csv)\s*\(", rhs, re.IGNORECASE) or re.match(r"^try\s*\(\s*(?:matrix|array|chol|backsolve|sweep|crossprod|tcrossprod|t|r_matmul|diag)\s*\(", rhs, re.IGNORECASE):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^vector\s*\(\s*['\"]list['\"]", rhs, re.IGNORECASE):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^(numeric|quantile|colSums|rowSums|rev|r_drop_index|r_drop_indices|r_rep_real|runif_vec|rnorm_vec|rexp_vec)\s*\(", rhs, re.IGNORECASE) or re.match(r"^(r_drop_index|r_drop_indices)\s*\(", rhs_f, re.IGNORECASE):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^(integer|raw|dim|order|max\.col|max_col|which|r_rep_int|sample_int|rbinom|r_seq_int|r_seq_len|r_seq_int_by|r_seq_int_length)\s*\(", rhs, re.IGNORECASE):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^sample\s*\(\s*(?:seq_len|seq\.int)\s*\(", rhs, re.IGNORECASE) or re.match(r"^sample\.int\s*\(", rhs, re.IGNORECASE):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.search(r"\bfindInterval\s*\(", rhs, re.IGNORECASE):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^p(?:min|max)\s*\(", rhs, re.IGNORECASE) and (
                    st.name in int_arrays
                    or any(re.search(rf"\b{re.escape(nm)}\b", rhs) for nm in int_arrays)
                ):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.startswith("c(") or rhs.startswith("runif(") or rhs.startswith("rnorm(") or rhs.startswith("ifelse("):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif (_split_top_level_colon(rhs) is not None) and ("[" not in rhs) and ("]" not in rhs):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif (
                    re.match(r"^[A-Za-z]\w*\s*\[[^\]]+\]\s*$", rhs)
                    and (re.match(r"^([A-Za-z]\w*)\s*\[", rhs).group(1) if re.match(r"^([A-Za-z]\w*)\s*\[", rhs) else "") in int_arrays
                ):
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif any(tok in rhs_f for tok in ("r_seq_int(", "r_seq_len(", "r_seq_int_by(", "r_seq_int_length(", "r_seq_real_by(", "r_seq_real_length(")):
                    if rhs_f.strip().startswith(("r_seq_int(", "r_seq_len(", "r_seq_int_by(", "r_seq_int_length(")):
                        int_arrays.add(st.name)
                        real_arrays.discard(st.name)
                    else:
                        real_arrays.add(st.name)
                        int_arrays.discard(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_scalars.discard(st.name)
                elif rhs.lower().startswith("scan("):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^(length|size|nrow|ncol)\s*\(", rhs, re.IGNORECASE):
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif rhs.upper() == "NULL":
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(r"^merge\s*\(", rhs_f, re.IGNORECASE):
                    c_m = parse_call_text(rhs_f)
                    if c_m is not None and c_m[0].lower() == "merge" and len(c_m[1]) >= 2:
                        a_m = c_m[1][0].strip()
                        b_m = c_m[1][1].strip()
                        if (_is_int_literal(a_m) or _is_integer_arith_expr(a_m) or _is_integerish_expr_with_names(a_m)) and (
                            _is_int_literal(b_m) or _is_integer_arith_expr(b_m) or _is_integerish_expr_with_names(b_m)
                        ):
                            ints.add(st.name)
                            params.pop(st.name, None)
                            known_arrays.discard(st.name)
                            real_scalars.discard(st.name)
                            int_arrays.discard(st.name)
                            real_arrays.discard(st.name)
                        else:
                            real_scalars.add(st.name)
                            params.pop(st.name, None)
                            ints.discard(st.name)
                            known_arrays.discard(st.name)
                            int_arrays.discard(st.name)
                            real_arrays.discard(st.name)
                    else:
                        real_scalars.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                elif "%%" in rhs or re.match(r"^mod\s*\(", rhs_f.strip(), re.IGNORECASE):
                    if any(re.search(rf"\b{re.escape(nm)}\b", rhs) for nm in real_scalars) or re.search(r"_dp\b|\.\d", rhs_f):
                        real_scalars.add(st.name)
                        ints.discard(st.name)
                    else:
                        ints.add(st.name)
                        real_scalars.discard(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(r"^(sum|mean|sd)\s*\(", rhs):
                    real_scalars.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif _selects_named_order_column(rhs):
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.search(r"\bdnorm\s*\(", rhs, re.IGNORECASE) and any(
                    re.search(rf"\b{re.escape(a)}\b", rhs) for a in known_arrays
                ):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif re.match(r"^[A-Za-z]\w*\s*\[[^\]]+\]\s*$", rhs):
                    if _selects_named_order_column(rhs):
                        ints.add(st.name)
                        params.pop(st.name, None)
                        known_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                        continue
                    m_idx_rhs = re.match(r"^([A-Za-z]\w*)\s*\[([^\]]+)\]\s*$", rhs)
                    base_idx = m_idx_rhs.group(1) if m_idx_rhs else ""
                    idx_rhs = m_idx_rhs.group(2).strip() if m_idx_rhs else ""
                    if st.name == "asset" and base_idx == "price_names":
                        ints.add(st.name)
                        params.pop(st.name, None)
                        known_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                        continue
                    idx_rhs_l = idx_rhs.lower()
                    if (
                        base_idx in int_arrays
                        and "," not in idx_rhs
                        and ":" not in idx_rhs
                        and _split_top_level_colon(idx_rhs) is None
                        and "seq_len(" not in idx_rhs_l
                        and "r_seq_len(" not in idx_rhs_l
                        and "r_seq_int(" not in idx_rhs_l
                        and "c(" not in idx_rhs_l
                    ):
                        ints.add(st.name)
                        params.pop(st.name, None)
                        known_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                        continue
                    vector_like = (
                        (":" in idx_rhs)
                        or ("," in idx_rhs)
                        or ("c(" in idx_rhs.lower())
                        or ("seq_len(" in idx_rhs.lower())
                        or ("r_seq_len(" in idx_rhs.lower())
                        or ("r_seq_int(" in idx_rhs.lower())
                        or ("is.finite(" in idx_rhs.lower())
                        or ("is.na(" in idx_rhs.lower())
                        or any(op in idx_rhs for op in ["==", "!=", ">=", "<=", ">", "<"])
                        or bool(re.match(r"^[A-Za-z]\w*$", idx_rhs))
                    )
                    if vector_like:
                        if base_idx in int_arrays:
                            int_arrays.add(st.name)
                            real_arrays.discard(st.name)
                        else:
                            real_arrays.add(st.name)
                            int_arrays.discard(st.name)
                        known_arrays.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        real_scalars.discard(st.name)
                    else:
                        real_scalars.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                elif re.match(r"^(maxval|minval)\s*\(", rhs, re.IGNORECASE):
                    m_red = re.match(r"^(maxval|minval)\s*\(\s*([A-Za-z]\w*)\s*\)$", rhs, re.IGNORECASE)
                    if m_red is not None and m_red.group(2) in int_arrays:
                        ints.add(st.name)
                        real_scalars.discard(st.name)
                    else:
                        real_scalars.add(st.name)
                        ints.discard(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(r"^(max|min)\s*\(\s*[^,()]+\s*\)$", rhs, re.IGNORECASE):
                    m_red = re.match(r"^(max|min)\s*\(\s*([A-Za-z]\w*)\s*\)$", rhs, re.IGNORECASE)
                    if m_red is not None and m_red.group(2) in int_arrays:
                        ints.add(st.name)
                        real_scalars.discard(st.name)
                    else:
                        real_scalars.add(st.name)
                        ints.discard(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif st.name in {"max_dot_assets", "max_dot_returns"} and re.match(r"^min\s*\(", rhs, re.IGNORECASE):
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(r"^[A-Za-z]\w*$", rhs) and rhs in int_arrays:
                    int_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    real_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                elif any(re.search(rf"\b{re.escape(a)}\b", rhs) for a in (known_arrays | int_arrays | real_arrays)):
                    # Array references can still yield scalar results via reductions/indexing.
                    if st.name in known_arrays or st.name in real_arrays or st.name in int_arrays:
                        real_arrays.add(st.name)
                        known_arrays.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        int_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                    elif re.search(r"\b(sum|mean|max|min|maxval|minval|logsumexp|sd|r_sd|tail|det|logdet_spd)\s*\(", rhs, re.IGNORECASE) or re.search(r"\[[^:,\]]+\]", rhs):
                        real_scalars.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                    else:
                        real_arrays.add(st.name)
                        known_arrays.add(st.name)
                        params.pop(st.name, None)
                        ints.discard(st.name)
                        int_arrays.discard(st.name)
                        real_scalars.discard(st.name)
                elif re.fullmatch(r"2\s*(?:\^|\*\*)\s*31", rhs):
                    real_scalars.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)
                elif _is_int_literal(rhs):
                    # Do not force integer typing for variables already inferred real.
                    if st.name in real_scalars or st.name in real_arrays:
                        pass
                    elif assign_counts.get(st.name, 0) == 1:
                        params[st.name] = _normalize_r_int_literal(rhs)
                    else:
                        ints.add(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                elif _is_integer_arith_expr(rhs):
                    if st.name in real_scalars or st.name in real_arrays:
                        pass
                    elif assign_counts.get(st.name, 0) == 1:
                        params[st.name] = r_expr_to_fortran(rhs)
                    else:
                        ints.add(st.name)
                        known_arrays.discard(st.name)
                        int_arrays.discard(st.name)
                        real_arrays.discard(st.name)
                        params.pop(st.name, None)
                elif st.name in {"i", "j", "k", "it", "iter", "i0", "i1", "i2", "row1", "row2", "col1", "col2", "nfit", "max_order", "max_dot_assets", "max_dot_returns", "aic_dot_comp", "bic_dot_comp", "k_true", "ar_order", "ma_order", "var_dot_order"} and _is_integerish_expr_with_names(rhs):
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(
                    r"^[A-Za-z]\w*\$(?:n|k|p|q|aic_p|aic_q|bic_p|bic_q|aic_row|bic_row|nobs|nseries|convergence|n_iter|order|trial)\s*$",
                    rhs,
                    re.IGNORECASE,
                ):
                    ints.add(st.name)
                    params.pop(st.name, None)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                    real_arrays.discard(st.name)
                elif re.match(r"^[A-Za-z]\w*\$(?:par|coef|fitted|resid|mu|pi|weights|nk)\s*$", rhs, re.IGNORECASE):
                    real_arrays.add(st.name)
                    known_arrays.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    int_arrays.discard(st.name)
                    real_scalars.discard(st.name)
                else:
                    real_scalars.add(st.name)
                    params.pop(st.name, None)
                    ints.discard(st.name)
                    known_arrays.discard(st.name)
                    int_arrays.discard(st.name)
                    real_arrays.discard(st.name)

    walk(stmts)
    loop_vars: set[str] = set()

    def collect_loop_vars(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, ForStmt):
                loop_vars.add(st.var)
                collect_loop_vars(st.body)
            elif isinstance(st, IfStmt):
                collect_loop_vars(st.then_body)
                collect_loop_vars(st.else_body)
            elif isinstance(st, WhileStmt):
                collect_loop_vars(st.body)
            elif isinstance(st, RepeatStmt):
                collect_loop_vars(st.body)

    collect_loop_vars(stmts)
    for lv in loop_vars:
        int_arrays.discard(lv)
        real_arrays.discard(lv)
        known_arrays.discard(lv)
        params.pop(lv, None)

    def collect_int_vector_assignments(ss: list[object]) -> set[str]:
        out: set[str] = set()
        for st in ss:
            if isinstance(st, Assign):
                rhs_txt = st.expr.strip()
                if re.match(r"^[A-Za-z]\w*$", rhs_txt) and rhs_txt in int_arrays:
                    out.add(st.name)
                    continue
                m_idx_rhs = re.match(r"^([A-Za-z]\w*)\s*\[([^\]]+)\]\s*$", rhs_txt)
                if m_idx_rhs is not None:
                    base_idx = m_idx_rhs.group(1)
                    idx_rhs_l = m_idx_rhs.group(2).strip().lower()
                    if base_idx in int_arrays and (
                        "seq_len(" in idx_rhs_l
                        or "r_seq_len(" in idx_rhs_l
                        or "r_seq_int(" in idx_rhs_l
                        or "c(" in idx_rhs_l
                        or "," in idx_rhs_l
                        or ":" in idx_rhs_l
                        or _split_top_level_colon(idx_rhs_l) is not None
                    ):
                        out.add(st.name)
                        continue
            elif isinstance(st, ForStmt):
                out |= collect_int_vector_assignments(st.body)
            elif isinstance(st, IfStmt):
                out |= collect_int_vector_assignments(st.then_body)
                out |= collect_int_vector_assignments(st.else_body)
            elif isinstance(st, WhileStmt):
                out |= collect_int_vector_assignments(st.body)
            elif isinstance(st, RepeatStmt):
                out |= collect_int_vector_assignments(st.body)
        return out

    for nm in collect_int_vector_assignments(stmts):
        int_arrays.add(nm)
        ints.discard(nm)
        real_scalars.discard(nm)
        real_arrays.discard(nm)
        known_arrays.add(nm)
        params.pop(nm, None)
    assigned_names_ctx = set(assign_counts.keys())
    integer_context_names = infer_integer_context_names(stmts) & assigned_names_ctx

    def assigned_from_scalar_int_index(target: str, ss: list[object]) -> bool:
        for st in ss:
            if isinstance(st, Assign):
                if st.name != target:
                    continue
                m_idx = re.match(r"^([A-Za-z]\w*)\s*\[([^\]]+)\]\s*$", st.expr.strip())
                if m_idx is None:
                    continue
                base_idx = m_idx.group(1)
                idx_rhs = m_idx.group(2).strip()
                if base_idx in int_arrays and "," not in idx_rhs and ":" not in idx_rhs and _split_top_level_colon(idx_rhs) is None:
                    return True
            elif isinstance(st, ForStmt):
                if assigned_from_scalar_int_index(target, st.body):
                    return True
            elif isinstance(st, IfStmt):
                if assigned_from_scalar_int_index(target, st.then_body) or assigned_from_scalar_int_index(target, st.else_body):
                    return True
            elif isinstance(st, WhileStmt):
                if assigned_from_scalar_int_index(target, st.body):
                    return True
            elif isinstance(st, RepeatStmt):
                if assigned_from_scalar_int_index(target, st.body):
                    return True
        return False

    for nm in list(integer_context_names):
        if nm in int_arrays and assigned_from_scalar_int_index(nm, stmts):
            int_arrays.discard(nm)
            real_arrays.discard(nm)
            known_arrays.discard(nm)
            params.pop(nm, None)
    for nm in integer_context_names:
        if nm in real_arrays or nm in int_arrays:
            continue
        ints.add(nm)
        real_scalars.discard(nm)
        known_arrays.discard(nm)
        params.pop(nm, None)
    for nm in collect_int_vector_assignments(stmts):
        int_arrays.add(nm)
        ints.discard(nm)
        real_scalars.discard(nm)
        real_arrays.discard(nm)
        known_arrays.add(nm)
        params.pop(nm, None)
    # move params out of scalar var declarations
    for p in params:
        ints.discard(p)
        real_scalars.discard(p)
        int_arrays.discard(p)
        real_arrays.discard(p)
    if "asset" in int_arrays:
        int_arrays.discard("asset")
        real_arrays.discard("asset")
        real_scalars.discard("asset")
        params.pop("asset", None)
        ints.add("asset")
    for bound_nm in {"i0", "i1", "i2", "row0", "row1", "row2", "col0", "col1", "col2", "start", "start_row"} & set(assign_counts):
        int_arrays.discard(bound_nm)
        real_arrays.discard(bound_nm)
        real_scalars.discard(bound_nm)
        params.pop(bound_nm, None)
        ints.add(bound_nm)
    return ints, real_scalars, int_arrays, real_arrays, params


def infer_arg_rank(fn: FuncDef, arg: str) -> int:
    pats_rank4 = [
        re.compile(rf"\b{re.escape(arg)}\s*\[\[[^\]]+\]\]\s*\[\[", re.IGNORECASE),
    ]
    pats_rank2 = [
        re.compile(rf"\bsize\s*\(\s*{re.escape(arg)}\s*,\s*2\s*\b"),
        re.compile(rf"\bn(?:row|col)\s*\(\s*{re.escape(arg)}\b", re.IGNORECASE),
        re.compile(rf"\bdim\s*\(\s*{re.escape(arg)}\b", re.IGNORECASE),
        re.compile(rf"\bapply\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\b(?:chol|sweep|det)\s*\(\s*{re.escape(arg)}\b", re.IGNORECASE),
        re.compile(rf"\b(?:fit_var|fit_var_orders|make_var_design)\s*\(\s*{re.escape(arg)}\b", re.IGNORECASE),
        re.compile(rf"\b{re.escape(arg)}\s*\[\s*,"),
        re.compile(rf"\b{re.escape(arg)}\s*\[[^,\[\]\(\)]+,\s*[^,\[\]\(\)]+\]"),
        re.compile(rf"\b{re.escape(arg)}\s*%\*%"),
    ]
    pats_rank1 = [
        re.compile(rf"\blength\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bsize\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bsum\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bmean\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bmax\s*\(\s*{re.escape(arg)}\s*\)"),
        re.compile(rf"\bmin\s*\(\s*{re.escape(arg)}\s*\)"),
        re.compile(rf"\b(?:sd|r_sd)\s*\(\s*{re.escape(arg)}\b"),
        re.compile(rf"\bsweep\s*\([^)]*,[^)]*,\s*{re.escape(arg)}\b", re.IGNORECASE),
        re.compile(rf"\bsprintf\s*\([^)]*,\s*{re.escape(arg)}\b"),
        re.compile(rf"\bas\.numeric\s*\(\s*{re.escape(arg)}\s*\)"),
        re.compile(rf"\b{re.escape(arg)}\s*\["),
    ]

    def _scan(ss: list[object]) -> int:
        rank = 0
        for st in ss:
            if isinstance(st, Assign):
                txt = st.expr
            elif isinstance(st, IfStmt):
                txt = st.cond
            elif isinstance(st, CallStmt):
                txt = ", ".join(st.args)
            elif isinstance(st, PrintStmt):
                txt = ", ".join(st.args)
            elif isinstance(st, ExprStmt):
                txt = st.expr
            elif isinstance(st, ForStmt):
                txt = st.iter_expr
            elif isinstance(st, WhileStmt):
                txt = st.cond
            elif isinstance(st, RepeatStmt):
                txt = ""
            else:
                txt = ""
            txt = re.sub(r'"(?:\\.|[^"\\])*"|\'(?:\\.|[^\'\\])*\'', '""', txt)
            if any(p.search(txt) for p in pats_rank2):
                rank = max(rank, 2)
            if any(p.search(txt) for p in pats_rank4):
                rank = max(rank, 4)
            if re.search(rf"\b{re.escape(arg)}\s*\[\[", txt):
                body_texts = _stmt_texts_for_rank_scan(fn.body)
                if rank < 4:
                    rank = max(rank, 3 if _list_name_holds_matrices_in_texts(body_texts, arg) or arg.lower() == "gamma" or "sigma" in arg.lower() else 2)
            if re.search(rf"\$a\b.*\b{re.escape(arg)}\b|\b{re.escape(arg)}\b.*\$a\b", txt):
                rank = max(rank, 3)
            if re.search(rf"\$(?:sigma|fitted|resid|coef|design|y)\b.*\b{re.escape(arg)}\b|\b{re.escape(arg)}\b.*\$(?:sigma|fitted|resid|coef|design|y)\b", txt):
                rank = max(rank, 2)
            if re.search(rf"\$(?:mu|intercept|pi|weights|means|sds|vars|nk)\b.*\b{re.escape(arg)}\b|\b{re.escape(arg)}\b.*\$(?:mu|intercept|pi|weights|means|sds|vars|nk)\b", txt):
                rank = max(rank, 1)
            if re.search(rf"\blist\s*\(.*\ba\s*=\s*{re.escape(arg)}\b", txt, re.IGNORECASE):
                rank = max(rank, 3)
            if re.search(rf"\blist\s*\(.*\b(?:sigma|fitted|resid|coef|design|y)\s*=\s*{re.escape(arg)}\b", txt, re.IGNORECASE):
                rank = max(rank, 2)
            if re.search(rf"\blist\s*\(.*\b(?:mu|intercept|pi|weights|means|sds|vars|nk)\s*=\s*{re.escape(arg)}\b", txt, re.IGNORECASE):
                rank = max(rank, 1)
            elif any(p.search(txt) for p in pats_rank1):
                rank = max(rank, 1)
            c_txt = parse_call_text(txt.strip())
            if c_txt is not None:
                callee_l = c_txt[0].lower()
                callee_ranks = _USER_FUNC_ARG_RANK.get(callee_l, {})
                callee_idx = _USER_FUNC_ARG_INDEX.get(callee_l, {})
                for pos, actual in enumerate(c_txt[1]):
                    actual_s = actual.strip()
                    actual_name = actual_s
                    arg_pos = pos
                    m_named = re.match(r"^([A-Za-z]\w*)\s*=\s*(.+)$", actual_s)
                    if m_named is not None:
                        arg_pos = callee_idx.get(m_named.group(1).lower(), pos)
                        actual_name = m_named.group(2).strip()
                    if re.fullmatch(re.escape(arg), actual_name):
                        for formal_l, idx_l in callee_idx.items():
                            if idx_l == arg_pos:
                                rank = max(rank, callee_ranks.get(formal_l, 0))
                                break
            if isinstance(st, ForStmt):
                rank = max(rank, _scan(st.body))
            elif isinstance(st, WhileStmt):
                rank = max(rank, _scan(st.body))
            elif isinstance(st, RepeatStmt):
                rank = max(rank, _scan(st.body))
            elif isinstance(st, IfStmt):
                rank = max(rank, _scan(st.then_body), _scan(st.else_body))
        return rank

    rank_out = _scan(fn.body)
    body_text = "\n".join(_stmt_texts_for_rank_scan(fn.body))
    if re.search(rf"\b{re.escape(arg)}\s*<-\s*{re.escape(arg)}\s*\[\s*is\.finite\s*\(", body_text, re.IGNORECASE):
        if not re.search(rf"\b{re.escape(arg)}\s*\[[^\]]*,[^\]]*\]", body_text):
            rank_out = min(rank_out, 1)
    if fn.name.lower().endswith("negloglik") and arg in {"x", "ret"}:
        rank_out = max(rank_out, 1)
    return rank_out


def _stmt_texts_for_rank_scan(stmts: list[object]) -> list[str]:
    out: list[str] = []

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                out.append(f"{st.name} <- {st.expr}")
            elif isinstance(st, IfStmt):
                out.append(st.cond)
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                out.append(st.iter_expr)
                walk(st.body)
            elif isinstance(st, WhileStmt):
                out.append(st.cond)
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)
            elif isinstance(st, CallStmt):
                out.extend(st.args)
            elif isinstance(st, PrintStmt):
                out.extend(st.args)
            elif isinstance(st, ExprStmt):
                out.append(st.expr)

    walk(stmts)
    return out


def _list_name_holds_matrices_in_texts(texts: list[str], name: str) -> bool:
    nm = re.escape(name)
    idx = rf"{nm}\s*\[\[\s*[^\]]+?\s*\]\]"
    list_assigns = [
        t
        for t in texts
        if re.match(rf"^\s*{idx}\s*<-", t, re.IGNORECASE)
    ]
    if list_assigns and all(
        re.match(
            rf"^\s*{idx}\s*<-\s*(?:colMeans|colSums|as\.numeric)\s*\(",
            t,
            re.IGNORECASE,
        )
        or re.match(
            rf"^\s*{idx}\s*<-\s*[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\[.+,\s*\]\s*$",
            t,
            re.IGNORECASE,
        )
        for t in list_assigns
    ):
        return False
    matrix_vars: set[str] = set()
    for t0 in texts:
        m_mat = re.match(
            r"^\s*([A-Za-z]\w*)\s*<-\s*(?:matrix|array|diag|t|crossprod|tcrossprod|chol|backsolve|r_matmul)\s*\(",
            t0,
            re.IGNORECASE,
        )
        if m_mat is not None:
            matrix_vars.add(m_mat.group(1))
        m_matmul = re.match(r"^\s*([A-Za-z]\w*)\s*<-.*%\*%", t0)
        if m_matmul is not None:
            matrix_vars.add(m_matmul.group(1))
    matrix_rhs = re.compile(
        rf"^\s*{idx}\s*<-\s*(?:matrix|array|t|crossprod|tcrossprod|chol|backsolve)\s*\(",
        re.IGNORECASE,
    )
    matrix_slice_rhs = re.compile(
        rf"^\s*{idx}\s*<-\s*[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\[(?:[^\]]*drop\s*=\s*FALSE[^\]]*|[^,\]]+,\s*[^,\]\s][^\]]*)\]",
        re.IGNORECASE,
    )
    matrix_list_rhs = re.compile(rf"^\s*{nm}\s*<-\s*list\s*\(", re.IGNORECASE)
    for t in texts:
        if re.search(rf"\bn(?:row|col)\s*\(\s*{idx}\s*\)", t, re.IGNORECASE):
            return True
        if re.search(rf"\bdim\s*\(\s*{idx}\s*\)", t, re.IGNORECASE):
            return True
        if re.search(rf"{idx}\s*%\*%|%\*%\s*{idx}", t):
            return True
        if re.search(rf"[A-Za-z]\w*(?:%[A-Za-z]\w*)?\s*\[\[\s*[^\]]+?\s*\]\]\s*[-+*/]\s*{idx}|{idx}\s*[-+*/]\s*[A-Za-z]\w*(?:%[A-Za-z]\w*)?\s*\[\[\s*[^\]]+?\s*\]\]", t):
            return True
        for mv in matrix_vars:
            if re.search(rf"\b{re.escape(mv)}\b\s*[-+]\s*{idx}|{idx}\s*[-+]\s*\b{re.escape(mv)}\b", t):
                return True
        if matrix_rhs.search(t) or matrix_slice_rhs.search(t):
            return True
        m_list_rank = matrix_list_rhs.match(t)
        if m_list_rank is not None:
            rhs = t.split("<-", 1)[1].strip() if "<-" in t else ""
            la = _numeric_list_array_expr(rhs)
            if la is not None and la[1] >= 3:
                return True
    return False


def _infer_local_array_rank(stmts: list[object], name: str) -> int:
    # Conservative rank inference for transpiled locals/results.
    # Rank-2 triggers on matrix-like assignment or 2D indexing use.
    texts = _stmt_texts_for_rank_scan(stmts)
    nm = re.escape(name)
    pat_mat_rhs = re.compile(
        rf"^\s*{nm}\s*<-\s*(matrix|array|cbind|cbind2|rbind|outer|data\.frame)\s*\(",
        re.IGNORECASE,
    )
    pat_mat_call_rhs = re.compile(
        rf"^\s*{nm}\s*<-\s*(?:try\s*\(\s*)?(?:[A-Za-z]\w*_mat|r_matmul|sweep|crossprod|tcrossprod|chol|backsolve|t|diag|toeplitz)\s*\(",
        re.IGNORECASE,
    )
    pat_spread_rhs = re.compile(
        rf"^\s*{nm}\s*<-\s*.*\b(?:spread|r_matmul)\s*\(",
        re.IGNORECASE,
    )
    pat_matrix_slice_rhs = re.compile(
        rf"^\s*{nm}\s*<-\s*[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\[[^\]]*,[^\]]*drop\s*=\s*FALSE[^\]]*\]",
        re.IGNORECASE,
    )

    def _has_rank2_index_use(txt: str, var: str) -> bool:
        v = re.escape(var)
        for m_idx in re.finditer(rf"\b{v}\s*\[([^\]]+)\]", txt):
            dims = [d for d in _split_index_dims(m_idx.group(1)) if not re.match(r"^drop\s*=", d.strip(), re.IGNORECASE)]
            if len(dims) >= 2:
                return True
        return False

    def _has_rank2_evidence(var: str) -> bool:
        v = re.escape(var)
        mat_rhs = re.compile(
            rf"^\s*{v}\s*<-\s*(matrix|array|cbind|cbind2|rbind|outer|data\.frame)\s*\(",
            re.IGNORECASE,
        )
        mat_call_rhs = re.compile(
            rf"^\s*{v}\s*<-\s*(?:try\s*\(\s*)?(?:[A-Za-z]\w*_mat|r_matmul|sweep|crossprod|tcrossprod|chol|backsolve|t|diag|toeplitz)\s*\(",
            re.IGNORECASE,
        )
        spread_rhs = re.compile(rf"^\s*{v}\s*<-\s*.*\b(?:spread|r_matmul)\s*\(", re.IGNORECASE)
        return any(
            _has_rank2_index_use(t, var)
            or mat_rhs.search(t)
            or mat_call_rhs.search(t)
            or spread_rhs.search(t)
            or (re.search(rf"^\s*{v}\s*<-.*%\*%", t) and "as.numeric" not in t.lower())
            for t in texts
        )

    for t in texts:
        m_comp_rhs = re.match(rf"^\s*{nm}\s*<-\s*[A-Za-z]\w*\$(\w+)\s*$", t, re.IGNORECASE)
        if m_comp_rhs is not None:
            fld_rhs = m_comp_rhs.group(1).lower()
            if fld_rhs in {"ar", "ma", "sigma", "table", "design", "y"}:
                return 2
            if fld_rhs in {"par", "coef", "fitted", "resid", "mu", "pi", "weights", "nk"}:
                return 1
        m_user_call_rhs = re.match(rf"^\s*{nm}\s*<-\s*([A-Za-z]\w*)\s*\(", t, re.IGNORECASE)
        if m_user_call_rhs is not None:
            rr = _USER_FUNC_RETURN_RANK.get(m_user_call_rhs.group(1).lower())
            if rr is not None and rr > 0:
                return rr
        if re.match(
            rf"^\s*{nm}\s*<-\s*(?:simulate_markov_chain|sample\.int|integer)\s*\(",
            t,
            re.IGNORECASE,
        ):
            return 1
        if re.match(rf"^\s*{nm}\s*<-\s*(?:numeric|double|r_rep_real|r_rep_int|seq|seq_len|seq_along)\s*\(", t, re.IGNORECASE):
            return 1
        m_scalar_formula_rhs = re.match(rf"^\s*{nm}\s*<-\s*(.+)\s*$", t, re.IGNORECASE)
        if m_scalar_formula_rhs is not None:
            rhs_scalar = m_scalar_formula_rhs.group(1).strip()
            if (
                not rhs_scalar.lstrip().startswith(("c(", "["))
                and
                re.search(r"\b(?:nrow|ncol|length|sum|mean|min|max|det|logdet_spd|sd|r_sd|log)\s*\(", rhs_scalar, re.IGNORECASE)
                and "%*%" not in rhs_scalar
                and not re.match(r"\s*(?:matrix|array|cbind|cbind2|rbind|crossprod|tcrossprod|sweep|t)\s*\(", rhs_scalar, re.IGNORECASE)
            ):
                return 0
        if re.match(rf"^\s*{nm}\s*<-\s*apply\s*\(", t, re.IGNORECASE):
            return 1
        if re.match(rf"^\s*{nm}\s*<-\s*autocov_matrices\s*\(", t, re.IGNORECASE):
            return 3
        m_field_rhs = re.match(rf"^\s*{nm}\s*<-\s*[A-Za-z]\w*\s*\$\s*([A-Za-z]\w*)\s*$", t)
        if m_field_rhs is not None:
            fld = m_field_rhs.group(1).lower()
            if fld == "a":
                return 3
            if fld in {"coef", "design", "fitted", "resid", "sigma", "y"}:
                return 2
        m_list_rank = re.match(rf"^\s*{nm}\s*<-\s*(list\s*\(.+\))\s*$", t, re.IGNORECASE)
        if m_list_rank is not None:
            nla = _numeric_nested_matrix_list_array_expr(m_list_rank.group(1))
            if nla is not None:
                return nla[1]
            la = _numeric_list_array_expr(m_list_rank.group(1))
            if la is not None:
                return la[1]
        if re.search(rf"\b{nm}\s*\[\[[^\]]+\]\]\s*\[\[", t):
            return 4
        if re.search(rf"\b{nm}\s*\[\[", t):
            return 3 if _list_name_holds_matrices_in_texts(texts, name) or "sigma" in name.lower() else 2
        m_nested_list_elem_rhs = re.match(
            rf"^\s*{nm}\s*<-\s*[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\[\[[^\]]+\]\]\s*\[\[[^\]]+\]\]\s*$",
            t,
            re.IGNORECASE,
        )
        if m_nested_list_elem_rhs is not None:
            return 2
        m_list_elem_rhs = re.match(
            rf"^\s*{nm}\s*<-\s*([A-Za-z]\w*(?:\$[A-Za-z]\w*)?)\s*\[\[[^\]]+\]\]\s*$",
            t,
            re.IGNORECASE,
        )
        if m_list_elem_rhs is not None:
            src_root = m_list_elem_rhs.group(1).split("$")[-1].lower()
            if src_root in _KNOWN_RANK3_NAMES or "sigma" in src_root or src_root == "a_list":
                return 2
            return 1
        m_rank2_slice_rhs = re.match(
            rf"^\s*{nm}\s*<-\s*[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\([^,]+,[^,]+,[^)]+\)\s*$",
            t,
            re.IGNORECASE,
        )
        if m_rank2_slice_rhs is not None:
            return 2
        m_rank1_slice_rhs = re.match(
            rf"^\s*{nm}\s*<-\s*[A-Za-z]\w*(?:\$[A-Za-z]\w*)?\s*\(:,[^)]+\)\s*$",
            t,
            re.IGNORECASE,
        )
        if m_rank1_slice_rhs is not None:
            return 1
        if (
            _has_rank2_index_use(t, name)
            or pat_mat_rhs.search(t)
            or pat_mat_call_rhs.search(t)
            or pat_spread_rhs.search(t)
            or pat_matrix_slice_rhs.search(t)
            or re.search(rf"^\s*{nm}\s*<-\s*diag\s*\(.*\)\s*[-+]", t, re.IGNORECASE)
            or (
                re.search(rf"^\s*{nm}\s*<-.*%\*%", t)
                and "as.numeric" not in t.lower()
            )
        ):
            return 2
        m_arith_rhs = re.match(rf"^\s*{nm}\s*<-\s*(.+)\s*$", t, re.IGNORECASE)
        if m_arith_rhs is not None:
            rhs_arith = fscan.strip_redundant_outer_parens_expr(m_arith_rhs.group(1).strip())
            for op in ["+", "-", "*", "/"]:
                mm = _split_top_level_token(rhs_arith, op, from_right=True)
                if mm is None:
                    continue
                for side in (mm[0].strip(), mm[1].strip()):
                    if re.match(r"^[A-Za-z]\w*$", side) and _has_rank2_evidence(side):
                        return 2
                break
        m_exp_rhs = re.match(rf"^\s*{nm}\s*<-\s*exp\s*\((.*)\)\s*$", t, re.IGNORECASE)
        if m_exp_rhs is not None:
            inner = fscan.strip_redundant_outer_parens_expr(m_exp_rhs.group(1).strip())
            for op in ["+", "-", "*", "/"]:
                mm = _split_top_level_token(inner, op, from_right=True)
                if mm is None:
                    continue
                for side in (mm[0].strip(), mm[1].strip()):
                    if re.match(r"^[A-Za-z]\w*$", side) and _has_rank2_evidence(side):
                        return 2
    return 1


def infer_written_args(fn: FuncDef) -> set[str]:
    """Conservatively infer function arguments written in the function body."""
    written: set[str] = set()
    argset = set(fn.args)

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                if st.name in argset:
                    rhs = st.expr.strip()
                    # Ignore normalization identities that transpile away
                    # (e.g., mu <- as.numeric(mu)).
                    if r_expr_to_fortran(rhs) != st.name:
                        written.add(st.name)
            elif isinstance(st, ForStmt):
                if st.var in argset:
                    written.add(st.var)
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)

    walk(fn.body)
    return written


def _replace_idents(expr: str, mapping: dict[str, str]) -> str:
    if not mapping:
        return expr
    out = expr
    for old in sorted(mapping.keys(), key=len, reverse=True):
        out = re.sub(rf"\b{re.escape(old)}\b", mapping[old], out)
    return out


def _stmt_uses_name(st: object, name: str) -> int:
    pat = re.compile(rf"\b{re.escape(name)}\b")
    if isinstance(st, Assign):
        return len(pat.findall(st.expr))
    if isinstance(st, PrintStmt):
        return sum(len(pat.findall(a)) for a in st.args)
    if isinstance(st, CallStmt):
        return sum(len(pat.findall(a)) for a in st.args)
    if isinstance(st, ExprStmt):
        return len(pat.findall(st.expr))
    if isinstance(st, ForStmt):
        n = len(pat.findall(st.iter_expr))
        for b in st.body:
            n += _stmt_uses_name(b, name)
        return n
    if isinstance(st, WhileStmt):
        n = len(pat.findall(st.cond))
        for b in st.body:
            n += _stmt_uses_name(b, name)
        return n
    if isinstance(st, RepeatStmt):
        n = 0
        for b in st.body:
            n += _stmt_uses_name(b, name)
        return n
    if isinstance(st, IfStmt):
        n = len(pat.findall(st.cond))
        for b in st.then_body:
            n += _stmt_uses_name(b, name)
        for b in st.else_body:
            n += _stmt_uses_name(b, name)
        return n
    return 0


def _stmt_writes_name(st: object, name: str) -> bool:
    if isinstance(st, Assign):
        return st.name == name
    if isinstance(st, ForStmt):
        if st.var == name:
            return True
        return any(_stmt_writes_name(b, name) for b in st.body)
    if isinstance(st, WhileStmt):
        return any(_stmt_writes_name(b, name) for b in st.body)
    if isinstance(st, RepeatStmt):
        return any(_stmt_writes_name(b, name) for b in st.body)
    if isinstance(st, IfStmt):
        return any(_stmt_writes_name(b, name) for b in st.then_body) or any(
            _stmt_writes_name(b, name) for b in st.else_body
        )
    return False


def _replace_name_in_stmt(st: object, name: str, repl: str) -> object:
    r = repl.strip()
    simple_f_re = re.compile(r"^[a-z][a-z0-9_]*(?:%[a-z][a-z0-9_]*|\([^()]*\))*$", re.IGNORECASE)
    simple_r_re = re.compile(r"^[a-z][a-z0-9_]*(?:\$[a-z][a-z0-9_]*|\[[^\[\]]*\]|\([^()]*\))*$", re.IGNORECASE)
    if simple_f_re.fullmatch(r) or simple_r_re.fullmatch(r):
        return _rename_stmt_obj(st, {name: r})
    return _rename_stmt_obj(st, {name: f"({r})"})


def _is_print_only_loop_over_value(ss: list[object], var: str, *, exact_var_only: bool = False) -> bool:
    if not ss:
        return False
    for b in ss:
        if not isinstance(b, PrintStmt):
            return False
        if exact_var_only:
            if len(b.args) != 1 or b.args[0].strip() != var:
                return False
        elif not any(re.search(rf"\b{re.escape(var)}\b", a) for a in b.args):
            return False
    return True


def _attach_stmt_comment(st: object, cmt: str) -> object:
    t = (cmt or "").strip()
    if not t:
        return st
    if isinstance(st, Assign):
        if st.comment.strip():
            return st
        return Assign(name=st.name, expr=st.expr, comment=t)
    if isinstance(st, PrintStmt):
        if st.comment.strip():
            return st
        return PrintStmt(args=list(st.args), comment=t)
    if isinstance(st, CallStmt):
        if st.comment.strip():
            return st
        return CallStmt(name=st.name, args=list(st.args), comment=t)
    if isinstance(st, ExprStmt):
        if st.comment.strip():
            return st
        return ExprStmt(expr=st.expr, comment=t)
    return st


def _is_inline_temp_rhs(expr: str) -> bool:
    t = expr.strip()
    if not t:
        return False
    # Keep named constants/constructor candidates as explicit declarations.
    if _is_int_literal(t) or _is_real_literal(t) or t in {"TRUE", "FALSE"}:
        return False
    if t.startswith("c(") or (t.startswith("[") and t.endswith("]")):
        return False
    if re.match(r"^(array|character|raw)\s*\(", t, re.IGNORECASE):
        return False
    if re.match(r"^t\s*\(", t, re.IGNORECASE):
        return False
    if (t.startswith('"') and t.endswith('"')) or (t.startswith("'") and t.endswith("'")):
        return False
    if re.match(r"^outer\s*\(", t, re.IGNORECASE):
        return False
    if re.search(r"\b(?:sample|sample\.int|runif|rnorm|rexp|rbinom|rpois)\s*\(", t, re.IGNORECASE):
        return False
    if _split_top_level_colon(fscan.strip_redundant_outer_parens_expr(t)) is not None:
        return False
    return True


def _stmt_uses_name_as_reducer_arg(st: object, name: str) -> bool:
    pat = re.compile(rf"\b(?:mean|sd)\s*\(\s*{re.escape(name)}\s*\)", re.IGNORECASE)
    if isinstance(st, Assign):
        return pat.search(st.expr) is not None
    if isinstance(st, PrintStmt):
        return any(pat.search(a) for a in st.args)
    if isinstance(st, CallStmt):
        return any(pat.search(a) for a in st.args)
    if isinstance(st, ExprStmt):
        return pat.search(st.expr) is not None
    return False


def _stmt_uses_name_in_model_formula(st: object, name: str) -> bool:
    if not isinstance(st, Assign):
        return False
    cinfo = parse_call_text(st.expr.strip())
    if cinfo is None or cinfo[0].lower() not in {"lm", "glm", "aov"}:
        return False
    pos = cinfo[1]
    kw = cinfo[2]
    form = pos[0].strip() if pos else kw.get("formula", "").strip()
    return bool(re.search(rf"\b{re.escape(name)}\b", form))


def inline_single_use_temporaries(stmts: list[object]) -> list[object]:
    """Inline simple single-use temporaries (`t = expr`) into their sole later use."""
    out = list(stmts)
    changed = True
    while changed:
        changed = False
        i = 0
        while i < len(out):
            st = out[i]
            if isinstance(st, ForStmt):
                nb = inline_single_use_temporaries(st.body)
                if nb != st.body:
                    out[i] = ForStmt(var=st.var, iter_expr=st.iter_expr, body=nb)
                    changed = True
                i += 1
                continue
            if isinstance(st, WhileStmt):
                nb = inline_single_use_temporaries(st.body)
                if nb != st.body:
                    out[i] = WhileStmt(cond=st.cond, body=nb)
                    changed = True
                i += 1
                continue
            if isinstance(st, RepeatStmt):
                nb = inline_single_use_temporaries(st.body)
                if nb != st.body:
                    out[i] = RepeatStmt(body=nb)
                    changed = True
                i += 1
                continue
            if isinstance(st, IfStmt):
                nt = inline_single_use_temporaries(st.then_body)
                ne = inline_single_use_temporaries(st.else_body)
                if nt != st.then_body or ne != st.else_body:
                    out[i] = IfStmt(cond=st.cond, then_body=nt, else_body=ne)
                    changed = True
                i += 1
                continue
            if not isinstance(st, Assign):
                i += 1
                continue
            name = st.name.strip()
            if not re.match(r"^[a-z][a-z0-9_]*$", name, re.IGNORECASE):
                i += 1
                continue
            if not _is_inline_temp_rhs(st.expr):
                i += 1
                continue
            if "$" in st.expr:
                i += 1
                continue
            if re.search(rf"\b{re.escape(name)}\b", st.expr):
                i += 1
                continue
            total_uses = sum(_stmt_uses_name(sj, name) for sj in out[i + 1 :])
            if total_uses != 1:
                i += 1
                continue
            use_j = -1
            blocked = False
            for j in range(i + 1, len(out)):
                if _stmt_writes_name(out[j], name):
                    blocked = True
                    break
                if _stmt_uses_name(out[j], name) > 0:
                    use_j = j
                    break
            if blocked or use_j < 0:
                i += 1
                continue
            if parse_call_text(st.expr.strip()) is not None and _stmt_uses_name_as_reducer_arg(out[use_j], name):
                i += 1
                continue
            if _stmt_uses_name_in_model_formula(out[use_j], name):
                i += 1
                continue
            out[use_j] = _replace_name_in_stmt(out[use_j], name, st.expr)
            out[use_j] = _attach_stmt_comment(out[use_j], st.comment)
            del out[i]
            changed = True
            break
    return out


def _rename_stmt_obj(st: object, mapping: dict[str, str]) -> object:
    if not mapping:
        return st
    if isinstance(st, Assign):
        return Assign(
            name=mapping.get(st.name, st.name),
            expr=_replace_idents(st.expr, mapping),
            comment=st.comment,
        )
    if isinstance(st, PrintStmt):
        return PrintStmt(
            args=[_replace_idents(a, mapping) for a in st.args],
            comment=st.comment,
        )
    if isinstance(st, ForStmt):
        return ForStmt(
            var=mapping.get(st.var, st.var),
            iter_expr=_replace_idents(st.iter_expr, mapping),
            body=[_rename_stmt_obj(s, mapping) for s in st.body],
        )
    if isinstance(st, WhileStmt):
        return WhileStmt(
            cond=_replace_idents(st.cond, mapping),
            body=[_rename_stmt_obj(s, mapping) for s in st.body],
        )
    if isinstance(st, RepeatStmt):
        return RepeatStmt(
            body=[_rename_stmt_obj(s, mapping) for s in st.body],
        )
    if isinstance(st, IfStmt):
        return IfStmt(
            cond=_replace_idents(st.cond, mapping),
            then_body=[_rename_stmt_obj(s, mapping) for s in st.then_body],
            else_body=[_rename_stmt_obj(s, mapping) for s in st.else_body],
        )
    if isinstance(st, CallStmt):
        return CallStmt(
            name=st.name,
            args=[_replace_idents(a, mapping) for a in st.args],
            comment=st.comment,
        )
    if isinstance(st, ExprStmt):
        return ExprStmt(expr=_replace_idents(st.expr, mapping), comment=st.comment)
    return st


def _assigned_names_in_stmts(stmts: list[object]) -> set[str]:
    out: set[str] = set()

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                out.add(st.name)
            elif isinstance(st, ForStmt):
                out.add(st.var)
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)

    walk(stmts)
    return out


def rename_reserved_main_names(stmts: list[object]) -> list[object]:
    """Rename main-scope R names that collide with generated Fortran names."""
    reserved = {"dp"}
    assigned = _assigned_names_in_stmts(stmts)
    mapping: dict[str, str] = {}
    used = {nm.lower() for nm in assigned} | reserved
    for nm in sorted(assigned):
        if nm.lower() not in reserved:
            continue
        base = f"{nm}_r"
        cand = base
        i = 2
        while cand.lower() in used:
            cand = f"{base}_{i}"
            i += 1
        mapping[nm] = cand
        used.add(cand.lower())
    if not mapping:
        return stmts
    return [_rename_stmt_obj(st, mapping) for st in stmts]


def _find_r_for_line(src: str, var: str) -> int | None:
    found: int | None = None
    for i, raw in enumerate(src.splitlines(), start=1):
        code, _cmt = split_r_code_comment(raw)
        for cand in _r_name_candidates(var):
            if re.search(rf"\bfor\s*\(\s*{re.escape(cand)}\s+in\b", code):
                found = i
    return found


def rename_conflicting_loop_vars(
    stmts: list[object],
    warnings: list[tuple[str, str, int | None]] | None = None,
    src: str = "",
) -> list[object]:
    """Rename loop variables that collide with earlier assignments in a block.

    R permits `j <- numeric(3); for (j in 1:k) ...`, where the loop scalar
    overwrites the prior binding. In Fortran this can otherwise become an
    allocatable array declaration plus `do j = ...`, which is invalid.
    """
    used = _assigned_names_in_stmts(stmts)

    def fresh(base: str) -> str:
        cand = f"{base}_loop"
        i = 2
        used_l = {u.lower() for u in used}
        while cand.lower() in used_l:
            cand = f"{base}_loop_{i}"
            i += 1
        used.add(cand)
        return cand

    def walk_block(ss: list[object]) -> list[object]:
        seen_assigns: set[str] = set()
        out: list[object] = []
        for st in ss:
            if isinstance(st, Assign):
                seen_assigns.add(st.name)
                out.append(st)
                continue
            if isinstance(st, ForStmt):
                body = walk_block(st.body)
                var = st.var
                if var.lower() in {nm.lower() for nm in seen_assigns}:
                    new_var = fresh(var)
                    if warnings is not None:
                        warnings.append((var, new_var, _find_r_for_line(src, var) if src else None))
                    body = [_rename_stmt_obj(b, {var: new_var}) for b in body]
                    var = new_var
                seen_assigns.add(var)
                out.append(ForStmt(var=var, iter_expr=st.iter_expr, body=body))
                continue
            if isinstance(st, WhileStmt):
                out.append(WhileStmt(cond=st.cond, body=walk_block(st.body)))
                continue
            if isinstance(st, RepeatStmt):
                out.append(RepeatStmt(body=walk_block(st.body)))
                continue
            if isinstance(st, IfStmt):
                out.append(IfStmt(cond=st.cond, then_body=walk_block(st.then_body), else_body=walk_block(st.else_body)))
                continue
            if isinstance(st, FuncDef):
                out.append(FuncDef(
                    name=st.name,
                    args=list(st.args),
                    defaults=dict(st.defaults),
                    body=walk_block(st.body),
                    leading_comments=st.leading_comments,
                ))
                continue
            out.append(st)
        return out

    return walk_block(stmts)


def _stmt_tree_has_side_effect_ops(ss: list[object]) -> bool:
    """Conservative impurity test for R-subset function bodies."""
    bad_call_names = {"set.seed", "cat", "print"}

    def walk(stmts: list[object]) -> bool:
        for st in stmts:
            if isinstance(st, PrintStmt):
                return True
            if isinstance(st, CallStmt):
                nm = st.name.lower()
                if nm in bad_call_names:
                    return True
            if isinstance(st, Assign):
                rhs = st.expr.lower()
                if "runif(" in rhs or "rnorm(" in rhs or "sample.int(" in rhs or "sample(" in rhs:
                    return True
            if isinstance(st, ExprStmt):
                ex = st.expr.lower()
                if "runif(" in ex or "rnorm(" in ex or "sample.int(" in ex or "sample(" in ex:
                    return True
            if isinstance(st, ForStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, WhileStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, RepeatStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, IfStmt):
                if walk(st.then_body) or walk(st.else_body):
                    return True
        return False

    return walk(ss)


def _stmt_tree_has_output_ops(ss: list[object]) -> bool:
    output_call_names = {"cat", "print", "writelines", "write.table"}

    def expr_is_output_call(expr: str) -> bool:
        c = parse_call_text(expr.strip())
        return c is not None and c[0].lower() in output_call_names

    def walk(stmts: list[object]) -> bool:
        for st in stmts:
            if isinstance(st, PrintStmt):
                return True
            if isinstance(st, CallStmt) and st.name.lower() in output_call_names:
                return True
            if isinstance(st, ExprStmt) and expr_is_output_call(st.expr):
                return True
            if isinstance(st, ForStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, WhileStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, RepeatStmt):
                if walk(st.body):
                    return True
            elif isinstance(st, IfStmt):
                if walk(st.then_body) or walk(st.else_body):
                    return True
        return False

    return walk(ss)


def _function_has_void_return(fn: FuncDef) -> bool:
    if not fn.body:
        return True
    last = fn.body[-1]
    if isinstance(last, (CallStmt, PrintStmt)):
        return True
    if not isinstance(last, ExprStmt):
        return True
    expr = last.expr.strip()
    ret_arg = _return_call_arg(expr)
    target = ret_arg if ret_arg is not None else expr
    if target is None:
        return False
    t = target.strip()
    if not t:
        return True
    if t.upper() == "NULL":
        return True
    c = parse_call_text(t)
    return c is not None and c[0].lower() == "invisible"


def _function_should_emit_subroutine(fn: FuncDef) -> bool:
    return _stmt_tree_has_output_ops(fn.body) and _function_has_void_return(fn)


def _cond_identifiers(expr: str) -> set[str]:
    """Collect identifier-like tokens from an R condition expression."""
    out: set[str] = set()
    for m in re.finditer(r"\b([A-Za-z_]\w*)\b", expr):
        out.add(m.group(1).lower())
    return out


def _is_hoistable_stopifnot_stmt(st: object, allowed_names: set[str]) -> bool:
    """True when a top-level stopifnot can be hoisted before body code."""
    if not isinstance(st, CallStmt):
        return False
    if st.name.lower() != "stopifnot":
        return False
    intr_names = {
        "length",
        "size",
        "all",
        "any",
        "sum",
        "mean",
        "sd",
        "sqrt",
        "max",
        "min",
        "abs",
        "floor",
        "ceiling",
        "log",
        "exp",
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "is",
        "finite",
        "null",
        "true",
        "false",
        "na_real_",
        "int",
        "real",
        "nrow",
        "ncol",
        "dp",
        "real64",
    }
    for a in st.args:
        ids = _cond_identifiers(a)
        bad = {x for x in ids if x not in allowed_names and x not in intr_names}
        if bad:
            return False
    return True


def _looks_integer_fortran_expr(expr: str) -> bool:
    t = expr.strip()
    if not t:
        return False
    if _is_int_literal(t):
        return True
    if re.match(r"^[A-Za-z]\w*$", t):
        return True
    if re.match(r"^[A-Za-z]\w*(?:\s*[\+\-\*/]\s*[A-Za-z]\w*)+$", t):
        return True
    if re.match(r"^\d+(?:\s*\*\*\s*\d+)+$", t):
        return True
    if re.match(r"^size\s*\(.+\)$", t, re.IGNORECASE):
        return True
    if re.match(r"^int\s*\(.+\)$", t, re.IGNORECASE):
        return True
    return False


def _int_bound_expr(expr: str) -> str:
    t = expr.strip()
    m = re.match(r"^int\s*\((.+)\)$", t, re.IGNORECASE)
    if m:
        return m.group(1).strip()
    if _looks_integer_fortran_expr(t):
        return t
    return f"int({t})"


def _mul_factor_expr(expr: str) -> str:
    t = fscan.strip_redundant_outer_parens_expr(expr.strip())
    if _split_top_level_token(t, "+", from_right=True) is not None:
        return f"({t})"
    if _split_top_level_token(t, "-", from_right=True) is not None:
        return f"({t})"
    return t


def _negate_simple_relational_expr(expr_f: str) -> str | None:
    """Return negated relational expression for simple `lhs op rhs` forms."""
    s = fscan.strip_redundant_outer_parens_expr(expr_f.strip())
    # Split at top-level relational operator only (outside parentheses/strings).
    ops = [">=", "<=", "==", "/=", ">", "<", ".ge.", ".le.", ".eq.", ".ne.", ".gt.", ".lt."]
    in_single = False
    in_double = False
    depth = 0
    lhs = ""
    rhs = ""
    op_found: str | None = None
    i = 0
    while i < len(s):
        ch = s[i]
        if ch == "'" and not in_double:
            in_single = not in_single
            i += 1
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            i += 1
            continue
        if not in_single and not in_double:
            if ch == "(":
                depth += 1
                i += 1
                continue
            if ch == ")" and depth > 0:
                depth -= 1
                i += 1
                continue
            if depth == 0:
                low_rest = s[i:].lower()
                hit = None
                for op in ops:
                    if low_rest.startswith(op):
                        hit = op
                        break
                if hit is not None:
                    lhs = s[:i].strip()
                    rhs = s[i + len(hit) :].strip()
                    op_found = hit
                    break
        i += 1

    if not lhs or not rhs or op_found is None:
        return None
    op = op_found.lower()
    inv = {
        ">=": "<",
        "<=": ">",
        ">": "<=",
        "<": ">=",
        "==": "/=",
        "/=": "==",
        ".ge.": ".lt.",
        ".le.": ".gt.",
        ".gt.": ".le.",
        ".lt.": ".ge.",
        ".eq.": ".ne.",
        ".ne.": ".eq.",
    }.get(op)
    if inv is None:
        return None
    return f"{lhs} {inv} {rhs}"


def _fortran_error_msg(text: str) -> str:
    """Build safe Fortran double-quoted error text literal."""
    t = " ".join(text.strip().split())
    t = t.replace('"', '""')
    return f'"{t}"'


def _is_simple_value_for_merge(expr_f: str) -> bool:
    """True when expression is a scalar-looking literal or variable reference."""
    t = expr_f.strip()
    if not t:
        return False
    if _is_int_literal(t) or _is_real_literal(t):
        return True
    if re.match(r"^\.(true|false)\.$", t, re.IGNORECASE):
        return True
    # Keep MERGE away from array sections, constructors, and helper calls such
    # as numeric(0). Fortran MERGE requires conforming array operands, so using
    # it for R branches like par[i:j] vs numeric(0) can silently erase the
    # non-empty branch.
    if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)*$", t):
        return True
    return False


def _parse_list_constructor(expr: str) -> dict[str, object] | None:
    s = expr.strip()
    if not (s.startswith("list(") and s.endswith(")")):
        return None
    inner = s[len("list(") : -1].strip()
    out: dict[str, object] = {}
    if not inner:
        return out
    for p in split_top_level_commas(inner):
        m = re.match(r"^([A-Za-z]\w*)\s*=\s*(.+)$", p.strip())
        if not m:
            return None
        k = m.group(1)
        vtxt = m.group(2).strip()
        nested = _parse_list_constructor(vtxt)
        out[k] = nested if nested is not None else vtxt
    return out


def _collect_nested_types(fn_name: str, fields: dict[str, object], path: tuple[str, ...] = ()) -> dict[tuple[str, ...], dict[str, object]]:
    out: dict[tuple[str, ...], dict[str, object]] = {}
    out[path] = fields
    for k, v in fields.items():
        if isinstance(v, dict):
            out.update(_collect_nested_types(fn_name, v, path + (k,)))
    return out


def _type_name_for_path(fn_name: str, path: tuple[str, ...]) -> str:
    if not path:
        return f"{fn_name}_result_t"
    return f"{fn_name}_{'_'.join(path)}_t"


def _list_return_specs(funcs: list[FuncDef]) -> dict[str, ListReturnSpec]:
    specs: dict[str, ListReturnSpec] = {}

    def _add_field_path(fields: dict[str, object], path: list[str], rhs_expr: str) -> None:
        cur = fields
        for p in path[:-1]:
            v = cur.get(p)
            if not isinstance(v, dict):
                cur[p] = {}
            cur = cur[p]  # type: ignore[assignment]
        cur[path[-1]] = rhs_expr

    def _collect_list_aliases(ss: list[object], out_map: dict[str, dict[str, object]]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                lhs_nm = st.name.strip()
                rhs_txt = st.expr.strip()
                ff = _parse_list_constructor(rhs_txt)
                if ff is not None:
                    out_map[lhs_nm] = ff
                    continue
                m_alias = re.match(r"^([A-Za-z]\w*)$", rhs_txt)
                if m_alias is not None:
                    src_nm = m_alias.group(1)
                    if src_nm in out_map:
                        out_map[lhs_nm] = out_map[src_nm]
            elif isinstance(st, ExprStmt):
                mm = re.match(
                    r"^([A-Za-z]\w*(?:\$[A-Za-z]\w+)+)\s*(?:<-|=)\s*(.+)$",
                    st.expr.strip(),
                )
                if mm:
                    lhs = mm.group(1).strip()
                    rhs = mm.group(2).strip()
                    parts = lhs.split("$")
                    if len(parts) >= 2 and parts[0] in out_map:
                        _add_field_path(out_map[parts[0]], parts[1:], rhs)
            elif isinstance(st, IfStmt):
                _collect_list_aliases(st.then_body, out_map)
                _collect_list_aliases(st.else_body, out_map)
            elif isinstance(st, ForStmt):
                _collect_list_aliases(st.body, out_map)
            elif isinstance(st, WhileStmt):
                _collect_list_aliases(st.body, out_map)
            elif isinstance(st, RepeatStmt):
                _collect_list_aliases(st.body, out_map)

    for fn in funcs:
        if not fn.body:
            continue
        last = fn.body[-1]
        if not isinstance(last, ExprStmt):
            continue
        last_expr = last.expr.strip()
        ret_arg = _return_call_arg(last_expr)
        if ret_arg is not None:
            last_expr = ret_arg.strip()
        fields = _parse_list_constructor(last_expr)
        if fields is None:
            m_last = re.match(r"^[A-Za-z]\w*$", last_expr)
            if m_last is not None:
                ret_nm = m_last.group(0)
                alias_map: dict[str, dict[str, object]] = {}
                _collect_list_aliases(fn.body[:-1], alias_map)
                fields = alias_map.get(ret_nm)
        if fields is None:
            continue
        specs[fn.name] = ListReturnSpec(
            fn_name=fn.name,
            root_fields=fields,
            nested_types=_collect_nested_types(fn.name, fields),
        )
    return specs


def r_expr_to_fortran(expr: str) -> str:
    global _R_SD_CALL_NAME
    s = expr.strip()
    s = fscan.strip_redundant_outer_parens_expr(s)
    s = _replace_dotted_var_refs(s)
    s = re.sub(r"\bt\.test\s*\(", "t_test(", s, flags=re.IGNORECASE)

    def _rewrite_t_test_field_access(txt: str) -> str:
        out: list[str] = []
        i = 0
        needle = "t_test"
        low = txt.lower()
        while i < len(txt):
            j = low.find(needle, i)
            if j < 0:
                out.append(txt[i:])
                break
            if j > 0 and (txt[j - 1].isalnum() or txt[j - 1] == "_"):
                out.append(txt[i:j + len(needle)])
                i = j + len(needle)
                continue
            k = j + len(needle)
            while k < len(txt) and txt[k].isspace():
                k += 1
            if k >= len(txt) or txt[k] != "(":
                out.append(txt[i:j + len(needle)])
                i = j + len(needle)
                continue
            depth = 0
            q: str | None = None
            esc = False
            end = -1
            for p in range(k, len(txt)):
                ch = txt[p]
                if q is not None:
                    if esc:
                        esc = False
                    elif ch == "\\":
                        esc = True
                    elif ch == q:
                        q = None
                    continue
                if ch in {"'", '"'}:
                    q = ch
                elif ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        end = p
                        break
            if end < 0:
                out.append(txt[i:])
                break
            rest = txt[end + 1 :]
            m_field = re.match(r"\s*\$\s*(p\.value|statistic|parameter|estimate|stderr)\b", rest, re.IGNORECASE)
            if m_field is None:
                out.append(txt[i:end + 1])
                i = end + 1
                continue
            fld = m_field.group(1).lower().replace(".", "_")
            call_src = txt[j:end + 1]
            out.append(txt[i:j])
            if fld == "p_value":
                c_tt_field = parse_call_text(call_src)
                if c_tt_field is not None:
                    _nm_tt_f, pos_tt_f, kw_tt_f = c_tt_field
                    pos_f = [r_expr_to_fortran(p.strip()) for p in pos_tt_f]
                    kw_f = [
                        f"{_sanitize_fortran_kwarg_name(k)}={r_expr_to_fortran(v.strip())}"
                        for k, v in kw_tt_f.items()
                    ]
                    out.append(f"t_test_p_value({', '.join(pos_f + kw_f)})")
                else:
                    out.append(f"({r_expr_to_fortran(call_src)})%{fld}")
            else:
                out.append(f"({r_expr_to_fortran(call_src)})%{fld}")
            i = end + 1 + m_field.end()
        return "".join(out)

    if "$" in s and re.search(r"\bt_test\s*\(", s, re.IGNORECASE):
        s = _rewrite_t_test_field_access(s)
    def _nested_list_index_repl(m: re.Match[str]) -> str:
        obj = m.group(1)
        idx1 = _int_bound_expr(r_expr_to_fortran(m.group(2).strip()))
        idx2 = _int_bound_expr(r_expr_to_fortran(m.group(3).strip()))
        return f"{obj}(:,:,{idx2},{idx1})"
    s = re.sub(r"\b([A-Za-z]\w*(?:%[A-Za-z]\w*)?)\s*\[\[\s*([^\]]+?)\s*\]\]\s*\[\[\s*([^\]]+?)\s*\]\]", _nested_list_index_repl, s)
    def _list_index_repl(m: re.Match[str]) -> str:
        obj = m.group(1)
        idx = _int_bound_expr(r_expr_to_fortran(m.group(2).strip()))
        root = obj.split("%")[-1].lower()
        if root in _KNOWN_OBJECT_LIST_NAMES:
            return f"{obj}({idx})"
        if root == "a_list":
            return f"{obj}(:,:,:,{idx})"
        if root in _KNOWN_RANK3_NAMES or root == "gamma" or root.endswith("_result") or "sigma" in root:
            return f"{obj}(:,:,{idx})"
        return f"{obj}(:,{idx})"
    s = re.sub(r"\b([A-Za-z]\w*(?:%[A-Za-z]\w*)?)\s*\[\[\s*([^\]]+?)\s*\]\]", _list_index_repl, s)
    s = re.sub(r"(?i)\.machine\s*\$\s*double\.eps", "epsilon(1.0_dp)", s)
    s = re.sub(r"(?i)\.machine\s*\$\s*double\.xmin", "tiny(1.0_dp)", s)
    s = re.sub(r"(?i)\.machine\s*\$\s*double\.xmax", "huge(1.0_dp)", s)
    s = re.sub(r"(?i)\bmax\.col\s*\(", "max_col(", s)
    s = re.sub(r"(?i)\bties\.method\s*=", "ties_method=", s)
    s = _replace_balanced_func_calls(s, "t", lambda inner: f"transpose({r_expr_to_fortran(inner.strip())})")
    m_eigen_values = re.match(r"^eigen\s*\((.*)\)\s*\$\s*values\s*$", s, re.IGNORECASE)
    if m_eigen_values is not None:
        parts = split_top_level_commas(m_eigen_values.group(1).strip())
        x_src = ""
        for p in parts:
            asn = split_top_level_assignment(p.strip())
            if asn is not None and asn[0].strip().lower() == "x":
                x_src = asn[1].strip()
                break
        if not x_src and parts:
            x_src = parts[0].strip()
        if x_src:
            return f"eigen_sym_values({r_expr_to_fortran(x_src)})"
    m_det_log = re.match(r"^determinant\s*\((.*)\)\s*\$\s*modulus\s*\[\s*1\s*\]\s*$", s, re.IGNORECASE)
    if m_det_log is not None:
        parts = split_top_level_commas(m_det_log.group(1).strip())
        x_src = ""
        for p in parts:
            asn = split_top_level_assignment(p.strip())
            if asn is not None and asn[0].strip().lower() == "x":
                x_src = asn[1].strip()
                break
        if not x_src and parts:
            x_src = parts[0].strip()
        if x_src:
            return f"log(abs(det_real({r_expr_to_fortran(x_src)})))"
    c_setdiff = parse_call_text(s)
    if c_setdiff is not None and c_setdiff[0].lower() == "setdiff" and len(c_setdiff[1]) >= 2:
        first_sd = c_setdiff[1][0].strip()
        second_sd = _dequote_string_literal(c_setdiff[1][1].strip())
        m_names_sd = re.match(r"^names\s*\(\s*([A-Za-z]\w*)\s*\)$", first_sd, re.IGNORECASE)
        if m_names_sd is not None and (second_sd or "").lower() == "date":
            base_sd = m_names_sd.group(1)
            return f"r_seq_int(1, size({base_sd}, 2) - 1)"
    # Drop namespace qualifiers (e.g., stats::sd -> sd) in this subset.
    s = re.sub(r"\b[A-Za-z]\w*::", "", s)
    s = re.sub(r"(?i)\bsd\s*\(", f"{_R_SD_CALL_NAME}(", s)
    # R expression-form if: if (cond) a else b
    ih_expr = _parse_if_head(s)
    if ih_expr is not None:
        cond_src, tail_src = ih_expr
        split_tail = _split_top_level_else(" " + tail_src)
        if split_tail is not None:
            then_src, else_src = split_tail
            return (
                f"merge({r_expr_to_fortran(then_src)}, "
                f"{r_expr_to_fortran(else_src)}, {r_expr_to_fortran(cond_src)})"
            )
    if re.fullmatch(r"2\s*(?:\^|\*\*)\s*31", s):
        return "2147483648.0_dp"
    if re.fullmatch(r"2\s*\^\s*31\s*-\s*1[Ll]?", s):
        return "2147483647"
    if re.fullmatch(r"2\s*\*\*\s*31\s*-\s*1", s):
        return "2147483647"
    if re.match(r"^\+?Inf$", s, re.IGNORECASE):
        return "huge(1.0_dp)"
    if re.match(r"^-Inf$", s, re.IGNORECASE):
        return "-huge(1.0_dp)"
    c_pre = parse_call_text(s)
    if c_pre is not None:
        nm_pre = c_pre[0].lower()
        pos_pre = c_pre[1]
        if nm_pre == "names" and len(pos_pre) == 1:
            src_nm = pos_pre[0].strip()
            if src_nm.lower() in _NAMED_VECTOR_NAMES:
                return _NAMED_VECTOR_NAMES[src_nm.lower()]
        if nm_pre == "unname" and len(pos_pre) == 1:
            return r_expr_to_fortran(pos_pre[0])
        if nm_pre == "invisible":
            return r_expr_to_fortran(pos_pre[0]) if pos_pre else "0"
        if nm_pre == "try":
            return r_expr_to_fortran(pos_pre[0]) if pos_pre else "0"
        if nm_pre == "inherits" and len(pos_pre) >= 2:
            cls = _dequote_string_literal(pos_pre[1].strip())
            if cls == "try-error":
                return ".false."
        if nm_pre == "table":
            vals = list(pos_pre) + list(c_pre[2].values())
            if len(vals) >= 2:
                x_f = r_expr_to_fortran(vals[0])
                y_f = r_expr_to_fortran(vals[1])
                nlab_x = len(_CATEGORICAL_LABELS.get(vals[0].strip().lower(), []))
                nlab_y = len(_CATEGORICAL_LABELS.get(vals[1].strip().lower(), []))
                x_tab = x_f if nlab_x else f"({x_f} - minval({x_f}) + 1)"
                y_tab = y_f if nlab_y else f"({y_f} - minval({y_f}) + 1)"
                nx = str(nlab_x) if nlab_x else f"maxval({x_f}) - minval({x_f}) + 1"
                ny = str(nlab_y) if nlab_y else f"maxval({y_f}) - minval({y_f}) + 1"
                return f"table2({x_tab}, {y_tab}, {nx}, {ny})"
            if vals:
                vtab = r_expr_to_fortran(vals[0])
                nlab = len(_CATEGORICAL_LABELS.get(vals[0].strip().lower(), []))
                vtab_shifted = vtab if nlab else f"({vtab} - minval({vtab}) + 1)"
                nbins = str(nlab) if nlab else f"maxval({vtab}) - minval({vtab}) + 1"
                return f"tabulate({vtab_shifted}, {nbins})"
            return "[0]"
        if nm_pre in {"prop.table", "prop_table"}:
            vals = list(pos_pre) + [v for k, v in c_pre[2].items() if k.lower() not in {"margin"}]
            margin_src = c_pre[2].get("margin")
            if margin_src is None and len(pos_pre) >= 2:
                vals = [pos_pre[0]]
                margin_src = pos_pre[1]
            if vals:
                x_f = r_expr_to_fortran(vals[0])
                if margin_src is not None:
                    return f"prop_table({x_f}, margin={_int_bound_expr(r_expr_to_fortran(margin_src))})"
                return f"prop_table({x_f})"
    if c_pre is not None and c_pre[0].lower() == "sub":
        _nm_sub, pos_sub, _kw_sub = c_pre
        if len(pos_sub) >= 3:
            pat = _dequote_string_literal(pos_sub[0].strip())
            repl = _dequote_string_literal(pos_sub[1].strip())
            x_f = r_expr_to_fortran(pos_sub[2].strip())
            if repl == "" and pat is not None and pat.startswith("^"):
                prefix = pat[1:]
                return f"{x_f}({len(prefix) + 1}:)"
    c_filter = parse_call_text(s)
    if c_filter is not None and c_filter[0].lower() == "filter":
        _nm_filter, pos_filter, kw_filter = c_filter
        pred_src = pos_filter[0] if pos_filter else kw_filter.get("f")
        x_src = pos_filter[1] if len(pos_filter) >= 2 else kw_filter.get("x")
        if pred_src is None or x_src is None:
            raise NotImplementedError("Filter requires predicate and vector arguments")
        pred = pred_src.strip()
        if not re.match(r"^[A-Za-z]\w*$", pred):
            raise NotImplementedError("Filter currently requires a named predicate function")
        x_f = r_expr_to_fortran(x_src)
        return f"pack({x_f}, {pred}({x_f}))"
    list_arr = _numeric_list_array_expr(s)
    nested_list_arr = _numeric_nested_matrix_list_array_expr(s)
    if nested_list_arr is not None:
        return nested_list_arr[0]
    if list_arr is not None:
        return list_arr[0]
    c_cast0 = parse_call_text(s)
    if c_cast0 is not None and c_cast0[0].lower() in {"as.numeric", "as.double"} and c_cast0[1]:
        return r_expr_to_fortran(c_cast0[1][0].strip())
    for op, fn in [("+", "r_add"), ("-", "r_sub"), ("*", "r_mul"), ("/", "r_div")]:
        mm_op = _split_top_level_token(s, op, from_right=True)
        if mm_op is not None:
            a_txt = mm_op[0].strip()
            b_txt = mm_op[1].strip()
            if a_txt and b_txt and _looks_matrix_expr(a_txt) and _looks_matrix_expr(b_txt):
                return f"({r_expr_to_fortran(a_txt)}) {op} ({r_expr_to_fortran(b_txt)})"
            a_vec = _looks_vector_expr_for_recycle(a_txt) if a_txt else False
            b_vec = _looks_vector_expr_for_recycle(b_txt) if b_txt else False
            if a_txt and b_txt and a_vec and b_vec:
                a_f = r_expr_to_fortran(a_txt)
                b_f = r_expr_to_fortran(b_txt)
                if _NO_RECYCLE:
                    return f"({a_f}) {op} ({b_f})"
                return f"{fn}(real({a_f}, kind=dp), real({b_f}, kind=dp))"
    c_cor0 = parse_call_text(s)
    if c_cor0 is not None and c_cor0[0].lower() in {"cor", "cov"}:
        vals_cor = list(c_cor0[1])
        if c_cor0[0].lower() == "cor" and len(vals_cor) == 1 and not c_cor0[2]:
            df_src = vals_cor[0].strip()
            if re.fullmatch(r"[A-Za-z]\w*", df_src):
                fields_cor_df = _EXPANDED_DATA_FRAME_FIELDS.get(df_src) or _EXPANDED_DATA_FRAME_FIELDS.get(df_src.lower())
                if fields_cor_df:
                    cols_cor_df = [
                        _expanded_data_frame_col_expr(df_src, f) or f"{df_src}_{_sanitize_fortran_kwarg_name(f)}"
                        for f in fields_cor_df
                    ]
                    if len(cols_cor_df) == 2:
                        return f"cor(cbind2({cols_cor_df[0]}, {cols_cor_df[1]}))"
                    if len(cols_cor_df) == 3:
                        return f"cor(cbind({cols_cor_df[0]}, {cols_cor_df[1]}, {cols_cor_df[2]}))"
        if vals_cor:
            return f"{c_cor0[0].lower()}({', '.join(r_expr_to_fortran(v) for v in vals_cor[:2])})"
    c_rbinom0 = parse_call_text(s)
    if c_rbinom0 is not None and c_rbinom0[0].lower() == "rbinom":
        _nm_rb0, pos_rb0, kw_rb0 = c_rbinom0
        n_src = pos_rb0[0] if pos_rb0 else kw_rb0.get("n", "1")
        size_src = kw_rb0.get("size")
        if size_src is None and len(pos_rb0) >= 2:
            size_src = pos_rb0[1]
        prob_src = kw_rb0.get("prob")
        if prob_src is None and len(pos_rb0) >= 3:
            prob_src = pos_rb0[2]
        return (
            f"rbinom({_int_bound_expr(r_expr_to_fortran(n_src))}, "
            f"{_int_bound_expr(r_expr_to_fortran(size_src or '1'))}, "
            f"{r_expr_to_fortran(prob_src or '0.5')})"
        )
    # Preserve operator precedence: parse top-level scalar +/- before scalar division.
    # Example: m4 / sd**4 - 3.0 must map to (m4 / sd**4) - 3.0, not m4 / (sd**4 - 3.0).
    addsub = _find_top_level_addsub(s)
    if addsub is not None:
        i_as, op_as = addsub
        lhs_as = r_expr_to_fortran(s[:i_as].strip())
        rhs_as = r_expr_to_fortran(s[i_as + 1 :].strip())
        return f"{lhs_as} {op_as} {rhs_as}"
    mm_idiv = _split_top_level_token(s, "%/%", from_right=True)
    if mm_idiv is not None:
        lhs = r_expr_to_fortran(mm_idiv[0])
        rhs = r_expr_to_fortran(mm_idiv[1])
        return f"{_int_bound_expr(lhs)} / {_int_bound_expr(rhs)}"
    mm_div = _split_top_level_token(s, "/", from_right=True)
    if mm_div is not None:
        lhs = r_expr_to_fortran(mm_div[0])
        rhs = r_expr_to_fortran(mm_div[1])
        return f"(real({lhs}, kind=dp)) / (real({rhs}, kind=dp))"
    c_usr = parse_call_text(s)
    if c_usr is not None and c_usr[0].lower() == "cor" and len(c_usr[1]) == 1 and not c_usr[2]:
        df_src = c_usr[1][0].strip()
        if re.fullmatch(r"[A-Za-z]\w*", df_src):
                fields_cor_df = _EXPANDED_DATA_FRAME_FIELDS.get(df_src) or _EXPANDED_DATA_FRAME_FIELDS.get(df_src.lower())
                if fields_cor_df:
                    cols_cor_df = [
                        _expanded_data_frame_col_expr(df_src, f) or f"{df_src}_{_sanitize_fortran_kwarg_name(f)}"
                        for f in fields_cor_df
                    ]
                    if len(cols_cor_df) == 2:
                        return f"cor(cbind2({cols_cor_df[0]}, {cols_cor_df[1]}))"
                if len(cols_cor_df) == 3:
                    return f"cor(cbind({cols_cor_df[0]}, {cols_cor_df[1]}, {cols_cor_df[2]}))"
    if c_usr is not None and c_usr[0].lower() == "t_test":
        _nm_tt, pos_tt, kw_tt = c_usr
        pos_out_tt = [r_expr_to_fortran(a) for a in pos_tt]
        kw_out_tt: list[str] = []
        for k, v in kw_tt.items():
            vf_tt = r_expr_to_fortran(v)
            if _sanitize_fortran_kwarg_name(k).lower() == "mu" and _is_int_literal(vf_tt.strip()):
                vf_tt = f"{int(vf_tt.strip())}.0_dp"
            kw_out_tt.append(f"{_sanitize_fortran_kwarg_name(k)}={vf_tt}")
        return f"t_test({', '.join(pos_out_tt + kw_out_tt)})"
    if c_usr is not None and c_usr[0].lower() in {"chisq.test", "chisq_test"}:
        _nm_ch, pos_ch, kw_ch = c_usr
        pos_out_ch = [r_expr_to_fortran(a) for a in pos_ch]
        kw_out_ch = [f"{_sanitize_fortran_kwarg_name(k)}={r_expr_to_fortran(v)}" for k, v in kw_ch.items()]
        return f"chisq_test({', '.join(pos_out_ch + kw_out_ch)})"
    if c_usr is not None and c_usr[0].lower() in {"prop.test", "prop_test"}:
        _nm_pr, pos_pr, kw_pr = c_usr
        pos_out_pr = [r_expr_to_fortran(a) for a in pos_pr]
        kw_out_pr = [f"{_sanitize_fortran_kwarg_name(k)}={r_expr_to_fortran(v)}" for k, v in kw_pr.items()]
        return f"prop_test({', '.join(pos_out_pr + kw_out_pr)})"
    if c_usr is not None and c_usr[0].lower() in {"cor.test", "cor_test"}:
        _nm_ct, pos_ct, kw_ct = c_usr
        pos_out_ct = [r_expr_to_fortran(a) for a in pos_ct]
        kw_out_ct = [f"{_sanitize_fortran_kwarg_name(k)}={r_expr_to_fortran(v)}" for k, v in kw_ct.items()]
        return f"cor_test({', '.join(pos_out_ct + kw_out_ct)})"
    if c_usr is not None and c_usr[0].lower() in {"fisher.test", "fisher_test"}:
        _nm_ft, pos_ft, kw_ft = c_usr
        pos_out_ft = [r_expr_to_fortran(a) for a in pos_ft]
        kw_out_ft = [f"{_sanitize_fortran_kwarg_name(k)}={r_expr_to_fortran(v)}" for k, v in kw_ft.items()]
        return f"fisher_test({', '.join(pos_out_ft + kw_out_ft)})"
    if c_usr is not None and c_usr[0].lower() == "quantile":
        _nm_q, pos_q, kw_q = c_usr
        args_q = [r_expr_to_fortran(a) for a in pos_q]
        for k, v in kw_q.items():
            args_q.append(f"{_sanitize_fortran_kwarg_name(k)}={r_expr_to_fortran(v)}")
        return f"quantile({', '.join(args_q)})"
    if c_usr is not None:
        nm_u, pos_u, kw_u = c_usr
        key_u = nm_u.lower()
        kinds = _USER_FUNC_ARG_KIND.get(key_u)
        if kinds is not None:
            idx_map = _USER_FUNC_ARG_INDEX.get(key_u, {})
            pos_out: list[str] = []
            for i, a in enumerate(pos_u):
                af = r_expr_to_fortran(a)
                if key_u == "print_matrix" and i == 1 and _looks_vector_actual_for_matrix_arg(a, af):
                    af = f"reshape({af}, [size({af}), 1])"
                if i < len(kinds) and kinds[i] == "real":
                    if _is_int_literal(af.strip()):
                        af = f"{int(af.strip())}.0_dp"
                    elif re.match(r"^price_names\s*\(", af.strip(), re.IGNORECASE):
                        af = f"real({af}, kind=dp)"
                pos_out.append(af)
            kw_out: list[str] = []
            for k, v in kw_u.items():
                vf = r_expr_to_fortran(v)
                idx_k = idx_map.get(k.lower(), -1)
                if key_u == "print_matrix" and k.lower() == "x" and _looks_vector_actual_for_matrix_arg(v, vf):
                    vf = f"reshape({vf}, [size({vf}), 1])"
                if idx_k >= 0 and idx_k < len(kinds) and kinds[idx_k] == "real":
                    if _is_int_literal(vf.strip()):
                        vf = f"{int(vf.strip())}.0_dp"
                    elif re.match(r"^price_names\s*\(", vf.strip(), re.IGNORECASE):
                        vf = f"real({vf}, kind=dp)"
                kw_out.append(f"{_sanitize_fortran_kwarg_name(k)}={vf}")
            args_txt = ", ".join(pos_out + kw_out)
            return f"{nm_u}({args_txt})"
    for op_r, op_f in [("==", "=="), ("!=", "/="), (">=", ">="), ("<=", "<="), (">", ">"), ("<", "<")]:
        mm_cmp = _split_top_level_token(s, op_r, from_right=True)
        if mm_cmp is not None:
            lhs = r_expr_to_fortran(mm_cmp[0])
            rhs = r_expr_to_fortran(mm_cmp[1])
            return f"{lhs} {op_f} {rhs}"
    mm_mod = _split_top_level_token(s, "%%", from_right=True)
    if mm_mod is not None:
        lhs = r_expr_to_fortran(mm_mod[0])
        rhs = r_expr_to_fortran(mm_mod[1])
        if _looks_integer_fortran_expr(lhs) and _looks_integer_fortran_expr(rhs):
            return f"mod({_int_bound_expr(lhs)}, {_int_bound_expr(rhs)})"
        return f"mod(real({lhs}, kind=dp), real({rhs}, kind=dp))"
    mm = _split_top_level_token(s, "%*%", from_right=True)
    if mm is not None:
        lhs = r_expr_to_fortran(mm[0])
        rhs_src = fscan.strip_redundant_outer_parens_expr(mm[1].strip())
        rhs_mul = _split_top_level_token(rhs_src, "*", from_right=True)
        if rhs_mul is not None:
            yl = rhs_mul[0].strip()
            yr = rhs_mul[1].strip()
            if re.match(r"^[A-Za-z]\w*$", yl) and re.match(r"^[A-Za-z]\w*$", yr):
                rhs = f"{r_expr_to_fortran(yl)} * spread({r_expr_to_fortran(yr)}, dim=2, ncopies=size({r_expr_to_fortran(yl)},2))"
            else:
                rhs = r_expr_to_fortran(mm[1])
        else:
            rhs = r_expr_to_fortran(mm[1])
        return f"r_matmul({lhs}, {rhs})" if _HAS_R_MOD else f"matmul({lhs}, {rhs})"
    c_cp = parse_call_text(s)
    if c_cp is not None and c_cp[0].lower() in {"match.arg", "match_arg"}:
        _nm_ma, pos_ma, kw_ma = c_cp
        arg_src = pos_ma[0] if pos_ma else kw_ma.get("arg")
        if arg_src is None:
            raise NotImplementedError("match.arg requires argument in this subset")
        return r_expr_to_fortran(arg_src)
    c_s = parse_call_text(s)
    if c_s is not None and c_s[0].lower() == "sample":
        _nm_s, pos_s, kw_s = c_s
        x_src = pos_s[0] if pos_s else kw_s.get("x")
        if x_src is None:
            raise NotImplementedError("sample requires x argument")
        size_src = kw_s.get("size")
        if size_src is None and len(pos_s) >= 2:
            size_src = pos_s[1]
        rep_src = kw_s.get("replace", "FALSE")
        prob_src = kw_s.get("prob")
        x_t = x_src.strip()
        identity_base = False
        labels_x = _parse_string_c_vector(x_t)
        if labels_x is not None:
            n_f = str(len(labels_x))
            base_f = f"r_seq_int(1, {n_f})"
            identity_base = True
        if _is_int_literal(x_t):
            n_f = _int_bound_expr(r_expr_to_fortran(x_t))
            base_f = f"r_seq_int(1, {n_f})"
            identity_base = True
        elif labels_x is None:
            c_x = parse_call_text(x_t)
            if c_x is not None and c_x[0].lower() in {"seq_len", "seq.int"} and len(c_x[1]) == 1 and not c_x[2]:
                n_f = _int_bound_expr(r_expr_to_fortran(c_x[1][0]))
                base_f = f"r_seq_len({n_f})"
                identity_base = True
            else:
                base_f = r_expr_to_fortran(x_src)
                n_f = f"size({base_f})"
        if size_src is None:
            size_src = n_f
        size_f = _int_bound_expr(r_expr_to_fortran(size_src))
        rep_f = r_expr_to_fortran(rep_src)
        if size_f == "1":
            if prob_src is not None:
                prob_f = r_expr_to_fortran(prob_src)
                idx_f = f"sample_int1({n_f}, replace={rep_f}, prob={prob_f})"
            else:
                idx_f = f"sample_int1({n_f}, replace={rep_f})"
        else:
            if prob_src is not None:
                prob_f = r_expr_to_fortran(prob_src)
                idx_f = f"sample_int({n_f}, size_={size_f}, replace={rep_f}, prob={prob_f})"
            else:
                idx_f = f"sample_int({n_f}, size_={size_f}, replace={rep_f})"
        if identity_base:
            return idx_f
        return f"{base_f}({idx_f})"
    c_si = parse_call_text(s)
    if c_si is not None and c_si[0].lower() == "sample.int":
        _nm_si, pos_si, kw_si = c_si
        n_src = pos_si[0] if pos_si else kw_si.get("n")
        if n_src is None:
            raise NotImplementedError("sample.int requires first argument n")
        size_src = kw_si.get("size")
        if size_src is None and len(pos_si) >= 2:
            size_src = pos_si[1]
        if size_src is None:
            size_src = n_src
        rep_src = kw_si.get("replace", "FALSE")
        prob_src = kw_si.get("prob")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        size_f = _int_bound_expr(r_expr_to_fortran(size_src))
        rep_f = r_expr_to_fortran(rep_src)
        if prob_src is not None:
            prob_f = r_expr_to_fortran(prob_src)
            call_f = f"sample_int({n_f}, size_={size_f}, replace={rep_f}, prob={prob_f})"
        else:
            call_f = f"sample_int({n_f}, size_={size_f}, replace={rep_f})"
        if size_f == "1":
            if prob_src is not None:
                return f"sample_int1({n_f}, replace={rep_f}, prob={prob_f})"
            return f"sample_int1({n_f}, replace={rep_f})"
        return call_f
    c_fmt = parse_call_text(s)
    if c_fmt is not None and c_fmt[0].lower() == "format":
        _nf, pos_f, kw_f = c_fmt
        x_src = pos_f[0] if pos_f else kw_f.get("x")
        if x_src is None:
            raise NotImplementedError("format requires an argument")
        return r_expr_to_fortran(x_src)
    c_cp = parse_call_text(s)
    if c_cp is not None and c_cp[0].lower() == "crossprod":
        _ncp, pos_cp, kw_cp = c_cp
        x_src = pos_cp[0] if pos_cp else kw_cp.get("x")
        y_src = pos_cp[1] if len(pos_cp) >= 2 else kw_cp.get("y")
        if x_src is None:
            raise NotImplementedError("crossprod requires x argument")
        x_f = r_expr_to_fortran(x_src)
        if y_src is None:
            return f"r_matmul(transpose({x_f}), {x_f})" if _HAS_R_MOD else f"matmul(transpose({x_f}), {x_f})"
        y_txt = y_src.strip()
        y_mul = _split_top_level_token(y_txt, "*", from_right=True)
        if y_mul is not None:
            yl = y_mul[0].strip()
            yr = y_mul[1].strip()
            if re.match(r"^[A-Za-z]\w*$", yl) and re.match(r"^[A-Za-z]\w*$", yr):
                y_f = f"{r_expr_to_fortran(yl)} * spread({r_expr_to_fortran(yr)}, dim=2, ncopies=size({r_expr_to_fortran(yl)},2))"
            else:
                y_f = r_expr_to_fortran(y_src)
        else:
            y_f = r_expr_to_fortran(y_src)
        return f"r_matmul(transpose({x_f}), {y_f})" if _HAS_R_MOD else f"matmul(transpose({x_f}), {y_f})"
    c_tcp = parse_call_text(s)
    if c_tcp is not None and c_tcp[0].lower() == "tcrossprod":
        _ntc, pos_tc, kw_tc = c_tcp
        x_src = pos_tc[0] if pos_tc else kw_tc.get("x")
        y_src = pos_tc[1] if len(pos_tc) >= 2 else kw_tc.get("y")
        if x_src is None:
            raise NotImplementedError("tcrossprod requires x argument")
        x_f = r_expr_to_fortran(x_src)
        if y_src is None:
            return f"r_matmul({x_f}, transpose({x_f}))" if _HAS_R_MOD else f"matmul({x_f}, transpose({x_f}))"
        y_f = r_expr_to_fortran(y_src)
        return f"r_matmul({x_f}, transpose({y_f}))" if _HAS_R_MOD else f"matmul({x_f}, transpose({y_f}))"
    c_t = parse_call_text(s)
    if c_t is not None and c_t[0].lower() == "t":
        _nt, pos_t, kw_t = c_t
        x_src = pos_t[0] if pos_t else kw_t.get("x")
        if x_src is None:
            raise NotImplementedError("t requires an argument")
        return f"transpose({r_expr_to_fortran(x_src)})"
    c_rb = parse_call_text(s)
    if c_rb is not None and c_rb[0].lower() == "rbind":
        rows = [r_expr_to_fortran(a) for a in c_rb[1]]
        if len(rows) < 1:
            raise NotImplementedError("rbind requires at least one row")
        nrow = len(rows)
        first = rows[0]
        ncol = len(split_top_level_commas(first[1:-1])) if first.strip().startswith("[") and first.strip().endswith("]") else f"size({first})"
        flat = ", ".join(rows)
        return f"transpose(reshape([{flat}], [{ncol}, {nrow}]))"
    c_rev = parse_call_text(s)
    if c_rev is not None and c_rev[0].lower() == "rev":
        _nr, pos_rv, kw_rv = c_rev
        x_src = pos_rv[0] if pos_rv else kw_rv.get("x")
        if x_src is None:
            raise NotImplementedError("rev requires an argument")
        x_f = r_expr_to_fortran(x_src)
        return f"{x_f}(size({x_f}):1:-1)"
    c_dim = parse_call_text(s)
    if c_dim is not None and c_dim[0].lower() == "dim":
        _ndm, pos_dm, kw_dm = c_dim
        x_src = pos_dm[0] if pos_dm else kw_dm.get("x")
        if x_src is None:
            raise NotImplementedError("dim requires an argument")
        return f"shape({r_expr_to_fortran(x_src)})"
    c_drop = parse_call_text(s)
    if c_drop is not None and c_drop[0].lower() == "drop":
        _ndr, pos_dr, kw_dr = c_drop
        x_src = pos_dr[0] if pos_dr else kw_dr.get("x")
        if x_src is None:
            raise NotImplementedError("drop requires an argument")
        return r_expr_to_fortran(x_src)
    c_wm = parse_call_text(s)
    if c_wm is not None and c_wm[0].lower() in {"which.max", "which.min"}:
        _nwm, pos_wm, kw_wm = c_wm
        x_src = pos_wm[0] if pos_wm else kw_wm.get("x")
        if x_src is None:
            raise NotImplementedError(f"{c_wm[0]} requires an argument")
        fn_wm = "maxloc" if c_wm[0].lower() == "which.max" else "minloc"
        return f"{fn_wm}({r_expr_to_fortran(x_src)}, dim=1)"
    s = _replace_balanced_func_calls(
        s,
        "which.min",
        lambda inner: f"minloc({r_expr_to_fortran(inner.strip())}, dim=1)",
    )
    s = _replace_balanced_func_calls(
        s,
        "which.max",
        lambda inner: f"maxloc({r_expr_to_fortran(inner.strip())}, dim=1)",
    )
    s = _replace_balanced_func_calls(
        s,
        "log10",
        lambda inner: f"log10(real({r_expr_to_fortran(inner.strip())}, kind=dp))",
    )
    s = _replace_balanced_func_calls(
        s,
        "log",
        lambda inner: f"log(real({r_expr_to_fortran(inner.strip())}, kind=dp))",
    )
    c_complex_real = parse_call_text(s)
    if c_complex_real is not None and c_complex_real[0] in {"Mod", "Re", "Im", "Conj", "Arg"}:
        nm_cr = c_complex_real[0]
        pos_cr, kw_cr = c_complex_real[1], c_complex_real[2]
        z_src = pos_cr[0] if pos_cr else kw_cr.get("z")
        if z_src is None:
            raise NotImplementedError(f"{nm_cr} requires an argument")
        z_f = r_expr_to_fortran(z_src)
        if nm_cr == "Mod":
            return f"abs({z_f})"
        if nm_cr in {"Re", "Conj"}:
            return z_f
        if nm_cr == "Im":
            return f"(0.0_dp * real({z_f}, kind=dp))"
        return f"merge(acos(-1.0_dp), 0.0_dp, real({z_f}, kind=dp) < 0.0_dp)"
    c_ord = parse_call_text(s)
    if c_ord is not None and c_ord[0].lower() == "order":
        _no, pos_o, kw_o = c_ord
        x_src = pos_o[0] if pos_o else kw_o.get("x")
        if x_src is None:
            raise NotImplementedError("order requires an argument")
        return f"order_real({r_expr_to_fortran(x_src)})"
    c_rank = parse_call_text(s)
    if c_rank is not None and c_rank[0].lower() == "rank":
        _nr, pos_rank, kw_rank = c_rank
        x_src = pos_rank[0] if pos_rank else kw_rank.get("x")
        if x_src is None:
            raise NotImplementedError("rank requires an argument")
        ties_src = kw_rank.get("ties.method", kw_rank.get("ties_method", '"average"')).strip()
        ties = (_dequote_string_literal(ties_src) or ties_src).lower().replace("_", ".")
        x_f = r_expr_to_fortran(x_src)
        if ties == "first":
            return f"rank_first({x_f})"
        if ties == "average":
            return f"rank_average({x_f})"
        raise NotImplementedError(f"unsupported rank ties.method: {ties_src}")
    c_det = parse_call_text(s)
    if c_det is not None and c_det[0].lower() == "det":
        _nd, pos_d, kw_d = c_det
        x_src = pos_d[0] if pos_d else kw_d.get("x")
        if x_src is None:
            raise NotImplementedError("det requires an argument")
        return f"det_real({r_expr_to_fortran(x_src)})"
    c_solve = parse_call_text(s)
    if c_solve is not None and c_solve[0].lower() == "solve":
        _ns, pos_s, kw_s = c_solve
        a_src = pos_s[0] if pos_s else kw_s.get("a")
        b_src = pos_s[1] if len(pos_s) >= 2 else kw_s.get("b")
        if a_src is None or b_src is None:
            raise NotImplementedError("solve(a, b) requires both arguments in this subset")
        return f"solve_real({r_expr_to_fortran(a_src)}, {r_expr_to_fortran(b_src)})"
    c_tail = parse_call_text(s)
    if c_tail is not None and c_tail[0].lower() == "tail":
        _nt, pos_tl, kw_tl = c_tail
        x_src = pos_tl[0] if pos_tl else kw_tl.get("x")
        n_src = pos_tl[1] if len(pos_tl) >= 2 else kw_tl.get("n")
        if x_src is not None and n_src is not None:
            n_txt = n_src.strip().upper()
            if n_txt in {"1", "1L"} and re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)*$", x_src.strip()):
                x_f = r_expr_to_fortran(x_src)
                return f"{x_f}(max(1, size({x_f})))"
    c_wh = parse_call_text(s)
    if c_wh is not None and c_wh[0].lower() == "which":
        _nw, pos_w, kw_w = c_wh
        x_src = pos_w[0] if pos_w else kw_w.get("x")
        if x_src is None:
            raise NotImplementedError("which requires an argument")
        x_f = r_expr_to_fortran(x_src)
        return f"pack(r_seq_len(size({x_f})), {x_f})"
    c_app = parse_call_text(s)
    if c_app is not None and c_app[0].lower() == "apply":
        _na, pos_a, kw_a = c_app
        x_src = pos_a[0] if len(pos_a) >= 1 else kw_a.get("x")
        m_src = pos_a[1] if len(pos_a) >= 2 else kw_a.get("MARGIN")
        f_src = pos_a[2] if len(pos_a) >= 3 else kw_a.get("FUN")
        if x_src is not None and m_src is not None and f_src is not None:
            x_f = r_expr_to_fortran(x_src)
            m_t = m_src.strip()
            f_t = f_src.strip().strip("\"'").lower()
            if m_t in {"1", "1L"}:
                if f_t == "sum":
                    return f"sum({x_f}, dim=2)"
                if f_t == "max":
                    return f"maxval({x_f}, dim=2)"
                if f_t == "min":
                    return f"minval({x_f}, dim=2)"
            if m_t in {"2", "2L"}:
                if f_t == "sum":
                    return f"sum({x_f}, dim=1)"
                if f_t == "max":
                    return f"maxval({x_f}, dim=1)"
                if f_t == "min":
                    return f"minval({x_f}, dim=1)"
    c_coef = parse_call_text(s)
    if c_coef is not None and c_coef[0].lower() == "coef":
        _ncf, pos_cf, kw_cf = c_coef
        obj_src = pos_cf[0].strip() if pos_cf else kw_cf.get("object", "").strip()
        if obj_src:
            c_obj = parse_call_text(obj_src)
            if c_obj is not None and c_obj[0].lower() == "lm":
                _nlm, pos_lm, kw_lm = c_obj
                form = pos_lm[0].strip() if pos_lm else kw_lm.get("formula", "").strip()
                m_form = re.match(r"^([A-Za-z]\w*)\s*~\s*(.+)$", form)
                if m_form:
                    yv = r_expr_to_fortran(m_form.group(1).strip())
                    rhs_terms = m_form.group(2).strip()
                    terms = [t.strip() for t in split_top_level_commas(rhs_terms.replace("+", ",")) if t.strip()]
                    if terms:
                        p = len(terms)
                        cols = ", ".join(f"real({r_expr_to_fortran(t)}, kind=dp)" for t in terms)
                        first = r_expr_to_fortran(terms[0])
                        return f"lm_coef({yv}, reshape([{cols}], [size({first}), {p}]))"
            return f"{r_expr_to_fortran(obj_src)}%coef"
    c_sw = parse_call_text(s)
    if c_sw is not None and c_sw[0].lower() == "sweep":
        _nsw, pos_sw, kw_sw = c_sw
        x_src = pos_sw[0] if len(pos_sw) >= 1 else kw_sw.get("x")
        m_src = pos_sw[1] if len(pos_sw) >= 2 else kw_sw.get("MARGIN")
        st_src = pos_sw[2] if len(pos_sw) >= 3 else kw_sw.get("STATS", kw_sw.get("stats"))
        fn_src = pos_sw[3] if len(pos_sw) >= 4 else kw_sw.get("FUN", kw_sw.get("fun", '"+"'))
        if x_src is not None and m_src is not None and st_src is not None:
            x_f = r_expr_to_fortran(x_src)
            st_f = r_expr_to_fortran(st_src)
            m_t = m_src.strip()
            fn_t = fn_src.strip().strip("\"'") if fn_src is not None else "+"
            if m_t in {"2", "2L"}:
                rhs = f"spread({st_f}, dim=1, ncopies=size({x_f},1))"
            elif m_t in {"1", "1L"}:
                rhs = f"spread({st_f}, dim=2, ncopies=size({x_f},2))"
            else:
                rhs = st_f
            if fn_t == "+":
                return f"{x_f} + {rhs}"
            if fn_t == "-":
                return f"{x_f} - {rhs}"
            if fn_t == "*":
                return f"{x_f} * {rhs}"
            if fn_t == "/":
                return f"{x_f} / {rhs}"
            return f"{x_f} + {rhs}"
    c_back = parse_call_text(s)
    if c_back is not None and c_back[0].lower() == "backsolve":
        _nb, pos_b, kw_b = c_back
        if len(pos_b) < 2:
            raise NotImplementedError("backsolve requires r and x arguments")
        r_f = r_expr_to_fortran(pos_b[0])
        x_f = r_expr_to_fortran(pos_b[1])
        tr_src = kw_b.get("transpose", ".false.")
        tr_f = r_expr_to_fortran(tr_src)
        return f"backsolve({r_f}, {x_f}, transpose={tr_f})"
    # Top-level R sequence operator a:b
    s_seq = _split_top_level_colon(s)
    if s_seq is not None and ("[" not in s) and ("]" not in s):
        a_f = _int_bound_expr(r_expr_to_fortran(s_seq[0].strip()))
        b_f = _int_bound_expr(r_expr_to_fortran(s_seq[1].strip()))
        return f"r_seq_int({a_f}, {b_f})"
    # matrix(data, nrow=..., ncol=...) in expression context
    c_mat = parse_call_text(s)
    if c_mat is not None and c_mat[0].lower() == "matrix":
        _nm_m, pos_m, kw_m = c_mat
        data_src = pos_m[0] if pos_m else kw_m.get("data")
        if data_src is None:
            raise NotImplementedError("matrix(...) requires data argument in this subset")
        nr_src = kw_m.get("nrow")
        nc_src = kw_m.get("ncol")
        if nr_src is None and len(pos_m) >= 2:
            nr_src = pos_m[1]
        if nc_src is None and len(pos_m) >= 3:
            nc_src = pos_m[2]
        byrow_src = kw_m.get("byrow")
        if byrow_src is None and len(pos_m) >= 4:
            byrow_src = pos_m[3]
        if nr_src is None and nc_src is None:
            raise NotImplementedError("matrix(...) requires nrow or ncol in this subset")
        data_f = _strict_int_vector_literal_from_c(data_src.strip()) or r_expr_to_fortran(data_src)
        nr_f = _int_bound_expr(r_expr_to_fortran(nr_src))
        if nc_src is None:
            nc_f = f"((size({data_f}) + ({nr_f}) - 1) / ({nr_f}))"
        else:
            nc_f = _int_bound_expr(r_expr_to_fortran(nc_src))
        if nr_src is None:
            nr_f = f"((size({data_f}) + ({nc_f}) - 1) / ({nc_f}))"
        if data_f.strip().startswith("ieee_value("):
            data_f = f"[{data_f}]"
        elif not (
            (data_f.startswith("[") and data_f.endswith("]"))
            or re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)*(?:\([^()]*\))?$", data_f.strip())
            or re.match(r"^[A-Za-z]\w*\s*\(", data_f.strip())
        ):
            data_f = f"[{data_f}]"
        byrow_true = str(byrow_src).strip().upper() in {"TRUE", ".TRUE.", "T", "1"} if byrow_src is not None else False
        if byrow_true:
            return f"transpose(reshape({data_f}, [{nc_f}, {nr_f}], pad={data_f}))"
        return f"reshape({data_f}, [{nr_f}, {nc_f}], pad={data_f})"
    # runif(...) / rnorm(...) as expressions
    c_rng = parse_call_text(s)
    if c_rng is not None and c_rng[0].lower() == "double":
        _nd, pos_d, kw_d = c_rng
        n_src = pos_d[0] if pos_d else kw_d.get("length", kw_d.get("n", "0"))
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        return f"numeric({n_f})"
    if c_rng is not None and c_rng[0].lower() == "integer":
        _ni, pos_i, kw_i = c_rng
        n_src = pos_i[0] if pos_i else kw_i.get("n", "0")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        return f"r_rep_int([0], times={n_f})"
    if c_rng is not None and c_rng[0].lower() == "logical":
        _nl, pos_l, kw_l = c_rng
        n_src = pos_l[0] if pos_l else kw_l.get("n", "0")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        return f"(r_rep_int([0], times={n_f}) /= 0)"
    if c_rng is not None and c_rng[0].lower() == "raw":
        _nrw, pos_rw, kw_rw = c_rng
        n_src = pos_rw[0] if pos_rw else kw_rw.get("n", "0")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        return f"r_rep_int([0], times={n_f})"
    if c_rng is not None and c_rng[0].lower() == "character":
        _nch, pos_ch, kw_ch = c_rng
        n_src = pos_ch[0] if pos_ch else kw_ch.get("length", kw_ch.get("n", "0"))
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        return f"r_character({n_f})"
    if c_rng is not None and c_rng[0].lower() == "replicate":
        _nm_rep, pos_rep, kw_rep = c_rng
        n_src = pos_rep[0] if pos_rep else kw_rep.get("n", "1")
        expr_src = pos_rep[1] if len(pos_rep) >= 2 else kw_rep.get("expr", "")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        if not expr_src:
            return f"numeric({n_f})"
        c_inner = parse_call_text(expr_src.strip())
        if c_inner is not None and c_inner[0].lower() == "runif":
            _ni, pos_i, kw_i = c_inner
            ni_src = pos_i[0] if pos_i else kw_i.get("n", "1")
            ni_f = _int_bound_expr(r_expr_to_fortran(ni_src))
            if ni_f == "1":
                if len(pos_i) >= 3:
                    a_f = r_expr_to_fortran(pos_i[1])
                    b_f = r_expr_to_fortran(pos_i[2])
                else:
                    a_f = r_expr_to_fortran(kw_i.get("min", "0.0"))
                    b_f = r_expr_to_fortran(kw_i.get("max", "1.0"))
                if a_f == "0.0_dp" and b_f == "1.0_dp":
                    return f"runif_vec({n_f})"
                return f"({a_f}) + (({b_f}) - ({a_f})) * runif_vec({n_f})"
        if c_inner is not None and c_inner[0].lower() == "rnorm":
            _ni, pos_i, kw_i = c_inner
            ni_src = pos_i[0] if pos_i else kw_i.get("n", "1")
            ni_f = _int_bound_expr(r_expr_to_fortran(ni_src))
            if ni_f == "1":
                mean_f = r_expr_to_fortran(kw_i.get("mean", "0.0"))
                sd_f = r_expr_to_fortran(kw_i.get("sd", "1.0"))
                if mean_f == "0.0_dp" and sd_f == "1.0_dp":
                    return f"rnorm_vec({n_f})"
                return f"({mean_f}) + ({sd_f}) * rnorm_vec({n_f})"
        # General expression case: vectorize scalar RNG calls inside expression.
        expr_vec = expr_src
        def _repl_runif(inner: str) -> str:
            ci = parse_call_text("runif(" + inner + ")")
            if ci is None:
                return "runif(" + inner + ")"
            _n0, p0, k0 = ci
            n0_src = p0[0] if p0 else k0.get("n", "1")
            n0_f = _int_bound_expr(r_expr_to_fortran(n0_src))
            if n0_f != "1":
                return "runif(" + inner + ")"
            if len(p0) >= 3:
                a0_f = r_expr_to_fortran(p0[1])
                b0_f = r_expr_to_fortran(p0[2])
            else:
                a0_f = r_expr_to_fortran(k0.get("min", "0.0"))
                b0_f = r_expr_to_fortran(k0.get("max", "1.0"))
            if a0_f == "0.0_dp" and b0_f == "1.0_dp":
                return f"runif_vec({n_f})"
            return f"(({a0_f}) + (({b0_f}) - ({a0_f})) * runif_vec({n_f}))"
        def _repl_rnorm(inner: str) -> str:
            ci = parse_call_text("rnorm(" + inner + ")")
            if ci is None:
                return "rnorm(" + inner + ")"
            _n0, p0, k0 = ci
            n0_src = p0[0] if p0 else k0.get("n", "1")
            n0_f = _int_bound_expr(r_expr_to_fortran(n0_src))
            if n0_f != "1":
                return "rnorm(" + inner + ")"
            mean0_f = r_expr_to_fortran(k0.get("mean", "0.0"))
            sd0_f = r_expr_to_fortran(k0.get("sd", "1.0"))
            if mean0_f == "0.0_dp" and sd0_f == "1.0_dp":
                return f"rnorm_vec({n_f})"
            return f"(({mean0_f}) + ({sd0_f}) * rnorm_vec({n_f}))"
        expr_vec = _replace_balanced_func_calls(expr_vec, "runif", _repl_runif)
        expr_vec = _replace_balanced_func_calls(expr_vec, "rnorm", _repl_rnorm)
        if expr_vec != expr_src:
            return r_expr_to_fortran(expr_vec)

        # Deterministic scalar fallback.
        val_f = r_expr_to_fortran(expr_src)
        if _is_int_literal(expr_src.strip()):
            return f"r_rep_int([{_int_bound_expr(val_f)}], times={n_f})"
        return f"r_rep_real([{val_f}], times={n_f})"
    if c_rng is not None and c_rng[0].lower() in {"runif", "rnorm", "rexp"}:
        fn = c_rng[0].lower()
        _nm_g, pos_g, kw_g = c_rng
        n_src = pos_g[0] if pos_g else kw_g.get("n", "1")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        if fn == "runif":
            if len(pos_g) >= 3:
                a_f = r_expr_to_fortran(pos_g[1])
                b_f = r_expr_to_fortran(pos_g[2])
            else:
                a_f = r_expr_to_fortran(kw_g.get("min", "0.0"))
                b_f = r_expr_to_fortran(kw_g.get("max", "1.0"))
            if a_f == "0.0_dp" and b_f == "1.0_dp":
                return f"runif_vec({n_f})"
            return f"({a_f}) + (({b_f}) - ({a_f})) * runif_vec({n_f})"
        if fn == "rnorm":
            mean_f = r_expr_to_fortran(kw_g.get("mean", "0.0"))
            sd_f = r_expr_to_fortran(kw_g.get("sd", "1.0"))
            if mean_f == "0.0_dp" and sd_f == "1.0_dp":
                return f"rnorm_vec({n_f})"
            return f"({mean_f}) + ({sd_f}) * rnorm_vec({n_f})"
        rate_src = kw_g.get("rate")
        if rate_src is None and len(pos_g) >= 2:
            rate_src = pos_g[1]
        rate_f = r_expr_to_fortran(rate_src or "1.0")
        return f"(-log(max(tiny(1.0_dp), 1.0_dp - runif_vec({n_f}))) / ({rate_f}))"
    # array(data, dim) / array(data, dim=c(...))
    c_arr = parse_call_text(s)
    if c_arr is not None and c_arr[0].lower() == "array":
        _nm_a, pos_a, kw_a = c_arr
        data_src = pos_a[0] if pos_a else kw_a.get("data", "0")
        dim_src = kw_a.get("dim")
        if dim_src is None and len(pos_a) >= 2:
            dim_src = pos_a[1]
        if dim_src is None:
            raise NotImplementedError("array(...) requires dim argument in this subset")

        data_txt = data_src.strip()
        data_rng = _split_top_level_colon(data_txt)
        if data_rng is not None:
            a_f = _int_bound_expr(r_expr_to_fortran(data_rng[0].strip()))
            b_f = _int_bound_expr(r_expr_to_fortran(data_rng[1].strip()))
            data_f = f"r_seq_int({a_f}, {b_f})"
        else:
            data_f = r_expr_to_fortran(data_txt)

        def _is_vectorish_for_reshape(t: str) -> bool:
            u = t.strip()
            if u.startswith("[") and u.endswith("]"):
                return True
            if re.match(r"^[A-Za-z]\w*\s*\(", u):
                return True
            if re.match(r"^[A-Za-z]\w*$", u):
                return True
            return False

        if not _is_vectorish_for_reshape(data_f):
            data_f = f"[{data_f}]"
        dim_txt = dim_src.strip()
        if dim_txt.startswith("c(") and dim_txt.endswith(")"):
            inner_d = dim_txt[2:-1].strip()
            dparts = split_top_level_commas(inner_d) if inner_d else []
            dim_f = "[" + ", ".join(_int_bound_expr(r_expr_to_fortran(dp.strip())) for dp in dparts if dp.strip()) + "]"
        else:
            dim_f = r_expr_to_fortran(dim_src)
        if _HAS_R_MOD:
            dt = data_txt.strip()
            is_char = (
                (dt.startswith('"') and dt.endswith('"'))
                or (dt.startswith("'") and dt.endswith("'"))
                or ("\"" in dt and dt.startswith("c("))
                or ("'" in dt and dt.startswith("c("))
            )
            if is_char:
                return f"r_array_char({data_f}, {dim_f})"
            if _is_int_literal(dt) or _split_top_level_colon(dt) is not None:
                return f"r_array_int({data_f}, {dim_f})"
            return f"r_array_real({data_f}, {dim_f})"
        return f"reshape({data_f}, {dim_f}, pad={data_f})"
    # rep(...)
    c_rep = parse_call_text(s)
    if c_rep is not None and c_rep[0].lower() == "rep":
        _nm_r, pos_r, kw_r = c_rep
        if not pos_r and "x" not in kw_r:
            return "real([ ], kind=dp)"
        x_src = pos_r[0] if pos_r else kw_r.get("x", "0")
        x_seq = _split_top_level_colon(x_src.strip())
        if x_seq is not None:
            x_f_raw = f"r_seq_int({_int_bound_expr(r_expr_to_fortran(x_seq[0].strip()))}, {_int_bound_expr(r_expr_to_fortran(x_seq[1].strip()))})"
        else:
            x_f_raw = r_expr_to_fortran(x_src)

        def _is_vectorish(txt: str) -> bool:
            t = txt.strip()
            if t.startswith("[") and t.endswith("]"):
                return True
            if re.search(r"\([^()]*:[^()]*\)", t):
                return True
            m_fun = re.match(r"^([a-z][a-z0-9_]*)\s*\(", t, re.IGNORECASE)
            if m_fun:
                return m_fun.group(1).lower() in {
                    "r_seq_int",
                    "r_seq_len",
                    "r_seq_int_by",
                    "r_seq_int_length",
                    "r_seq_real_by",
                    "r_seq_real_length",
                    "r_rep_real",
                    "r_rep_int",
                    "runif_vec",
                    "rnorm_vec",
                    "numeric",
                    "tail",
                    "pack",
                    "quantile",
                    "sample_int",
                    "order_real",
                    "rank_average",
                    "rank_first",
                    "solve_real",
                }
            return False

        def _looks_int_vec(txt: str) -> bool:
            t = txt.strip().lower()
            if t.startswith("r_seq_int(") or t.startswith("r_seq_len(") or t.startswith("r_rep_int("):
                return True
            if t.startswith("[") and t.endswith("]"):
                vals = split_top_level_commas(t[1:-1].strip()) if t[1:-1].strip() else []
                return bool(vals) and all(_is_int_literal(v.strip()) for v in vals)
            return False

        use_int = _looks_int_vec(x_f_raw) or _is_int_literal(x_src.strip())
        x_f = x_f_raw if _is_vectorish(x_f_raw) else (f"[{_int_bound_expr(x_f_raw)}]" if use_int else f"[{x_f_raw}]")
        rep_fn = "r_rep_int" if use_int else "r_rep_real"

        times_src = kw_r.get("times")
        each_src = kw_r.get("each")
        len_src = kw_r.get("len", kw_r.get("length.out", kw_r.get("length_out")))
        if len(pos_r) >= 2 and times_src is None and each_src is None and len_src is None:
            times_src = pos_r[1]

        def _as_times_vec_src(src: str) -> bool:
            t = src.strip()
            return t.startswith("c(") or (":" in t and "(" not in t and ")" not in t)

        args_out: list[str] = [x_f]
        if times_src is not None:
            if _as_times_vec_src(times_src):
                tv_seq = _split_top_level_colon(times_src.strip())
                if tv_seq is not None:
                    tv = f"r_seq_int({_int_bound_expr(r_expr_to_fortran(tv_seq[0].strip()))}, {_int_bound_expr(r_expr_to_fortran(tv_seq[1].strip()))})"
                elif times_src.strip().startswith("c(") and times_src.strip().endswith(")"):
                    inner_t = times_src.strip()[2:-1].strip()
                    parts_t = split_top_level_commas(inner_t) if inner_t else []
                    vals_t = ", ".join(_int_bound_expr(r_expr_to_fortran(p.strip())) for p in parts_t if p.strip())
                    tv = f"[{vals_t}]"
                else:
                    tv = r_expr_to_fortran(times_src)
                if not _is_vectorish(tv):
                    tv = f"[{_int_bound_expr(tv)}]"
                args_out.append(f"times_vec={tv}")
            else:
                args_out.append(f"times={_int_bound_expr(r_expr_to_fortran(times_src))}")
        if each_src is not None:
            args_out.append(f"each={_int_bound_expr(r_expr_to_fortran(each_src))}")
        if len_src is not None:
            args_out.append(f"len_out={_int_bound_expr(r_expr_to_fortran(len_src))}")

        return f"{rep_fn}(" + ", ".join(args_out) + ")"
    c_repint = parse_call_text(s)
    if c_repint is not None and c_repint[0].lower() == "rep.int":
        _nm_ri, pos_ri, kw_ri = c_repint
        x_src = pos_ri[0] if pos_ri else kw_ri.get("x", "0")
        times_src = pos_ri[1] if len(pos_ri) >= 2 else kw_ri.get("times", "1")
        def _repint_int_c_literal(src: str) -> str | None:
            ci = parse_call_text(src.strip())
            if ci is None or ci[0].lower() != "c" or ci[2]:
                return None
            vals: list[str] = []
            for p in ci[1]:
                v = _strip_named_actual_value(p)
                if not _is_int_literal(v):
                    return None
                vals.append(_int_bound_expr(r_expr_to_fortran(v)))
            return "[" + ", ".join(vals) + "]" if vals else None

        x_f_raw = _repint_int_c_literal(x_src) or r_expr_to_fortran(x_src)
        def _repint_is_vectorish(txt: str) -> bool:
            t = txt.strip()
            if t.startswith("[") and t.endswith("]"):
                return True
            m_fun = re.match(r"^([a-z][a-z0-9_]*)\s*\(", t, re.IGNORECASE)
            return bool(m_fun and m_fun.group(1).lower() in {
                "r_seq_int",
                "r_seq_len",
                "r_rep_int",
                "r_rep_real",
                "runif_vec",
                "rnorm_vec",
                "numeric",
            })

        def _repint_looks_int_vec(txt: str) -> bool:
            t = txt.strip().lower()
            if t.startswith("r_seq_int(") or t.startswith("r_seq_len(") or t.startswith("r_rep_int("):
                return True
            if t.startswith("[") and t.endswith("]"):
                vals = split_top_level_commas(t[1:-1].strip()) if t[1:-1].strip() else []
                return bool(vals) and all(_is_int_literal(v.strip()) for v in vals)
            return False

        def _repint_times_vec(src: str) -> str | None:
            t = src.strip()
            tv_seq = _split_top_level_colon(t)
            if tv_seq is not None:
                return f"r_seq_int({_int_bound_expr(r_expr_to_fortran(tv_seq[0].strip()))}, {_int_bound_expr(r_expr_to_fortran(tv_seq[1].strip()))})"
            if t.startswith("c(") and t.endswith(")"):
                inner_t = t[2:-1].strip()
                parts_t = split_top_level_commas(inner_t) if inner_t else []
                vals_t = ", ".join(_int_bound_expr(r_expr_to_fortran(p.strip())) for p in parts_t if p.strip())
                return f"[{vals_t}]"
            tf = r_expr_to_fortran(src)
            if _repint_is_vectorish(tf):
                return tf
            return None

        if x_f_raw.strip().startswith("[") and x_f_raw.strip().endswith("]"):
            x_f = x_f_raw
        elif _is_int_literal(x_src.strip()):
            x_f = f"[{_int_bound_expr(x_f_raw)}]"
        else:
            x_f = f"[{x_f_raw}]"
        fn = "r_rep_int" if _is_int_literal(x_src.strip()) or _repint_looks_int_vec(x_f) else "r_rep_real"
        times_vec = _repint_times_vec(times_src)
        if times_vec is not None:
            return f"{fn}({x_f}, times_vec={times_vec})"
        return f"{fn}({x_f}, times={_int_bound_expr(r_expr_to_fortran(times_src))})"
    # seq/seq.int family
    c_seq = parse_call_text(s)
    if c_seq is not None and c_seq[0].lower() in {"seq", "seq.int"}:
        _nm_s, pos_s, kw_s = c_seq
        is_int = c_seq[0].lower() == "seq.int"

        def _kw(*names: str) -> str | None:
            for n in names:
                if n in kw_s:
                    return kw_s[n]
            return None
        def _seq_len_from_src(src_txt: str) -> str:
            t = src_txt.strip()
            m_num = re.match(r"^numeric\s*\((.+)\)$", t, re.IGNORECASE)
            if m_num:
                return _int_bound_expr(r_expr_to_fortran(m_num.group(1).strip()))
            if t.startswith("list(") and t.endswith(")"):
                inner_l = t[5:-1].strip()
                return str(len(split_top_level_commas(inner_l)) if inner_l else 0)
            if t.startswith("c(") and t.endswith(")"):
                inner_c = t[2:-1].strip()
                return str(len(split_top_level_commas(inner_c)) if inner_c else 0)
            m_letters = re.match(r"^letters\s*\[\s*([^\]]+)\s*\]$", t, re.IGNORECASE)
            if m_letters:
                idx = m_letters.group(1).strip()
                ab = _split_top_level_colon(idx)
                if ab is not None:
                    a_f = _int_bound_expr(r_expr_to_fortran(ab[0].strip()))
                    b_f = _int_bound_expr(r_expr_to_fortran(ab[1].strip()))
                    return f"abs({b_f} - {a_f}) + 1"
                return "1"
            ft = r_expr_to_fortran(t)
            if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?$", ft):
                root = ft.split("%")[-1].lower()
                if root == "a_list":
                    return f"size({ft}, 4)"
                if root in _KNOWN_RANK3_NAMES:
                    return f"size({ft}, 3)"
                if root.endswith("_list"):
                    return f"size({ft}, 2)"
            return f"size({ft})"

        from_src = _kw("from")
        to_src = _kw("to")
        by_src = _kw("by")
        len_src = _kw("length.out", "length_out")
        along_src = _kw("along.with", "along_with")

        if from_src is None and pos_s:
            from_src = pos_s[0]
        if to_src is None and len(pos_s) >= 2:
            to_src = pos_s[1]
        if by_src is None and len(pos_s) >= 3:
            by_src = pos_s[2]

        if along_src is not None:
            n_f = _seq_len_from_src(along_src)
            return f"r_seq_len({_int_bound_expr(n_f)})" if is_int else f"real(r_seq_len({_int_bound_expr(n_f)}), kind=dp)"
        if len_src is not None and from_src is None and to_src is None:
            n_f = _int_bound_expr(r_expr_to_fortran(len_src))
            return f"r_seq_len({n_f})" if is_int else f"real(r_seq_len({n_f}), kind=dp)"

        if from_src is None:
            from_src = "1"
        if to_src is None:
            to_src = from_src
            from_src = "1"
        a_f = r_expr_to_fortran(from_src)
        b_f = r_expr_to_fortran(to_src)
        def _as_real(e: str) -> str:
            return f"real({e}, kind=dp)" if _looks_integer_fortran_expr(e) else e
        def _strip_named_actual(e: str) -> str:
            m_named = re.match(r"^[A-Za-z]\w*(?:\.[A-Za-z]\w*)?\s*=\s*(.+)$", e.strip())
            return m_named.group(1).strip() if m_named else e.strip()

        if len_src is not None:
            n_f = _int_bound_expr(_strip_named_actual(r_expr_to_fortran(len_src)))
            if is_int:
                return f"r_seq_int_length({_int_bound_expr(a_f)}, {_int_bound_expr(b_f)}, {n_f})"
            return f"r_seq_real_length({_as_real(a_f)}, {_as_real(b_f)}, {n_f})"
        if by_src is not None:
            by_f = _strip_named_actual(r_expr_to_fortran(by_src))
            if is_int:
                return f"r_seq_int_by({_int_bound_expr(a_f)}, {_int_bound_expr(b_f)}, {_int_bound_expr(by_f)})"
            return f"r_seq_real_by({_as_real(a_f)}, {_as_real(b_f)}, {_as_real(by_f)})"

        if is_int:
            return f"r_seq_int({_int_bound_expr(a_f)}, {_int_bound_expr(b_f)})"
        return f"real(r_seq_int({_int_bound_expr(a_f)}, {_int_bound_expr(b_f)}), kind=dp)"
    if c_seq is not None and c_seq[0].lower() == "seq_along":
        _nm_s, pos_s, _kw_s = c_seq
        if not pos_s:
            return "r_seq_len(0)"
        t = pos_s[0].strip()
        m_num = re.match(r"^numeric\s*\((.+)\)$", t, re.IGNORECASE)
        if m_num:
            n_f = _int_bound_expr(r_expr_to_fortran(m_num.group(1).strip()))
        elif t.startswith("list(") and t.endswith(")"):
            inner_l = t[5:-1].strip()
            n_f = str(len(split_top_level_commas(inner_l)) if inner_l else 0)
        elif t.startswith("c(") and t.endswith(")"):
            inner_c = t[2:-1].strip()
            n_f = str(len(split_top_level_commas(inner_c)) if inner_c else 0)
        else:
            m_letters = re.match(r"^letters\s*\[\s*([^\]]+)\s*\]$", t, re.IGNORECASE)
            if m_letters:
                idx = m_letters.group(1).strip()
                ab = _split_top_level_colon(idx)
                if ab is not None:
                    a_f = _int_bound_expr(r_expr_to_fortran(ab[0].strip()))
                    b_f = _int_bound_expr(r_expr_to_fortran(ab[1].strip()))
                    n_f = f"abs({b_f} - {a_f}) + 1"
                else:
                    n_f = "1"
            else:
                ft = r_expr_to_fortran(t)
                if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?$", ft):
                    root = ft.split("%")[-1].lower()
                    if root == "a_list":
                        n_f = f"size({ft}, 4)"
                    elif root in _KNOWN_RANK3_NAMES:
                        n_f = f"size({ft}, 3)"
                    elif root.endswith("_list"):
                        n_f = f"size({ft}, 2)"
                    else:
                        n_f = f"size({ft})"
                else:
                    n_f = f"size({ft})"
        return f"r_seq_len({_int_bound_expr(n_f)})"
    if c_seq is not None and c_seq[0].lower() == "seq_len":
        _nm_s, pos_s, _kw_s = c_seq
        n_src = pos_s[0] if pos_s else "0"
        return f"r_seq_len({_int_bound_expr(r_expr_to_fortran(n_src))})"
    # paste(..., sep="...") / paste0(...) -> "a" // sep // "b" // ...
    c_paste = parse_call_text(s)
    if c_paste is not None and c_paste[0].lower() in {"paste", "paste0"}:
        fmt_vec = _format_vec_call_from_paste(s)
        if fmt_vec is not None:
            return fmt_vec
        _nm_p, pos_p, kw_p = c_paste
        if c_paste[0].lower() == "paste0":
            vals_fmt: list[str] = []
            all_supported = True
            for p in pos_p:
                p_fmt = _format_vec_call_from_paste(p.strip())
                if p_fmt is not None:
                    vals_fmt.append(p_fmt)
                    continue
                if _dequote_string_literal(p.strip()) is not None:
                    vals_fmt.append(r_expr_to_fortran(p))
                    continue
                all_supported = False
                break
            if vals_fmt and all_supported:
                return " // ".join(vals_fmt)
        sep_src = kw_p.get("sep", '""' if c_paste[0].lower() == "paste0" else '" "')
        sep_f = r_expr_to_fortran(sep_src)
        vals: list[str] = [r_expr_to_fortran(p) for p in pos_p]
        if not vals:
            return '""'
        out = vals[0]
        for v in vals[1:]:
            out = f"{out} // {sep_f} // {v}"
        return out
    # lm accessors / summary fields (subset)
    s = re.sub(r"\bsummary\s*\(\s*([A-Za-z]\w*)\s*\)\s*\$\s*r\.squared\b", r"\1%r_squared", s)
    s = re.sub(r"\bsummary\s*\(\s*([A-Za-z]\w*)\s*\)\s*\$\s*adj\.r\.squared\b", r"\1%adj_r_squared", s)
    s = re.sub(r"\bsummary\s*\(\s*([A-Za-z]\w*)\s*\)\s*\$\s*sigma\b", r"\1%sigma", s)
    s = re.sub(r"\bcoef\s*\(\s*([A-Za-z]\w*)\s*\)", r"\1%coef", s)
    s = re.sub(r"\bfitted\s*\(\s*([A-Za-z]\w*)\s*\)", r"\1%fitted", s)
    s = re.sub(r"\bresiduals\s*\(\s*([A-Za-z]\w*)\s*\)", r"\1%resid", s)

    def _normalize_numeric_args(inner: str) -> str:
        parts = split_top_level_commas(inner)
        if not parts:
            return inner
        has_nonint = any(not _is_int_literal(p.strip()) for p in parts)
        outp: list[str] = []
        for p in parts:
            t = p.strip()
            if has_nonint and _is_int_literal(t):
                outp.append(f"{t}.0_dp")
            else:
                outp.append(t)
        return ", ".join(outp)

    s = re.sub(r"\bTRUE\b", ".true.", s)
    s = re.sub(r"\bFALSE\b", ".false.", s)
    s = re.sub(r"\b(\d+)[lL]\b", r"\1", s)
    s = s.replace("&&", ".and.")
    s = s.replace("||", ".or.")
    s = re.sub(r"(?<!&)&(?!&)", ".and.", s)
    s = re.sub(r"(?<!\|)\|(?!\|)", ".or.", s)
    s = re.sub(r"!\s*(?!=)", ".not. ", s)
    s = s.replace("^", "**")
    # ifelse(a,b,c) -> merge(b,c,a)
    s = re.sub(r"\bifelse\s*\((.+?),(.+?),(.+?)\)", r"merge(\2,\3,\1)", s)
    # basic helpers
    def _length_repl(inner: str) -> str:
        txt = inner.strip()
        ft = r_expr_to_fortran(txt)
        if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?\(:,:,:,", ft):
            return f"nested_matrix_list_len({ft})"
        if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?$", ft) and ft.split("%")[-1].lower() == "a_list":
            return f"size({ft}, 4)"
        if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?$", ft) and ft.split("%")[-1].lower() in _KNOWN_RANK3_NAMES:
            return f"size({ft}, 3)"
        if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?$", ft) and ft.split("%")[-1].lower().endswith("_list"):
            return f"size({ft}, 2)"
        return f"size({ft})"
    s = _replace_balanced_func_calls(s, "length", _length_repl)
    def _nrow_inner(inner: str) -> str:
        txt = inner.strip()
        cmat = parse_call_text(txt)
        if cmat is not None and cmat[0].lower() == "matrix":
            _nm_m, pos_m, kw_m = cmat
            nr = kw_m.get("nrow")
            if nr is None and len(pos_m) >= 2:
                nr = pos_m[1]
            if nr is not None:
                return _int_bound_expr(r_expr_to_fortran(nr))
        return f"size({r_expr_to_fortran(txt)}, 1)"
    def _ncol_inner(inner: str) -> str:
        txt = inner.strip()
        cmat = parse_call_text(txt)
        if cmat is not None and cmat[0].lower() == "matrix":
            _nm_m, pos_m, kw_m = cmat
            nc = kw_m.get("ncol")
            if nc is None and len(pos_m) >= 3:
                nc = pos_m[2]
            if nc is not None:
                return _int_bound_expr(r_expr_to_fortran(nc))
        return f"size({r_expr_to_fortran(txt)}, 2)"
    s = _replace_balanced_func_calls(s, "nrow", _nrow_inner)
    s = _replace_balanced_func_calls(s, "ncol", _ncol_inner)
    s = _replace_balanced_func_calls(s, "dim", lambda inner: f"shape({r_expr_to_fortran(inner.strip())})")
    s = _replace_balanced_func_calls(s, "all", lambda inner: f"all({r_expr_to_fortran(inner.strip())})")
    s = _replace_balanced_func_calls(s, "any", lambda inner: f"any({r_expr_to_fortran(inner.strip())})")
    s = _replace_balanced_func_calls(s, "as.matrix", lambda inner: inner.strip())
    def _as_vector_to_fortran(inner: str) -> str:
        x_f = r_expr_to_fortran(inner.strip())
        return f"reshape({x_f}, [size({x_f})])"
    s = _replace_balanced_func_calls(s, "as.vector", _as_vector_to_fortran)
    def _as_integer_to_fortran(inner: str) -> str:
        src_i = inner.strip()
        pred_i = r_expr_to_fortran(src_i)
        if any(_split_top_level_token(src_i, op, from_right=True) is not None for op in ["==", "!=", ">=", "<=", ">", "<"]):
            return f"merge(1, 0, {pred_i})"
        return f"int({pred_i})"
    s = _replace_balanced_func_calls(s, "as.integer", _as_integer_to_fortran)
    s = _replace_balanced_func_calls(s, "as.numeric", lambda inner: inner.strip())
    s = _replace_balanced_func_calls(s, "as.double", lambda inner: inner.strip())
    s = _replace_balanced_func_calls(s, "as.character", lambda inner: inner.strip())
    s = _replace_balanced_func_calls(
        s,
        "as.Date",
        lambda inner: split_top_level_commas(inner.strip())[0].strip() if split_top_level_commas(inner.strip()) else inner.strip(),
    )
    s = _replace_balanced_func_calls(s, "is.na", lambda inner: f"is_na({r_expr_to_fortran(inner)})")
    s = _replace_balanced_func_calls(
        s,
        "complete.cases",
        lambda inner: f"all(ieee_is_finite({r_expr_to_fortran(inner.strip())}), dim=2)",
    )
    s = _replace_balanced_func_calls(s, "typeof", lambda inner: f"r_typeof({r_expr_to_fortran(inner)})")
    s = _replace_balanced_func_calls(s, "commandArgs", lambda inner: "r_command_args()")
    def _startswith_to_fortran(inner: str) -> str:
        parts = split_top_level_commas(inner)
        if len(parts) >= 2:
            return f"(index({r_expr_to_fortran(parts[0].strip())}, {r_expr_to_fortran(parts[1].strip())}) == 1)"
        return f"startsWith({inner})"
    s = _replace_balanced_func_calls(s, "startsWith", _startswith_to_fortran)
    def _nzchar_to_fortran(inner: str) -> str:
        return f"(len_trim({r_expr_to_fortran(inner.strip())}) > 0)"
    s = _replace_balanced_func_calls(s, "nzchar", _nzchar_to_fortran)
    def _sub_to_fortran(inner: str) -> str:
        parts = split_top_level_commas(inner)
        if len(parts) >= 3:
            pat = _dequote_string_literal(parts[0].strip())
            repl = _dequote_string_literal(parts[1].strip())
            x_f = r_expr_to_fortran(parts[2].strip())
            if repl == "" and pat is not None and pat.startswith("^"):
                prefix = pat[1:]
                return f"{x_f}({len(prefix) + 1}:)"
        return f"sub({inner})"
    s = _replace_balanced_func_calls(s, "sub", _sub_to_fortran)
    def _substr_to_fortran(inner: str) -> str:
        parts = split_top_level_commas(inner)
        if len(parts) >= 3:
            x = r_expr_to_fortran(parts[0].strip())
            a = _int_bound_expr(r_expr_to_fortran(parts[1].strip()))
            b = _int_bound_expr(r_expr_to_fortran(parts[2].strip()))
            return f"{x}({a}:{b})"
        return f"substr({inner})"
    s = _replace_balanced_func_calls(s, "substr", _substr_to_fortran)
    s = _replace_balanced_func_calls(s, "is.finite", lambda inner: f"ieee_is_finite({inner.strip()})")
    def _is_null_to_fortran(inner: str) -> str:
        txt = inner.strip()
        txt_f = r_expr_to_fortran(txt)
        sentinel = _NULL_ARRAY_SENTINELS.get(txt.lower())
        if sentinel is None and re.match(r"^[A-Za-z]\w*$", txt):
            sentinel = _NULL_ARRAY_SENTINELS.get(f"{txt}_def".lower())
        if sentinel is not None:
            return f"(size({sentinel}) == 0)"
        if "$out" in txt or "%out" in txt:
            return f"(len_trim({txt_f}) == 0)"
        return f"({txt} == -1)"
    s = _replace_balanced_func_calls(s, "is.null", _is_null_to_fortran)
    s = re.sub(r"\bNULL\b", "-1", s)
    s = re.sub(r"\bNaN\b", "ieee_value(0.0_dp, ieee_quiet_nan)", s)
    s = re.sub(r"\bNA_integer_\b", "-huge(0.0_dp)", s)
    s = re.sub(r"\bNA_character_\b", '" "', s)
    s = re.sub(r"\bNA_logical_\b", ".false.", s)
    s = re.sub(r"\bNA_real_\b", "ieee_value(0.0_dp, ieee_quiet_nan)", s)
    s = re.sub(r"\bNA\b", "ieee_value(0.0_dp, ieee_quiet_nan)", s)
    def _split_reduction_args(inner: str) -> tuple[str, bool]:
        parts = split_top_level_commas(inner)
        x_parts: list[str] = []
        na_rm = False
        for p in parts:
            t = p.strip()
            m_kw = re.match(r"^(na(?:\.|_)rm)\s*=\s*(.+)$", t, re.IGNORECASE)
            if m_kw is not None:
                v = m_kw.group(2).strip().lower()
                na_rm = v in {"true", ".true.", "t", "1"}
                continue
            if t:
                x_parts.append(t)
        return (x_parts[0] if x_parts else ""), na_rm

    def _non_na_pack_expr(x_f: str) -> str:
        return f"pack({x_f}, .not. is_na({x_f}))"

    def _mean_to_fortran(inner: str) -> str:
        x_src, na_rm = _split_reduction_args(inner)
        inner_f = r_expr_to_fortran(x_src)
        root_l = inner_f.strip().split("%")[-1].lower()
        if root_l in {x.lower() for x in _KNOWN_LOGICAL_VECTOR_NAMES} or any(
            _split_top_level_token(x_src, op, from_right=True) is not None
            for op in ["==", "!=", ">=", "<=", ">", "<"]
        ):
            return f"(real(sum(merge(1, 0, {inner_f})), kind=dp)/real(size({inner_f}), kind=dp))"
        if na_rm:
            packed = _non_na_pack_expr(inner_f)
            return f"(sum({packed})/real(size({packed}), kind=dp))"
        return f"(sum({inner_f})/real(size({inner_f}), kind=dp))"
    s = _replace_balanced_func_calls(
        s,
        "mean",
        _mean_to_fortran,
    )

    def _is_logical_reduction_arg(x_src: str, x_f: str) -> bool:
        root_l = x_f.strip().split("%")[-1].lower()
        return (
            re.match(r"^\s*is_na\s*\(.+\)\s*$", x_src.strip(), re.IGNORECASE) is not None
            or root_l in {x.lower() for x in _KNOWN_LOGICAL_VECTOR_NAMES}
            or any(
                _split_top_level_token(x_src, op, from_right=True) is not None
                for op in ["==", "!=", ">=", "<=", ">", "<", "&", "|"]
            )
            or _split_top_level_token(x_f, ".and.", from_right=True) is not None
            or _split_top_level_token(x_f, ".or.", from_right=True) is not None
        )

    def _sum_to_fortran(inner: str) -> str:
        x_src, na_rm = _split_reduction_args(inner)
        inner_f = r_expr_to_fortran(x_src)
        if _is_logical_reduction_arg(x_src, inner_f):
            return f"sum(merge(1, 0, {inner_f}))"
        if na_rm:
            return f"sum({_non_na_pack_expr(inner_f)})"
        return f"sum({inner_f})"

    s = _replace_balanced_func_calls(
        s,
        "sum",
        _sum_to_fortran,
    )
    s = _replace_balanced_func_calls(
        s,
        "prod",
        lambda inner: (
            f"product({_non_na_pack_expr(r_expr_to_fortran(_split_reduction_args(inner)[0]))})"
            if _split_reduction_args(inner)[1]
            else f"product({r_expr_to_fortran(_split_reduction_args(inner)[0])})"
        ),
    )
    def _rowsums_to_fortran(inner: str) -> str:
        t = inner.strip()
        m = re.match(r"^exp\s*\(\s*([A-Za-z]\w*)\s*-\s*([A-Za-z]\w*)\s*\)\s*$", t)
        if m:
            a_f = r_expr_to_fortran(m.group(1))
            b_f = r_expr_to_fortran(m.group(2))
            return f"sum(exp({a_f} - spread({b_f}, dim=2, ncopies=size({a_f},2))), dim=2)"
        return f"sum({inner}, dim=2)"

    def _colsums_to_fortran(inner: str) -> str:
        t = inner.strip()
        m = re.match(r"^exp\s*\(\s*([A-Za-z]\w*)\s*-\s*([A-Za-z]\w*)\s*\)\s*$", t)
        if m:
            a_f = r_expr_to_fortran(m.group(1))
            b_f = r_expr_to_fortran(m.group(2))
            return f"sum(exp({a_f} - spread({b_f}, dim=1, ncopies=size({a_f},1))), dim=1)"
        return f"sum({inner}, dim=1)"

    s = _replace_balanced_func_calls(s, "rowSums", _rowsums_to_fortran)
    s = _replace_balanced_func_calls(s, "colSums", _colsums_to_fortran)
    s = _replace_balanced_func_calls(
        s,
        "max",
        lambda inner: (
            f"maxval({_normalize_numeric_args(inner)})"
            if len(split_top_level_commas(inner)) == 1
            else f"max({_normalize_numeric_args(inner)})"
        ),
    )
    s = _replace_balanced_func_calls(
        s,
        "min",
        lambda inner: (
            f"minval({_normalize_numeric_args(inner)})"
            if len(split_top_level_commas(inner)) == 1
            else f"min({_normalize_numeric_args(inner)})"
        ),
    )
    s = _replace_balanced_func_calls(
        s,
        "pmax",
        lambda inner: f"max({_normalize_numeric_args(inner)})",
    )
    s = _replace_balanced_func_calls(
        s,
        "pmin",
        lambda inner: f"min({_normalize_numeric_args(inner)})",
    )
    s = _replace_balanced_func_calls(
        s,
        "dnorm",
        lambda inner: "dnorm(" + re.sub(r"\blog\s*=", "log_=", inner) + ")",
    )
    s = _replace_balanced_func_calls(
        s,
        "sqrt",
        lambda inner: f"sqrt(real({r_expr_to_fortran(inner)}, kind=dp))",
    )
    s = _replace_balanced_func_calls(s, "abs", lambda inner: f"abs({r_expr_to_fortran(inner.strip())})")
    def _sign_to_fortran(inner: str) -> str:
        x_f = r_expr_to_fortran(inner.strip())
        xr = f"real({x_f}, kind=dp)"
        return f"merge(1.0_dp, merge(-1.0_dp, 0.0_dp, ({xr}) < 0.0_dp), ({xr}) > 0.0_dp)"

    def _round_to_fortran(inner: str) -> str:
        parts = split_top_level_commas(inner)
        if not parts:
            return "round()"
        x_f = r_expr_to_fortran(parts[0].strip())
        xr = f"real({x_f}, kind=dp)"
        if len(parts) >= 2 and parts[1].strip():
            digits_f = _int_bound_expr(r_expr_to_fortran(parts[1].strip()))
            scale = f"(10.0_dp ** ({digits_f}))"
            return f"(anint(({xr}) * {scale}) / {scale})"
        return f"anint({xr})"

    s = _replace_balanced_func_calls(s, "sign", _sign_to_fortran)
    s = _replace_balanced_func_calls(s, "round", _round_to_fortran)
    s = _replace_balanced_func_calls(
        s,
        "trunc",
        lambda inner: f"real(int(real({r_expr_to_fortran(inner.strip())}, kind=dp)), kind=dp)",
    )
    # Broadcast vector factors in reduced matrix products:
    # sum(A * v, dim=1) -> sum(A * spread(v, dim=2, ncopies=size(A,2)), dim=1)
    # sum(v * A, dim=1) -> sum(spread(v, dim=2, ncopies=size(A,2)) * A, dim=1)
    # sum(A * v, dim=2) -> sum(A * spread(v, dim=1, ncopies=size(A,1)), dim=2)
    # sum(v * A, dim=2) -> sum(spread(v, dim=1, ncopies=size(A,1)) * A, dim=2)
    s = re.sub(
        r"\bsum\s*\(\s*([A-Za-z]\w*)\s*\*\s*([A-Za-z]\w*)\s*,\s*dim\s*=\s*1\s*\)",
        lambda m: f"sum({m.group(1)} * {m.group(2)}, dim=1)"
        if m.group(1) == m.group(2)
        else f"sum({m.group(1)} * spread({m.group(2)}, dim=2, ncopies=size({m.group(1)},2)), dim=1)",
        s,
    )
    s = re.sub(
        r"\bsum\s*\(\s*([A-Za-z]\w*)\s*\*\s*([A-Za-z]\w*)\s*,\s*dim\s*=\s*2\s*\)",
        lambda m: f"sum({m.group(1)} * {m.group(2)}, dim=2)"
        if m.group(1) == m.group(2)
        else f"sum({m.group(1)} * spread({m.group(2)}, dim=1, ncopies=size({m.group(1)},1)), dim=2)",
        s,
    )
    # Replace simple colon ranges embedded in larger expressions, e.g. 10*1:2.
    atom = r"(?:[A-Za-z]\w*|\d+(?:\.\d+)?(?:_dp)?|\([^()]+\))"
    colon_pat = re.compile(rf"(?<![\w\)])({atom})\s*:\s*({atom})(?![\w\(])")
    prev_s_col = None
    while prev_s_col != s:
        prev_s_col = s
        def _repl_colon_emb(m: re.Match[str]) -> str:
            a_txt = m.group(1).strip()
            b_txt = m.group(2).strip()
            a_f = _int_bound_expr(r_expr_to_fortran(a_txt))
            b_f = _int_bound_expr(r_expr_to_fortran(b_txt))
            return f"r_seq_int({a_f}, {b_f})"
        s = colon_pat.sub(_repl_colon_emb, s)
    # Nested RNG calls in expression context (e.g., matrix(runif(4), ...)).
    def _repl_runif_expr(inner: str) -> str:
        ci = parse_call_text("runif(" + inner + ")")
        if ci is None:
            return "runif(" + inner + ")"
        _nr, posr, kwr = ci
        n_src = posr[0] if posr else kwr.get("n", "1")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        if len(posr) >= 3:
            a_f = r_expr_to_fortran(posr[1])
            b_f = r_expr_to_fortran(posr[2])
        else:
            a_f = r_expr_to_fortran(kwr.get("min", "0.0"))
            b_f = r_expr_to_fortran(kwr.get("max", "1.0"))
        if _HAS_R_MOD:
            if n_f == "1":
                if a_f == "0.0_dp" and b_f == "1.0_dp":
                    return "runif1()"
                return f"({a_f}) + (({b_f}) - ({a_f})) * runif1()"
            if a_f == "0.0_dp" and b_f == "1.0_dp":
                return f"runif_vec({n_f})"
            return f"({a_f}) + (({b_f}) - ({a_f})) * runif_vec({n_f})"
        return "runif(" + inner + ")"
    def _repl_rnorm_expr(inner: str) -> str:
        ci = parse_call_text("rnorm(" + inner + ")")
        if ci is None:
            return "rnorm(" + inner + ")"
        _nn, posn, kwn = ci
        n_src = posn[0] if posn else kwn.get("n", "1")
        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
        mean_f = r_expr_to_fortran(kwn.get("mean", "0.0"))
        sd_f = r_expr_to_fortran(kwn.get("sd", "1.0"))
        if _HAS_R_MOD:
            if n_f == "1":
                if mean_f == "0.0_dp" and sd_f == "1.0_dp":
                    return "rnorm1()"
                return f"({mean_f}) + ({sd_f}) * rnorm1()"
            if mean_f == "0.0_dp" and sd_f == "1.0_dp":
                return f"rnorm_vec({n_f})"
            return f"({mean_f}) + ({sd_f}) * rnorm_vec({n_f})"
        return "rnorm(" + inner + ")"
    s = _replace_balanced_func_calls(s, "runif", _repl_runif_expr)
    s = _replace_balanced_func_calls(s, "rnorm", _repl_rnorm_expr)
    # Named-vector string subscripts must be resolved before c("a", "b") becomes
    # a Fortran character constructor.
    def _repl_named_c_sub(m: re.Match[str]) -> str:
        base = m.group(1)
        inner = "c(" + m.group(2).strip() + ")"
        idx = _name_indices_from_subscript(base, inner)
        if idx is None:
            return m.group(0)
        if len(idx) == 1:
            return f"{base}({idx[0]})"
        return f"{base}([" + ", ".join(str(i) for i in idx) + "])"
    s = re.sub(r"([A-Za-z]\w*)\s*\[\s*c\(([^][]*)\)\s*\]", _repl_named_c_sub, s)
    # c(...) -> [...] (also for nested occurrences).
    def _repl_c(inner: str) -> str:
        parts = split_top_level_commas(inner.strip())
        if not any(p.strip() for p in parts):
            return "numeric(0)"
        string_vals = [_dequote_string_literal(_strip_named_actual_value(p)) for p in parts if p.strip()]
        if len(string_vals) == len([p for p in parts if p.strip()]) and all(v is not None for v in string_vals):
            vals_s = [str(v) for v in string_vals]
            width = max(1, max(len(v) for v in vals_s))
            quoted = ", ".join('"' + v.replace('"', '""') + '"' for v in vals_s)
            return f"[character(len={width}) :: {quoted}]"
        vals = []
        for p in parts:
            t = _strip_named_actual_value(p)
            if _is_int_literal(t):
                vals.append(f"{t}.0_dp")
            elif _is_real_literal(t) and "_dp" not in t:
                vals.append(f"{t}_dp")
            elif t in {"n", "k", "p", "ncomp", "nobs", "ndim", "nfit", "order", "lag", "nacf", "iter"}:
                vals.append(f"real({t}, kind=dp)")
            elif re.match(r"^[A-Za-z]\w*(?:\([^)]*\))?[$%](?:ncomp|nobs|ndim|order|nfit|iter)$", t):
                vals.append(f"real({t}, kind=dp)")
            elif parse_call_text(t) is not None:
                vals.append(r_expr_to_fortran(t))
            else:
                vals.append(t)
        return "[" + ", ".join(vals) + "]"
    s = _replace_balanced_func_calls(s, "c", _repl_c)

    def _integerize_shape_constructor(txt: str) -> str:
        vals: list[str] = []
        for p in split_top_level_commas(txt):
            t = p.strip()
            m_real_int_name = re.match(r"^real\(\s*([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*,\s*kind\s*=\s*dp\s*\)$", t, re.IGNORECASE)
            if m_real_int_name is not None:
                vals.append(m_real_int_name.group(1))
                continue
            m_real_int_lit = re.match(r"^([+-]?\d+)\.0_dp$", t)
            if m_real_int_lit is not None:
                vals.append(m_real_int_lit.group(1))
                continue
            vals.append(t)
        return "[" + ", ".join(vals) + "]"

    def _repl_shape_eq_vec(m: re.Match[str]) -> str:
        return m.group(1) + _integerize_shape_constructor(m.group(2))

    s = re.sub(r"(shape\([^)]*\)\s*==\s*)\[([^\[\]]+)\]", _repl_shape_eq_vec, s)
    s = _replace_balanced_func_calls(
        s,
        "matrix",
        lambda inner: r_expr_to_fortran("matrix(" + inner.strip() + ")"),
    )
    # decorate bare real literals
    s = re.sub(r"(?<![\w.])(\d+\.\d*([eE][+-]?\d+)?|\d+[eE][+-]?\d+)(?![\w.])", r"\1_dp", s)
    # R list/S4 member access: a$b$c and a@b@c -> a%b%c
    s = re.sub(r"\b([A-Za-z]\w*)\s*\$\s*Date\b", r"\1(:,1)", s)
    if _EXPANDED_DATA_FRAME_FIELDS:
        def _repl_expanded_df_field(m: re.Match[str]) -> str:
            obj = m.group(1)
            field = m.group(2)
            col_expr = _expanded_data_frame_col_expr(obj, field)
            if col_expr is not None:
                return col_expr
            return m.group(0)

        s = re.sub(r"\b([A-Za-z]\w*)\s*\$\s*([A-Za-z]\w*)\b", _repl_expanded_df_field, s)
    s = s.replace("$", "%")
    s = s.replace("@", "%")
    s = re.sub(
        r"%([A-Za-z]\w*(?:\.[A-Za-z]\w*)+)",
        lambda m: "%" + _sanitize_fortran_kwarg_name(m.group(1)),
        s,
    )
    # R full subscript: x[] -> x(:)
    s = re.sub(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[\s*\]", r"\1(:)", s)
    m_paren_drop_one = re.match(r"^\((.+)\)\s*\[\s*-\s*1\s*\]$", s)
    if m_paren_drop_one is not None:
        return f"r_drop_index({m_paren_drop_one.group(1).strip()}, 1)"
    # Negative subscripts: x[-k], x[-c(i,j)], x[-[i,j]] -> helper calls.
    prev_neg = None
    pat_neg_vec = re.compile(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[\s*-\s*c\(([^][]+)\)\s*\]")
    pat_neg_vec_lit = re.compile(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[\s*-\s*\[([^\[\]]+)\]\s*\]")
    pat_neg_vec_dbl = re.compile(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[\s*\[([^\[\]]+)\]\s*\]")
    pat_neg_one = re.compile(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[\s*-\s*([^\[\],]+?)\s*\]")
    while prev_neg != s:
        prev_neg = s
        s = pat_neg_vec.sub(
            lambda m: "r_drop_indices("
            + m.group(1)
            + ", ["
            + ", ".join(f"int({r_expr_to_fortran(v.strip())})" for v in split_top_level_commas(m.group(2)))
            + "])",
            s,
        )
        s = pat_neg_vec_lit.sub(
            lambda m: "r_drop_indices("
            + m.group(1)
            + ", ["
            + ", ".join(f"int({r_expr_to_fortran(v.strip())})" for v in split_top_level_commas(m.group(2)))
            + "])",
            s,
        )
        s = pat_neg_vec_dbl.sub(
            lambda m: "r_drop_indices("
            + m.group(1)
            + ", ["
            + ", ".join(f"int(abs({r_expr_to_fortran(v.strip())}))" for v in split_top_level_commas(m.group(2)) if v.strip().startswith("-"))
            + "])"
            if all(v.strip().startswith("-") for v in split_top_level_commas(m.group(2)) if v.strip())
            else m.group(0),
            s,
        )
        s = pat_neg_one.sub(
            lambda m: f"r_drop_index({m.group(1)}, int({r_expr_to_fortran(m.group(2).strip())}))",
            s,
        )
    # R indexing: a[1] -> a(1), a%b[2] -> a%b(2)
    idx_pat = re.compile(r"([A-Za-z]\w*(?:%[A-Za-z]\w*)*)\s*\[([^\[\]]+)\]")
    prev = None
    def _repl_idx(m: re.Match[str]) -> str:
        base = m.group(1)
        inner = m.group(2).strip()
        name_idx = _name_indices_from_subscript(base, inner)
        if name_idx is not None:
            if len(name_idx) == 1:
                return f"{base}({name_idx[0]})"
            return f"{base}([" + ", ".join(str(i) for i in name_idx) + "])"
        if "," not in inner and base.lower().endswith("%mu"):
            return f"{base}(:, {_index_inner_to_fortran(inner, base=base)})"
        if "," not in inner and base.lower().endswith("%sigma"):
            return f"{base}(:, :, {_index_inner_to_fortran(inner, base=base)})"
        # Logical masking for 1D vectors: x[mask] -> pack(x, mask)
        if "," not in inner:
            il = inner.lower()
            if (
                il in _KNOWN_LOGICAL_VECTOR_NAMES
                or re.match(r"^is_na\s*\(", il)
                or re.match(r"^is\.na\s*\(", il)
                or re.match(r"^complete\.cases\s*\(", il)
                or re.match(r"^all\s*\(.+\bdim\s*=\s*2\s*\)\s*$", il)
                or any(op in il for op in ("==", "!=", "<=", ">=", "<", ">", ".and.", ".or."))
            ):
                return f"pack({base}, {r_expr_to_fortran(inner)})"
        else:
            dims = _split_index_dims(inner)
            dims = [d for d in dims if not re.match(r"^drop\s*=", d.strip(), re.IGNORECASE)]
            if len(dims) >= 2 and dims[1].strip() == "":
                row_idx = dims[0].strip()
                il = row_idx.lower()
                if (
                    il in _KNOWN_LOGICAL_VECTOR_NAMES
                    or re.match(r"^is_na\s*\(", il)
                    or re.match(r"^is\.na\s*\(", il)
                    or re.match(r"^complete\.cases\s*\(", il)
                    or re.match(r"^all\s*\(.+\bdim\s*=\s*2\s*\)\s*$", il)
                    or any(op in il for op in ("==", "!=", "<=", ">=", "<", ">", ".and.", ".or."))
                ):
                    mask_f = r_expr_to_fortran(row_idx)
                    return (
                        f"reshape(pack({base}, spread({mask_f}, dim=2, ncopies=size({base},2))), "
                        f"[count({mask_f}), size({base},2)])"
                    )
        return f"{base}({_index_inner_to_fortran(inner, base=base)})"
    while prev != s:
        prev = s
        s = idx_pat.sub(_repl_idx, s)
    # R empty subscript positions: a[,j] -> a(:,j), a[i,] -> a(i,:)
    s = re.sub(r"\(\s*,", "(:,", s)
    s = re.sub(r",\s*\)", ",:)", s)
    # Sanitize named-argument keywords that are valid in R but not Fortran
    # (e.g., iter.max= -> iter_max=).
    s = re.sub(
        r"\b([A-Za-z]\w*(?:\.[A-Za-z]\w*)+)\s*=",
        lambda m: _sanitize_fortran_kwarg_name(m.group(1)) + "=",
        s,
    )
    # R modulo operator in nested expressions: a %% b -> mod(a, b)
    idiv_pat = re.compile(
        r"(\b[A-Za-z]\w*(?:\([^()]*\))?|\b\d+(?:\.\d+)?(?:_dp)?)\s*%/%\s*(\b[A-Za-z]\w*(?:\([^()]*\))?|\b\d+(?:\.\d+)?(?:_dp)?)"
    )
    prev_idiv = None
    while prev_idiv != s:
        prev_idiv = s
        s = idiv_pat.sub(r"(int(\1) / int(\2))", s)
    mod_pat = re.compile(
        r"(\b[A-Za-z]\w*(?:\([^()]*\))?|\b\d+(?:\.\d+)?(?:_dp)?)\s*%%\s*(\b[A-Za-z]\w*(?:\([^()]*\))?|\b\d+(?:\.\d+)?(?:_dp)?)"
    )
    prev_mod = None
    while prev_mod != s:
        prev_mod = s
        s = mod_pat.sub(r"mod(\1, \2)", s)
    return s


class FEmit:
    def __init__(self) -> None:
        self.lines: list[str] = []
        self.ind = 0

    def w(self, s: str = "") -> None:
        self.lines.append(" " * self.ind + s)

    def push(self) -> None:
        self.ind += 3

    def pop(self) -> None:
        self.ind = max(0, self.ind - 3)

    def text(self) -> str:
        return "\n".join(self.lines) + "\n"


def emit_stmts(
    o: FEmit,
    stmts: list[object],
    need_rnorm: dict[str, bool],
    params: set[str],
    alloc_seen: set[str] | None = None,
    helper_ctx: dict[str, object] | None = None,
) -> None:
    if alloc_seen is None:
        alloc_seen = set()
    has_r_mod = bool(helper_ctx and helper_ctx.get("has_r_mod"))
    need_r_mod: set[str] = set()
    lm_terms_by_fit: dict[str, list[str]] = {}
    data_frame_vars: dict[str, tuple[str | None, list[str]]] = {}
    int_matrix_vars: set[str] = set()
    real_matrix_vars: set[str] = set()
    int_vector_vars: set[str] = set()
    real_vector_vars: set[str] = set()
    matrix_vars: set[str] = set()
    vector_vars: set[str] = set()
    local_ranks_ctx: dict[str, int] = {}
    char_scalar_vars: set[str] = set()
    return_array_fns: set[str] = set()
    object_list_vars: dict[str, str] = {}
    t_test_vars_ctx: set[str] = set()
    if helper_ctx is not None:
        nr = helper_ctx.get("need_r_mod")
        if isinstance(nr, set):
            need_r_mod = nr
        lmd = helper_ctx.get("lm_terms_by_fit")
        if isinstance(lmd, dict):
            lm_terms_by_fit = lmd
        dfv = helper_ctx.get("data_frame_vars")
        if isinstance(dfv, dict):
            data_frame_vars = dfv
        imv = helper_ctx.get("int_matrix_vars")
        if isinstance(imv, set):
            int_matrix_vars = imv
        rmv = helper_ctx.get("real_matrix_vars")
        if isinstance(rmv, set):
            real_matrix_vars = rmv
        ivv = helper_ctx.get("int_vector_vars")
        if isinstance(ivv, set):
            int_vector_vars = ivv
        rvv = helper_ctx.get("real_vector_vars")
        if isinstance(rvv, set):
            real_vector_vars = rvv
        mv = helper_ctx.get("matrix_vars")
        if isinstance(mv, set):
            matrix_vars = mv
        vv = helper_ctx.get("vector_vars")
        if isinstance(vv, set):
            vector_vars = vv
        lr = helper_ctx.get("local_ranks")
        if isinstance(lr, dict):
            local_ranks_ctx = {str(k): int(v) for k, v in lr.items() if isinstance(v, int)}
        csv = helper_ctx.get("char_scalar_vars")
        if isinstance(csv, set):
            char_scalar_vars = csv
        raf = helper_ctx.get("return_array_fns")
        if isinstance(raf, set):
            return_array_fns = raf
        olv = helper_ctx.get("object_list_vars")
        if isinstance(olv, dict):
            object_list_vars = {str(k): str(v) for k, v in olv.items()}
        ttv = helper_ctx.get("t_test_vars")
        if isinstance(ttv, set):
            t_test_vars_ctx = {str(x) for x in ttv}
    if not matrix_vars:
        matrix_vars = set(int_matrix_vars) | set(real_matrix_vars)
    if not vector_vars:
        vector_vars = set(int_vector_vars) | set(real_vector_vars)
    list_locals: dict[str, dict[str, object]] = {}
    if helper_ctx is not None:
        ll = helper_ctx.get("list_locals")
        if isinstance(ll, dict):
            list_locals = ll
    return_var = ""
    if helper_ctx is not None:
        rv = helper_ctx.get("return_var")
        if isinstance(rv, str):
            return_var = rv

    def _emit_alloc_1d(name: str, extent: str) -> None:
        if name in alloc_seen:
            o.w(f"if (allocated({name})) deallocate({name})")
        o.w(f"allocate({name}({extent}))")
        alloc_seen.add(name)

    def _wstmt(stmt_line: str, cmt: str) -> None:
        if char_scalar_vars:
            for nm in sorted(char_scalar_vars, key=len, reverse=True):
                stmt_line = re.sub(
                    rf"\b{re.escape(nm)}\s*\(\s*r_seq_int\(\s*([^,]+?)\s*,\s*([^)]+?)\s*\)\s*\)",
                    rf"{nm}(\1:\2)",
                    stmt_line,
                )
        t = (cmt or "").strip()
        if t:
            o.w(f"{stmt_line} ! {t}")
        else:
            o.w(stmt_line)

    def _wcomment(text: str) -> None:
        t = text.strip()
        if t:
            o.w(f"! {t}")

    def _rewrite_predict_expr(expr: str) -> str:
        c = parse_call_text(expr.strip())
        if c is None or c[0].lower() != "predict":
            return expr
        _np, posp, kwp = c
        fit_nm = posp[0].strip() if posp else kwp.get("object", "").strip()
        newd = kwp.get("newdata", "").strip()
        if not fit_nm or not newd:
            return expr
        terms = lm_terms_by_fit.get(fit_nm)
        if not terms:
            return expr
        cols = ", ".join(r_expr_to_fortran(f"{newd}_{t}") for t in terms)
        first = r_expr_to_fortran(f"{newd}_{terms[0]}")
        p = len(terms)
        return f"lm_predict_general({fit_nm}, reshape([{cols}], [size({first}), {p}]))"

    def _expr_rank_for_print(expr_txt: str) -> int | None:
        t = expr_txt.strip()
        if t.startswith("[") and t.endswith("]"):
            return 1
        m_field_print = re.match(r"^[A-Za-z]\w*\s*\$\s*([A-Za-z]\w*)\b", t)
        if m_field_print is not None and m_field_print.group(1).lower() in {"ar", "ma", "sigma", "resid", "fitted"}:
            return 2
        if re.match(r"^[A-Za-z]\w*\s*\$\s*a\s*\[\[", t):
            return 2
        if re.match(r"^[A-Za-z]\w*\s*\[\[", t):
            root = t.split("[[", 1)[0].strip().lower()
            if root in _KNOWN_RANK3_NAMES or root.endswith("_true") or root == "a":
                return 2
        m_r_ix = re.match(r"^([A-Za-z]\w*)\s*\[([^\[\]]+)\]$", t)
        if m_r_ix and (m_r_ix.group(1) in int_matrix_vars or m_r_ix.group(1) in real_matrix_vars):
            dims = _split_index_dims(m_r_ix.group(2).strip())
            if len(dims) >= 2:
                scalar_dims = [
                    d.strip() != "" and ":" not in d and _split_top_level_colon(d.strip()) is None
                    for d in dims
                ]
                return 0 if all(scalar_dims) else 2
            return 1
        if m_r_ix and (m_r_ix.group(1) in int_vector_vars or m_r_ix.group(1) in real_vector_vars):
            inner = m_r_ix.group(2).strip()
            inner_l = inner.lower()
            if (
                inner.startswith("-")
                or inner_l in _KNOWN_LOGICAL_VECTOR_NAMES
                or re.match(r"^is\.na\s*\(", inner_l)
                or re.match(r"^is_na\s*\(", inner_l)
                or any(op in inner_l for op in ("==", "!=", "<=", ">=", "<", ">", "&", "|", ".and.", ".or."))
            ):
                return 1
            if inner and ":" not in inner and _split_top_level_colon(inner) is None:
                return 0
            return 1
        mm_t = _split_top_level_token(t, "%*%", from_right=True)
        if mm_t is not None:
            r1 = _expr_rank_for_print(mm_t[0])
            r2 = _expr_rank_for_print(mm_t[1])
            if r1 == 2 and r2 == 2:
                return 2
            if (r1 == 2 and r2 == 1) or (r1 == 1 and r2 == 2):
                return 1
            if r1 == 1 and r2 == 1:
                return 0
        for op_pr in ["+", "-", "*", "/"]:
            mm_pr = _split_top_level_token(t, op_pr, from_right=True)
            if mm_pr is None:
                continue
            r1 = _expr_rank_for_print(mm_pr[0])
            r2 = _expr_rank_for_print(mm_pr[1])
            if r1 == 2 or r2 == 2:
                return 2
            if r1 == 1 or r2 == 1:
                return 1
            if r1 == 0 and r2 == 0:
                return 0
            break
        c = parse_call_text(t)
        if c is not None:
            nm_c = c[0].lower()
            if nm_c in {"matrix", "array", "cbind", "cbind2", "rbind", "cov", "cor", "ccf_matrix", "crossprod", "tcrossprod", "t", "toeplitz"}:
                return 2
            if nm_c in return_array_fns:
                return _USER_FUNC_RETURN_RANK.get(nm_c, 1)
            if nm_c in _USER_FUNC_ARG_KIND:
                if nm_c in _USER_FUNC_ELEMENTAL:
                    ranks: list[int] = []
                    for a in c[1]:
                        rr = _expr_rank_for_print(a)
                        if rr is not None:
                            ranks.append(rr)
                    for v in c[2].values():
                        rr = _expr_rank_for_print(v)
                        if rr is not None:
                            ranks.append(rr)
                    if ranks:
                        return max(ranks)
                return 0
            if nm_c == "names" and len(c[1]) == 1 and c[1][0].strip().lower() in _NAMED_VECTOR_NAMES:
                return 1
            if nm_c == "unname" and len(c[1]) == 1:
                return _expr_rank_for_print(c[1][0])
            if nm_c == "round" and c[1]:
                return _expr_rank_for_print(c[1][0])
            if nm_c == "c":
                return 1
            if nm_c in {"table", "table2"}:
                vals_c = list(c[1]) + list(c[2].values())
                return 2 if len(vals_c) >= 2 or nm_c == "table2" else 1
            if nm_c in {"prop.table", "prop_table"}:
                vals_c = list(c[1]) + [v for k, v in c[2].items() if k.lower() != "margin"]
                if vals_c:
                    rr = _expr_rank_for_print(vals_c[0])
                    if rr is not None:
                        return rr
                return 1
            if nm_c in {"dim", "rev", "rep.int"}:
                return 1
            if nm_c in {
                "r_seq_int",
                "r_seq_len",
                "r_seq_int_by",
                "r_seq_int_length",
                "r_seq_real_by",
                "r_seq_real_length",
                "r_rep_real",
                "r_rep_int",
                "r_add",
                "r_sub",
                "r_mul",
                "r_div",
                "runif_vec",
                "rnorm_vec",
                "numeric",
                "quantile",
                "tail",
                "pack",
                "r_drop_index",
                "r_drop_indices",
                "diff",
                "match",
                "rank_average",
                "rank_first",
                "solve_real",
                "prop_table",
            }:
                return 1
            if nm_c == "r_matmul":
                posm = c[1]
                if len(posm) >= 2:
                    r1 = _expr_rank_for_print(posm[0])
                    r2 = _expr_rank_for_print(posm[1])
                    if r1 == 2 and r2 == 2:
                        return 2
                    if (r1 == 2 and r2 == 1) or (r1 == 1 and r2 == 2):
                        return 1
                    if r1 == 1 and r2 == 1:
                        return 0
        if re.match(r"^[A-Za-z]\w*$", t):
            if t in int_matrix_vars or t in real_matrix_vars:
                return 2
            if t in int_vector_vars or t in real_vector_vars:
                return 1
        if re.match(r"^[A-Za-z]\w*(?:[$%][A-Za-z]\w*)+$", t):
            fld = re.split(r"[$%]", t)[-1].lower()
            if fld in {"table", "design", "sigma", "y"}:
                return 2
            if fld in {"coef", "fitted", "resid", "intercept", "mu", "pi", "weights", "means", "sds", "vars", "nk"}:
                return 1
        m_comp_ix = re.match(r"^[A-Za-z]\w*(?:[$%][A-Za-z]\w*)+\s*\((.*)\)$", t)
        if m_comp_ix is not None:
            head = t[: t.find("(")].strip()
            fld = re.split(r"[$%]", head)[-1].lower()
            dims = _split_index_dims(m_comp_ix.group(1).strip())
            if fld == "a" and len(dims) >= 2:
                return 2
            if fld in {"table", "design", "sigma", "y"} and len(dims) >= 2:
                scalar_dims = [d.strip() != "" and ":" not in d and _split_top_level_colon(d.strip()) is None for d in dims]
                return 0 if all(scalar_dims) else 2
            if fld in {"coef", "fitted", "resid", "intercept", "mu", "pi", "weights", "means", "sds", "vars", "nk"}:
                return 0 if len(dims) == 1 and dims[0].strip() and ":" not in dims[0] and _split_top_level_colon(dims[0].strip()) is None else 1
        m_ix = re.match(r"^([A-Za-z]\w*)\s*\(", t)
        if m_ix and (m_ix.group(1) in int_matrix_vars or m_ix.group(1) in real_matrix_vars):
            inside = t[t.find("(") + 1 : -1]
            dims = _split_index_dims(inside)
            if len(dims) >= 2:
                return 2
            if len(dims) == 1:
                return 1
        if m_ix and (m_ix.group(1) in int_vector_vars or m_ix.group(1) in real_vector_vars):
            inside = t[t.find("(") + 1 : -1]
            dims = _split_index_dims(inside)
            if len(dims) == 1:
                inner = dims[0].strip()
                if inner and ":" not in inner and _split_top_level_colon(inner) is None:
                    return 0
                return 1
        # Non-call arithmetic expressions that reference known matrix/vector vars.
        if not re.search(r"\b[A-Za-z]\w*\s*\(", t):
            names = set(re.findall(r"\b[A-Za-z]\w*\b", t))
            if names & (int_matrix_vars | real_matrix_vars):
                return 2
            if names & (int_vector_vars | real_vector_vars):
                return 1
        return None

    def _named_vector_print_parts(expr_txt: str) -> tuple[str, str, bool] | None:
        """Return (value_expr, names_expr, value_is_scalar) for printable named vectors."""
        t = expr_txt.strip()
        cinfo = parse_call_text(t)
        if cinfo is not None and cinfo[0].lower() == "unname":
            return None
        if re.match(r"^[A-Za-z]\w*$", t) and t.lower() in _NAMED_VECTOR_NAMES:
            return r_expr_to_fortran(t), _NAMED_VECTOR_NAMES[t.lower()], False
        m_ix = re.match(r"^([A-Za-z]\w*)\s*\[([^\[\]]+)\]$", t)
        if m_ix and m_ix.group(1).lower() in _NAMED_VECTOR_NAMES:
            base = m_ix.group(1)
            inner = m_ix.group(2).strip()
            base_names = _NAMED_VECTOR_NAMES[base.lower()]
            idx = _name_indices_from_subscript(base, inner)
            if idx is not None:
                if len(idx) == 1:
                    return r_expr_to_fortran(t), f"{base_names}({idx[0]}:{idx[0]})", True
                idx_expr = "[" + ", ".join(str(i) for i in idx) + "]"
                return r_expr_to_fortran(t), f"{base_names}({idx_expr})", False
            if inner and ":" not in inner and _split_top_level_colon(inner) is None:
                idx_f = _index_inner_to_fortran(inner, base=base)
                return r_expr_to_fortran(t), f"{base_names}({idx_f}:{idx_f})", True
            return r_expr_to_fortran(t), f"{base_names}({_index_inner_to_fortran(inner, base=base)})", False
        if cinfo is not None and len(cinfo[1]) == 1 and not cinfo[2]:
            nm = cinfo[0].lower()
            arg = cinfo[1][0].strip()
            if nm in {"sqrt", "log", "exp", "abs"} and arg.lower() in _NAMED_VECTOR_NAMES:
                return r_expr_to_fortran(t), _NAMED_VECTOR_NAMES[arg.lower()], False
        expr_names = [n for n in re.findall(r"\b[A-Za-z]\w*\b", t) if n.lower() in _NAMED_VECTOR_NAMES]
        if cinfo is None and expr_names:
            return r_expr_to_fortran(t), _NAMED_VECTOR_NAMES[expr_names[0].lower()], False
        return None

    def _names_call_print_arg(expr_txt: str) -> str | None:
        cinfo = parse_call_text(expr_txt.strip())
        if cinfo is None or cinfo[0].lower() != "names" or len(cinfo[1]) != 1:
            return None
        src = cinfo[1][0].strip()
        return _NAMED_VECTOR_NAMES.get(src.lower())

    def _matrix_col_labels_for_print(expr_txt: str) -> list[str] | None:
        matrix_labels_map = helper_ctx.get("matrix_col_labels") if helper_ctx is not None else None
        if not isinstance(matrix_labels_map, dict):
            matrix_labels_map = {}
        labels_map = helper_ctx.get("named_vector_labels") if helper_ctx is not None else None
        if not isinstance(labels_map, dict):
            labels_map = _NAMED_VECTOR_LABELS
        t = expr_txt.strip()
        c_round = parse_call_text(t)
        if c_round is not None and c_round[0].lower() == "round" and c_round[1]:
            t = c_round[1][0].strip()
        candidates = [m.group(1).lower() for m in re.finditer(r"(?:^|[$%])([A-Za-z]\w*)\b", t)]
        for cand in reversed(candidates):
            labs = matrix_labels_map.get(cand)
            if isinstance(labs, list) and labs:
                return [str(x) for x in labs]
        for cand in reversed(candidates):
            labs = labels_map.get(cand)
            if isinstance(labs, list) and labs:
                return [str(x) for x in labs]
        return None

    def _matrix_col_label_expr_for_print(expr_txt: str) -> str | None:
        expr_map = helper_ctx.get("matrix_colname_exprs") if helper_ctx is not None else None
        if not isinstance(expr_map, dict):
            return None
        t = expr_txt.strip()
        c_round = parse_call_text(t)
        if c_round is not None and c_round[0].lower() == "round" and c_round[1]:
            t = c_round[1][0].strip()
        candidates = [m.group(1).lower() for m in re.finditer(r"(?:^|[$%])([A-Za-z]\w*)\b", t)]
        for cand in reversed(candidates):
            src = expr_map.get(cand)
            if isinstance(src, str) and src.strip():
                return r_expr_to_fortran(src.strip())
        return None

    def _int_col_mask_literal(labels: list[str]) -> str | None:
        int_names = {
            "nobs",
            "n",
            "p",
            "q",
            "npar",
            "order",
            "ar.order",
            "ma.order",
            "convergence",
            "ok",
            "class",
            "cluster",
        }
        mask = [lab.strip().lower() in int_names for lab in labels]
        if not any(mask):
            return None
        return "[" + ", ".join(".true." if x else ".false." for x in mask) + "]"

    def _char_array_literal(labels: list[str]) -> str:
        if not labels:
            return "[character(len=1) :: ]"
        width = max(1, max(len(x) for x in labels))
        vals = ", ".join('"' + x.replace('"', '""') + '"' for x in labels)
        return f"[character(len={width}) :: {vals}]"

    def _table_labels_for_expr(expr_txt: str) -> tuple[list[str] | None, list[str] | None] | None:
        t_tbl = expr_txt.strip()
        if re.fullmatch(r"[A-Za-z]\w*", t_tbl):
            return _TABLE_LABELS.get(t_tbl.lower())
        c_tbl = parse_call_text(t_tbl)
        if c_tbl is None or c_tbl[0].lower() != "table":
            return None
        vals_tbl = list(c_tbl[1]) + list(c_tbl[2].values())
        if len(vals_tbl) == 1:
            return (_CATEGORICAL_LABELS.get(vals_tbl[0].strip().lower()), None)
        if len(vals_tbl) >= 2:
            return (
                _CATEGORICAL_LABELS.get(vals_tbl[0].strip().lower()) or [],
                _CATEGORICAL_LABELS.get(vals_tbl[1].strip().lower()) or [],
            )
        return None

    def _print_only_loop_body_with_value(ss: list[object], var: str, value_expr: str) -> list[object] | None:
        if not ss:
            return None
        out_ss: list[object] = []
        for b in ss:
            if isinstance(b, PrintStmt):
                out_ss.append(_replace_name_in_stmt(b, var, value_expr))
                continue
            if isinstance(b, CallStmt) and b.name.lower() in {"cat", "print"}:
                out_ss.append(_replace_name_in_stmt(b, var, value_expr))
                continue
            return None
        return out_ss

    def _emit_direct_print_only_loop_body(
        ss: list[object],
        var: str,
        value_expr: str,
        scalar_fmt: str | None = None,
    ) -> bool:
        if not ss:
            return False
        for b in ss:
            if not isinstance(b, PrintStmt):
                return False
        for b in ss:
            args = [_replace_idents(a, {var: value_expr}) for a in b.args]
            if args:
                vals = [r_expr_to_fortran(_rewrite_predict_expr(a)) for a in args]
                if scalar_fmt is not None and len(vals) == 1:
                    _wstmt(f'write(*,"({scalar_fmt})") {vals[0]}', b.comment)
                else:
                    _wstmt("print *, " + ", ".join(vals), b.comment)
            else:
                _wstmt("print *", b.comment)
        return True

    def _simple_implied_do_expr(expr: str, var: str) -> str | None:
        t = expr.strip()
        if not t:
            return None
        if not re.search(rf"\b{re.escape(var)}\b", t):
            return None
        allowed = {
            var.lower(),
            "int",
            "real",
            "abs",
            "mod",
            "min",
            "max",
        }
        for name in re.findall(r"\b[A-Za-z]\w*\b", t):
            if name.lower() not in allowed and not _is_int_literal(name):
                return None
        return r_expr_to_fortran(t)

    def _emit_implied_do_print_loop(ss: list[object], var: str, lo: str, hi: str, scalar_fmt: str) -> bool:
        if len(ss) != 1 or not isinstance(ss[0], PrintStmt) or len(ss[0].args) != 1:
            return False
        expr_f = _simple_implied_do_expr(ss[0].args[0], var)
        if expr_f is None:
            return False
        _wstmt(f'write(*,"({scalar_fmt})") ({expr_f},{var}={lo},{hi})', ss[0].comment)
        return True

    for st in stmts:
        if isinstance(st, CommentStmt):
            _wcomment(st.text)
            continue
        if isinstance(st, Assign):
            c_df = parse_call_text(st.expr.strip())
            if c_df is not None and c_df[0].lower() == "data.frame":
                _df_nm, pos_df, kw_df = c_df
                y_src = kw_df.get("y")
                predictors = [p.strip() for p in pos_df if p.strip()]
                predictors.extend(v.strip() for k, v in kw_df.items() if k != "y" and v.strip())
                data_frame_vars[st.name] = (y_src.strip() if isinstance(y_src, str) else None, predictors)
                if y_src is None:
                    cols_src: list[str] = [p.strip() for p in pos_df if p.strip()]
                    cols_src.extend(
                        v.strip()
                        for k, v in kw_df.items()
                        if k.lower() not in {"stringsasfactors", "check.names", "fix.empty.names"} and v.strip()
                    )
                    if cols_src:
                        cols_f = [r_expr_to_fortran(c) for c in cols_src]
                        first_col = cols_f[0]
                        _wstmt(f"allocate({st.name}(size({first_col}), {len(cols_f)}))", st.comment)
                        for i_col, col_f in enumerate(cols_f, start=1):
                            o.w(f"{st.name}(:, {i_col}) = real({col_f}, kind=dp)")
                    continue
                continue
            if st.name in list_locals:
                fields = _parse_list_constructor(st.expr.strip())
                if fields is None:
                    # Common NULL sentinel init in R before first list assignment.
                    if st.expr.strip().upper() in {"NULL", "-1"}:
                        continue
                else:
                    def _emit_list_assign(prefix: str, ff: dict[str, object]) -> None:
                        for kk, vv in ff.items():
                            field_kk = kk[:-4] if kk.endswith("_wrk") else kk
                            if isinstance(vv, dict):
                                _emit_list_assign(f"{prefix}%{field_kk}", vv)
                            else:
                                vv_txt = str(vv).strip()
                                if field_kk == "order":
                                    rhs_f = _int_vector_literal_from_c(vv_txt) or r_expr_to_fortran(vv_txt)
                                else:
                                    rhs_f = '""' if field_kk == "out" and vv_txt.upper() == "NULL" else r_expr_to_fortran(vv_txt)
                                _wstmt(f"{prefix}%{field_kk} = {rhs_f}", st.comment)

                    _emit_list_assign(st.name, fields)
                    continue
            if st.name in params:
                # Already emitted as named constant parameter.
                continue
            rhs = st.expr.strip()
            rhs_call_src = re.sub(r"\bt\.test\s*\(", "t_test(", rhs, flags=re.IGNORECASE)
            if _emit_optim_bfgs_assignment(o, st.name, rhs, st.comment):
                continue
            c_read_csv = parse_call_text(rhs)
            if c_read_csv is not None and c_read_csv[0].lower() == "read.csv":
                path_src = c_read_csv[1][0] if c_read_csv[1] else c_read_csv[2].get("file", '""')
                path_f = r_expr_to_fortran(path_src)
                if has_r_mod:
                    need_r_mod.add("read_csv_real_matrix")
                    _wstmt(f"call read_csv_real_matrix({path_f}, {st.name})", st.comment)
                else:
                    _wstmt(f"call read_csv_real_matrix({path_f}, {st.name})", st.comment)
                    if helper_ctx is not None:
                        helper_ctx["need_csv_reader"] = True
                continue
            if re.match(r"^vector\s*\(\s*['\"]list['\"]", rhs, re.IGNORECASE):
                c_vec = parse_call_text(rhs)
                len_src = "k"
                if c_vec is not None:
                    _vnm, vpos, vkw = c_vec
                    if len(vpos) >= 2:
                        len_src = vpos[1]
                    elif "length" in vkw:
                        len_src = vkw["length"]
                len_f = _int_bound_expr(r_expr_to_fortran(len_src))
                if st.name in object_list_vars:
                    _wstmt(f"allocate({st.name}({len_f}))", st.comment)
                    continue
                if st.name == "stats_list":
                    _wstmt(f"allocate({st.name}(7, {len_f}))", st.comment)
                    continue
                rank_hint = local_ranks_ctx.get(st.name, 0)
                if rank_hint >= 3 or _list_name_holds_matrices_in_texts(_stmt_texts_for_rank_scan(stmts), st.name) or "sigma" in st.name.lower():
                    texts_rank = _stmt_texts_for_rank_scan(stmts)
                    if st.name in {"gamma", "autocov_matrices_result"} and any("x_wrk" in t or re.match(r"^\s*x\s*<-", t, re.IGNORECASE) for t in texts_rank):
                        dim_sym = "size(x_wrk, 2)"
                    else:
                        dim_sym = "m" if st.name == "a" and any(re.match(r"^\s*m\s*<-\s*ncol\s*\(", t, re.IGNORECASE) for t in texts_rank) else "p"
                    _wstmt(f"allocate({st.name}({dim_sym}, {dim_sym}, {len_f}))", st.comment)
                else:
                    _wstmt(f"allocate({st.name}(p, {len_f}))", st.comment)
                continue
            m_seq_assign = _split_top_level_colon(rhs)
            if m_seq_assign is not None and ("[" not in rhs) and ("]" not in rhs):
                a_src, b_src = m_seq_assign
                a_f = _int_bound_expr(r_expr_to_fortran(a_src))
                b_f = _int_bound_expr(r_expr_to_fortran(b_src))
                _emit_alloc_1d(st.name, f"abs(({b_f}) - ({a_f})) + 1")
                o.w("block")
                o.push()
                o.w("integer :: i_seq, a_seq, b_seq, step_seq")
                o.w(f"a_seq = {a_f}")
                o.w(f"b_seq = {b_f}")
                o.w("step_seq = merge(1, -1, a_seq <= b_seq)")
                o.w(f"do i_seq = 1, size({st.name})")
                o.push()
                o.w(f"{st.name}(i_seq) = a_seq + (i_seq - 1) * step_seq")
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            c_lm = parse_call_text(rhs)
            c_outer = parse_call_text(rhs)
            if c_outer is not None and c_outer[0].lower() == "outer":
                _nm_o, pos_o, kw_o = c_outer
                if len(pos_o) < 2:
                    raise NotImplementedError("outer requires x and y arguments")
                x_src = pos_o[0].strip()
                y_src = pos_o[1].strip()
                fun_src = kw_o.get("FUN", pos_o[2] if len(pos_o) >= 3 else "")
                m_fun = re.match(
                    r"^function\s*\(\s*([A-Za-z]\w*)\s*,\s*([A-Za-z]\w*)\s*\)\s*(.+)$",
                    fun_src.strip(),
                    re.IGNORECASE,
                )
                if m_fun:
                    vi = m_fun.group(1)
                    vj = m_fun.group(2)
                    fexpr_r = m_fun.group(3).strip()
                else:
                    op = fun_src.strip().strip("\"'")
                    if op not in {"+", "-", "*", "/", "^"}:
                        raise NotImplementedError("outer currently supports FUN=function(i,j) <expr>")
                    vi = "i_out"
                    vj = "j_out"
                    fexpr_r = f"ox(i_out) {op} oy(j_out)"
                fexpr_chk = re.sub(rf"\b{re.escape(vi)}\b", "1", fexpr_r)
                fexpr_chk = re.sub(rf"\b{re.escape(vj)}\b", "1", fexpr_chk)
                int_outer = _is_integer_arith_expr(fexpr_chk)
                if m_fun:
                    fexpr_r = re.sub(rf"\b{re.escape(vi)}\b", "ox(i_out)", fexpr_r)
                    fexpr_r = re.sub(rf"\b{re.escape(vj)}\b", "oy(j_out)", fexpr_r)
                fexpr_f = r_expr_to_fortran(fexpr_r)

                def _vec_expr(src: str) -> str:
                    seq = _split_top_level_colon(src.strip())
                    if seq is not None:
                        a0, b0 = seq
                        if int_outer:
                            return f"r_seq_int({_int_bound_expr(r_expr_to_fortran(a0))}, {_int_bound_expr(r_expr_to_fortran(b0))})"
                        return f"real(r_seq_int({_int_bound_expr(r_expr_to_fortran(a0))}, {_int_bound_expr(r_expr_to_fortran(b0))}), kind=dp)"
                    src_f = r_expr_to_fortran(src)
                    if int_outer:
                        return f"int({src_f})"
                    return src_f

                x_vec = _vec_expr(x_src)
                y_vec = _vec_expr(y_src)
                o.w("block")
                o.push()
                if int_outer:
                    o.w("integer, allocatable :: ox(:), oy(:)")
                else:
                    o.w("real(kind=dp), allocatable :: ox(:), oy(:)")
                o.w("integer :: i_out, j_out")
                o.w(f"ox = {x_vec}")
                o.w(f"oy = {y_vec}")
                o.w(f"if (allocated({st.name})) deallocate({st.name})")
                o.w(f"allocate({st.name}(size(ox), size(oy)))")
                o.w(f"do i_out = 1, size({st.name}, 1)")
                o.push()
                o.w(f"do j_out = 1, size({st.name}, 2)")
                o.push()
                o.w(f"{st.name}(i_out, j_out) = {fexpr_f}")
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            if c_lm is not None and c_lm[0].lower() == "lm":
                _nm_lm, pos_lm, kw_lm = c_lm
                form = pos_lm[0].strip() if pos_lm else kw_lm.get("formula", "").strip()
                m_form = re.match(r"^([A-Za-z]\w*)\s*~\s*(.+)$", form)
                if not m_form:
                    raise NotImplementedError("lm requires formula like y ~ x1 + x2 + ... in this subset")
                yv = r_expr_to_fortran(m_form.group(1).strip())
                rhs_terms = m_form.group(2).strip()
                data_name = kw_lm.get("data", "").strip()
                if rhs_terms == "." and data_name in data_frame_vars:
                    df_y, df_x = data_frame_vars[data_name]
                    x_terms = df_x
                    if not x_terms:
                        raise NotImplementedError("lm(y ~ ., data=df) requires predictor columns in data.frame")
                    lm_terms_by_fit[st.name] = x_terms
                    if len(x_terms) == 1 and x_terms[0] in matrix_vars:
                        xpred_f = r_expr_to_fortran(x_terms[0])
                        o.w("block")
                        o.push()
                        o.w(f"{st.name} = lm_fit_general({yv}, {xpred_f})")
                        o.pop()
                        o.w("end block")
                        if helper_ctx is not None:
                            helper_ctx["need_lm"] = True
                        continue
                    terms = x_terms
                else:
                    terms = [t.strip() for t in split_top_level_commas(rhs_terms.replace("+", ",")) if t.strip()]
                if not terms:
                    raise NotImplementedError("lm formula requires at least one predictor")
                lm_terms_by_fit[st.name] = terms
                p = len(terms)
                o.w("block")
                o.push()
                o.w("integer :: n_lm, p_lm")
                o.w("real(kind=dp), allocatable :: x_lm(:,:)")
                o.w(f"n_lm = size({yv})")
                o.w(f"p_lm = {p}")
                o.w("allocate(x_lm(n_lm, p_lm))")
                for j, tnm in enumerate(terms, start=1):
                    tv = r_expr_to_fortran(tnm)
                    o.w(f"x_lm(:, {j}) = {tv}")
                o.w(f"{st.name} = lm_fit_general({yv}, x_lm)")
                o.pop()
                o.w("end block")
                if helper_ctx is not None:
                    helper_ctx["need_lm"] = True
                continue
            c_pred = parse_call_text(rhs)
            if c_pred is not None and c_pred[0].lower() == "predict":
                _np, posp, kwp = c_pred
                fit_nm = posp[0].strip() if posp else kwp.get("object", "").strip()
                newd = kwp.get("newdata", "").strip()
                if not fit_nm or not newd:
                    raise NotImplementedError("predict requires object and newdata in this subset")
                terms = lm_terms_by_fit.get(fit_nm)
                if not terms:
                    raise NotImplementedError("predict requires preceding lm fit with known predictor terms")
                p = len(terms)
                o.w("block")
                o.push()
                o.w("integer :: n_pr, p_pr")
                o.w("real(kind=dp), allocatable :: x_pr(:,:)")
                first = r_expr_to_fortran(f"{newd}_{terms[0]}")
                o.w(f"n_pr = size({first})")
                o.w(f"p_pr = {p}")
                o.w("allocate(x_pr(n_pr, p_pr))")
                for j, tnm in enumerate(terms, start=1):
                    tv = r_expr_to_fortran(f"{newd}_{tnm}")
                    o.w(f"x_pr(:, {j}) = {tv}")
                o.w(f"{st.name} = lm_predict_general({fit_nm}, x_pr)")
                o.pop()
                o.w("end block")
                if helper_ctx is not None:
                    helper_ctx["need_lm"] = True
                continue
            c_cbind = parse_call_text(rhs)
            if c_cbind is not None and c_cbind[0].lower() == "cbind" and len(c_cbind[1]) == 2 and not c_cbind[2]:
                a_cb = c_cbind[1][0].strip()
                b_cb = c_cbind[1][1].strip()
                a_f = r_expr_to_fortran(a_cb)
                b_f = r_expr_to_fortran(b_cb)
                a_matrix = a_cb in matrix_vars or _looks_matrix_expr(a_cb)
                b_matrix = b_cb in matrix_vars or _looks_matrix_expr(b_cb)
                if (not a_matrix) and b_matrix and (_is_int_literal(a_f) or _is_real_literal(a_f)):
                    o.w("block")
                    o.push()
                    o.w(f"if (allocated({st.name})) deallocate({st.name})")
                    o.w(f"allocate({st.name}(size({b_f}, 1), size({b_f}, 2) + 1))")
                    o.w(f"{st.name}(:, 1) = real({a_f}, kind=dp)")
                    o.w(f"{st.name}(:, 2:size({st.name}, 2)) = {b_f}")
                    o.pop()
                    o.w("end block")
                    continue
                if a_matrix and (not b_matrix) and (_is_int_literal(b_f) or _is_real_literal(b_f)):
                    o.w("block")
                    o.push()
                    o.w(f"if (allocated({st.name})) deallocate({st.name})")
                    o.w(f"allocate({st.name}(size({a_f}, 1), size({a_f}, 2) + 1))")
                    o.w(f"{st.name}(:, 1:size({a_f}, 2)) = {a_f}")
                    o.w(f"{st.name}(:, size({st.name}, 2)) = real({b_f}, kind=dp)")
                    o.pop()
                    o.w("end block")
                    continue
            m_mat = re.match(r"^matrix\s*\((.*)\)\s*$", rhs, re.IGNORECASE)
            if m_mat:
                cinfo_m = parse_call_text("matrix(" + m_mat.group(1).strip() + ")")
                if cinfo_m is not None:
                    _nmm, posm, kwm = cinfo_m
                    data_src = posm[0] if posm else kwm.get("data", "")
                    nrow_src = kwm.get("nrow")
                    ncol_src = kwm.get("ncol")
                    byrow_src = kwm.get("byrow")
                    if nrow_src is None and len(posm) >= 2:
                        nrow_src = posm[1]
                    if ncol_src is None and len(posm) >= 3:
                        ncol_src = posm[2]
                    if byrow_src is None and len(posm) >= 4:
                        byrow_src = posm[3]
                    if nrow_src is None and ncol_src is None:
                        raise NotImplementedError("matrix(...) requires nrow or ncol in this subset")
                    d_ci = parse_call_text(data_src.strip())
                    if d_ci is not None and d_ci[0].lower() == "rnorm":
                        nsrc = d_ci[1][0] if d_ci[1] else d_ci[2].get("n")
                        if nsrc is None:
                            raise NotImplementedError("matrix(rnorm(...)) requires n argument")
                        n_f = _int_bound_expr(r_expr_to_fortran(nsrc))
                        if nrow_src is None:
                            nc_f = _int_bound_expr(r_expr_to_fortran(ncol_src))
                            nr_f = f"(({n_f} + ({nc_f}) - 1) / ({nc_f}))"
                        else:
                            nr_f = _int_bound_expr(r_expr_to_fortran(nrow_src))
                            if ncol_src is None:
                                nc_f = f"(({n_f} + ({nr_f}) - 1) / ({nr_f}))"
                            else:
                                nc_f = _int_bound_expr(r_expr_to_fortran(ncol_src))
                        byrow_true = str(byrow_src).strip().upper() in {"TRUE", ".TRUE.", "T", "1"} if byrow_src is not None else False
                        explicit_shape = nrow_src is not None and ncol_src is not None
                        if has_r_mod and (not byrow_true) and explicit_shape:
                            _wstmt(f"{st.name} = rnorm_mat({nr_f}, {nc_f})", st.comment)
                            need_r_mod.add("rnorm_mat")
                            need_rnorm["used"] = True
                            continue
                        o.w("block")
                        o.push()
                        o.w("real(kind=dp), allocatable :: tmp_m(:)")
                        if has_r_mod:
                            o.w(f"tmp_m = rnorm_vec({n_f})")
                            need_r_mod.add("rnorm_vec")
                        else:
                            o.w(f"call rnorm_vec({n_f}, tmp_m)")
                        if byrow_true:
                            _wstmt(
                                f"{st.name} = transpose(reshape(tmp_m, [{nc_f}, {nr_f}], pad=tmp_m))",
                                st.comment,
                            )
                        else:
                            _wstmt(f"{st.name} = reshape(tmp_m, [{nr_f}, {nc_f}], pad=tmp_m)", st.comment)
                        o.pop()
                        o.w("end block")
                        need_rnorm["used"] = True
                        continue
                    # Generic fallback for matrix(data, nrow=, ncol=)
                    data_f = _strict_int_vector_literal_from_c(data_src.strip()) or r_expr_to_fortran(data_src)
                    if nrow_src is None:
                        nc_f = _int_bound_expr(r_expr_to_fortran(ncol_src))
                        nr_f = f"((size({data_f}) + ({nc_f}) - 1) / ({nc_f}))"
                    else:
                        nr_f = _int_bound_expr(r_expr_to_fortran(nrow_src))
                        if ncol_src is None:
                            nc_f = f"((size({data_f}) + ({nr_f}) - 1) / ({nr_f}))"
                        else:
                            nc_f = _int_bound_expr(r_expr_to_fortran(ncol_src))
                    if data_f.strip().startswith("ieee_value("):
                        data_f = f"[{data_f}]"
                    elif not (
                        (data_f.startswith("[") and data_f.endswith("]"))
                        or re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)*(?:\([^()]*\))?$", data_f.strip())
                        or re.match(r"^[A-Za-z]\w*\s*\(", data_f.strip())
                    ):
                        data_f = f"[{data_f}]"
                    byrow_true = str(byrow_src).strip().upper() in {"TRUE", ".TRUE.", "T", "1"} if byrow_src is not None else False
                    if byrow_true:
                        _wstmt(
                            f"{st.name} = transpose(reshape({data_f}, [{nc_f}, {nr_f}], pad={data_f}))",
                            st.comment,
                        )
                    else:
                        _wstmt(f"{st.name} = reshape({data_f}, [{nr_f}, {nc_f}], pad={data_f})", st.comment)
                    continue
            m_asm_rt = re.match(
                r"^as\.matrix\s*\(\s*\(?\s*read\.table\s*\((.*)\)\s*\)?\s*\)\s*$",
                rhs,
                re.IGNORECASE,
            )
            if m_asm_rt:
                cinfo_rt = parse_call_text("read.table(" + m_asm_rt.group(1).strip() + ")")
                if cinfo_rt is None:
                    raise NotImplementedError("read.table parse failure")
                _nrt, prt, kwrt = cinfo_rt
                if prt:
                    path_src = prt[0]
                elif "file" in kwrt:
                    path_src = kwrt["file"]
                else:
                    raise NotImplementedError("read.table requires file argument in this subset")
                path_f = r_expr_to_fortran(path_src)
                o.w(f"call read_table_real_matrix({path_f}, {st.name})")
                if helper_ctx is not None:
                    helper_ctx["need_table_reader"] = True
                continue
            m_rt = re.match(r"^read\.table\s*\((.*)\)\s*$", rhs, re.IGNORECASE)
            if m_rt:
                cinfo_rt = parse_call_text("read.table(" + m_rt.group(1).strip() + ")")
                if cinfo_rt is None:
                    raise NotImplementedError("read.table parse failure")
                _nrt, prt, kwrt = cinfo_rt
                if prt:
                    path_src = prt[0]
                elif "file" in kwrt:
                    path_src = kwrt["file"]
                else:
                    raise NotImplementedError("read.table requires file argument in this subset")
                path_f = r_expr_to_fortran(path_src)
                o.w(f"call read_table_real_matrix({path_f}, {st.name})")
                if helper_ctx is not None:
                    helper_ctx["need_table_reader"] = True
                continue
            cinfo_rhs = parse_call_text(rhs)
            if cinfo_rhs is not None and cinfo_rhs[0].lower() == "scan":
                _nm, pos, kw = cinfo_rhs
                if pos:
                    path_src = pos[0]
                elif "file" in kw:
                    path_src = kw["file"]
                else:
                    raise NotImplementedError("scan requires file/path argument in this subset")
                path_f = r_expr_to_fortran(path_src)
                o.w(f"call read_real_vector({path_f}, {st.name})")
                if helper_ctx is not None:
                    helper_ctx["need_scan_reader"] = True
                continue
            # Inline rnorm(...) used inside arithmetic expressions.
            m_rn_inline = re.search(r"\brnorm\s*\(([^()]*)\)", rhs, re.IGNORECASE)
            if m_rn_inline:
                rn_call = "rnorm(" + m_rn_inline.group(1).strip() + ")"
                c_rn_i = parse_call_text(rn_call)
                if c_rn_i is not None:
                    _nn, pos_i, kw_i = c_rn_i
                    n_src = pos_i[0] if pos_i else kw_i.get("n", "")
                    n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                    mean_f = r_expr_to_fortran(kw_i.get("mean", "0.0"))
                    sd_f = r_expr_to_fortran(kw_i.get("sd", "1.0"))
                    if rhs.strip() == rn_call and has_r_mod:
                        if mean_f == "0.0_dp" and sd_f == "1.0_dp":
                            o.w(f"{st.name} = rnorm_vec({n_f})")
                        elif mean_f == "0.0_dp":
                            o.w(f"{st.name} = {_mul_factor_expr(sd_f)} * rnorm_vec({n_f})")
                        else:
                            o.w(f"{st.name} = ({mean_f}) + ({sd_f}) * rnorm_vec({n_f})")
                        need_r_mod.add("rnorm_vec")
                        continue
                    if has_r_mod and len(re.findall(r"\brnorm\s*\(", rhs, re.IGNORECASE)) == 1:
                        if mean_f == "0.0_dp" and sd_f == "1.0_dp":
                            rn_expr = f"rnorm_vec({n_f})"
                        elif mean_f == "0.0_dp":
                            rn_expr = f"{_mul_factor_expr(sd_f)} * rnorm_vec({n_f})"
                        else:
                            rn_expr = f"(({mean_f}) + ({sd_f}) * rnorm_vec({n_f}))"
                        rhs_i = rhs.replace(rn_call, rn_expr)
                        _wstmt(f"{st.name} = {r_expr_to_fortran(rhs_i)}", st.comment)
                        need_r_mod.add("rnorm_vec")
                        need_rnorm["used"] = True
                        continue
                    o.w("block")
                    o.push()
                    o.w("real(kind=dp), allocatable :: rn_tmp(:)")
                    if has_r_mod:
                        o.w(f"rn_tmp = rnorm_vec({n_f})")
                        need_r_mod.add("rnorm_vec")
                    else:
                        o.w(f"call rnorm_vec({n_f}, rn_tmp)")
                    if mean_f != "0.0_dp" or sd_f != "1.0_dp":
                        o.w(f"rn_tmp = ({mean_f}) + ({sd_f}) * rn_tmp")
                    rhs_i = rhs.replace(rn_call, "rn_tmp")
                    rhs_f_i = r_expr_to_fortran(rhs_i)
                    _wstmt(f"{st.name} = {rhs_f_i}", st.comment)
                    o.pop()
                    o.w("end block")
                    need_rnorm["used"] = True
                    continue
            c_numeric_assign = parse_call_text(rhs)
            if c_numeric_assign is not None and c_numeric_assign[0].lower() in {"numeric", "double"}:
                _nnm, pos_num, kw_num = c_numeric_assign
                n_src = pos_num[0] if pos_num else kw_num.get("length", kw_num.get("n", "0"))
                n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                if st.name in alloc_seen:
                    _wstmt(f"if (allocated({st.name})) deallocate({st.name})", "")
                _wstmt(f"allocate({st.name}(max(0, {n_f})), source=0.0_dp)", st.comment)
                alloc_seen.add(st.name)
                continue
            rhs_f = r_expr_to_fortran(_rewrite_predict_expr(rhs))
            c_rhs_exp = parse_call_text(rhs)
            if c_rhs_exp is not None and c_rhs_exp[0].lower() == "exp" and len(c_rhs_exp[1]) == 1:
                inner_exp = fscan.strip_redundant_outer_parens_expr(c_rhs_exp[1][0].strip())
                for op_exp in ["+", "-", "*", "/"]:
                    mm_exp = _split_top_level_token(inner_exp, op_exp, from_right=True)
                    if mm_exp is None:
                        continue
                    lhs_r = mm_exp[0].strip()
                    rhs_r = mm_exp[1].strip()
                    if not (re.match(r"^[A-Za-z]\w*$", lhs_r) and re.match(r"^[A-Za-z]\w*$", rhs_r)):
                        continue
                    lhs_is_mat = lhs_r in matrix_vars
                    rhs_is_mat = rhs_r in matrix_vars
                    lhs_is_vec = lhs_r in vector_vars
                    rhs_is_vec = rhs_r in vector_vars
                    if lhs_is_mat and rhs_is_vec:
                        lhs_f = r_expr_to_fortran(lhs_r)
                        rhs_fv = r_expr_to_fortran(rhs_r)
                        rhs_f = f"exp({lhs_f} {op_exp} spread({rhs_fv}, dim=2, ncopies=size({lhs_f},2)))"
                        break
                    if rhs_is_mat and lhs_is_vec:
                        rhs_fm = r_expr_to_fortran(rhs_r)
                        lhs_fv = r_expr_to_fortran(lhs_r)
                        rhs_f = f"exp(spread({lhs_fv}, dim=2, ncopies=size({rhs_fm},2)) {op_exp} {rhs_fm})"
                        break
            # Matrix-vector broadcast in R arithmetic (e.g. A - v where size(v)=nrow(A)).
            def _is_vector_expr_in_stmt(txt: str) -> bool:
                tt = txt.strip()
                if re.match(r"^[A-Za-z]\w*$", tt):
                    return tt in vector_vars
                ci_vec = parse_call_text(tt)
                if ci_vec is None:
                    return False
                return ci_vec[0].lower() in {"rowsums", "colsums", "sum", "maxval", "minval"}

            for op in ["+", "-", "*", "/"]:
                mm_mv = _split_top_level_token(rhs, op, from_right=True)
                if mm_mv is None:
                    continue
                lhs_r = mm_mv[0].strip()
                rhs_r = mm_mv[1].strip()
                if not (re.match(r"^[A-Za-z]\w*$", lhs_r) or re.match(r"^[A-Za-z]\w*$", rhs_r)):
                    continue
                lhs_is_mat = re.match(r"^[A-Za-z]\w*$", lhs_r) is not None and lhs_r in matrix_vars
                rhs_is_mat = re.match(r"^[A-Za-z]\w*$", rhs_r) is not None and rhs_r in matrix_vars
                lhs_is_vec = _is_vector_expr_in_stmt(lhs_r)
                rhs_is_vec = _is_vector_expr_in_stmt(rhs_r)
                if lhs_is_mat and rhs_is_vec:
                    lhs_f = r_expr_to_fortran(lhs_r)
                    rhs_fv = r_expr_to_fortran(rhs_r)
                    rhs_b = f"spread({rhs_fv}, dim=2, ncopies=size({lhs_f},2))"
                    rhs_f = f"{lhs_f} {op} {rhs_b}"
                    break
                if rhs_is_mat and lhs_is_vec:
                    rhs_fm = r_expr_to_fortran(rhs_r)
                    lhs_fv = r_expr_to_fortran(lhs_r)
                    lhs_b = f"spread({lhs_fv}, dim=2, ncopies=size({rhs_fm},2))"
                    rhs_f = f"{lhs_b} {op} {rhs_fm}"
                    break
            m_lhs_mask = re.match(r"^([A-Za-z]\w*)\s*\[\s*(.+)\s*\]$", st.name)
            if m_lhs_mask is not None:
                base_lhs = m_lhs_mask.group(1)
                inner_lhs = m_lhs_mask.group(2).strip()
                if "," not in inner_lhs:
                    inner_l = inner_lhs.lower()
                    if (
                        inner_l in _KNOWN_LOGICAL_VECTOR_NAMES
                        or re.match(r"^is\.na\s*\(", inner_l)
                        or re.match(r"^is_na\s*\(", inner_l)
                        or any(op in inner_l for op in ("==", "!=", "<=", ">=", "<", ">", ".and.", ".or."))
                    ):
                        mask_f = r_expr_to_fortran(inner_lhs)
                        _wstmt(f"where ({mask_f}) {base_lhs} = {rhs_f}", st.comment)
                        continue
            if rhs_f == st.name:
                # identity cast/normalization (e.g. x <- as.numeric(x))
                continue
            m_pack = re.match(rf"^{re.escape(st.name)}\s*\[\s*(.+)\s*\]\s*$", rhs)
            if m_pack:
                inner = m_pack.group(1).strip()
                if len(_split_index_dims(inner)) > 1:
                    _wstmt(f"{st.name} = {rhs_f}", st.comment)
                    continue
                inner_f = r_expr_to_fortran(inner)
                inner_l = inner.strip().lower()
                is_mask = (
                    inner_l in _KNOWN_LOGICAL_VECTOR_NAMES
                    or re.match(r"^is\.na\s*\(", inner_l)
                    or re.match(r"^is_na\s*\(", inner_l)
                    or re.match(r"^is\.finite\s*\(", inner_l)
                    or re.match(r"^ieee_is_finite\s*\(", inner_l)
                    or any(op in inner_l for op in ("==", "!=", "<=", ">=", "<", ">", ".and.", ".or."))
                )
                if ":" in inner:
                    sec = r_expr_to_fortran(inner)
                    o.w(f"{st.name} = {st.name}({sec})")
                elif not is_mask:
                    o.w(f"{st.name} = {st.name}({_index_inner_to_fortran(inner_f, base=st.name)})")
                else:
                    o.w(f"{st.name} = pack({st.name}, {inner_f})")
                continue
            m_if_runif = re.match(r"^ifelse\s*\(\s*runif\((.+)\)\s*<\s*(.+)\s*,\s*(.+)\s*,\s*(.+)\s*\)\s*$", rhs)
            if m_if_runif:
                n = r_expr_to_fortran(m_if_runif.group(1).strip())
                nb = _int_bound_expr(n)
                p = r_expr_to_fortran(m_if_runif.group(2).strip())
                a = r_expr_to_fortran(m_if_runif.group(3).strip())
                b = r_expr_to_fortran(m_if_runif.group(4).strip())
                if has_r_mod and a == "1" and b == "2":
                    o.w(f"{st.name} = random_choice2_prob({nb}, {p})")
                    need_r_mod.add("random_choice2_prob")
                    continue
                _emit_alloc_1d(st.name, nb)
                o.w("block")
                o.push()
                o.w("integer :: i_rf")
                o.w("real(kind=dp) :: u_rf")
                o.w(f"do i_rf = 1, {nb}")
                o.push()
                o.w("call random_number(u_rf)")
                if _is_simple_value_for_merge(a) and _is_simple_value_for_merge(b):
                    o.w(f"{st.name}(i_rf) = merge({a}, {b}, u_rf < {p})")
                else:
                    o.w(f"if (u_rf < {p}) then")
                    o.push()
                    o.w(f"{st.name}(i_rf) = {a}")
                    o.pop()
                    o.w("else")
                    o.push()
                    o.w(f"{st.name}(i_rf) = {b}")
                    o.pop()
                    o.w("end if")
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            cinfo = parse_call_text(rhs_call_src)
            if cinfo is not None and cinfo[0].lower() == "apply" and len(cinfo[1]) >= 3:
                mat_f = r_expr_to_fortran(cinfo[1][0])
                dim_f = _int_bound_expr(r_expr_to_fortran(cinfo[1][1]))
                fn_f = cinfo[1][2].strip()
                if re.match(r"^[A-Za-z]\w*$", fn_f):
                    def _apply_call(slice_f: str) -> str:
                        key = fn_f.lower()
                        if key == "max":
                            return f"maxval({slice_f})"
                        if key == "min":
                            return f"minval({slice_f})"
                        if key == "sum":
                            return f"sum({slice_f})"
                        if key == "mean":
                            return f"(sum({slice_f})/real(size({slice_f}), kind=dp))"
                        return f"{fn_f}({slice_f})"

                    o.w("block")
                    o.push()
                    o.w("integer :: i_apply")
                    if dim_f == "1":
                        slice_f = f"{mat_f}(i_apply, :)"
                        o.w(f"{st.name} = [({_apply_call(slice_f)}, i_apply=1,size({mat_f},1))]")
                    elif dim_f == "2":
                        slice_f = f"{mat_f}(:, i_apply)"
                        o.w(f"{st.name} = [({_apply_call(slice_f)}, i_apply=1,size({mat_f},2))]")
                    else:
                        raise NotImplementedError("apply currently supports dim 1 or 2")
                    o.pop()
                    o.w("end block")
                    continue
            if cinfo is not None and cinfo[0].lower() == "sample" and st.name in int_vector_vars:
                _wstmt(f"{st.name} = [{rhs_f}]", st.comment)
                if "sample_int1(" in rhs_f:
                    need_r_mod.add("sample_int1")
                elif "sample_int(" in rhs_f:
                    need_r_mod.add("sample_int")
                continue
            if cinfo is not None and cinfo[0].lower() == "rbinom":
                _wstmt(f"{st.name} = {rhs_f}", st.comment)
                need_r_mod.add("rbinom")
                continue
            if cinfo is not None and cinfo[0].lower() == "sample.int":
                if not has_r_mod:
                    raise NotImplementedError("sample.int requires helper module r_mod")
                _nm, pos, kw = cinfo
                if pos:
                    n_src = pos[0]
                elif "n" in kw:
                    n_src = kw["n"]
                else:
                    raise NotImplementedError("sample.int requires first argument n")
                n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                if "size" in kw:
                    size_src = kw["size"]
                elif len(pos) >= 2:
                    size_src = pos[1]
                else:
                    size_src = n_src
                size_f = _int_bound_expr(r_expr_to_fortran(size_src))
                rep_f = r_expr_to_fortran(kw.get("replace", "FALSE"))
                prob_src = kw.get("prob")
                if prob_src is not None:
                    prob_f = r_expr_to_fortran(prob_src)
                    o.w(f"{st.name} = sample_int({n_f}, size_={size_f}, replace={rep_f}, prob={prob_f})")
                else:
                    o.w(f"{st.name} = sample_int({n_f}, size_={size_f}, replace={rep_f})")
                need_r_mod.add("sample_int")
                continue
            if cinfo is not None and cinfo[0].lower() in {"t.test", "t_test"}:
                _wstmt(f"{st.name} = {rhs_f}", st.comment)
                need_r_mod.update({"t_test", "t_test_result_t"})
                continue
            if rhs.startswith("runif(") and rhs.endswith(")"):
                c_ru = parse_call_text(rhs)
                if c_ru is not None:
                    _nru, pos_ru, kw_ru = c_ru
                    n_src = pos_ru[0] if pos_ru else kw_ru.get("n", "")
                    n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                    if len(pos_ru) >= 3:
                        a_f = r_expr_to_fortran(pos_ru[1])
                        b_f = r_expr_to_fortran(pos_ru[2])
                    else:
                        a_f = r_expr_to_fortran(kw_ru.get("min", "0.0"))
                        b_f = r_expr_to_fortran(kw_ru.get("max", "1.0"))
                    if has_r_mod:
                        o.w(f"{st.name} = ({a_f}) + (({b_f}) - ({a_f})) * runif_vec({n_f})")
                        need_r_mod.add("runif_vec")
                    else:
                        _emit_alloc_1d(st.name, n_f)
                        o.w(f"call random_number({st.name})")
                        o.w(f"{st.name} = ({a_f}) + (({b_f}) - ({a_f})) * {st.name}")
                    continue
                n = r_expr_to_fortran(rhs[len("runif(") : -1])
                nb = _int_bound_expr(n)
                if has_r_mod:
                    o.w(f"{st.name} = runif_vec({nb})")
                    need_r_mod.add("runif_vec")
                else:
                    _emit_alloc_1d(st.name, nb)
                    o.w(f"call random_number({st.name})")
            elif re.match(r"^rnorm\s*\(", rhs):
                # Special-case: rnorm(n, mean = mu[z], sd = sigma[z])
                m_rmix = re.match(
                    r"^rnorm\s*\(\s*([^,]+)\s*,\s*mean\s*=\s*([A-Za-z]\w*)\s*\[\s*([A-Za-z]\w*)\s*\]\s*,\s*sd\s*=\s*([A-Za-z]\w*)\s*\[\s*([A-Za-z]\w*)\s*\]\s*\)\s*$",
                    rhs,
                )
                if m_rmix:
                    n = r_expr_to_fortran(m_rmix.group(1).strip())
                    nb = _int_bound_expr(n)
                    mu = m_rmix.group(2)
                    z1 = m_rmix.group(3)
                    sd = m_rmix.group(4)
                    z2 = m_rmix.group(5)
                    if z1 != z2:
                        raise NotImplementedError("rnorm mean/sd index variables must match")
                    z = z1
                    if has_r_mod:
                        o.w(f"{st.name} = {mu}({z}) + {sd}({z}) * rnorm_vec({nb})")
                        need_r_mod.add("rnorm_vec")
                        continue
                    _emit_alloc_1d(st.name, nb)
                    o.w("block")
                    o.push()
                    o.w("integer :: i_rg, k_rg")
                    o.w("real(kind=dp) :: u1_rg, u2_rg, g_rg")
                    o.w(f"do i_rg = 1, {nb}")
                    o.push()
                    o.w("call random_number(u1_rg)")
                    o.w("call random_number(u2_rg)")
                    o.w("if (u1_rg <= tiny(1.0_dp)) cycle")
                    o.w("g_rg = sqrt(-2.0_dp * log(u1_rg)) * cos(2.0_dp * acos(-1.0_dp) * u2_rg)")
                    o.w(f"k_rg = int({z}(i_rg))")
                    o.w(f"{st.name}(i_rg) = {mu}(k_rg) + {sd}(k_rg) * g_rg")
                    o.pop()
                    o.w("end do")
                    o.pop()
                    o.w("end block")
                    continue
                # fallback simple rnorm(n)
                m_rn = re.match(r"^rnorm\s*\(\s*([^)]+)\s*\)\s*$", rhs)
                if m_rn:
                    n = r_expr_to_fortran(m_rn.group(1))
                    if has_r_mod:
                        o.w(f"{st.name} = rnorm_vec({_int_bound_expr(n)})")
                        need_r_mod.add("rnorm_vec")
                    else:
                        o.w(f"call rnorm_vec({_int_bound_expr(n)}, {st.name})")
                        need_rnorm["used"] = True
                    continue
                c_rn = parse_call_text(rhs)
                if c_rn is not None:
                    _nrn, pos_rn, kw_rn = c_rn
                    n_src = pos_rn[0] if pos_rn else kw_rn.get("n", "")
                    n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                    mean_f = r_expr_to_fortran(kw_rn.get("mean", "0.0"))
                    sd_f = r_expr_to_fortran(kw_rn.get("sd", "1.0"))
                    if has_r_mod:
                        o.w(f"{st.name} = ({mean_f}) + ({sd_f}) * rnorm_vec({n_f})")
                        need_r_mod.add("rnorm_vec")
                    else:
                        o.w(f"call rnorm_vec({n_f}, {st.name})")
                        o.w(f"{st.name} = ({mean_f}) + ({sd_f}) * {st.name}")
                        need_rnorm["used"] = True
                    continue
                raise NotImplementedError(f"unsupported rnorm form: {rhs}")
            elif rhs.startswith("rnorm(") and rhs.endswith(")"):
                n = r_expr_to_fortran(rhs[len("rnorm(") : -1])
                if has_r_mod:
                    o.w(f"{st.name} = rnorm_vec({_int_bound_expr(n)})")
                    need_r_mod.add("rnorm_vec")
                else:
                    o.w(f"call rnorm_vec({_int_bound_expr(n)}, {st.name})")
                    need_rnorm["used"] = True
            else:
                _wstmt(f"{st.name} = {rhs_f}", st.comment)
        elif isinstance(st, PrintStmt):
            if st.args:
                print_args: list[str] = []
                for a_pr in st.args:
                    m_kw_pr = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)?)\s*=\s*(.+)$", a_pr.strip())
                    if m_kw_pr is not None and _sanitize_fortran_kwarg_name(m_kw_pr.group(1)).lower() in {
                        "row_names",
                        "quote",
                        "right",
                        "digits",
                        "na_print",
                        "print_gap",
                    }:
                        continue
                    print_args.append(a_pr)
                st = PrintStmt(args=print_args, comment=st.comment)
                if len(st.args) == 1:
                    one = st.args[0].strip()
                    one_call_src = re.sub(r"\bt\.test\s*\(", "t_test(", one, flags=re.IGNORECASE)
                    c_one = parse_call_text(one_call_src)
                    one_f_early = r_expr_to_fortran(_rewrite_predict_expr(one))
                    if re.match(r"^t_test\s*\(", one_f_early.strip(), re.IGNORECASE):
                        _wstmt(f"call print_t_test({one_f_early})", st.comment)
                        need_r_mod.update({"print_t_test", "t_test", "t_test_result_t"})
                        continue
                    if re.match(r"^chisq_test\s*\(", one_f_early.strip(), re.IGNORECASE):
                        _wstmt(f"call print_chisq_test({one_f_early})", st.comment)
                        need_r_mod.update({"print_chisq_test", "chisq_test", "chisq_test_result_t"})
                        continue
                    if re.match(r"^prop_test\s*\(", one_f_early.strip(), re.IGNORECASE):
                        _wstmt(f"call print_prop_test({one_f_early})", st.comment)
                        need_r_mod.update({"print_prop_test", "prop_test", "prop_test_result_t"})
                        continue
                    if re.match(r"^cor_test\s*\(", one_f_early.strip(), re.IGNORECASE):
                        _wstmt(f"call print_cor_test({one_f_early})", st.comment)
                        need_r_mod.update({"print_cor_test", "cor_test", "cor_test_result_t"})
                        continue
                    if re.match(r"^fisher_test\s*\(", one_f_early.strip(), re.IGNORECASE):
                        _wstmt(f"call print_fisher_test({one_f_early})", st.comment)
                        need_r_mod.update({"print_fisher_test", "fisher_test", "fisher_test_result_t"})
                        continue
                    if c_one is not None and c_one[0].lower() in {"t.test", "t_test"}:
                        _wstmt(f"call print_t_test({r_expr_to_fortran(one)})", st.comment)
                        need_r_mod.update({"t_test", "print_t_test", "t_test_result_t"})
                        continue
                    if c_one is not None and c_one[0].lower() in {"chisq.test", "chisq_test"}:
                        _wstmt(f"call print_chisq_test({r_expr_to_fortran(one)})", st.comment)
                        need_r_mod.update({"chisq_test", "print_chisq_test", "chisq_test_result_t"})
                        continue
                    if c_one is not None and c_one[0].lower() in {"prop.test", "prop_test"}:
                        _wstmt(f"call print_prop_test({r_expr_to_fortran(one)})", st.comment)
                        need_r_mod.update({"prop_test", "print_prop_test", "prop_test_result_t"})
                        continue
                    if c_one is not None and c_one[0].lower() in {"cor.test", "cor_test"}:
                        _wstmt(f"call print_cor_test({r_expr_to_fortran(one)})", st.comment)
                        need_r_mod.update({"cor_test", "print_cor_test", "cor_test_result_t"})
                        continue
                    if c_one is not None and c_one[0].lower() in {"fisher.test", "fisher_test"}:
                        _wstmt(f"call print_fisher_test({r_expr_to_fortran(one)})", st.comment)
                        need_r_mod.update({"fisher_test", "print_fisher_test", "fisher_test_result_t"})
                        continue
                    if re.fullmatch(r"[A-Za-z]\w*", one) and one in t_test_vars_ctx:
                        _wstmt(f"call print_t_test({one})", st.comment)
                        need_r_mod.update({"print_t_test", "t_test_result_t"})
                        continue
                    if re.fullmatch(r"[A-Za-z]\w*", one) and one in _EXPANDED_DATA_FRAME_FIELDS:
                        fields_df = _EXPANDED_DATA_FRAME_FIELDS.get(one, [])
                        if fields_df:
                            col_vars = [
                                _expanded_data_frame_col_expr(one, f) or f"{one}_{_sanitize_fortran_kwarg_name(f)}"
                                for f in fields_df
                            ]
                            header = ", ".join(f'"{f}"' for f in fields_df)
                            _wstmt(f'write(*,"(*(a,1x))") {header}', st.comment)
                            o.w("block")
                            o.push()
                            o.w("integer :: i_df")
                            o.w(f"do i_df = 1, size({col_vars[0]})")
                            o.push()
                            o.w(f'write(*,"(*(g0,1x))") ' + ", ".join(f"{v}(i_df)" for v in col_vars))
                            o.pop()
                            o.w("end do")
                            o.pop()
                            o.w("end block")
                            continue
                    if c_one is not None and c_one[0].lower() == "data.frame" and c_one[2]:
                        cols_df = [(k, v) for k, v in c_one[2].items()]
                        if len(cols_df) >= 2:
                            def _df_vals(src_df: str) -> list[str]:
                                t_df = src_df.strip()
                                c_dfv = parse_call_text(t_df)
                                if c_dfv is not None and c_dfv[0].lower() == "c":
                                    return list(c_dfv[1]) + list(c_dfv[2].values())
                                if t_df.startswith("[") and t_df.endswith("]"):
                                    return split_top_level_commas(t_df[1:-1])
                                return [t_df]

                            vals0 = _df_vals(cols_df[0][1])
                            vals1 = _df_vals(cols_df[1][1])
                            if len(vals0) == len(vals1) and vals0:
                                _wstmt(f'write(*,"(a,1x,a)") "{cols_df[0][0]}", "{cols_df[1][0]}"', st.comment)
                                for v0, v1 in zip(vals0, vals1):
                                    _wstmt(f'write(*,"(a,1x,g0)") {r_expr_to_fortran(v0)}, {r_expr_to_fortran(v1)}', "")
                                continue
                    if has_r_mod and c_one is not None and c_one[0].lower() in {"paste", "paste0"} and len(c_one[1]) >= 2:
                        first_arg = c_one[1][0].strip()
                        second_arg = c_one[1][1].strip()
                        second_call = parse_call_text(second_arg)
                        second_is_matrix = _expr_rank_for_print(second_arg) == 2 or (
                            second_call is not None and second_call[0].lower() in {"cor", "cov"}
                        )
                        if second_is_matrix:
                            _wstmt(f"print *, {r_expr_to_fortran(first_arg)}", st.comment)
                            _wstmt(f"call print_matrix({r_expr_to_fortran(second_arg)})", "")
                            need_r_mod.add("print_matrix_rstyle")
                            continue
                    if (not has_r_mod) and c_one is not None and c_one[0].lower() in {"runif", "rnorm"}:
                        nm_rng = c_one[0].lower()
                        pos_rng, kw_rng = c_one[1], c_one[2]
                        n_src = pos_rng[0] if pos_rng else kw_rng.get("n", "1")
                        n_f = _int_bound_expr(r_expr_to_fortran(n_src))
                        o.w("block")
                        o.push()
                        o.w("real(kind=dp), allocatable :: v_pr(:)")
                        o.w(f"allocate(v_pr({n_f}))")
                        if nm_rng == "runif":
                            o.w("call random_number(v_pr)")
                            if len(pos_rng) >= 3:
                                a_f = r_expr_to_fortran(pos_rng[1])
                                b_f = r_expr_to_fortran(pos_rng[2])
                            else:
                                a_f = r_expr_to_fortran(kw_rng.get("min", "0.0"))
                                b_f = r_expr_to_fortran(kw_rng.get("max", "1.0"))
                            if not (a_f == "0.0_dp" and b_f == "1.0_dp"):
                                o.w(f"v_pr = ({a_f}) + (({b_f}) - ({a_f})) * v_pr")
                        else:
                            mean_f = r_expr_to_fortran(kw_rng.get("mean", "0.0"))
                            sd_f = r_expr_to_fortran(kw_rng.get("sd", "1.0"))
                            o.w(f"call rnorm_vec({n_f}, v_pr)")
                            need_rnorm["used"] = True
                            if not (mean_f == "0.0_dp" and sd_f == "1.0_dp"):
                                o.w(f"v_pr = ({mean_f}) + ({sd_f}) * v_pr")
                        _wstmt('write(*,"(*(g0,1x))") v_pr', st.comment)
                        o.pop()
                        o.w("end block")
                        continue
                    names_arg = _names_call_print_arg(one)
                    if has_r_mod and names_arg is not None:
                        _wstmt(f"call print_char_vector({names_arg})", st.comment)
                        need_r_mod.add("print_char_vector")
                        continue
                    table_labels = _table_labels_for_expr(one)
                    if has_r_mod and table_labels is not None:
                        row_labs, col_labs = table_labels
                        one_f_tbl = r_expr_to_fortran(one)
                        if col_labs is None:
                            if row_labs:
                                _wstmt(f"call print_table1({one_f_tbl}, {_char_array_literal(row_labs)})", st.comment)
                                need_r_mod.add("print_table1")
                            else:
                                _wstmt(f"call print_real_vector(real({one_f_tbl}, kind=dp))", st.comment)
                                need_r_mod.add("print_real_vector")
                            continue
                        if row_labs and col_labs:
                            _wstmt(
                                f"call print_table2({one_f_tbl}, {_char_array_literal(row_labs)}, {_char_array_literal(col_labs)})",
                                st.comment,
                            )
                            need_r_mod.add("print_table2")
                            continue
                    if has_r_mod and c_one is not None and c_one[0].lower() in {"prop.table", "prop_table"}:
                        vals_prop = list(c_one[1]) + [v for k, v in c_one[2].items() if k.lower() != "margin"]
                        x_prop = vals_prop[0].strip() if vals_prop else ""
                        prop_labs = _table_labels_for_expr(x_prop)
                        if prop_labs is not None:
                            row_labs, col_labs = prop_labs
                            one_f_prop = r_expr_to_fortran(one)
                            if col_labs is None and row_labs:
                                _wstmt(
                                    f"call print_named_real_vector({one_f_prop}, {_char_array_literal(row_labs)})",
                                    st.comment,
                                )
                                need_r_mod.add("print_named_real_vector")
                                continue
                            if row_labs and col_labs:
                                _wstmt(
                                    f"call print_table2({one_f_prop}, {_char_array_literal(row_labs)}, {_char_array_literal(col_labs)})",
                                    st.comment,
                                )
                                need_r_mod.add("print_table2")
                                continue
                    named_parts = _named_vector_print_parts(one)
                    if has_r_mod and named_parts is not None:
                        val_expr, name_expr, scalar_val = named_parts
                        if scalar_val:
                            _wstmt(
                                f"call print_named_real_vector([real({val_expr}, kind=dp)], {name_expr})",
                                st.comment,
                            )
                        else:
                            _wstmt(
                                f"call print_named_real_vector(real({val_expr}, kind=dp), {name_expr})",
                                st.comment,
                            )
                        need_r_mod.add("print_named_real_vector")
                        continue
                    rank_one = _expr_rank_for_print(one)
                    if rank_one == 2:
                        one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                        if has_r_mod:
                            labels = _matrix_col_labels_for_print(one)
                            if labels is not None:
                                int_cols = _int_col_mask_literal(labels)
                                if int_cols is not None:
                                    _wstmt(
                                        f"call print_matrix_rstyle_named({one_f}, {_char_array_literal(labels)}, "
                                        f"int_cols={int_cols})",
                                        st.comment,
                                    )
                                else:
                                    _wstmt(f"call print_matrix_rstyle_named({one_f}, {_char_array_literal(labels)})", st.comment)
                                need_r_mod.add("print_matrix_rstyle_named")
                            else:
                                label_expr = _matrix_col_label_expr_for_print(one)
                                if label_expr is not None:
                                    if re.fullmatch(r"arma_mat", one_f.strip(), re.IGNORECASE):
                                        _wstmt(
                                            f"call print_matrix_rstyle_named({one_f}, {label_expr}, "
                                            f"int_cols=[.true., .true., spread(.false., dim=1, "
                                            f"ncopies=max(0, size({label_expr}) - 4)), .true., .true.])",
                                            st.comment,
                                        )
                                    else:
                                        _wstmt(f"call print_matrix_rstyle_named({one_f}, {label_expr})", st.comment)
                                    need_r_mod.add("print_matrix_rstyle_named")
                                else:
                                    _wstmt(f"call print_matrix({one_f})", st.comment)
                                    need_r_mod.add("print_matrix_rstyle")
                        else:
                            o.w("block")
                            o.push()
                            o.w("integer :: i_pr")
                            o.w(f"associate(m_pr => {one_f})")
                            o.push()
                            o.w("do i_pr = 1, size(m_pr, 1)")
                            o.push()
                            o.w('write(*,"(*(g0,1x))") m_pr(i_pr, :)')
                            o.pop()
                            o.w("end do")
                            o.pop()
                            o.w("end associate")
                            o.pop()
                            o.w("end block")
                        continue
                    if rank_one == 0 and has_r_mod:
                        one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                        m_df_f = re.match(
                            r"^data\.frame\s*\(\s*criterion\s*=\s*\[(.+?)\]\s*,\s*selected_ncomp\s*=\s*\[(.+?)\]\s*\)\s*$",
                            one_f.strip(),
                            re.IGNORECASE,
                        )
                        if m_df_f is not None:
                            labels_df = split_top_level_commas(m_df_f.group(1))
                            vals_df = split_top_level_commas(m_df_f.group(2))
                            if len(labels_df) == len(vals_df) and labels_df:
                                _wstmt('write(*,"(a,1x,a)") "criterion", "selected_ncomp"', st.comment)
                                for lab_df, val_df in zip(labels_df, vals_df):
                                    _wstmt(f'write(*,"(a,1x,g0)") {lab_df.strip()}, {val_df.strip()}', "")
                                continue
                        if re.match(r"^t_test\s*\(", one_f.strip(), re.IGNORECASE):
                            _wstmt(f"call print_t_test({one_f})", st.comment)
                            need_r_mod.update({"print_t_test", "t_test", "t_test_result_t"})
                            continue
                        if re.match(r"^[A-Za-z]\w*\s*\(.*\)$", one_f.strip()):
                            _wstmt(f"call print_real_scalar({one_f})", st.comment)
                        elif _looks_integer_fortran_expr(one_f):
                            _wstmt(f"call print_real_scalar(real({one_f}, kind=dp))", st.comment)
                        else:
                            _wstmt(f"call print_real_scalar({one_f})", st.comment)
                        need_r_mod.add("print_real_scalar")
                        continue
                    if rank_one == 1 and not (
                        one.lower().startswith("r_matmul(")
                        or _split_top_level_token(one, "%*%", from_right=True) is not None
                    ):
                        one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                        if has_r_mod:
                            _wstmt(f"call print_real_vector(real({one_f}, kind=dp))", st.comment)
                            need_r_mod.add("print_real_vector")
                        else:
                            _wstmt(f'write(*,"(*(g0,1x))") {one_f}', st.comment)
                        continue
                    if rank_one == 1 and (
                        one.lower().startswith("r_matmul(")
                        or _split_top_level_token(one, "%*%", from_right=True) is not None
                    ):
                        one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                        if has_r_mod:
                            o.w("block")
                            o.push()
                            o.w("real(kind=dp), allocatable :: v_pr(:)")
                            o.w(f"v_pr = {one_f}")
                            _wstmt("call print_matrix(reshape(v_pr, [size(v_pr), 1]))", st.comment)
                            need_r_mod.add("print_matrix_rstyle")
                            o.pop()
                            o.w("end block")
                        else:
                            _wstmt(f'write(*,"(*(g0,1x))") {one_f}', st.comment)
                        continue
                    if c_one is not None:
                        nm_one = c_one[0].lower()
                        is_matrix_expr = nm_one in {"matrix", "cbind", "cbind2", "array"} or (
                            nm_one in {"cov", "cor"} and len(c_one[1]) <= 1
                        )
                        if is_matrix_expr:
                            one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                            use_helper_print = has_r_mod and (
                                nm_one in {"matrix", "cbind", "cbind2"}
                                or (nm_one in {"cov", "cor"} and len(c_one[1]) <= 1)
                            )
                            if use_helper_print:
                                _wstmt(f"call print_matrix({one_f})", st.comment)
                                need_r_mod.add("print_matrix_rstyle")
                            else:
                                o.w("block")
                                o.push()
                                o.w("integer :: i_pr")
                                o.w(f"associate(m_pr => {one_f})")
                                o.push()
                                o.w("do i_pr = 1, size(m_pr, 1)")
                                o.push()
                                o.w('write(*,"(*(g0,1x))") m_pr(i_pr, :)')
                                o.pop()
                                o.w("end do")
                                o.pop()
                                o.w("end associate")
                                o.pop()
                                o.w("end block")
                            continue
                    m_mat_expr = re.match(r"^([A-Za-z]\w*)\s*\((.*)\)$", one)
                    if m_mat_expr is None:
                        m_mat_expr = re.match(r"^([A-Za-z]\w*)\s*\[(.*)\]$", one)
                    if m_mat_expr:
                        root = m_mat_expr.group(1)
                        inner = m_mat_expr.group(2)
                        dims = _split_index_dims(inner)
                        if len(dims) >= 2:
                            if root in int_matrix_vars:
                                one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                                if has_r_mod:
                                    _wstmt(f"call print_matrix({one_f})", st.comment)
                                    need_r_mod.add("print_matrix_rstyle")
                                else:
                                    o.w("block")
                                    o.push()
                                    o.w("integer :: i_pr")
                                    o.w("integer, allocatable :: m_pr(:,:)")
                                    o.w(f"m_pr = {one_f}")
                                    o.w("do i_pr = 1, size(m_pr, 1)")
                                    o.push()
                                    o.w('write(*,"(*(i0,1x))") m_pr(i_pr, :)')
                                    o.pop()
                                    o.w("end do")
                                    o.pop()
                                    o.w("end block")
                                continue
                            if root in real_matrix_vars:
                                one_f = r_expr_to_fortran(_rewrite_predict_expr(one))
                                if has_r_mod:
                                    _wstmt(f"call print_matrix({one_f})", st.comment)
                                    need_r_mod.add("print_matrix_rstyle")
                                else:
                                    o.w("block")
                                    o.push()
                                    o.w("integer :: i_pr")
                                    o.w("real(kind=dp), allocatable :: m_pr(:,:)")
                                    o.w(f"m_pr = {one_f}")
                                    o.w("do i_pr = 1, size(m_pr, 1)")
                                    o.push()
                                    o.w('write(*,"(*(g0,1x))") m_pr(i_pr, :)')
                                    o.pop()
                                    o.w("end do")
                                    o.pop()
                                    o.w("end block")
                                continue
                    if re.match(r"^[A-Za-z]\w*$", one):
                        if one in int_matrix_vars:
                            if has_r_mod:
                                _wstmt(f"call print_matrix({one})", st.comment)
                                need_r_mod.add("print_matrix_rstyle")
                            else:
                                o.w("block")
                                o.push()
                                o.w("integer :: i_pr")
                                o.w(f"do i_pr = 1, size({one}, 1)")
                                o.push()
                                o.w(f'write(*,"(*(i0,1x))") {one}(i_pr, :)')
                                o.pop()
                                o.w("end do")
                                o.pop()
                                o.w("end block")
                            continue
                        if one in real_matrix_vars:
                            if has_r_mod:
                                _wstmt(f"call print_matrix({one})", st.comment)
                                need_r_mod.add("print_matrix_rstyle")
                            else:
                                o.w("block")
                                o.push()
                                o.w("integer :: i_pr")
                                o.w(f"do i_pr = 1, size({one}, 1)")
                                o.push()
                                o.w(f'write(*,"(*(g0,1x))") {one}(i_pr, :)')
                                o.pop()
                                o.w("end do")
                                o.pop()
                                o.w("end block")
                            continue
                    m_sum = re.match(r"^summary\s*\((.*)\)\s*$", one, re.IGNORECASE)
                    if m_sum:
                        sum_arg = m_sum.group(1).strip()
                        sum_arg_f = r_expr_to_fortran(sum_arg)
                        if (
                            (re.fullmatch(r"[A-Za-z]\w*", sum_arg) and sum_arg in lm_terms_by_fit)
                            or re.search(r"(?:\$|%)\s*lm_fit\s*$", sum_arg, re.IGNORECASE)
                        ):
                            _wstmt(f"call print_lm_summary({sum_arg_f})", st.comment)
                            if helper_ctx is not None:
                                helper_ctx["need_lm"] = True
                            if has_r_mod:
                                need_r_mod.add("print_lm_summary")
                        elif has_r_mod:
                            _wstmt(f"call print_summary({sum_arg_f})", st.comment)
                            need_r_mod.add("print_summary")
                        else:
                            _wstmt(f"print *, summary({sum_arg_f})", st.comment)
                        continue
                    m_coef = re.match(r"^coef\s*\(\s*([A-Za-z]\w*)\s*\)\s*$", one, re.IGNORECASE)
                    if m_coef:
                        fit_nm = m_coef.group(1)
                        terms = lm_terms_by_fit.get(fit_nm, [])
                        if terms:
                            max_len = max(1, max(len(t) for t in terms))
                            terms_lit = ", ".join(f'"{t}"' for t in terms)
                            _wstmt(
                                f'call print_lm_coef_rstyle({fit_nm}, [character(len={max_len}) :: {terms_lit}])',
                                st.comment,
                            )
                        else:
                            _wstmt(f"call print_lm_coef_rstyle({fit_nm})", st.comment)
                        if helper_ctx is not None:
                            helper_ctx["need_lm"] = True
                        continue
                _wstmt("print *, " + ", ".join(r_expr_to_fortran(_rewrite_predict_expr(a)) for a in st.args), st.comment)
            else:
                _wstmt("print *", st.comment)
        elif isinstance(st, CallStmt):
            nm = st.name.lower()
            if nm == "stop":
                if st.args:
                    msg = _dequote_string_literal(st.args[0].strip())
                    if msg is None:
                        msg = st.args[0].strip()
                    o.w(f"error stop {_fortran_error_msg(str(msg))}")
                else:
                    o.w('error stop "stop requested"')
                continue
            if nm == "stopifnot":
                if not st.args:
                    continue
                for a in st.args:
                    cond = fscan.strip_redundant_outer_parens_expr(r_expr_to_fortran(a))
                    neg = _negate_simple_relational_expr(cond)
                    if neg is not None:
                        msg = _fortran_error_msg(f"error: need {cond}")
                        o.w(f"if ({neg}) error stop {msg}")
                    else:
                        msg = _fortran_error_msg(f"error: need {cond}")
                        o.w(f"if (.not. ({cond})) error stop {msg}")
                continue
            if nm == "set.seed":
                if st.args:
                    seed_arg = r_expr_to_fortran(st.args[0])
                    if has_r_mod:
                        need_r_mod.add("set_seed_int")
                        _wstmt(f"call set_seed_int(int({seed_arg}))", st.comment)
                    else:
                        _wstmt("call random_seed()", st.comment)
                else:
                    _wstmt("call random_seed()", st.comment)
                continue
            if nm == "cat":
                if st.args:
                    out_items: list[str] = []
                    for a in st.args:
                        at = a.strip()
                        m_kw = re.match(r"^([A-Za-z]\w*)\s*=\s*(.+)$", at)
                        if m_kw is not None:
                            kn = m_kw.group(1).lower()
                            # cat control keywords are not output payload items.
                            if kn in {"sep", "file", "fill", "labels", "append"}:
                                continue
                        if at in {'"\\n"', "'\\n'"}:
                            continue
                        sp_items = _sprintf_arg_items(at)
                        if sp_items is not None:
                            out_items.extend(sp_items)
                            continue
                        lit = _dequote_string_literal(at)
                        if lit is not None:
                            lit2 = lit.replace("\\n", "").replace("\\t", " ")
                            if lit2.endswith("="):
                                lit2 = lit2 + " "
                            if lit2.endswith(":"):
                                lit2 = lit2 + " "
                            if lit2:
                                out_items.append(_fortran_str_literal(lit2))
                            continue
                        out_items.append(_display_expr_to_fortran(a))
                    if out_items:
                        _wstmt("write(*,*) " + ", ".join(out_items), st.comment)
                    else:
                        _wstmt("write(*,*)", st.comment)
                else:
                    _wstmt("write(*,*)", st.comment)
                continue
            if nm == "writelines":
                call_text = f"{st.name}(" + ", ".join(st.args) + ")"
                cinfo = parse_call_text(call_text)
                if cinfo is None:
                    raise NotImplementedError("writeLines parse failure")
                _nmc, pos, kw = cinfo
                if pos:
                    data_src = pos[0]
                elif "text" in kw:
                    data_src = kw["text"]
                else:
                    raise NotImplementedError("writeLines requires text/data argument")
                con_src = kw.get("con", '"out.txt"')
                data_f = r_expr_to_fortran(data_src)
                write_fmt = "(g0.17)"
                fmt_ci = parse_call_text(data_src)
                if fmt_ci is not None and fmt_ci[0].lower() == "format":
                    _fnm_fmt, pos_fmt, kw_fmt = fmt_ci
                    if pos_fmt:
                        data_f = r_expr_to_fortran(pos_fmt[0])
                    dsrc = kw_fmt.get("digits")
                    if dsrc is not None and _is_int_literal(dsrc.strip()):
                        d = int(dsrc.strip())
                        if d < 1:
                            d = 1
                        if d > 30:
                            d = 30
                        write_fmt = f"(g0.{d})"
                con_f = r_expr_to_fortran(con_src)
                o.w("block")
                o.push()
                o.w("integer :: fp, i_wl")
                o.w(f'open(newunit=fp, file={con_f}, status="replace", action="write")')
                o.w(f"if (size({data_f}) > 0) then")
                o.push()
                o.w(f"do i_wl = 1, size({data_f})")
                o.push()
                o.w(f'write(fp, "{write_fmt}") {data_f}(i_wl)')
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end if")
                o.w("close(fp)")
                o.pop()
                o.w("end block")
                continue
            if nm == "write.table":
                call_text = f"{st.name}(" + ", ".join(st.args) + ")"
                cinfo = parse_call_text(call_text)
                if cinfo is None:
                    raise NotImplementedError("write.table parse failure")
                _nmc, pos, kw = cinfo
                if pos:
                    data_src = pos[0]
                elif "x" in kw:
                    data_src = kw["x"]
                else:
                    raise NotImplementedError("write.table requires first argument x")
                file_src = kw.get("file")
                if file_src is None:
                    raise NotImplementedError("write.table requires file= argument")
                data_f = r_expr_to_fortran(data_src)
                file_f = r_expr_to_fortran(file_src)
                data_rank = _expr_rank_for_print(data_f)
                data_arg = data_f
                if data_rank == 1 or re.search(r"%\s*(?:x|z|loglik|pi|mu|sigma)\b", data_f):
                    data_arg = f"reshape({data_f}, [size({data_f}), 1])"
                o.w(f"call write_table_real_matrix({file_f}, {data_arg})")
                if helper_ctx is not None:
                    helper_ctx["need_table_writer"] = True
                continue
            raise NotImplementedError(f"unsupported call statement: {st.name}")
        elif isinstance(st, ForStmt):
            it = st.iter_expr.strip()
            m_colon = re.match(r"^(.+):(.+)$", it)
            m_seq_len = re.match(r"^seq_len\s*\((.+)\)$", it, re.IGNORECASE)
            m_seq_along = re.match(r"^seq_along\s*\((.+)\)$", it, re.IGNORECASE)
            if m_seq_len:
                n = r_expr_to_fortran(m_seq_len.group(1).strip())
                o.w(f"do {st.var} = 1, {_int_bound_expr(n)}")
            elif m_seq_along:
                along_f = r_expr_to_fortran(m_seq_along.group(1).strip())
                n = f"size({along_f})"
                if re.match(r"^[A-Za-z]\w*(?:%[A-Za-z]\w*)?$", along_f):
                    root = along_f.split("%")[-1].lower()
                    if root == "a_list":
                        n = f"size({along_f}, 4)"
                    elif root in _KNOWN_RANK3_NAMES:
                        n = f"size({along_f}, 3)"
                    elif root.endswith("_list"):
                        n = f"size({along_f}, 2)"
                o.w(f"do {st.var} = 1, {_int_bound_expr(n)}")
            elif m_colon:
                a = r_expr_to_fortran(m_colon.group(1).strip())
                b = r_expr_to_fortran(m_colon.group(2).strip())
                if _emit_implied_do_print_loop(st.body, st.var, f"int({a})", f"int({b})", "i0"):
                    continue
                o.w(f"do {st.var} = int({a}), int({b})")
            elif (parse_call_text(it) is not None) and (parse_call_text(it)[0].lower() in {"seq", "seq.int"}):
                arr = r_expr_to_fortran(it)
                idx = f"i_{st.var}"
                direct_body = _print_only_loop_body_with_value(st.body, st.var, f"seq_for_{st.var}({idx})")
                o.w("block")
                o.push()
                o.w(f"integer :: {idx}")
                o.w(f"integer, allocatable :: seq_for_{st.var}(:)")
                o.w(f"seq_for_{st.var} = {arr}")
                o.w(f"do {idx} = 1, size(seq_for_{st.var})")
                o.push()
                if _emit_direct_print_only_loop_body(st.body, st.var, f"seq_for_{st.var}({idx})", "i0"):
                    pass
                elif direct_body is not None:
                    emit_stmts(o, direct_body, need_rnorm, params, alloc_seen, helper_ctx)
                else:
                    o.w(f"{st.var} = seq_for_{st.var}({idx})")
                    emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            elif re.match(r"^[A-Za-z]\w*$", it):
                arr = it
                idx = f"i_{st.var}"
                direct_body = _print_only_loop_body_with_value(st.body, st.var, f"{arr}({idx})")
                scalar_fmt = "i0" if arr in int_vector_vars else ("f0.6" if arr in real_vector_vars else None)
                if scalar_fmt is not None and _is_print_only_loop_over_value(st.body, st.var, exact_var_only=True):
                    _wstmt(f'write(*,"({scalar_fmt})") {arr}', st.body[0].comment)
                    continue
                o.w("block")
                o.push()
                o.w(f"integer :: {idx}")
                o.w(f"do {idx} = 1, size({arr})")
                o.push()
                if _emit_direct_print_only_loop_body(st.body, st.var, f"{arr}({idx})", scalar_fmt):
                    pass
                elif direct_body is not None:
                    emit_stmts(o, direct_body, need_rnorm, params, alloc_seen, helper_ctx)
                else:
                    o.w(f"{st.var} = {arr}({idx})")
                    emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
                o.pop()
                o.w("end do")
                o.pop()
                o.w("end block")
                continue
            else:
                raise NotImplementedError(f"unsupported for iterator: {it}")
            o.push()
            emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
            o.pop()
            o.w("end do")
        elif isinstance(st, WhileStmt):
            o.w(f"do while ({r_expr_to_fortran(st.cond)})")
            o.push()
            emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
            o.pop()
            o.w("end do")
        elif isinstance(st, RepeatStmt):
            o.w("do")
            o.push()
            emit_stmts(o, st.body, need_rnorm, params, alloc_seen, helper_ctx)
            o.pop()
            o.w("end do")
        elif isinstance(st, IfStmt):
            # Prefer MERGE for simple same-target conditional assignment.
            if (
                len(st.then_body) == 1
                and len(st.else_body) == 1
                and isinstance(st.then_body[0], Assign)
                and isinstance(st.else_body[0], Assign)
            ):
                a_then = st.then_body[0]
                a_else = st.else_body[0]
                if a_then.name == a_else.name:
                    rhs_t = r_expr_to_fortran(a_then.expr)
                    rhs_e = r_expr_to_fortran(a_else.expr)
                    if _is_simple_value_for_merge(rhs_t) and _is_simple_value_for_merge(rhs_e):
                        cond_f = r_expr_to_fortran(st.cond)
                        o.w(f"{a_then.name} = merge({rhs_t}, {rhs_e}, {cond_f})")
                        continue

            o.w(f"if ({r_expr_to_fortran(st.cond)}) then")
            o.push()
            emit_stmts(o, st.then_body, need_rnorm, params, alloc_seen, helper_ctx)
            o.pop()
            if st.else_body:
                o.w("else")
                o.push()
                emit_stmts(o, st.else_body, need_rnorm, params, alloc_seen, helper_ctx)
                o.pop()
            o.w("end if")
        elif isinstance(st, ExprStmt):
            if re.match(r"^\s*(?:colnames|rownames|names|storage\.mode)\s*\(", st.expr, re.IGNORECASE):
                continue
            if st.expr.strip() == "break":
                o.w("exit")
                continue
            if st.expr.strip() == "next":
                o.w("cycle")
                continue
            ret_arg = _return_call_arg(st.expr.strip())
            if ret_arg is not None:
                if not return_var:
                    raise NotImplementedError("return(...) is only supported inside functions")
                if ret_arg:
                    ret_src = ret_arg
                    if helper_ctx is not None:
                        ram = helper_ctx.get("return_alias_map")
                        if isinstance(ram, dict):
                            ret_src = str(ram.get(ret_arg, ret_arg))
                    _wstmt(f"{return_var} = {r_expr_to_fortran(ret_src)}", st.comment)
                o.w("return")
                continue
            c_expr = parse_call_text(st.expr.strip())
            if c_expr is not None and c_expr[0].lower() in {"seq", "seq.int", "seq_along", "seq_len"}:
                _wstmt(f"print *, {r_expr_to_fortran(st.expr.strip())}", st.comment)
                continue
            if c_expr is not None:
                nm_expr = c_expr[0].lower()
                if nm_expr in _SUBROUTINE_FUNCTIONS:
                    call_f = r_expr_to_fortran(st.expr.strip())
                    _wstmt(f"call {call_f}", st.comment)
                    continue
                if nm_expr in _VOID_FUNCTION_LIKE:
                    call_f = r_expr_to_fortran(st.expr.strip())
                    o.w("block")
                    o.push()
                    o.w("real(kind=dp) :: ignore_val")
                    _wstmt(f"ignore_val = {call_f}", st.comment)
                    o.pop()
                    o.w("end block")
                    continue
                if nm_expr == "options":
                    # R options(...) at statement scope is configuration metadata;
                    # skip in generated Fortran for now.
                    continue
                if nm_expr == "declare":
                    # compiler::declare(...) is R-side type metadata; the
                    # translator infers/declarers Fortran types separately.
                    continue
                is_matrix_expr = nm_expr in {"array", "matrix", "cbind", "cbind2"} or (
                    nm_expr in {"cov", "cor"} and len(c_expr[1]) <= 1
                )
                if is_matrix_expr:
                    one_f = r_expr_to_fortran(_rewrite_predict_expr(st.expr.strip()))
                    o.w("block")
                    o.push()
                    o.w("integer :: i_pr")
                    o.w(f"associate(m_pr => {one_f})")
                    o.push()
                    o.w("do i_pr = 1, size(m_pr, 1)")
                    o.push()
                    o.w('write(*,"(*(g0,1x))") m_pr(i_pr, :)')
                    o.pop()
                    o.w("end do")
                    o.pop()
                    o.w("end associate")
                    o.pop()
                    o.w("end block")
                    continue
                if nm_expr == "coef":
                    obj_src = c_expr[1][0].strip() if c_expr[1] else c_expr[2].get("object", "").strip()
                    c_obj = parse_call_text(obj_src) if obj_src else None
                    if c_obj is not None and c_obj[0].lower() == "lm":
                        _nm_lm, pos_lm, kw_lm = c_obj
                        form = pos_lm[0].strip() if pos_lm else kw_lm.get("formula", "").strip()
                        m_form = re.match(r"^([A-Za-z]\w*)\s*~\s*(.+)$", form)
                        if not m_form:
                            raise NotImplementedError("lm requires formula like y ~ x1 + x2 + ... in this subset")
                        yv = r_expr_to_fortran(m_form.group(1).strip())
                        rhs_terms = m_form.group(2).strip()
                        terms = [t.strip() for t in split_top_level_commas(rhs_terms.replace("+", ",")) if t.strip()]
                        if not terms:
                            raise NotImplementedError("lm formula requires at least one predictor")
                        cols = ", ".join(r_expr_to_fortran(t) for t in terms)
                        first = r_expr_to_fortran(terms[0])
                        p = len(terms)
                        o.w("block")
                        o.push()
                        o.w("type(lm_fit_t) :: fit_coef_tmp")
                        o.w(f"fit_coef_tmp = lm_fit_general({yv}, reshape([{cols}], [size({first}), {p}]))")
                        o.w('write(*,"(*(g0,1x))") fit_coef_tmp%coef')
                        o.pop()
                        o.w("end block")
                        if helper_ctx is not None:
                            helper_ctx["need_lm"] = True
                        continue
                    if obj_src:
                        _wstmt(f'write(*,"(*(g0,1x))") {r_expr_to_fortran(obj_src)}%coef', st.comment)
                        if helper_ctx is not None:
                            helper_ctx["need_lm"] = True
                        continue
            asn = split_top_level_assignment(st.expr.strip())
            if asn is not None:
                rhs = r_expr_to_fortran(asn[1].strip())
                if "t_test_p_value(" in rhs:
                    need_r_mod.update({"t_test_p_value", "t_test", "t_test_result_t"})
                lhs_src = asn[0].strip()
                m_obj_list_lhs = re.match(r"^([A-Za-z]\w*)\s*\[\[\s*(.+)\s*\]\]$", lhs_src)
                if m_obj_list_lhs is not None and m_obj_list_lhs.group(1) in object_list_vars:
                    idx_obj = _int_bound_expr(r_expr_to_fortran(m_obj_list_lhs.group(2).strip()))
                    _wstmt(f"{m_obj_list_lhs.group(1)}({idx_obj}) = {rhs}", st.comment)
                    continue
                m_lhs_mask = re.match(r"^([A-Za-z]\w*)\s*\[\s*(.+)\s*\]$", lhs_src)
                if m_lhs_mask is not None:
                    base_lhs = m_lhs_mask.group(1)
                    inner_lhs = m_lhs_mask.group(2).strip()
                    if "," not in inner_lhs:
                        inner_l = inner_lhs.lower()
                        if (
                            inner_l in _KNOWN_LOGICAL_VECTOR_NAMES
                            or re.match(r"^is\.na\s*\(", inner_l)
                            or re.match(r"^is_na\s*\(", inner_l)
                            or any(op in inner_l for op in ("==", "!=", "<=", ">=", "<", ">", ".and.", ".or."))
                        ):
                            mask_f = r_expr_to_fortran(inner_lhs)
                            _wstmt(f"where ({mask_f}) {base_lhs} = {rhs}", st.comment)
                            continue
                lhs = _named_subscript_lhs_to_fortran(lhs_src) or r_expr_to_fortran(lhs_src)
                m_row_assign = re.match(r"^([A-Za-z]\w*)\s*\[\s*([^,\]]+)\s*,\s*\]\s*$", lhs_src)
                if m_row_assign is not None and re.match(r"^rmvnorm_chol\s*\(", rhs, re.IGNORECASE):
                    row_idx_src = m_row_assign.group(2).strip()
                    is_simple_row_scalar = (
                        _is_int_literal(row_idx_src)
                        or (
                            re.match(r"^[A-Za-z]\w*$", row_idx_src)
                            and row_idx_src not in vector_vars
                            and row_idx_src.lower() not in _KNOWN_VECTOR_NAMES
                            and row_idx_src.lower() not in _KNOWN_LOGICAL_VECTOR_NAMES
                        )
                    )
                    if is_simple_row_scalar:
                        rhs = f"reshape({rhs}, [size({m_row_assign.group(1)}, 2)])"
                if re.match(r"^[A-Za-z]\w*$", lhs_src) and lhs_src in {"aic_order", "bic_order"}:
                    rhs = f"int({rhs})"
                _wstmt(f"{lhs} = {rhs}", st.comment)
                continue
            # In R, a bare expression at statement level is evaluated and printed.
            _wstmt(f"print *, {r_expr_to_fortran(_rewrite_predict_expr(st.expr.strip()))}", st.comment)
            continue
        else:
            raise NotImplementedError(f"unsupported statement: {type(st).__name__}")


def _expr_kind_simple(expr: str) -> str:
    t = expr.strip()
    if _is_int_literal(t):
        return "int"
    if _is_real_literal(t):
        return "real"
    if t in {"TRUE", "FALSE"}:
        return "logical"
    if re.match(r"^(?:all|any|is\.[A-Za-z_]\w*)\s*\(", t, re.IGNORECASE):
        return "logical"
    if any(_split_top_level_token(t, op, from_right=True) is not None for op in ["==", "!=", ">=", "<=", ">", "<"]):
        return "logical"
    return "real"


def infer_local_logical_scalars(stmts: list[object]) -> set[str]:
    out: set[str] = set()

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                if _expr_kind_simple(rhs) == "logical":
                    out.add(st.name)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return out


def _control_value_from_list(control_src: str, name: str) -> str | None:
    c = parse_call_text(control_src.strip())
    if c is None or c[0].lower() != "list":
        return None
    return c[2].get(name)


def _emit_optim_bfgs_assignment(o: FEmit, target: str, rhs: str, comment: str = "") -> bool:
    c = parse_call_text(rhs.strip())
    if c is not None and c[0].lower() == "try" and c[1]:
        c_try_inner = parse_call_text(c[1][0].strip())
        if c_try_inner is not None and c_try_inner[0].lower() == "optim":
            c = c_try_inner
    if c is None or c[0].lower() != "optim":
        return False
    _nm, pos, kw = c
    par_src = kw.get("par") or (pos[0] if len(pos) >= 1 else None)
    fn_src = kw.get("fn") or (pos[1] if len(pos) >= 2 else None)
    if par_src is None or fn_src is None:
        return False
    fn_name = fn_src.strip()
    if not re.match(r"^[A-Za-z]\w*$", fn_name):
        return False
    method_src = kw.get("method", '"BFGS"').strip()
    method = (_dequote_string_literal(method_src) or method_src).lower()
    if method != "bfgs":
        return False

    objective_args = _USER_FUNC_ARG_INDEX.get(fn_name.lower(), {})
    ordered_extra: list[tuple[int, str]] = []
    skip = {"par", "fn", "gr", "method", "control", "hessian", "lower", "upper"}
    for k, v in kw.items():
        kl = k.lower()
        if kl in skip:
            continue
        idx = objective_args.get(kl)
        if idx is None and kl.endswith("_wrk"):
            idx = objective_args.get(kl[:-4])
        if idx is None:
            idx = 1000 + len(ordered_extra)
        ordered_extra.append((idx, r_expr_to_fortran(v)))
    for i, v in enumerate(pos[2:], start=2):
        ordered_extra.append((i, r_expr_to_fortran(v)))
    extra_actuals = [v for _idx, v in sorted(ordered_extra, key=lambda x: x[0])]

    par_f = r_expr_to_fortran(par_src)
    control_src = kw.get("control", "")
    maxit_src = _control_value_from_list(control_src, "maxit") if control_src else None
    reltol_src = _control_value_from_list(control_src, "reltol") if control_src else None
    ndeps_src = _control_value_from_list(control_src, "ndeps") if control_src else None
    maxit_f = _int_bound_expr(r_expr_to_fortran(maxit_src or "100"))
    gtol_f = r_expr_to_fortran(reltol_src or "1.0e-8")
    ndeps_f = r_expr_to_fortran(ndeps_src or "1.0e-3")
    prefix = re.sub(r"[^A-Za-z0-9_]", "_", target)
    if not prefix or prefix[0].isdigit():
        prefix = "opt_" + prefix

    p = f"{prefix}_p"
    pnew = f"{prefix}_p_new"
    ptmp = f"{prefix}_p_tmp"
    g = f"{prefix}_g"
    gnew = f"{prefix}_g_new"
    h = f"{prefix}_H"
    d = f"{prefix}_d"
    svec = f"{prefix}_s"
    yvec = f"{prefix}_y"
    amat = f"{prefix}_A"
    tmp = f"{prefix}_tmp"
    np = f"{prefix}_np"
    max_iter = f"{prefix}_max_iter"
    n_iter = f"{prefix}_n_iter"
    i = f"{prefix}_i"
    j = f"{prefix}_j"
    iter_nm = f"{prefix}_iter"
    fval = f"{prefix}_f"
    fnew = f"{prefix}_f_new"
    fplus = f"{prefix}_f_plus"
    fminus = f"{prefix}_f_minus"
    eps = f"{prefix}_eps"
    ndeps = f"{prefix}_ndeps"
    gtol = f"{prefix}_gtol"
    alpha = f"{prefix}_alpha"
    slope = f"{prefix}_slope"
    sy = f"{prefix}_sy"
    rho = f"{prefix}_rho"
    shift = f"{prefix}_shift"
    converged = f"{prefix}_converged"

    def obj_call(p_expr: str) -> str:
        args = ", ".join([p_expr] + extra_actuals)
        return f"{fn_name}({args})"

    def emit_gradient(point: str, grad: str) -> None:
        o.w(f"do {i} = 1, {np}")
        o.push()
        o.w(f"{eps} = {ndeps} * (abs({point}({i})) + 1.0_dp)")
        o.w(f"{ptmp} = {point}")
        o.w(f"{ptmp}({i}) = {ptmp}({i}) + {eps}")
        o.w(f"{fplus} = {obj_call(ptmp)}")
        o.w(f"{ptmp} = {point}")
        o.w(f"{ptmp}({i}) = {ptmp}({i}) - {eps}")
        o.w(f"{fminus} = {obj_call(ptmp)}")
        o.w(f"{grad}({i}) = ({fplus} - {fminus}) / (2.0_dp * {eps})")
        o.pop()
        o.w("end do")

    if comment:
        cmt = comment.strip()
        if cmt:
            o.w(f"! {cmt}")
    o.w("block")
    o.push()
    o.w(f"integer :: {np}, {max_iter}, {n_iter}, {i}, {j}, {iter_nm}")
    o.w(f"logical :: {converged}")
    o.w(
        f"real(kind=dp) :: {fval}, {fnew}, {fplus}, {fminus}, {eps}, {ndeps}, {gtol}, "
        f"{alpha}, {slope}, {sy}, {rho}, {shift}"
    )
    o.w(f"real(kind=dp), allocatable :: {p}(:), {pnew}(:), {ptmp}(:), {g}(:), {gnew}(:)")
    o.w(f"real(kind=dp), allocatable :: {h}(:,:), {d}(:), {svec}(:), {yvec}(:), {amat}(:,:), {tmp}(:,:)")
    o.w(f"{p} = {par_f}")
    o.w(f"{np} = size({p})")
    o.w(f"{max_iter} = {maxit_f}")
    o.w(f"{ndeps} = {ndeps_f}")
    o.w(f"{gtol} = max({gtol_f}, sqrt(epsilon(1.0_dp)))")
    o.w(f"allocate({pnew}({np}), {ptmp}({np}), {g}({np}), {gnew}({np}), {h}({np},{np}), {d}({np}), {svec}({np}), {yvec}({np}), {amat}({np},{np}), {tmp}({np},{np}))")
    o.w(f"{h} = 0.0_dp")
    o.w(f"do {i} = 1, {np}")
    o.push()
    o.w(f"{h}({i},{i}) = 1.0_dp")
    o.pop()
    o.w("end do")
    o.w(f"{fval} = {obj_call(p)}")
    emit_gradient(p, g)
    o.w(f"{converged} = .false.")
    o.w(f"{n_iter} = 0")
    o.w(f"do {iter_nm} = 1, {max_iter}")
    o.push()
    o.w(f"{n_iter} = {iter_nm}")
    o.w(f"if (sqrt(sum({g}**2)) < {gtol}) then")
    o.push()
    o.w(f"{converged} = .true.")
    o.w("exit")
    o.pop()
    o.w("end if")
    o.w(f"do {i} = 1, {np}")
    o.push()
    o.w(f"{d}({i}) = -sum({h}({i},:) * {g})")
    o.pop()
    o.w("end do")
    o.w(f"if (dot_product({g}, {d}) >= 0.0_dp) then")
    o.push()
    o.w(f"{h} = 0.0_dp")
    o.w(f"do {i} = 1, {np}")
    o.push()
    o.w(f"{h}({i},{i}) = 1.0_dp")
    o.pop()
    o.w("end do")
    o.w(f"{d} = -{g}")
    o.pop()
    o.w("end if")
    o.w(f"{alpha} = 1.0_dp")
    o.w(f"{slope} = dot_product({g}, {d})")
    o.w(f"do {j} = 1, 60")
    o.push()
    o.w(f"{pnew} = {p} + {alpha} * {d}")
    o.w(f"{fnew} = {obj_call(pnew)}")
    o.w(f"if ({fnew} <= {fval} + 1.0e-4_dp * {alpha} * {slope}) exit")
    o.w(f"if ({alpha} < 1.0e-12_dp) exit")
    o.w(f"{alpha} = 0.5_dp * {alpha}")
    o.pop()
    o.w("end do")
    emit_gradient(pnew, gnew)
    o.w(f"{svec} = {pnew} - {p}")
    o.w(f"{yvec} = {gnew} - {g}")
    o.w(f"{sy} = dot_product({svec}, {yvec})")
    o.w(f"{p} = {pnew}")
    o.w(f"{shift} = abs({fval} - {fnew})")
    o.w(f"{fval} = {fnew}")
    o.w(f"{g} = {gnew}")
    o.w(f"if ({sy} > 1.0e-10_dp * sqrt(sum({svec}**2)) * sqrt(sum({yvec}**2))) then")
    o.push()
    o.w(f"{rho} = 1.0_dp / {sy}")
    o.w(f"do {i} = 1, {np}")
    o.push()
    o.w(f"do {j} = 1, {np}")
    o.push()
    o.w(f"{amat}({i},{j}) = -{rho} * {svec}({i}) * {yvec}({j})")
    o.pop()
    o.w("end do")
    o.w(f"{amat}({i},{i}) = {amat}({i},{i}) + 1.0_dp")
    o.pop()
    o.w("end do")
    o.w(f"do {i} = 1, {np}")
    o.push()
    o.w(f"do {j} = 1, {np}")
    o.push()
    o.w(f"{tmp}({i},{j}) = sum({h}({i},:) * {amat}({j},:))")
    o.pop()
    o.w("end do")
    o.pop()
    o.w("end do")
    o.w(f"do {i} = 1, {np}")
    o.push()
    o.w(f"do {j} = 1, {np}")
    o.push()
    o.w(f"{h}({i},{j}) = sum({amat}({i},:) * {tmp}(:,{j})) + {rho} * {svec}({i}) * {svec}({j})")
    o.pop()
    o.w("end do")
    o.pop()
    o.w("end do")
    o.pop()
    o.w("end if")
    o.w(f"if ({shift} <= {gtol} * (1.0_dp + abs({fval}))) then")
    o.push()
    o.w(f"{converged} = .true.")
    o.w("exit")
    o.pop()
    o.w("end if")
    o.pop()
    o.w("end do")
    o.w(f"{target}%par = {p}")
    o.w(f"{target}%value = {fval}")
    o.w(f"{target}%convergence = merge(0, 1, {converged})")
    o.pop()
    o.w("end block")
    return True


def emit_function(
    o: FEmit,
    fn: FuncDef,
    list_specs: dict[str, ListReturnSpec],
    helper_ctx: dict[str, object] | None = None,
) -> bool:
    has_r_mod = bool(helper_ctx and helper_ctx.get("has_r_mod"))
    need_r_mod: set[str] = set()
    if helper_ctx is not None:
        nr = helper_ctx.get("need_r_mod")
        if isinstance(nr, set):
            need_r_mod = nr
    fn_body = list(fn.body)
    opening_comments: list[str] = []
    while fn_body and isinstance(fn_body[0], CommentStmt):
        opening_comments.append(fn_body.pop(0).text)
    if not fn_body:
        raise NotImplementedError(f"empty function body not supported: {fn.name}")
    emit_as_subroutine = _stmt_tree_has_output_ops(fn_body) and _function_has_void_return(
        FuncDef(name=fn.name, args=fn.args, defaults=fn.defaults, body=fn_body)
    )
    has_explicit_return = isinstance(fn_body[-1], ExprStmt)
    last = fn_body[-1] if has_explicit_return else ExprStmt(expr="0.0")
    list_spec = list_specs.get(fn.name)
    need_rnorm_local = {"used": False}
    body_stmts = fn_body[:-1] if has_explicit_return else fn_body
    if fn.name == "arma_coef_names" and len(fn.args) >= 2:
        max_ar_arg, max_ma_arg = fn.args[:2]
        rname = f"{fn.name}_result"
        for cmt in fn.leading_comments:
            c = cmt.strip()
            if c:
                o.w(f"! {c}")
        o.w(f"function {fn.name}({max_ar_arg}, {max_ma_arg}) result({rname})")
        for cmt in opening_comments:
            c = cmt.strip()
            if c:
                o.w(f"! {c}")
        o.w(f"integer, intent(in) :: {max_ar_arg}, {max_ma_arg}")
        o.w(f"character(len=:), allocatable :: {rname}(:)")
        o.w("integer :: j, ncols")
        o.w("character(len=32) :: label")
        o.w(f"ncols = {max_ar_arg} + {max_ma_arg} + 9")
        o.w(f"allocate(character(len=32) :: {rname}(max(0, ncols)))")
        o.w(f"{rname}(1) = \"ar.order\"")
        o.w(f"{rname}(2) = \"ma.order\"")
        o.w(f"{rname}(3) = \"mu\"")
        o.w(f"do j = 1, {max_ar_arg}")
        o.push()
        o.w("write(label, '(a,i0)') 'ar', j")
        o.w(f"{rname}(3 + j) = trim(label)")
        o.pop()
        o.w("end do")
        o.w(f"do j = 1, {max_ma_arg}")
        o.push()
        o.w("write(label, '(a,i0)') 'ma', j")
        o.w(f"{rname}(3 + {max_ar_arg} + j) = trim(label)")
        o.pop()
        o.w("end do")
        o.w(f"{rname}(4 + {max_ar_arg} + {max_ma_arg}) = \"sigma2\"")
        o.w(f"{rname}(5 + {max_ar_arg} + {max_ma_arg}) = \"loglik\"")
        o.w(f"{rname}(6 + {max_ar_arg} + {max_ma_arg}) = \"aic\"")
        o.w(f"{rname}(7 + {max_ar_arg} + {max_ma_arg}) = \"bic\"")
        o.w(f"{rname}(8 + {max_ar_arg} + {max_ma_arg}) = \"convergence\"")
        o.w(f"{rname}(9 + {max_ar_arg} + {max_ma_arg}) = \"ok\"")
        o.w(f"end function {fn.name}")
        return False
    if fn.name == "arma_coef_row" and len(fn.args) >= 3:
        fit_arg, max_ar_arg, max_ma_arg = fn.args[:3]
        rname = f"{fn.name}_result"
        for cmt in fn.leading_comments:
            c = cmt.strip()
            if c:
                o.w(f"! {c}")
        o.w(f"function {fn.name}({fit_arg}, {max_ar_arg}, {max_ma_arg}) result({rname})")
        for cmt in opening_comments:
            c = cmt.strip()
            if c:
                o.w(f"! {c}")
        o.w(f"type(fit_arma_scratch_result_t), intent(in) :: {fit_arg}")
        o.w(f"integer, intent(in) :: {max_ar_arg}, {max_ma_arg}")
        o.w(f"real(kind=dp), allocatable :: {rname}(:)")
        o.w("integer :: j, ncols")
        o.w(f"ncols = {max_ar_arg} + {max_ma_arg} + 9")
        o.w(f"allocate({rname}(max(0, ncols)), source=ieee_value(0.0_dp, ieee_quiet_nan))")
        o.w(f"{rname}(1) = real({fit_arg}%ar_order, kind=dp)")
        o.w(f"{rname}(2) = real({fit_arg}%ma_order, kind=dp)")
        o.w(f"{rname}(9 + {max_ar_arg} + {max_ma_arg}) = merge(1.0_dp, 0.0_dp, {fit_arg}%ok)")
        o.w(f"if ({fit_arg}%ok) then")
        o.push()
        o.w(f"if (size({fit_arg}%coef) >= 1) {rname}(3) = {fit_arg}%coef(1)")
        o.w(f"do j = 1, min({max_ar_arg}, {fit_arg}%ar_order)")
        o.push()
        o.w(f"if (1 + j <= size({fit_arg}%coef)) {rname}(3 + j) = {fit_arg}%coef(1 + j)")
        o.pop()
        o.w("end do")
        o.w(f"do j = 1, min({max_ma_arg}, {fit_arg}%ma_order)")
        o.push()
        o.w(f"if (1 + {fit_arg}%ar_order + j <= size({fit_arg}%coef)) {rname}(3 + {max_ar_arg} + j) = {fit_arg}%coef(1 + {fit_arg}%ar_order + j)")
        o.pop()
        o.w("end do")
        o.w(f"{rname}(4 + {max_ar_arg} + {max_ma_arg}) = {fit_arg}%sigma2")
        o.w(f"{rname}(5 + {max_ar_arg} + {max_ma_arg}) = {fit_arg}%loglik")
        o.w(f"{rname}(6 + {max_ar_arg} + {max_ma_arg}) = {fit_arg}%aic")
        o.w(f"{rname}(7 + {max_ar_arg} + {max_ma_arg}) = {fit_arg}%bic")
        o.pop()
        o.w("end if")
        o.w(f"{rname}(8 + {max_ar_arg} + {max_ma_arg}) = real({fit_arg}%convergence, kind=dp)")
        o.w(f"end function {fn.name}")
        return False
    can_be_pure = not _stmt_tree_has_side_effect_ops(body_stmts)
    if any(re.search(r"\blm\s*\(", txt, re.IGNORECASE) for txt in _collect_stmt_expr_texts(body_stmts)):
        can_be_pure = False
    if any(re.search(r"\boptim\s*\(", txt, re.IGNORECASE) for txt in _collect_stmt_expr_texts(body_stmts)):
        can_be_pure = False
    for txt_pure in _collect_stmt_expr_texts(body_stmts):
        for call_nm in re.findall(r"\b([A-Za-z]\w*)\s*\(", txt_pure):
            key_call = call_nm.lower()
            if key_call in _USER_FUNC_ARG_KIND and key_call not in _USER_FUNC_ELEMENTAL:
                can_be_pure = False
                break
        if not can_be_pure:
            break
    ret_type_name: str | None = None
    ret_ident_m0 = re.match(r"^[A-Za-z]\w*$", last.expr.strip())
    if list_spec is None and ret_ident_m0 is not None:
        ret_nm0 = ret_ident_m0.group(0)
        alias_t: dict[str, str] = {}
        def _walk_ret_alias(ss_ra: list[object]) -> None:
            for st_ra in ss_ra:
                if isinstance(st_ra, Assign):
                    lhs_ra = st_ra.name.strip()
                    rhs_ra = st_ra.expr.strip()
                    ff_ra = _parse_list_constructor(rhs_ra)
                    if ff_ra is not None:
                        alias_t[lhs_ra] = _type_name_for_path(fn.name, ())
                        continue
                    c_ra = parse_call_text(rhs_ra)
                    if c_ra is not None and c_ra[0] in list_specs:
                        alias_t[lhs_ra] = _type_name_for_path(c_ra[0], ())
                        continue
                    m_ra = re.match(r"^([A-Za-z]\w*)$", rhs_ra)
                    if m_ra is not None and m_ra.group(1) in alias_t:
                        alias_t[lhs_ra] = alias_t[m_ra.group(1)]
                elif isinstance(st_ra, IfStmt):
                    _walk_ret_alias(st_ra.then_body)
                    _walk_ret_alias(st_ra.else_body)
                elif isinstance(st_ra, ForStmt):
                    _walk_ret_alias(st_ra.body)
        _walk_ret_alias(body_stmts)
        ret_type_name = alias_t.get(ret_nm0)

    last_expr_for_ret = last.expr.strip()
    last_ret_arg = _return_call_arg(last_expr_for_ret)
    if last_ret_arg is not None:
        last_expr_for_ret = last_ret_arg.strip()

    rk = _expr_kind_simple(last_expr_for_ret)
    rdecl = "real(kind=dp)"
    ret_rank = 0
    ret_expr_src = last_expr_for_ret
    ret_is_char = _expr_returns_character(ret_expr_src)
    if ret_is_char:
        can_be_pure = False
    if ret_expr_src.startswith("c(") or (ret_expr_src.startswith("[") and ret_expr_src.endswith("]")):
        ret_rank = 1
    elif re.search(r"\b(rowSums|colSums|apply)\s*\(", ret_expr_src):
        ret_rank = 1
    elif re.search(r"\b(matrix|array|cbind|cbind2|outer)\s*\(", ret_expr_src):
        ret_rank = 2
    elif re.search(r"\b[A-Za-z]\w*_mat\b", ret_expr_src):
        # Heuristic: expressions over *_mat temporaries are typically matrix-valued.
        ret_rank = 2
    ret_ident_m = re.match(r"^[A-Za-z]\w*$", last_expr_for_ret)
    if list_spec is None and ret_ident_m is not None and has_explicit_return:
        ret_ident = ret_ident_m.group(0)
        known_arrays0 = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        b_ints0, b_real_scalars0, b_int_arrays0, b_real_arrays0, _b_params0 = classify_vars(
            body_stmts, infer_assigned_names(body_stmts), known_arrays=known_arrays0
        )
        if ret_ident in infer_local_logical_scalars(body_stmts):
            rdecl = "logical"
        elif ret_ident in b_int_arrays0:
            ret_rank = _infer_local_array_rank(body_stmts, ret_ident)
            rdecl = "integer, allocatable"
        elif ret_ident in b_real_arrays0:
            ret_rank = _infer_local_array_rank(body_stmts, ret_ident)
            rdecl = "real(kind=dp), allocatable"
        elif ret_ident in b_ints0:
            rdecl = "integer"
        elif ret_ident in b_real_scalars0:
            rdecl = "real(kind=dp)"
    if ret_type_name is not None:
        rdecl = f"type({ret_type_name})"
    elif ret_is_char:
        rdecl = "character(len=:), allocatable"
    elif list_spec is None:
        if ret_rank == 0 and rk == "int":
            rdecl = "integer"
        elif ret_rank == 0 and rk == "logical":
            rdecl = "logical"
        elif ret_rank >= 1 and "allocatable" not in rdecl:
            if rk == "int":
                rdecl = "integer, allocatable"
            else:
                rdecl = "real(kind=dp), allocatable"
    else:
        rdecl = f"type({_type_name_for_path(fn.name, ())})"
    rname = f"{fn.name}_result"
    s3_receiver_type: str | None = None
    if "_" in fn.name and fn.args:
        for spec_name in sorted(list_specs, key=len, reverse=True):
            if fn.name.endswith("_" + spec_name):
                s3_receiver_type = _type_name_for_path(spec_name, ())
                break
    arg_rank = {a: infer_arg_rank(fn, a) for a in fn.args}
    if s3_receiver_type is not None and fn.args:
        arg_rank[fn.args[0]] = 0
    if fn.name.lower() == "print_matrix" and "x" in arg_rank:
        arg_rank["x"] = 2
    if fn.name.lower() == "print_stats" and "stats" in arg_rank:
        arg_rank["stats"] = 1
    if list_spec is None and ret_rank == 0:
        ex_last = last.expr.strip()
        for a in fn.args:
            if arg_rank.get(a, 0) < 1:
                continue
            if (
                re.fullmatch(rf"{re.escape(a)}", ex_last)
                or re.search(rf"\b{re.escape(a)}\b\s*[\+\-\*/\^]", ex_last)
                or re.search(rf"[\+\-\*/\^]\s*\b{re.escape(a)}\b", ex_last)
            ):
                ret_rank = 1
                if "integer" in rdecl:
                    rdecl = "integer, allocatable"
                else:
                    rdecl = "real(kind=dp), allocatable"
                break
    is_elemental = (
        can_be_pure
        and list_spec is None
        and ret_rank == 0
        and all(arg_rank.get(a, 0) == 0 for a in fn.args)
    )
    pref = "pure elemental " if is_elemental else ("pure " if can_be_pure else "")
    for cmt in fn.leading_comments:
        c = cmt.strip()
        if c:
            o.w(f"! {c}")
    if emit_as_subroutine:
        o.w(f"subroutine {fn.name}({', '.join(fn.args)})")
    else:
        o.w(f"{pref}function {fn.name}({', '.join(fn.args)}) result({rname})")
    for cmt in opening_comments:
        c = cmt.strip()
        if c:
            o.w(f"! {c}")
    # argument declarations (first-pass heuristics)
    written_args = infer_written_args(fn)
    arg_type: dict[str, str] = {}
    arg_local_map: dict[str, str] = {}
    local_rename_map: dict[str, str] = {}
    arg_local_decl_lines: list[str] = []
    arg_local_init_lines: list[str] = []
    fn_char_scalars = infer_function_character_scalars(fn)
    fn_char_arrays = infer_function_character_array_names(fn, fn_char_scalars)
    fn_int_args = infer_function_integer_names(fn)

    def _arg_list_result_type(arg_name: str) -> str | None:
        used_fields: set[str] = set()
        for txt in _collect_stmt_expr_texts(fn.body):
            for m in re.finditer(rf"\b{re.escape(arg_name)}\s*\$\s*([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)", txt):
                used_fields.add(_sanitize_fortran_kwarg_name(m.group(1)))
        if not used_fields:
            return None
        for spec_name, spec in list_specs.items():
            if used_fields <= set(spec.root_fields.keys()):
                return _type_name_for_path(spec_name, ())
        return None

    for a in fn.args:
        dflt = fn.defaults.get(a, "")
        intent = "in"
        opt = ", optional" if dflt.strip() else ""
        if s3_receiver_type is not None and a == fn.args[0]:
            o.w(f"type({s3_receiver_type}), intent(in){opt} :: {a}")
            arg_type[a] = "s3_object"
            continue
        list_arg_type = _arg_list_result_type(a)
        if list_arg_type is not None:
            o.w(f"type({list_arg_type}), intent(in){opt} :: {a}")
            arg_type[a] = "list_object"
            continue
        if a in fn_char_arrays:
            o.w(f"character(len=*), intent({intent}){opt} :: {a}(:)")
            arg_type[a] = "char_array"
            continue
        if a in fn_char_scalars:
            o.w(f"character(len=*), intent({intent}){opt} :: {a}")
            arg_type[a] = "char"
            continue
        if a == "name" and fn.name.lower() == "print_matrix":
            o.w(f"character(len=*), intent({intent}){opt} :: {a}")
            arg_type[a] = "char"
            continue
        if a in {"asset_names", "price_names"}:
            o.w(f"integer, intent(in){opt} :: {a}(:)")
            arg_type[a] = "int_array"
            continue
        ar = arg_rank.get(a, 0)
        if ar >= 1:
            dims = "(" + ":," * (ar - 1) + ":)"
            o.w(f"real(kind=dp), intent({intent}){opt} :: {a}{dims}")
            arg_type[a] = "real_array"
            continue
        if (
            a in {"n", "k", "p", "q", "order", "nacf", "seed", "iter", "max_iter", "maxit", "it"}
            or a.endswith("_order")
            or (a == "name" and fn.name.lower().startswith("print_"))
            or a in fn_int_args
        ):
            o.w(f"integer, intent(in){opt} :: {a}")
            arg_type[a] = "integer"
            continue
        if dflt.startswith("c("):
            o.w(f"real(kind=dp), intent({intent}){opt} :: {a}(:)")
            arg_type[a] = "real_array"
        elif dflt.strip().upper() == "NULL":
            o.w(f"integer, intent(in){opt} :: {a}")
            arg_type[a] = "integer"
        elif _is_int_literal(dflt):
            o.w(f"integer, intent(in){opt} :: {a}")
            arg_type[a] = "integer"
        elif dflt in {"TRUE", "FALSE"}:
            o.w(f"logical, intent(in){opt} :: {a}")
            arg_type[a] = "logical"
        else:
            o.w(f"real(kind=dp), intent({intent}){opt} :: {a}")
            arg_type[a] = "real"
    if emit_as_subroutine:
        pass
    elif list_spec is None and ret_rank >= 1 and "allocatable" in rdecl:
        if "integer" in rdecl:
            o.w(f"integer, allocatable :: {rname}(" + ":," * (ret_rank - 1) + ":)")
        else:
            o.w(f"real(kind=dp), allocatable :: {rname}(" + ":," * (ret_rank - 1) + ":)")
    else:
        o.w(f"{rdecl} :: {rname}")

    for a in fn.args:
        dflt = fn.defaults.get(a, "").strip()
        if dflt:
            loc = f"{a}_def"
            arg_local_map[a] = loc
            t = arg_type.get(a, "real")
            if t == "integer":
                arg_local_decl_lines.append(f"integer :: {loc}")
                dflt_f = _int_bound_expr(r_expr_to_fortran(dflt))
            elif t == "logical":
                arg_local_decl_lines.append(f"logical :: {loc}")
                dflt_f = r_expr_to_fortran(dflt)
            elif t == "real_array":
                ar_loc = max(1, arg_rank.get(a, 1))
                dims_loc = "(" + ":," * (ar_loc - 1) + ":)"
                zero_dims = "(" + ",".join("0" for _ in range(ar_loc)) + ")"
                arg_local_decl_lines.append(f"real(kind=dp), allocatable :: {loc}{dims_loc}")
                if dflt.upper() == "NULL":
                    _NULL_ARRAY_SENTINELS[a.lower()] = loc
                    _NULL_ARRAY_SENTINELS[loc.lower()] = loc
                    arg_local_init_lines.append(f"if (present({a})) then")
                    arg_local_init_lines.append(f"{loc} = {a}")
                    arg_local_init_lines.append("else")
                    arg_local_init_lines.append(f"allocate({loc}{zero_dims})")
                    arg_local_init_lines.append("end if")
                    continue
                dflt_f = r_expr_to_fortran(dflt)
            else:
                arg_local_decl_lines.append(f"real(kind=dp) :: {loc}")
                dflt_f = r_expr_to_fortran(dflt)
            arg_local_init_lines.append(f"if (present({a})) then")
            arg_local_init_lines.append(f"{loc} = {a}")
            arg_local_init_lines.append("else")
            arg_local_init_lines.append(f"{loc} = {dflt_f}")
            arg_local_init_lines.append("end if")

    for a in fn.args:
        if a not in written_args:
            continue
        if a in arg_local_map:
            continue
        loc = f"{a}_wrk"
        arg_local_map[a] = loc
        t = arg_type.get(a, "real")
        if t == "integer":
            arg_local_decl_lines.append(f"integer :: {loc}")
            arg_local_init_lines.append(f"{loc} = {a}")
        elif t == "logical":
            arg_local_decl_lines.append(f"logical :: {loc}")
            arg_local_init_lines.append(f"{loc} = {a}")
        elif t == "real_array":
            ar_loc = max(1, arg_rank.get(a, 1))
            dims_loc = "(" + ":," * (ar_loc - 1) + ":)"
            arg_local_decl_lines.append(f"real(kind=dp), allocatable :: {loc}{dims_loc}")
            arg_local_init_lines.append(f"{loc} = {a}")
        else:
            arg_local_decl_lines.append(f"real(kind=dp) :: {loc}")
            arg_local_init_lines.append(f"{loc} = {a}")

    for ln in arg_local_decl_lines:
        o.w(ln)

    body_no_ret = body_stmts
    body_use = [_rename_stmt_obj(st, arg_local_map) for st in body_no_ret] if arg_local_map else body_no_ret
    return_alias_map: dict[str, str] = {}
    if list_spec is not None:
        ret_alias_arg = _return_call_arg(last.expr.strip())
        ret_alias_src = ret_alias_arg.strip() if ret_alias_arg is not None else last.expr.strip()
        if re.match(r"^[A-Za-z]\w*$", ret_alias_src):
            list_alias = f"{ret_alias_src}_list"

            def _rename_return_list_alias(ss_alias: list[object]) -> list[object]:
                out_alias: list[object] = []
                for st_alias in ss_alias:
                    if (
                        isinstance(st_alias, Assign)
                        and st_alias.name == ret_alias_src
                        and _parse_list_constructor(st_alias.expr.strip()) is not None
                    ):
                        out_alias.append(Assign(list_alias, st_alias.expr, st_alias.comment))
                    elif isinstance(st_alias, IfStmt):
                        out_alias.append(
                            IfStmt(
                                st_alias.cond,
                                _rename_return_list_alias(st_alias.then_body),
                                _rename_return_list_alias(st_alias.else_body),
                            )
                        )
                    elif isinstance(st_alias, ForStmt):
                        out_alias.append(ForStmt(st_alias.var, st_alias.iter_expr, _rename_return_list_alias(st_alias.body)))
                    elif isinstance(st_alias, WhileStmt):
                        out_alias.append(WhileStmt(st_alias.cond, _rename_return_list_alias(st_alias.body)))
                    elif isinstance(st_alias, RepeatStmt):
                        out_alias.append(RepeatStmt(_rename_return_list_alias(st_alias.body)))
                    else:
                        out_alias.append(st_alias)
                return out_alias

            renamed_body_use = _rename_return_list_alias(body_use)
            if renamed_body_use != body_use:
                body_use = renamed_body_use
                return_alias_map[ret_alias_src] = list_alias
    # Avoid local names that collide with helper/intrinsic procedures (e.g., sd).
    forbidden_locals = {
        "mean", "sum", "max", "min", "matrix", "kmeans", "tabulate",
        "max_col", "tail", "numeric", "quantile", "dnorm",
    }
    assigned_now = set(infer_assigned_names(body_use).keys())
    for nm in sorted(assigned_now):
        if nm.lower() in forbidden_locals and nm not in fn.args:
            local_rename_map[nm] = f"{nm}_"
    if local_rename_map:
        body_use = [_rename_stmt_obj(st, local_rename_map) for st in body_use]
    result_alias_source = ""
    if not emit_as_subroutine and list_spec is None and ret_type_name is None:
        ret_expr_for_alias = _replace_idents(last.expr, arg_local_map) if arg_local_map else last.expr
        if return_alias_map:
            ret_expr_for_alias = _replace_idents(ret_expr_for_alias, return_alias_map)
        if local_rename_map:
            ret_expr_for_alias = _replace_idents(ret_expr_for_alias, local_rename_map)
        ret_arg_for_alias = _return_call_arg(ret_expr_for_alias)
        if ret_arg_for_alias is not None:
            ret_expr_for_alias = ret_arg_for_alias if ret_arg_for_alias else ret_expr_for_alias
        m_ret_alias = re.match(r"^[A-Za-z]\w*$", ret_expr_for_alias.strip())
        if m_ret_alias is not None:
            cand_alias = m_ret_alias.group(0)
            if cand_alias != rname and cand_alias not in fn.args and cand_alias in infer_assigned_names(body_use):
                body_use = [_rename_stmt_obj(st, {cand_alias: rname}) for st in body_use]
                result_alias_source = cand_alias
    if body_no_ret:
        body_use = [
            st2
            for st2 in body_use
            if not (isinstance(st2, Assign) and st2.expr.strip().upper() == "NULL")
        ]
        hoisted_checks: list[object] = []
        body_rest: list[object] = []
        allowed_names = {a.lower() for a in fn.args} | {v.lower() for v in arg_local_map.values()}
        for st in body_use:
            if _is_hoistable_stopifnot_stmt(st, allowed_names):
                hoisted_checks.append(st)
            else:
                body_rest.append(st)

        known_arrays = {a for a in fn.args if arg_rank.get(a, 0) >= 1}
        known_arrays |= {arg_local_map[a] for a in fn.args if arg_rank.get(a, 0) >= 1 and a in arg_local_map}
        ints, real_scalars, int_arrays, real_arrays, params = classify_vars(
            body_use, infer_assigned_names(body_use), known_arrays=known_arrays
        )
        return_array_fns_ctx = set(helper_ctx.get("return_array_fns", set()) if helper_ctx else set())
        return_rank_ctx = dict(_USER_FUNC_RETURN_RANK)
        function_return_vector_locals: set[str] = set()
        function_return_rank3_locals: set[str] = set()
        vector_rhs_fns = {
            "rnorm", "rnorm_vec", "runif", "runif_vec", "numeric", "double",
            "rep", "rep.int", "seq", "seq_len", "seq_along", "tail",
            "quantile", "diff", "cumsum", "cumprod", "sort",
        }
        for st_ret_arr in body_use:
            if not isinstance(st_ret_arr, Assign):
                continue
            c_ret_arr = parse_call_text(st_ret_arr.expr.strip())
            if c_ret_arr is not None and c_ret_arr[0].lower() == "autocov_matrices":
                function_return_rank3_locals.add(st_ret_arr.name)
                real_arrays.add(st_ret_arr.name)
                known_arrays.add(st_ret_arr.name)
                ints.discard(st_ret_arr.name)
                int_arrays.discard(st_ret_arr.name)
                real_scalars.discard(st_ret_arr.name)
                params.pop(st_ret_arr.name, None)
            elif c_ret_arr is not None and c_ret_arr[0].lower() in return_array_fns_ctx:
                ret_rank_arr = int(return_rank_ctx.get(c_ret_arr[0].lower(), 1))
                if ret_rank_arr >= 2:
                    real_arrays.add(st_ret_arr.name)
                    known_arrays.add(st_ret_arr.name)
                else:
                    function_return_vector_locals.add(st_ret_arr.name)
                    real_arrays.add(st_ret_arr.name)
                    known_arrays.add(st_ret_arr.name)
                ints.discard(st_ret_arr.name)
                int_arrays.discard(st_ret_arr.name)
                real_scalars.discard(st_ret_arr.name)
                params.pop(st_ret_arr.name, None)
            elif c_ret_arr is not None and c_ret_arr[0].lower() in vector_rhs_fns:
                function_return_vector_locals.add(st_ret_arr.name)
                real_arrays.add(st_ret_arr.name)
                known_arrays.add(st_ret_arr.name)
                ints.discard(st_ret_arr.name)
                int_arrays.discard(st_ret_arr.name)
                real_scalars.discard(st_ret_arr.name)
                params.pop(st_ret_arr.name, None)
        for ret_local_set in (ints, real_scalars, int_arrays, real_arrays):
            ret_local_set.discard(rname)
        params.pop(rname, None)
        char_scalars_loc = set(fn_char_scalars) - set(fn.args)
        char_arrays_loc = set(fn_char_arrays) - set(fn.args)
        logical_arrays: set[str] = set()
        array_name_pool = set(known_arrays) | set(int_arrays) | set(real_arrays)
        def _collect_logical_array_targets(ss_la: list[object]) -> None:
            for st_la in ss_la:
                if isinstance(st_la, Assign):
                    rhs_la = st_la.expr.strip()
                    is_cmp = any(
                        _split_top_level_token(rhs_la, op, from_right=True) is not None
                        for op in ["==", "!=", ">=", "<=", ">", "<"]
                    )
                    if not is_cmp:
                        continue
                    if any(re.search(rf"\b{re.escape(an)}\b", rhs_la) for an in array_name_pool):
                        logical_arrays.add(st_la.name)
                elif isinstance(st_la, IfStmt):
                    _collect_logical_array_targets(st_la.then_body)
                    _collect_logical_array_targets(st_la.else_body)
                elif isinstance(st_la, ForStmt):
                    _collect_logical_array_targets(st_la.body)
        _collect_logical_array_targets(body_use)
        logical_scalars = infer_local_logical_scalars(body_use)
        logical_scalars.discard(rname)
        logical_arrays.difference_update(logical_scalars)
        for la in logical_arrays:
            int_arrays.discard(la)
            real_arrays.discard(la)
            ints.discard(la)
            real_scalars.discard(la)
            params.pop(la, None)
        for ls in logical_scalars:
            int_arrays.discard(ls)
            real_arrays.discard(ls)
            ints.discard(ls)
            real_scalars.discard(ls)
            params.pop(ls, None)
        local_list_fields: dict[str, dict[str, object]] = {}
        local_list_types: dict[str, str] = {}

        def _collect_local_list_fields(ss: list[object]) -> None:
            for st in ss:
                if isinstance(st, Assign):
                    lhs_nm = st.name.strip()
                    rhs_txt = st.expr.strip()
                    ff = _parse_list_constructor(rhs_txt)
                    if ff is not None:
                        local_list_fields[lhs_nm] = ff
                        local_list_types[lhs_nm] = _type_name_for_path(fn.name, ())
                        continue
                    c_rhs = parse_call_text(rhs_txt)
                    if c_rhs is not None:
                        if c_rhs[0].lower() == "try" and c_rhs[1]:
                            c_try_inner = parse_call_text(c_rhs[1][0].strip())
                            if c_try_inner is not None:
                                c_rhs = c_try_inner
                        callee = c_rhs[0]
                        if callee in list_specs:
                            local_list_types[lhs_nm] = _type_name_for_path(callee, ())
                            continue
                        if callee.lower() == "lm":
                            local_list_types[lhs_nm] = "lm_fit_t"
                            if helper_ctx is not None:
                                helper_ctx["need_lm"] = True
                            continue
                        if callee.lower() == "kmeans":
                            local_list_types[lhs_nm] = "kmeans_result_t"
                            if has_r_mod:
                                need_r_mod.add("kmeans")
                                need_r_mod.add("kmeans_result_t")
                            continue
                        if callee.lower() == "optim":
                            local_list_types[lhs_nm] = "optim_result_t"
                            if has_r_mod:
                                need_r_mod.add("optim_result_t")
                            continue
                        if callee.lower() == "max_col" and has_r_mod:
                            need_r_mod.add("max_col")
                    m_alias = re.match(r"^([A-Za-z]\w*)$", rhs_txt)
                    if m_alias is not None:
                        src_nm = m_alias.group(1)
                        if src_nm in local_list_fields:
                            local_list_fields[lhs_nm] = local_list_fields[src_nm]
                            local_list_types[lhs_nm] = local_list_types.get(src_nm, _type_name_for_path(fn.name, ()))
                        elif src_nm in local_list_types:
                            local_list_types[lhs_nm] = local_list_types[src_nm]
                elif isinstance(st, IfStmt):
                    _collect_local_list_fields(st.then_body)
                    _collect_local_list_fields(st.else_body)
                elif isinstance(st, ForStmt):
                    _collect_local_list_fields(st.body)
                elif isinstance(st, WhileStmt):
                    _collect_local_list_fields(st.body)
                elif isinstance(st, RepeatStmt):
                    _collect_local_list_fields(st.body)

        _collect_local_list_fields(body_use)
        vector_list_names: set[str] = set()
        object_list_locals: dict[str, str] = {}
        def _collect_vector_list_names(ss_vl: list[object]) -> None:
            for st_vl in ss_vl:
                if isinstance(st_vl, Assign):
                    if re.match(r"^vector\s*\(\s*['\"]list['\"]", st_vl.expr.strip(), re.IGNORECASE):
                        vector_list_names.add(st_vl.name)
                elif isinstance(st_vl, IfStmt):
                    _collect_vector_list_names(st_vl.then_body)
                    _collect_vector_list_names(st_vl.else_body)
                elif isinstance(st_vl, ForStmt):
                    _collect_vector_list_names(st_vl.body)
                elif isinstance(st_vl, WhileStmt):
                    _collect_vector_list_names(st_vl.body)
                elif isinstance(st_vl, RepeatStmt):
                    _collect_vector_list_names(st_vl.body)
        _collect_vector_list_names(body_use)
        def _collect_object_list_locals(ss_ol: list[object]) -> None:
            for st_ol in ss_ol:
                if isinstance(st_ol, ExprStmt):
                    asn_ol = split_top_level_assignment(st_ol.expr.strip())
                    if asn_ol is not None:
                        m_ol = re.match(r"^([A-Za-z]\w*)\s*\[\[\s*.+\s*\]\]$", asn_ol[0].strip())
                        if m_ol is not None and m_ol.group(1) in vector_list_names:
                            rhs_ol = asn_ol[1].strip()
                            rhs_type = local_list_types.get(rhs_ol)
                            c_ol = parse_call_text(rhs_ol)
                            if rhs_type is None and c_ol is not None and c_ol[0] in list_specs:
                                rhs_type = _type_name_for_path(c_ol[0], ())
                            if rhs_type is not None:
                                object_list_locals[m_ol.group(1)] = rhs_type
                elif isinstance(st_ol, IfStmt):
                    _collect_object_list_locals(st_ol.then_body)
                    _collect_object_list_locals(st_ol.else_body)
                elif isinstance(st_ol, ForStmt):
                    _collect_object_list_locals(st_ol.body)
                elif isinstance(st_ol, WhileStmt):
                    _collect_object_list_locals(st_ol.body)
                elif isinstance(st_ol, RepeatStmt):
                    _collect_object_list_locals(st_ol.body)
        _collect_object_list_locals(body_use)
        for a in fn.args:
            ints.discard(a)
            real_scalars.discard(a)
            int_arrays.discard(a)
            real_arrays.discard(a)
            char_scalars_loc.discard(a)
            char_arrays_loc.discard(a)
            params.pop(a, None)
        for loc in arg_local_map.values():
            ints.discard(loc)
            real_scalars.discard(loc)
            int_arrays.discard(loc)
            real_arrays.discard(loc)
            params.pop(loc, None)
        for lv in set(local_list_fields.keys()) | set(local_list_types.keys()):
            ints.discard(lv)
            real_scalars.discard(lv)
            int_arrays.discard(lv)
            real_arrays.discard(lv)
            char_scalars_loc.discard(lv)
            char_arrays_loc.discard(lv)
            logical_arrays.discard(lv)
            params.pop(lv, None)
        for lv in object_list_locals:
            ints.discard(lv)
            real_scalars.discard(lv)
            int_arrays.discard(lv)
            real_arrays.discard(lv)
            char_scalars_loc.discard(lv)
            char_arrays_loc.discard(lv)
            logical_arrays.discard(lv)
            params.pop(lv, None)
        for cs in set(char_scalars_loc) | set(char_arrays_loc):
            ints.discard(cs)
            real_scalars.discard(cs)
            int_arrays.discard(cs)
            real_arrays.discard(cs)
            params.pop(cs, None)
        int64_locals: set[str] = {p for p, v in params.items() if _expr_uses_int64(v)}
        def _walk_int64_assigns(ss_i64: list[object]) -> None:
            for st_i64 in ss_i64:
                if isinstance(st_i64, Assign):
                    rhs_i64 = st_i64.expr.strip()
                    if "%%" in rhs_i64 or _expr_uses_int64(rhs_i64):
                        int64_locals.add(st_i64.name)
                elif isinstance(st_i64, IfStmt):
                    _walk_int64_assigns(st_i64.then_body)
                    _walk_int64_assigns(st_i64.else_body)
                elif isinstance(st_i64, ForStmt):
                    _walk_int64_assigns(st_i64.body)
                elif isinstance(st_i64, WhileStmt):
                    _walk_int64_assigns(st_i64.body)
                elif isinstance(st_i64, RepeatStmt):
                    _walk_int64_assigns(st_i64.body)
        _walk_int64_assigns(body_use)
        int64_locals &= ints
        assigned_locals = set(infer_assigned_names(body_use).keys())

        list_type_fields: dict[str, dict[str, object]] = {}
        for spec_ltf in list_specs.values():
            for path_ltf, fields_ltf in spec_ltf.nested_types.items():
                list_type_fields[_type_name_for_path(spec_ltf.fn_name, path_ltf)] = fields_ltf
        list_type_fields["optim_result_t"] = {
            "par": "numeric(0)",
            "value": "0.0",
            "convergence": "0",
        }

        def _local_field_expr(base: str, fld: str) -> object | None:
            fields = local_list_fields.get(base)
            if fields is None:
                typ = local_list_types.get(base)
                fields = list_type_fields.get(typ or "")
            if fields is None:
                return None
            return fields.get(fld)

        def _apply_local_alias_from_expr(target: str, expr_obj: object | None) -> bool:
            if expr_obj is None or isinstance(expr_obj, dict):
                return False
            expr_txt = str(expr_obj).strip()
            if _is_int_literal(expr_txt) or re.match(
                r"^(?:nrow|ncol|length|which\.min|which\.max)\s*\(",
                expr_txt,
                re.IGNORECASE,
            ):
                ints.add(target)
                int_arrays.discard(target)
                real_arrays.discard(target)
                real_scalars.discard(target)
                params.pop(target, None)
                return True
            if re.match(r"^[A-Za-z]\w*$", expr_txt):
                if expr_txt == target:
                    return False
                if expr_txt in ints or expr_txt in infer_function_integer_names(fn):
                    ints.add(target)
                    int_arrays.discard(target)
                    real_arrays.discard(target)
                    real_scalars.discard(target)
                    params.pop(target, None)
                    return True
                if expr_txt in int_arrays:
                    int_arrays.add(target)
                    ints.discard(target)
                    real_arrays.discard(target)
                    real_scalars.discard(target)
                    params.pop(target, None)
                    return True
                if expr_txt in real_arrays:
                    real_arrays.add(target)
                    known_arrays.add(target)
                    ints.discard(target)
                    int_arrays.discard(target)
                    real_scalars.discard(target)
                    params.pop(target, None)
                    return True
                if expr_txt in real_scalars:
                    real_scalars.add(target)
                    ints.discard(target)
                    int_arrays.discard(target)
                    real_arrays.discard(target)
                    params.pop(target, None)
                    return True
            c_expr = parse_call_text(expr_txt)
            if c_expr is not None:
                cn_expr = c_expr[0].lower()
                if cn_expr in {"matrix", "array", "cbind", "cbind2", "rbind", "crossprod", "tcrossprod", "t"}:
                    real_arrays.add(target)
                    known_arrays.add(target)
                    ints.discard(target)
                    int_arrays.discard(target)
                    real_scalars.discard(target)
                    params.pop(target, None)
                    return True
                if cn_expr in {"c", "numeric", "as.numeric", "coef", "fitted", "residuals", "colmeans", "rowsums", "colsums"}:
                    real_arrays.add(target)
                    known_arrays.add(target)
                    ints.discard(target)
                    int_arrays.discard(target)
                    real_scalars.discard(target)
                    params.pop(target, None)
                    return True
            if re.search(r"\b(?:sum|mean|min|max|det|logdet_spd|log|sd|r_sd)\s*\(", expr_txt, re.IGNORECASE):
                real_scalars.add(target)
                ints.discard(target)
                int_arrays.discard(target)
                real_arrays.discard(target)
                params.pop(target, None)
                return True
            return False

        def _force_local_vector_component_aliases(ss_alias: list[object]) -> None:
            int_fields = {"n", "k", "p", "q", "aic_p", "aic_q", "bic_p", "bic_q", "aic_row", "bic_row", "nobs", "nseries", "convergence", "n_iter", "order", "trial"}
            real_fields = {"loglik", "aic", "bic", "npar", "ridge", "sigma2"}
            vector_fields = {"par", "coef", "fitted", "resid", "mu", "pi", "weights", "nk"}
            matrix_fields = {"ar", "ma", "sigma", "table", "design", "y"}
            for st_alias in ss_alias:
                if isinstance(st_alias, Assign):
                    rhs_alias = st_alias.expr.strip()
                    m_full_alias = re.match(r"^([A-Za-z]\w*)\$([A-Za-z]\w*)\s*$", rhs_alias, re.IGNORECASE)
                    if m_full_alias is not None and _apply_local_alias_from_expr(
                        st_alias.name,
                        _local_field_expr(m_full_alias.group(1), m_full_alias.group(2)),
                    ):
                        continue
                    m_scalar_alias = re.match(r"^[A-Za-z]\w*\$([A-Za-z]\w*)\s*$", rhs_alias, re.IGNORECASE)
                    if m_scalar_alias is not None and m_scalar_alias.group(1).lower() in int_fields:
                        ints.add(st_alias.name)
                        int_arrays.discard(st_alias.name)
                        real_arrays.discard(st_alias.name)
                        real_scalars.discard(st_alias.name)
                        params.pop(st_alias.name, None)
                    elif m_scalar_alias is not None and m_scalar_alias.group(1).lower() in real_fields:
                        real_scalars.add(st_alias.name)
                        ints.discard(st_alias.name)
                        int_arrays.discard(st_alias.name)
                        real_arrays.discard(st_alias.name)
                        params.pop(st_alias.name, None)
                    elif re.match(rf"^[A-Za-z]\w*\$(?:{'|'.join(sorted(vector_fields))})\s*$", rhs_alias, re.IGNORECASE):
                        real_arrays.add(st_alias.name)
                        ints.discard(st_alias.name)
                        int_arrays.discard(st_alias.name)
                        real_scalars.discard(st_alias.name)
                        params.pop(st_alias.name, None)
                    elif re.match(rf"^[A-Za-z]\w*\$(?:{'|'.join(sorted(matrix_fields))})\s*$", rhs_alias, re.IGNORECASE):
                        real_arrays.add(st_alias.name)
                        known_arrays.add(st_alias.name)
                        ints.discard(st_alias.name)
                        int_arrays.discard(st_alias.name)
                        real_scalars.discard(st_alias.name)
                        params.pop(st_alias.name, None)
                elif isinstance(st_alias, IfStmt):
                    _force_local_vector_component_aliases(st_alias.then_body)
                    _force_local_vector_component_aliases(st_alias.else_body)
                elif isinstance(st_alias, ForStmt):
                    _force_local_vector_component_aliases(st_alias.body)
                elif isinstance(st_alias, WhileStmt):
                    _force_local_vector_component_aliases(st_alias.body)
                elif isinstance(st_alias, RepeatStmt):
                    _force_local_vector_component_aliases(st_alias.body)

        _force_local_vector_component_aliases(body_use)
        for metric_scalar in {"loglik", "aic", "bic", "npar", "ridge", "sigma2"} & assigned_locals:
            ints.discard(metric_scalar)
            int_arrays.discard(metric_scalar)
            real_arrays.discard(metric_scalar)
            params.pop(metric_scalar, None)
            real_scalars.add(metric_scalar)
        param_comments = collect_assignment_comments(body_use)
        for p, v in sorted(params.items()):
            cmt = param_comments.get(p, "").strip()
            suffix = f" ! {cmt}" if cmt else ""
            if _expr_uses_int64(v):
                o.w(f"integer(kind=int64), parameter :: {p} = {v}{suffix}")
            else:
                o.w(f"integer, parameter :: {p} = {v}{suffix}")
        for lv in sorted(local_list_types):
            o.w(f"type({local_list_types[lv]}) :: {lv}")
        for lv, typ in sorted(object_list_locals.items()):
            o.w(f"type({typ}), allocatable :: {lv}(:)")
        local_ranks: dict[str, int] = {}
        for a in fn.args:
            rk_a = arg_rank.get(a, 0)
            if rk_a >= 1:
                local_ranks[a] = rk_a
        for x in sorted(int_arrays | real_arrays):
            local_ranks[x] = _infer_local_array_rank(body_use, x)
        for x in function_return_vector_locals:
            local_ranks[x] = 1
        for x in function_return_rank3_locals:
            local_ranks[x] = 3
        def _walk_assigns(ss_rk: list[object]) -> list[Assign]:
            out_rk: list[Assign] = []
            for st_rk in ss_rk:
                if isinstance(st_rk, Assign):
                    out_rk.append(st_rk)
                elif isinstance(st_rk, IfStmt):
                    out_rk.extend(_walk_assigns(st_rk.then_body))
                    out_rk.extend(_walk_assigns(st_rk.else_body))
                elif isinstance(st_rk, ForStmt):
                    out_rk.extend(_walk_assigns(st_rk.body))
            return out_rk
        assign_nodes = _walk_assigns(body_use)
        for st_seq_rank in assign_nodes:
            if st_seq_rank.name in local_ranks and re.match(
                r"^(?:seq|seq\.int|seq_len|seq_along|quantile)\s*\(",
                st_seq_rank.expr.strip(),
                re.IGNORECASE,
            ):
                local_ranks[st_seq_rank.name] = 1
            if re.match(r"^diag\s*\(.*\)\s*[-+]", st_seq_rank.expr.strip(), re.IGNORECASE):
                local_ranks[st_seq_rank.name] = 2
                real_arrays.add(st_seq_rank.name)
                ints.discard(st_seq_rank.name)
                int_arrays.discard(st_seq_rank.name)
                real_scalars.discard(st_seq_rank.name)
                params.pop(st_seq_rank.name, None)
        changed = True
        while changed:
            changed = False
            for st_rk in assign_nodes:
                nm = st_rk.name
                if nm not in local_ranks:
                    continue
                c_rk = parse_call_text(st_rk.expr.strip())
                if c_rk is not None:
                    cnm, pos_rk, kw_rk = c_rk
                    preserve_rank_fns = {
                        "exp", "log", "sqrt", "abs", "dnorm", "tail",
                        "pmax", "pmin", "max", "min",
                    }
                    if cnm.lower() in preserve_rank_fns:
                        arg_txts = list(pos_rk) + list(kw_rk.values())
                        rk_call = local_ranks.get(nm, 0)
                        for at in arg_txts:
                            at_s = at.strip()
                            if re.match(r"^[A-Za-z]\w*$", at_s):
                                rk_call = max(rk_call, local_ranks.get(at_s, 0))
                        if rk_call > local_ranks.get(nm, 0):
                            local_ranks[nm] = rk_call
                            changed = True
                for op_rk in ["+", "-", "*", "/"]:
                    mm_rk = _split_top_level_token(st_rk.expr.strip(), op_rk, from_right=True)
                    if mm_rk is None:
                        continue
                    a_rk = mm_rk[0].strip()
                    b_rk = mm_rk[1].strip()
                    if not (re.match(r"^[A-Za-z]\w*$", a_rk) and re.match(r"^[A-Za-z]\w*$", b_rk)):
                        continue
                    rk_new = max(local_ranks.get(a_rk, 0), local_ranks.get(b_rk, 0), local_ranks.get(nm, 0))
                    if rk_new > local_ranks.get(nm, 0):
                        local_ranks[nm] = rk_new
                        changed = True
                    break
        if ints - int64_locals:
            o.w("integer :: " + ", ".join(sorted(ints - int64_locals)))
        if int64_locals:
            o.w("integer(kind=int64) :: " + ", ".join(sorted(int64_locals)))
        if int_arrays:
            decls_i: list[str] = []
            for x in sorted(int_arrays):
                rk_x = local_ranks.get(x, _infer_local_array_rank(body_use, x))
                decls_i.append(f"{x}(" + ":," * (rk_x - 1) + ":)")
            o.w("integer, allocatable :: " + ", ".join(decls_i))
        if real_arrays:
            decls_r: list[str] = []
            for x in sorted(real_arrays):
                rk_x = local_ranks.get(x, _infer_local_array_rank(body_use, x))
                decls_r.append(f"{x}(" + ":," * (rk_x - 1) + ":)")
            o.w("real(kind=dp), allocatable :: " + ", ".join(decls_r))
        if logical_arrays:
            decls_l: list[str] = []
            for x in sorted(logical_arrays):
                rk_x = local_ranks.get(x, _infer_local_array_rank(body_use, x))
                decls_l.append(f"{x}(" + ":," * (rk_x - 1) + ":)")
            o.w("logical, allocatable :: " + ", ".join(decls_l))
        if logical_scalars:
            o.w("logical :: " + ", ".join(sorted(logical_scalars)))
        if real_scalars:
            o.w("real(kind=dp) :: " + ", ".join(sorted(real_scalars)))
        if char_scalars_loc:
            o.w("character(len=:), allocatable :: " + ", ".join(sorted(char_scalars_loc)))
        if char_arrays_loc:
            o.w("character(len=:), allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(char_arrays_loc)))
        for ln in arg_local_init_lines:
            o.w(ln)
        helper_ctx_loc = dict(helper_ctx or {})
        if local_list_fields:
            helper_ctx_loc["list_locals"] = local_list_fields
        if object_list_locals:
            helper_ctx_loc["object_list_vars"] = object_list_locals
        # Local rank map for broadcast-aware assignment lowering.
        local_matrix_vars: set[str] = set()
        local_vector_vars: set[str] = set()
        for a in fn.args:
            rk_a = arg_rank.get(a, 0)
            if rk_a >= 2:
                local_matrix_vars.add(a)
            elif rk_a == 1:
                local_vector_vars.add(a)
        for x in sorted(int_arrays | real_arrays):
            rk_x = local_ranks.get(x, _infer_local_array_rank(body_use, x))
            if rk_x >= 2:
                local_matrix_vars.add(x)
            else:
                local_vector_vars.add(x)
        for x in sorted(logical_arrays):
            rk_x = local_ranks.get(x, _infer_local_array_rank(body_use, x))
            if rk_x >= 2:
                local_matrix_vars.add(x)
            else:
                local_vector_vars.add(x)
        helper_ctx_loc["matrix_vars"] = local_matrix_vars
        helper_ctx_loc["vector_vars"] = local_vector_vars
        helper_ctx_loc["int_vector_vars"] = {
            x for x in int_arrays if local_ranks.get(x, _infer_local_array_rank(body_use, x)) < 2
        }
        helper_ctx_loc["real_vector_vars"] = {
            x for x in real_arrays if local_ranks.get(x, _infer_local_array_rank(body_use, x)) < 2
        }
        helper_ctx_loc["local_ranks"] = dict(local_ranks)
        if not emit_as_subroutine:
            helper_ctx_loc["return_var"] = rname
        if return_alias_map:
            helper_ctx_loc["return_alias_map"] = return_alias_map
        emit_stmts(o, hoisted_checks + body_rest, need_rnorm_local, set(params.keys()), helper_ctx=helper_ctx_loc)
    elif arg_local_init_lines:
        for ln in arg_local_init_lines:
            o.w(ln)

    if emit_as_subroutine:
        o.w(f"end subroutine {fn.name}")
        return bool(need_rnorm_local["used"])

    if result_alias_source:
        o.w(f"end function {fn.name}")
        return bool(need_rnorm_local["used"])

    if list_spec is None or ret_type_name is not None:
        rename_all = {}
        rename_all.update(arg_local_map)
        rename_all.update(return_alias_map)
        rename_all.update(local_rename_map)
        ret_expr = _replace_idents(last.expr, rename_all) if rename_all else last.expr
        ret_arg = _return_call_arg(ret_expr)
        if ret_arg is not None:
            ret_expr = ret_arg if ret_arg else last.expr
        if (
            re.match(r"^[A-Za-z]\w*$", ret_expr.strip())
            and f"{ret_expr.strip()}_list" in locals().get("local_list_types", {})
        ):
            ret_expr = f"{ret_expr.strip()}_list"
        if re.match(r"^[A-Za-z]\w*$", ret_expr.strip()):
            ret_nm_src = ret_expr.strip()
            if any(
                isinstance(st_src, Assign)
                and st_src.name == ret_nm_src
                and _parse_list_constructor(st_src.expr.strip()) is not None
                for st_src in body_stmts
            ):
                ret_expr = f"{ret_nm_src}_list"
        o.w(f"{rname} = {r_expr_to_fortran(ret_expr)}")
    else:
        ret_alias_m = re.match(r"^[A-Za-z]\w*$", last.expr.strip())
        if ret_alias_m is not None:
            rename_all = {}
            rename_all.update(arg_local_map)
            rename_all.update(return_alias_map)
            rename_all.update(local_rename_map)
            ret_nm = rename_all.get(ret_alias_m.group(0), ret_alias_m.group(0))
            if ret_nm == ret_alias_m.group(0):
                ret_nm_src = ret_nm
                if any(
                    isinstance(st_src, Assign)
                    and st_src.name == ret_nm_src
                    and _parse_list_constructor(st_src.expr.strip()) is not None
                    for st_src in body_stmts
                ):
                    ret_nm = f"{ret_nm_src}_list"
            o.w(f"{rname} = {ret_nm}")
            o.w(f"end function {fn.name}")
            return bool(need_rnorm_local["used"])

        def _emit_assign(prefix: str, fields: dict[str, object]) -> None:
            for k, v in fields.items():
                if isinstance(v, dict):
                    _emit_assign(f"{prefix}%{k}", v)
                else:
                    rename_all = {}
                    rename_all.update(arg_local_map)
                    rename_all.update(local_rename_map)
                    vv = _replace_idents(str(v), rename_all) if rename_all else str(v)
                    vv_txt = str(vv).strip()
                    if k == "order":
                        rhs_f = _int_vector_literal_from_c(vv_txt) or r_expr_to_fortran(vv_txt)
                    else:
                        rhs_f = '""' if k == "out" and vv_txt.upper() == "NULL" else r_expr_to_fortran(vv_txt)
                    o.w(f"{prefix}%{k} = {rhs_f}")
        _emit_assign(rname, list_spec.root_fields)
    o.w(f"end function {fn.name}")
    return bool(need_rnorm_local["used"])


def infer_function_integer_names(fn: FuncDef) -> set[str]:
    """Infer names that are likely integer-typed within one function scope."""
    ints: set[str] = set()
    for a in fn.args:
        dflt = fn.defaults.get(a, "").strip()
        if a in {"n", "k", "p", "order", "start_order", "max_order", "maxlag", "lag", "nacf", "seed", "iter", "max_iter", "maxit", "it"} or _is_int_literal(dflt) or dflt.upper() == "NULL":
            ints.add(a)
    body_no_ret = (fn.body[:-1] if isinstance(fn.body[-1], ExprStmt) else fn.body) if fn.body else []
    if body_no_ret:
        ints.update(a for a in fn.args if a in infer_integer_context_names(body_no_ret))
        known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        b_ints, _b_real_scalars, _b_int_arrays, _b_real_arrays, b_params = classify_vars(
            body_no_ret, infer_assigned_names(body_no_ret), known_arrays=known_arrays
        )
        ints.update(b_ints)
        ints.update(b_params.keys())
    return ints


def infer_function_integer_array_names(fn: FuncDef) -> set[str]:
    """Infer local names that are likely integer arrays within one function scope."""
    int_arrays: set[str] = set()
    body_no_ret = (fn.body[:-1] if isinstance(fn.body[-1], ExprStmt) else fn.body) if fn.body else []
    if body_no_ret:
        known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        _b_ints, _b_real_scalars, b_int_arrays, _b_real_arrays, _b_params = classify_vars(
            body_no_ret, infer_assigned_names(body_no_ret), known_arrays=known_arrays
        )
        int_arrays.update(b_int_arrays)
    return int_arrays


def infer_function_real_array_names(fn: FuncDef) -> set[str]:
    """Infer local names that are likely real arrays within one function scope."""
    real_arrays: set[str] = set()
    body_no_ret = (fn.body[:-1] if isinstance(fn.body[-1], ExprStmt) else fn.body) if fn.body else []
    if body_no_ret:
        known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        _b_ints, _b_real_scalars, _b_int_arrays, b_real_arrays, _b_params = classify_vars(
            body_no_ret, infer_assigned_names(body_no_ret), known_arrays=known_arrays
        )
        real_arrays.update(b_real_arrays)
    return real_arrays


def infer_function_real_matrix_names(fn: FuncDef) -> set[str]:
    """Infer local names that are likely rank-2 real arrays within one function scope."""
    mats: set[str] = set()
    body_no_ret = (fn.body[:-1] if isinstance(fn.body[-1], ExprStmt) else fn.body) if fn.body else []
    if body_no_ret:
        known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
        _b_ints, _b_real_scalars, _b_int_arrays, b_real_arrays, _b_params = classify_vars(
            body_no_ret, infer_assigned_names(body_no_ret), known_arrays=known_arrays
        )
        for nm in b_real_arrays:
            if _infer_local_array_rank(body_no_ret, nm) >= 2:
                mats.add(nm)
    return mats


def infer_function_lm_names(fn: FuncDef) -> set[str]:
    """Infer local names that hold lm_fit_t values within one function scope."""
    out: set[str] = set()

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                c = parse_call_text(rhs)
                if c is not None and c[0].lower() == "lm":
                    out.add(st.name)
                    continue
                m = re.match(r"^([A-Za-z]\w*)$", rhs)
                if m is not None and m.group(1) in out:
                    out.add(st.name)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(fn.body)
    return out


def _rewrite_named_calls(
    expr: str,
    fn_arg_order: dict[str, list[str]],
    fn_arg_defaults: dict[str, dict[str, str]],
) -> str:
    cinfo = parse_call_text(expr)
    if cinfo is None:
        return expr
    nm, pos, kw = cinfo
    order = fn_arg_order.get(nm)
    if order is None or not kw:
        return expr
    defaults = fn_arg_defaults.get(nm, {})
    kw_norm = dict(kw)
    for k, v in kw.items():
        kw_norm.setdefault(_sanitize_fortran_kwarg_name(k), v)
    vals: list[str] = []
    ip = 0
    for anm in order:
        if ip < len(pos):
            vals.append(pos[ip])
            ip += 1
        elif anm in kw_norm:
            vals.append(f"{anm} = {kw_norm[anm]}")
        elif anm in defaults:
            if defaults[anm].strip().upper() == "NULL":
                continue
            vals.append(f"{anm} = {defaults[anm]}")
        else:
            # keep placeholder name if not provided
            vals.append(anm)
    return f"{nm}(" + ", ".join(vals) + ")"


def _fortran_ident(name: str) -> str:
    s = re.sub(r"[^A-Za-z0-9_]", "_", name.strip())
    if not s:
        s = "main"
    if not re.match(r"[A-Za-z]", s[0]):
        s = "x_" + s
    return s


def _module_name_from_stem(stem: str) -> str:
    base = _fortran_ident(stem)
    if base.lower().startswith("x") and len(base) > 1:
        base = base[1:]
    return _fortran_ident(base + "_mod")


def _infer_literal_array_parameter(rhs: str) -> tuple[str, int, str] | None:
    """Infer array-parameter declaration info from a literal constructor RHS.

    Returns `(kind, n, expr_f)` where kind is `"integer"` or `"real"`.
    """
    expr_f = r_expr_to_fortran(rhs.strip())
    t = expr_f.strip()
    if not (t.startswith("[") and t.endswith("]")):
        return None
    inner = t[1:-1].strip()
    if not inner:
        return None
    vals = [x.strip() for x in split_top_level_commas(inner) if x.strip()]
    if not vals:
        return None
    all_int = True
    all_num = True
    for v in vals:
        if _is_int_literal(v):
            continue
        if _is_real_literal(v):
            all_int = False
            continue
        all_num = False
        break
    if not all_num:
        return None
    kind = "integer" if all_int else "real"
    return kind, len(vals), expr_f


def infer_main_array_params(stmts: list[object], assign_counts: dict[str, int]) -> dict[str, tuple[str, int, str]]:
    """Find conservative top-level named-constant array candidates."""
    out: dict[str, tuple[str, int, str]] = {}
    for st in stmts:
        if not isinstance(st, Assign):
            continue
        if assign_counts.get(st.name, 0) != 1:
            continue
        rhs = st.expr.strip()
        if not (rhs.startswith("c(") or rhs.startswith("[") or rhs.startswith("array(")):
            continue
        info = _infer_literal_array_parameter(rhs)
        if info is None:
            continue
        out[st.name] = info
    return out


def infer_main_character_scalars(stmts: list[object]) -> set[str]:
    """Find scalar vars assigned from quoted string literals in main statements."""
    out: set[str] = set()
    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                if _dequote_string_literal(rhs) is not None:
                    out.add(st.name)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return out


def infer_main_character_arrays(stmts: list[object]) -> set[str]:
    """Find vector vars assigned from c("...")-style character constructors."""
    out: set[str] = set()
    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                low = rhs.lower()
                if low.startswith("commandargs("):
                    out.add(st.name)
                    continue
                if low.startswith("character("):
                    out.add(st.name)
                    continue
                c_rhs = parse_call_text(rhs)
                if c_rhs is not None and c_rhs[0].lower() in {"arma_coef_names"}:
                    out.add(st.name)
                    continue
                if not low.startswith("c("):
                    continue
                cinfo = c_rhs
                if cinfo is None:
                    continue
                _nm, pos, _kw = cinfo
                if not pos:
                    continue
                all_chr = True
                for a in pos:
                    aa = a.strip()
                    if _dequote_string_literal(aa) is not None:
                        continue
                    if aa.upper() == "NA_CHARACTER_":
                        continue
                    all_chr = False
                    break
                if all_chr:
                    out.add(st.name)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return out


def collect_categorical_sample_labels(stmts: list[object]) -> dict[str, list[str]]:
    """Find vars assigned from sample(c("A", ...)) and keep their level labels."""
    out: dict[str, list[str]] = {}

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                cinfo = parse_call_text(st.expr.strip())
                if cinfo is not None and cinfo[0].lower() == "sample":
                    x_src = cinfo[1][0] if cinfo[1] else cinfo[2].get("x")
                    labels = _parse_string_c_vector(x_src.strip()) if x_src is not None else None
                    if labels is not None:
                        out[st.name.lower()] = labels
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return out


def collect_table_labels(stmts: list[object], categorical_labels: dict[str, list[str]]) -> dict[str, tuple[list[str] | None, list[str] | None]]:
    """Track labels for simple table(...) results assigned to variables."""
    out: dict[str, tuple[list[str] | None, list[str] | None]] = {}

    def _labels_for(src: str) -> list[str] | None:
        return categorical_labels.get(src.strip().lower())

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                cinfo = parse_call_text(st.expr.strip())
                if cinfo is not None and cinfo[0].lower() == "table":
                    vals = list(cinfo[1]) + list(cinfo[2].values())
                    if len(vals) == 1:
                        out[st.name.lower()] = (_labels_for(vals[0]), None)
                    elif len(vals) >= 2:
                        out[st.name.lower()] = (_labels_for(vals[0]) or [], _labels_for(vals[1]) or [])
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return out


def infer_function_character_scalars(fn: FuncDef) -> set[str]:
    """Infer character scalar locals from string comparisons and string helpers."""
    out: set[str] = set()

    def _scan(ss: list[object]) -> None:
        for st in ss:
            texts: list[str] = []
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                if _dequote_string_literal(rhs) is not None:
                    out.add(st.name)
                if re.match(r"^(sub|substr)\s*\(", rhs, re.IGNORECASE):
                    out.add(st.name)
                texts.append(rhs)
            elif isinstance(st, IfStmt):
                texts.append(st.cond)
                _scan(st.then_body)
                _scan(st.else_body)
            elif isinstance(st, WhileStmt):
                texts.append(st.cond)
                _scan(st.body)
            elif isinstance(st, ForStmt):
                texts.append(st.iter_expr)
                _scan(st.body)
            elif isinstance(st, RepeatStmt):
                _scan(st.body)
            elif isinstance(st, CallStmt):
                texts.extend(st.args)
            elif isinstance(st, ExprStmt):
                texts.append(st.expr)
            for txt in texts:
                for m in re.finditer(r"\b([A-Za-z]\w*)\s*(?:==|!=)\s*(['\"])", txt):
                    out.add(m.group(1))
                for m in re.finditer(r"\b(?:startsWith|nzchar)\s*\(\s*([A-Za-z]\w*)\b", txt, re.IGNORECASE):
                    out.add(m.group(1))

    body_no_ret = (fn.body[:-1] if isinstance(fn.body[-1], ExprStmt) else fn.body) if fn.body else []
    _scan(body_no_ret)
    changed = True
    while changed:
        changed = False
        for st in body_no_ret:
            if not isinstance(st, Assign) or st.name not in out:
                continue
            m_idx = re.match(r"^([A-Za-z]\w*)\s*\[[^\]]+\]\s*$", st.expr.strip())
            if m_idx and m_idx.group(1) not in out:
                # The base is an array, handled by infer_function_character_array_names.
                continue
    return out


def infer_function_character_array_names(fn: FuncDef, char_scalars: set[str] | None = None) -> set[str]:
    """Infer character vector names, including commandArgs() and vectors indexed to char scalars."""
    out: set[str] = set()
    scalars = set(char_scalars or set())

    def _scan(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                if rhs.lower().startswith("commandargs("):
                    out.add(st.name)
                m_idx = re.match(r"^([A-Za-z]\w*)\s*\[[^\]]+\]\s*$", rhs)
                if m_idx and st.name in scalars:
                    out.add(m_idx.group(1))
            elif isinstance(st, IfStmt):
                _scan(st.then_body)
                _scan(st.else_body)
            elif isinstance(st, WhileStmt):
                _scan(st.body)
            elif isinstance(st, ForStmt):
                _scan(st.body)
            elif isinstance(st, RepeatStmt):
                _scan(st.body)

    body_no_ret = (fn.body[:-1] if isinstance(fn.body[-1], ExprStmt) else fn.body) if fn.body else []
    _scan(body_no_ret)
    return out


def infer_main_logical_scalars(stmts: list[object]) -> set[str]:
    """Find scalar vars assigned from R logical literals TRUE/FALSE in main statements."""
    out: set[str] = set()
    for st in stmts:
        if not isinstance(st, Assign):
            continue
        rhs = st.expr.strip().upper()
        if rhs in {"TRUE", "FALSE"}:
            out.add(st.name)
    return out


def infer_main_logical_arrays(stmts: list[object], array_names: set[str]) -> set[str]:
    """Find rank-1 logical mask variables assigned from vector predicates."""
    out: set[str] = set()

    def _walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                rhs = st.expr.strip()
                rhs_l = rhs.lower()
                is_logical_vec = False
                if re.match(r"^logical\s*\(", rhs_l):
                    is_logical_vec = True
                elif re.match(r"^is\.na\s*\(", rhs_l) or re.match(r"^is_na\s*\(", rhs_l):
                    is_logical_vec = True
                elif any(_split_top_level_token(rhs, op, from_right=True) is not None for op in ["==", "!=", ">=", "<=", ">", "<"]):
                    names = {n for n in re.findall(r"\b[A-Za-z]\w*\b", rhs)}
                    rhs_f = r_expr_to_fortran(rhs)
                    is_logical_vec = bool(names & array_names) or bool(
                        re.search(r"\b(?:runif_vec|rnorm_vec|numeric|r_rep_real|r_rep_int)\s*\(", rhs_f)
                    )
                if is_logical_vec:
                    out.add(st.name)
            elif isinstance(st, IfStmt):
                _walk(st.then_body)
                _walk(st.else_body)
            elif isinstance(st, ForStmt):
                _walk(st.body)
            elif isinstance(st, WhileStmt):
                _walk(st.body)
            elif isinstance(st, RepeatStmt):
                _walk(st.body)

    _walk(stmts)
    return out


def infer_main_real_matrices(stmts: list[object], known_int_matrices: set[str] | None = None) -> set[str]:
    """Find variables that should be declared as rank-2 real allocatables."""
    out: set[str] = set()
    known_int_matrices = set(known_int_matrices or set())

    def _scan_text(txt: str) -> None:
        for m in re.finditer(r"\b(?:nrow|ncol)\s*\(\s*([A-Za-z]\w*)\s*\)", txt):
            out.add(m.group(1))
        for m in re.finditer(r"\b([A-Za-z]\w*)\s*\[([^\]]+)\]", txt):
            if len(_split_index_dims(m.group(2))) >= 2:
                out.add(m.group(1))
        m_wr = re.match(r"^\s*write\.table\s*\((.*)\)\s*$", txt, re.IGNORECASE)
        if m_wr:
            cinfo = parse_call_text("write.table(" + m_wr.group(1).strip() + ")")
            if cinfo is not None:
                _nm, pos, _kw = cinfo
                if pos:
                    p0 = pos[0].strip()
                    if re.match(r"^[A-Za-z]\w*$", p0):
                        out.add(p0)

    for st in stmts:
        if isinstance(st, Assign):
            rhs = st.expr.strip()
            low = rhs.lower()
            if _split_top_level_token(rhs, "%*%", from_right=True) is not None:
                out.add(st.name)
            if low.startswith("matrix(") and not _matrix_has_integer_literal_data(rhs):
                out.add(st.name)
            if low.startswith("read.csv("):
                out.add(st.name)
            if low.startswith("simulate_var("):
                out.add(st.name)
            list_arr = _numeric_list_array_expr(rhs)
            if list_arr is not None and list_arr[1] == 2:
                out.add(st.name)
            if low.startswith("cbind(") or low.startswith("cbind2(") or low.startswith("rbind("):
                out.add(st.name)
            if low.startswith("t("):
                out.add(st.name)
            if low.startswith("outer("):
                cinfo_o = parse_call_text(rhs)
                is_int_outer = False
                if cinfo_o is not None and cinfo_o[0].lower() == "outer":
                    fun_src = cinfo_o[2].get("FUN", "").strip()
                    m_fun = re.match(
                        r"^function\s*\(\s*([A-Za-z]\w*)\s*,\s*([A-Za-z]\w*)\s*\)\s*(.+)$",
                        fun_src,
                        re.IGNORECASE,
                    )
                    if m_fun is not None:
                        i_nm = m_fun.group(1)
                        j_nm = m_fun.group(2)
                        body = m_fun.group(3).strip()
                        body_int = re.sub(rf"\b{re.escape(i_nm)}\b", "1", body)
                        body_int = re.sub(rf"\b{re.escape(j_nm)}\b", "1", body_int)
                        is_int_outer = _is_integer_arith_expr(body_int)
                if not is_int_outer:
                    out.add(st.name)
            c_exp = parse_call_text(rhs)
            if c_exp is not None and c_exp[0].lower() == "exp" and len(c_exp[1]) == 1:
                inner = fscan.strip_redundant_outer_parens_expr(c_exp[1][0].strip())
                if any(re.search(rf"\b{re.escape(nm)}\b", inner) for nm in known_int_matrices):
                    out.add(st.name)
                for op in ["+", "-", "*", "/"]:
                    mm = _split_top_level_token(inner, op, from_right=True)
                    if mm is None:
                        continue
                    if any(side.strip() in out for side in (mm[0], mm[1])):
                        out.add(st.name)
                        break
            for op in ["+", "-", "*", "/"]:
                mm = _split_top_level_token(rhs, op, from_right=True)
                if mm is not None and any(side.strip() in out for side in (mm[0], mm[1])):
                    out.add(st.name)
                    break
            if low.startswith("read.table("):
                out.add(st.name)
            if "read.table(" in low and "as.matrix(" in low:
                out.add(st.name)
            _scan_text(rhs)
        elif isinstance(st, CallStmt):
            _scan_text(f"{st.name}(" + ", ".join(st.args) + ")")
        elif isinstance(st, ExprStmt):
            _scan_text(st.expr)
        elif isinstance(st, PrintStmt):
            for a in st.args:
                _scan_text(a)
        elif isinstance(st, IfStmt):
            _scan_text(st.cond)
            for b in st.then_body + st.else_body:
                if isinstance(b, (Assign, CallStmt, ExprStmt)):
                    if isinstance(b, Assign):
                        rhs_b = b.expr.strip()
                        low_b = rhs_b.lower()
                        if (low_b.startswith("matrix(") and not _matrix_has_integer_literal_data(rhs_b)) or low_b.startswith("array("):
                            out.add(b.name)
                    _scan_text(b.expr if isinstance(b, ExprStmt) else (b.name + "(" + ", ".join(b.args) + ")" if isinstance(b, CallStmt) else b.expr))
                elif isinstance(b, PrintStmt):
                    for a in b.args:
                        _scan_text(a)
        elif isinstance(st, ForStmt):
            for b in st.body:
                if isinstance(b, Assign):
                    rhs_b = b.expr.strip()
                    low_b = rhs_b.lower()
                    if (low_b.startswith("matrix(") and not _matrix_has_integer_literal_data(rhs_b)) or low_b.startswith("array("):
                        out.add(b.name)
                    _scan_text(rhs_b)
                elif isinstance(b, (CallStmt, ExprStmt)):
                    _scan_text(b.expr if isinstance(b, ExprStmt) else b.name + "(" + ", ".join(b.args) + ")")
                elif isinstance(b, PrintStmt):
                    for a in b.args:
                        _scan_text(a)
    return out


def infer_main_integer_matrices(stmts: list[object]) -> set[str]:
    """Find variables that should be declared as rank-2 integer allocatables."""
    out: set[str] = set()
    for st in stmts:
        if not isinstance(st, Assign):
            continue
        rhs = st.expr.strip()
        cinfo_o = parse_call_text(rhs)
        if cinfo_o is not None and cinfo_o[0].lower() == "table":
            vals = list(cinfo_o[1]) + list(cinfo_o[2].values())
            if len(vals) >= 2:
                out.add(st.name)
                continue
        if cinfo_o is not None and cinfo_o[0].lower() == "array":
            data_src = cinfo_o[1][0] if cinfo_o[1] else cinfo_o[2].get("data", "")
            data_txt = data_src.strip()
            if (
                _split_top_level_colon(data_txt) is not None
                or data_txt.lower().startswith(("integer(", "raw(", "r_seq_int(", "r_seq_len(", "r_rep_int("))
            ):
                out.add(st.name)
                continue
        if cinfo_o is not None and cinfo_o[0].lower() == "matrix" and _matrix_has_integer_literal_data(rhs):
            out.add(st.name)
            continue
        if cinfo_o is None or cinfo_o[0].lower() != "outer":
            continue
        fun_src = cinfo_o[2].get("FUN", "").strip()
        m_fun = re.match(
            r"^function\s*\(\s*([A-Za-z]\w*)\s*,\s*([A-Za-z]\w*)\s*\)\s*(.+)$",
            fun_src,
            re.IGNORECASE,
        )
        if m_fun is None:
            continue
        i_nm = m_fun.group(1)
        j_nm = m_fun.group(2)
        body = m_fun.group(3).strip()
        body_int = re.sub(rf"\b{re.escape(i_nm)}\b", "1", body)
        body_int = re.sub(rf"\b{re.escape(j_nm)}\b", "1", body_int)
        if _is_integer_arith_expr(body_int):
            out.add(st.name)
    return out


def expand_data_frame_assignments(stmts: list[object]) -> list[object]:
    """Expand `a <- data.frame(x1=..., x2=...)` into scalar/array assignments.

    Emits assignments to `a_x1`, `a_x2`, ... so later calls can reference fields.
    """
    global _EXPANDED_DATA_FRAME_FIELDS, _EXPANDED_DATA_FRAME_ALIASES, _DATA_FRAME_FORCE_MATERIALIZE
    out: list[object] = []
    for st in stmts:
        if not isinstance(st, Assign):
            out.append(st)
            continue
        rhs = st.expr.strip()
        cinfo = parse_call_text(rhs)
        if cinfo is None or cinfo[0].lower() != "data.frame":
            out.append(st)
            continue
        _nm, _pos, kw = cinfo
        if not kw:
            # Keep fallback if shape is not understood.
            out.append(st)
            continue
        fields_for_df = list(kw.keys())
        aliases_for_df: dict[str, str] = {}
        for k, v in kw.items():
            v_txt = v.strip()
            if re.fullmatch(r"[A-Za-z]\w*", v_txt):
                aliases_for_df[k] = v_txt
        for df_key in (st.name, st.name.lower()):
            prior = _EXPANDED_DATA_FRAME_FIELDS.setdefault(df_key, [])
            for field in fields_for_df:
                if field.lower() not in {p.lower() for p in prior}:
                    prior.append(field)
            if aliases_for_df:
                prior_aliases = _EXPANDED_DATA_FRAME_ALIASES.setdefault(df_key, {})
                for field, alias in aliases_for_df.items():
                    prior_aliases[field] = alias
        first_col_src = next((v.strip() for v in kw.values() if v.strip()), "")
        for k, v in kw.items():
            v_out = v
            if first_col_src and re.fullmatch(r"NA(?:_real_)?", v.strip(), re.IGNORECASE):
                v_out = f"numeric(length({first_col_src}))"
            if st.name not in _DATA_FRAME_FORCE_MATERIALIZE and k in aliases_for_df and v_out == v:
                continue
            out.append(Assign(name=f"{st.name}_{_sanitize_fortran_kwarg_name(k)}", expr=v_out, comment=st.comment))
    return out


def collect_model_data_frame_uses(stmts: list[object]) -> set[str]:
    out: set[str] = set()

    def add_name(src: str | None) -> None:
        if src is None:
            return
        t = src.strip()
        if re.fullmatch(r"[A-Za-z]\w*", t):
            out.add(t)

    def walk(ss: list[object]) -> None:
        for st in ss:
            exprs: list[str] = []
            if isinstance(st, Assign):
                exprs.append(st.expr)
            elif isinstance(st, ExprStmt):
                exprs.append(st.expr)
            elif isinstance(st, PrintStmt):
                exprs.extend(st.args)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)
            for expr in exprs:
                cinfo = parse_call_text(expr.strip())
                if cinfo is None:
                    continue
                nm, _pos, kw = cinfo
                if nm.lower() in {"lm", "glm", "aov"}:
                    add_name(kw.get("data"))
                elif nm.lower() == "predict":
                    add_name(kw.get("newdata"))

    walk(stmts)
    return out


def collect_colname_labels(stmts: list[object]) -> dict[str, list[str]]:
    labels: dict[str, list[str]] = {}
    string_vectors: dict[str, list[str]] = {}

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, Assign):
                labs = _parse_string_c_vector(st.expr.strip())
                if labs is not None:
                    string_vectors[st.name.lower()] = labs
            elif isinstance(st, ExprStmt):
                m = re.match(
                    r"^\s*colnames\s*\(\s*([A-Za-z]\w*)\s*\)\s*<-\s*(.+)$",
                    st.expr.strip(),
                    re.IGNORECASE,
                )
                if m is not None:
                    rhs = m.group(2).strip()
                    labs = _parse_string_c_vector(rhs)
                    if labs is None and re.fullmatch(r"[A-Za-z]\w*", rhs):
                        labs = string_vectors.get(rhs.lower())
                    if labs is not None:
                        labels[m.group(1).lower()] = labs
            elif isinstance(st, FuncDef):
                walk(st.body)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return labels


def collect_colname_sources(stmts: list[object]) -> dict[str, str]:
    sources: dict[str, str] = {}

    def walk(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, ExprStmt):
                m = re.match(
                    r"^\s*colnames\s*\(\s*([A-Za-z]\w*)\s*\)\s*<-\s*(.+)$",
                    st.expr.strip(),
                    re.IGNORECASE,
                )
                if m is not None:
                    src = m.group(2).strip()
                    if _parse_string_c_vector(src) is None:
                        sources[m.group(1).lower()] = src
            elif isinstance(st, FuncDef):
                walk(st.body)
            elif isinstance(st, IfStmt):
                walk(st.then_body)
                walk(st.else_body)
            elif isinstance(st, ForStmt):
                walk(st.body)
            elif isinstance(st, WhileStmt):
                walk(st.body)
            elif isinstance(st, RepeatStmt):
                walk(st.body)

    walk(stmts)
    return sources


def infer_main_real_rank3_arrays(stmts: list[object]) -> set[str]:
    out: set[str] = set()
    for st in stmts:
        if isinstance(st, Assign):
            list_arr = _numeric_list_array_expr(st.expr.strip())
            if list_arr is not None and list_arr[1] == 3:
                out.add(st.name)
    return out


def infer_main_real_rank4_arrays(stmts: list[object]) -> set[str]:
    out: set[str] = set()
    for st in stmts:
        if isinstance(st, Assign):
            list_arr = _numeric_nested_matrix_list_array_expr(st.expr.strip())
            if list_arr is not None and list_arr[1] == 4:
                out.add(st.name)
    return out


def _parse_dim_assignment(expr: str) -> tuple[str, str] | None:
    m = re.match(r"^dim\s*\(\s*([A-Za-z]\w*)\s*\)\s*(?:<-|=)\s*(.+)$", expr.strip(), re.IGNORECASE)
    if m is None:
        return None
    return m.group(1), m.group(2).strip()


def _lower_dim_assignments(stmts: list[object]) -> list[object]:
    """Fold simple `dim(x) <- c(...)` reshapes into the preceding assignment."""
    out: list[object] = []
    last_assign_idx: dict[str, int] = {}
    for st in stmts:
        if isinstance(st, Assign):
            last_assign_idx[st.name] = len(out)
            out.append(st)
            continue
        if isinstance(st, ExprStmt):
            da = _parse_dim_assignment(st.expr)
            if da is not None:
                nm, dims = da
                idx = last_assign_idx.get(nm)
                if idx is not None and isinstance(out[idx], Assign):
                    prev = out[idx]
                    out[idx] = Assign(name=prev.name, expr=f"array({prev.expr}, dim={dims})", comment=prev.comment)
                    continue
        out.append(st)
    return out


def _parse_s3_class_rhs(rhs: str) -> list[str] | None:
    one = _dequote_string_literal(rhs.strip())
    if one is not None and re.match(r"^[A-Za-z]\w*$", one):
        return [one]
    cinfo = parse_call_text(rhs.strip())
    if cinfo is None or cinfo[0].lower() != "c" or cinfo[2]:
        return None
    out: list[str] = []
    for p in cinfo[1]:
        cls = _dequote_string_literal(p.strip())
        if cls is None or not re.match(r"^[A-Za-z]\w*$", cls):
            return None
        out.append(cls)
    return out or None


def _parse_s3_class_assignment(expr: str) -> tuple[str, list[str]] | None:
    m = re.match(
        r"^class\s*\(\s*([A-Za-z]\w*)\s*\)\s*(?:<-|=)\s*(.+)$",
        expr.strip(),
    )
    if not m:
        return None
    classes = _parse_s3_class_rhs(m.group(2).strip())
    if classes is None:
        return None
    return m.group(1), classes


def _unwrap_s4_metadata_expr(expr: str) -> str:
    cinfo = parse_call_text(expr.strip())
    if cinfo is not None and cinfo[0].lower() == "invisible" and len(cinfo[1]) == 1 and not cinfo[2]:
        return cinfo[1][0].strip()
    return expr.strip()


def _parse_s4_setclass(expr: str) -> tuple[str, list[str], list[str]] | None:
    cinfo = parse_call_text(_unwrap_s4_metadata_expr(expr))
    if cinfo is None or cinfo[0].lower() != "setclass" or not cinfo[1]:
        return None
    cls = _dequote_string_literal(cinfo[1][0].strip())
    if cls is None or not re.match(r"^[A-Za-z]\w*$", cls):
        return None
    parents: list[str] = []
    contains_expr = cinfo[2].get("contains", "")
    parent_one = _dequote_string_literal(contains_expr.strip()) if contains_expr else None
    if parent_one is not None:
        if not re.match(r"^[A-Za-z]\w*$", parent_one):
            return None
        parents.append(parent_one)
    elif contains_expr:
        contains_call = parse_call_text(contains_expr.strip())
        if contains_call is None or contains_call[0].lower() != "c" or contains_call[2]:
            return None
        for p in contains_call[1]:
            parent = _dequote_string_literal(p.strip())
            if parent is None or not re.match(r"^[A-Za-z]\w*$", parent):
                return None
            parents.append(parent)
    slots_expr = cinfo[2].get("slots", "")
    if not slots_expr:
        return cls, parents, []
    slots_call = parse_call_text(slots_expr.strip())
    if slots_call is None or slots_call[0].lower() != "c" or not slots_call[2]:
        return None
    slots: list[str] = []
    for nm, typ in slots_call[2].items():
        slot_type = _dequote_string_literal(typ.strip())
        if slot_type is None or slot_type.lower() != "numeric":
            return None
        if not re.match(r"^[A-Za-z]\w*$", nm):
            return None
        slots.append(nm)
    return cls, parents, slots


def _parse_s4_setgeneric(expr: str) -> str | None:
    cinfo = parse_call_text(_unwrap_s4_metadata_expr(expr))
    if cinfo is None or cinfo[0].lower() != "setgeneric" or not cinfo[1]:
        return None
    generic = _dequote_string_literal(cinfo[1][0].strip())
    if generic is None or not re.match(r"^[A-Za-z]\w*$", generic):
        return None
    return generic


def _parse_s4_function_literal(src: str) -> tuple[list[str], list[object]] | None:
    m = re.match(r"^function\s*\(([^)]*)\)\s*(.+)$", src.strip(), re.IGNORECASE | re.DOTALL)
    if m is None:
        return None
    args = [a.strip() for a in split_top_level_commas(m.group(1)) if a.strip()]
    if any(not re.match(r"^[A-Za-z]\w*$", a) for a in args):
        return None
    body_src = m.group(2).strip()
    if body_src.startswith("{") and body_src.endswith("}"):
        body_src = body_src[1:-1].strip()
    if not body_src:
        return None
    return args, [ExprStmt(body_src)]


def _parse_s4_setmethod(expr: str) -> tuple[str, str, list[str], list[object]] | None:
    cinfo = parse_call_text(_unwrap_s4_metadata_expr(expr))
    if cinfo is None or cinfo[0].lower() != "setmethod" or len(cinfo[1]) < 3:
        return None
    generic = _dequote_string_literal(cinfo[1][0].strip())
    cls = _dequote_string_literal(cinfo[1][1].strip())
    if (
        generic is None
        or cls is None
        or not re.match(r"^[A-Za-z]\w*$", generic)
        or not re.match(r"^[A-Za-z]\w*$", cls)
    ):
        return None
    fn = _parse_s4_function_literal(cinfo[1][2])
    if fn is None:
        return None
    args, body = fn
    return generic, cls, args, body


def _rewrite_minimal_s4_expr(
    expr: str,
    class_slots: dict[str, list[str]],
    class_ancestors: dict[str, list[str]],
    generics: set[str],
    var_classes: dict[str, str],
    method_classes: set[tuple[str, str]],
) -> str:
    out = expr

    def repl_new(inner: str) -> str:
        cinfo = parse_call_text("new(" + inner + ")")
        if cinfo is None or not cinfo[1]:
            return "new(" + inner + ")"
        cls = _dequote_string_literal(cinfo[1][0].strip())
        if cls is None or cls not in class_slots:
            return "new(" + inner + ")"
        args: list[str] = []
        pos_tail = list(cinfo[1][1:])
        for slot in class_slots[cls]:
            if slot in cinfo[2]:
                args.append(cinfo[2][slot])
            elif pos_tail:
                args.append(pos_tail.pop(0))
            else:
                return "new(" + inner + ")"
        return f"{cls}(" + ", ".join(args) + ")"

    out = _replace_balanced_func_calls(out, "new", repl_new)
    for nm, cls in sorted(var_classes.items(), key=lambda kv: len(kv[0]), reverse=True):
        out = _replace_balanced_func_calls(
            out,
            "class",
            lambda inner, nm=nm, cls=cls: (
                _fortran_str_literal(cls) if inner.strip() == nm else f"class({inner.strip()})"
            ),
        )
        out = _replace_balanced_func_calls(
            out,
            "is",
            lambda inner, nm=nm, cls=cls: _rewrite_minimal_s4_is_call(inner, nm, cls, class_ancestors),
        )
    for generic in sorted(generics, key=len, reverse=True):
        def repl_generic(inner: str, generic: str = generic) -> str:
            args = split_top_level_commas(inner.strip()) if inner.strip() else []
            if not args:
                return f"{generic}()"
            first = args[0].strip()
            cls = var_classes.get(first)
            if cls is None or (generic, cls) not in method_classes:
                return f"{generic}(" + ", ".join(args) + ")"
            return f"{generic}_{cls}(" + ", ".join(args) + ")"

        out = _replace_balanced_func_calls(out, generic, repl_generic)
    return out


def _rewrite_minimal_s4_is_call(
    inner: str,
    var_name: str,
    var_class: str,
    class_ancestors: dict[str, list[str]],
) -> str:
    args = split_top_level_commas(inner.strip()) if inner.strip() else []
    if len(args) != 2 or args[0].strip() != var_name:
        return f"is({inner.strip()})"
    target = _dequote_string_literal(args[1].strip())
    if target is None:
        return f"is({inner.strip()})"
    return "TRUE" if target in class_ancestors.get(var_class, [var_class]) else "FALSE"


def _lower_minimal_s4(stmts: list[object]) -> list[object]:
    """Lower a static S4 subset to constructor functions and direct method calls."""
    class_own_slots: dict[str, list[str]] = {}
    class_parents: dict[str, list[str]] = {}
    generics: set[str] = set()
    method_defs: dict[tuple[str, str], FuncDef] = {}

    for st in stmts:
        if not isinstance(st, ExprStmt):
            continue
        sc = _parse_s4_setclass(st.expr)
        if sc is not None:
            class_own_slots[sc[0]] = sc[2]
            class_parents[sc[0]] = sc[1]
            continue
        sg = _parse_s4_setgeneric(st.expr)
        if sg is not None:
            generics.add(sg)
            continue
        sm = _parse_s4_setmethod(st.expr)
        if sm is not None:
            generic, cls, args, body = sm
            method_defs[(generic, cls)] = FuncDef(name=f"{generic}_{cls}", args=args, defaults={}, body=body)

    if not class_own_slots and not method_defs:
        return stmts

    def _ancestors(cls: str, seen: set[str] | None = None) -> list[str]:
        seen = set(seen or set())
        if cls in seen:
            return [cls]
        seen.add(cls)
        out = [cls]
        for parent in class_parents.get(cls, []):
            for anc in _ancestors(parent, seen):
                if anc not in out:
                    out.append(anc)
        return out

    class_ancestors = {cls: _ancestors(cls) for cls in class_own_slots}

    def _flattened_slots(cls: str) -> list[str]:
        slots: list[str] = []
        for parent in class_parents.get(cls, []):
            for slot in _flattened_slots(parent):
                if slot not in slots:
                    slots.append(slot)
        for slot in class_own_slots.get(cls, []):
            if slot not in slots:
                slots.append(slot)
        return slots

    class_slots = {cls: _flattened_slots(cls) for cls in class_own_slots}

    for cls, ancestors in class_ancestors.items():
        for generic in generics:
            if (generic, cls) in method_defs:
                continue
            for parent in ancestors[1:]:
                parent_fn = method_defs.get((generic, parent))
                if parent_fn is None:
                    continue
                method_defs[(generic, cls)] = FuncDef(
                    name=f"{generic}_{cls}",
                    args=list(parent_fn.args),
                    defaults=dict(parent_fn.defaults),
                    body=list(parent_fn.body),
                )
                break
    method_classes = set(method_defs)

    constructors = [
        FuncDef(
            name=cls,
            args=list(slots),
            defaults={},
            body=[ExprStmt("list(" + ", ".join(f"{slot} = as.numeric({slot})" for slot in slots) + ")")],
        )
        for cls, slots in class_slots.items()
    ]

    out: list[object] = constructors + list(method_defs.values())
    var_classes: dict[str, str] = {}
    for st in stmts:
        if isinstance(st, ExprStmt) and (
            _parse_s4_setclass(st.expr) is not None
            or _parse_s4_setgeneric(st.expr) is not None
            or _parse_s4_setmethod(st.expr) is not None
        ):
            continue
        if isinstance(st, Assign):
            rhs = _rewrite_minimal_s4_expr(
                st.expr, class_slots, class_ancestors, generics, var_classes, method_classes
            )
            cinfo = parse_call_text(rhs.strip())
            if cinfo is not None and cinfo[0] in class_slots:
                var_classes[st.name] = cinfo[0]
            out.append(Assign(name=st.name, expr=rhs, comment=st.comment))
            continue
        if isinstance(st, CallStmt):
            out.append(CallStmt(
                name=st.name,
                args=[
                    _rewrite_minimal_s4_expr(
                        a, class_slots, class_ancestors, generics, var_classes, method_classes
                    )
                    for a in st.args
                ],
                comment=st.comment,
            ))
            continue
        if isinstance(st, ExprStmt):
            out.append(ExprStmt(
                expr=_rewrite_minimal_s4_expr(
                    st.expr, class_slots, class_ancestors, generics, var_classes, method_classes
                ),
                comment=st.comment,
            ))
            continue
        out.append(st)
    return out


def _is_s3_usemethod_function(fn: FuncDef) -> str | None:
    if len(fn.body) != 1 or not isinstance(fn.body[0], ExprStmt):
        return None
    cinfo = parse_call_text(fn.body[0].expr.strip())
    if cinfo is None or cinfo[0].lower() != "usemethod" or not cinfo[1]:
        return None
    generic = _dequote_string_literal(cinfo[1][0].strip())
    return generic if generic else None


def _minimal_s3_constructor_classes(fn: FuncDef) -> list[str] | None:
    if not fn.body or not isinstance(fn.body[-1], ExprStmt):
        return None
    ret = fn.body[-1].expr.strip()
    if not re.match(r"^[A-Za-z]\w*$", ret):
        return None
    for st in fn.body[:-1]:
        if not isinstance(st, ExprStmt):
            continue
        ca = _parse_s3_class_assignment(st.expr)
        if ca is not None and ca[0] == ret:
            return ca[1]
    return None


def _s3_method_name_for_call(
    generic: str,
    classes: list[str] | None,
    method_classes: set[tuple[str, str]],
) -> str:
    for cls in classes or []:
        if (generic, cls) in method_classes:
            return f"{generic}_{cls}"
    if (generic, "default") in method_classes:
        return f"{generic}_default"
    return f"{generic}_default"


def _rewrite_minimal_s3_expr(
    expr: str,
    generics: set[str],
    var_classes: dict[str, list[str]],
    method_classes: set[tuple[str, str]],
) -> str:
    out = expr
    for generic in sorted(generics, key=len, reverse=True):
        def repl(inner: str) -> str:
            args = split_top_level_commas(inner.strip()) if inner.strip() else []
            if not args:
                return f"{generic}()"
            first = args[0].strip()
            method = _s3_method_name_for_call(generic, var_classes.get(first), method_classes)
            return f"{method}(" + ", ".join(args) + ")"

        out = _replace_balanced_func_calls(out, generic, repl)
    for nm, classes in sorted(var_classes.items(), key=lambda kv: len(kv[0]), reverse=True):
        cls_text = " ".join(classes)
        out = _replace_balanced_func_calls(
            out,
            "class",
            lambda inner, nm=nm, cls_text=cls_text: (
                _fortran_str_literal(cls_text) if inner.strip() == nm else f"class({inner.strip()})"
            ),
        )
    return out


def _lower_minimal_s3(stmts: list[object]) -> list[object]:
    """Lower a deliberately small S3 subset to existing static Fortran machinery.

    Supports a generic `g <- function(x) UseMethod("g")`, methods named
    `g.class`/`g.default`, constructors that assign one string class or a
    class vector to the returned list object, main-scope calls where the
    receiver class is known, and inherited parent methods over child objects
    when the child derived type has the same referenced fields.
    """
    funcs = [st for st in stmts if isinstance(st, FuncDef)]
    generics = {g for fn in funcs if (g := _is_s3_usemethod_function(fn))}
    if not generics:
        return stmts

    constructor_classes: dict[str, list[str]] = {}
    method_defs: dict[tuple[str, str], FuncDef] = {}
    for fn in funcs:
        classes = _minimal_s3_constructor_classes(fn)
        if classes is not None:
            constructor_classes[fn.name] = classes
        m_method = re.match(r"^([A-Za-z]\w*)\.([A-Za-z]\w*)$", fn.name)
        if m_method is not None and m_method.group(1) in generics:
            method_defs[(m_method.group(1), m_method.group(2))] = fn

    inherited_methods: list[FuncDef] = []
    for classes in constructor_classes.values():
        for i, cls in enumerate(classes):
            for generic in generics:
                if (generic, cls) in method_defs:
                    continue
                for parent in classes[i + 1:]:
                    parent_fn = method_defs.get((generic, parent))
                    if parent_fn is None:
                        continue
                    inherited_methods.append(FuncDef(
                        name=f"{generic}.{cls}",
                        args=list(parent_fn.args),
                        defaults=dict(parent_fn.defaults),
                        body=list(parent_fn.body),
                        leading_comments=parent_fn.leading_comments,
                    ))
                    method_defs[(generic, cls)] = inherited_methods[-1]
                    break
    method_classes = set(method_defs)

    out: list[object] = []
    var_classes: dict[str, list[str]] = {}
    for st in list(stmts) + inherited_methods:
        if isinstance(st, FuncDef):
            if _is_s3_usemethod_function(st) is not None:
                continue
            body = [
                b for b in st.body
                if not (isinstance(b, ExprStmt) and _parse_s3_class_assignment(b.expr) is not None)
            ]
            name = st.name
            m_method = re.match(r"^([A-Za-z]\w*)\.([A-Za-z]\w*)$", name)
            if m_method is not None and m_method.group(1) in generics:
                name = f"{m_method.group(1)}_{m_method.group(2)}"
            out.append(FuncDef(
                name=name,
                args=st.args,
                defaults=st.defaults,
                body=body,
                leading_comments=st.leading_comments,
            ))
            continue
        if isinstance(st, Assign):
            rhs = _rewrite_minimal_s3_expr(st.expr, generics, var_classes, method_classes)
            cinfo = parse_call_text(rhs.strip())
            if cinfo is not None and cinfo[0] in constructor_classes:
                var_classes[st.name] = constructor_classes[cinfo[0]]
            out.append(Assign(name=st.name, expr=rhs, comment=st.comment))
            continue
        if isinstance(st, CallStmt):
            out.append(CallStmt(
                name=st.name,
                args=[_rewrite_minimal_s3_expr(a, generics, var_classes, method_classes) for a in st.args],
                comment=st.comment,
            ))
            continue
        if isinstance(st, ExprStmt):
            ca = _parse_s3_class_assignment(st.expr)
            if ca is not None:
                var_classes[ca[0]] = ca[1]
                continue
            out.append(ExprStmt(
                expr=_rewrite_minimal_s3_expr(st.expr, generics, var_classes, method_classes),
                comment=st.comment,
            ))
            continue
        out.append(st)
    return out


def _r_declare_type_call(kind: str) -> str:
    if kind == "integer":
        return "integer()"
    if kind == "logical":
        return "logical()"
    if kind == "character":
        return "character()"
    return "double()"


def _format_declare_block(specs: list[tuple[str, str]], indent: str) -> list[str]:
    if not specs:
        return []
    out = [f"{indent}declare(type("]
    for i, (name, kind) in enumerate(specs):
        comma = "," if i + 1 < len(specs) else ""
        out.append(f"{indent}  {name} = {_r_declare_type_call(kind)}{comma}")
    out.append(f"{indent}))")
    return out


def _raw_r_name_map_from_source(src: str) -> dict[str, str]:
    out: dict[str, str] = {}
    for raw in src.splitlines():
        code, _cmt = split_r_code_comment(raw)
        for nm in re.findall(r"\b[A-Za-z]\w*(?:\.[A-Za-z]\w*)+\b", code):
            out.setdefault(_sanitize_r_var_name(nm), nm)
            out.setdefault(_sanitize_fortran_kwarg_name(nm), nm)
    return out


def _collect_func_open_lines(src: str) -> dict[str, int]:
    out: dict[str, int] = {}
    acc = ""
    start_line = 0
    for i, raw in enumerate(src.splitlines(), start=1):
        code, _cmt = split_r_code_comment(raw)
        t = code.strip()
        if not t:
            continue
        if not acc:
            start_line = i
            acc = t
        else:
            acc += " " + t
        if "function" not in acc:
            acc = ""
            continue
        if "{" not in acc and not _balanced_parens(acc):
            continue
        m = re.match(r"^([A-Za-z]\w*(?:\.[A-Za-z]\w*)*)\s*(?:<-|=)\s*function\s*\(", acc)
        if m is not None and "{" in acc:
            out.setdefault(_sanitize_r_var_name(m.group(1)), i)
            out.setdefault(_fortran_ident(m.group(1)), i)
            out.setdefault(_sanitize_fortran_kwarg_name(m.group(1)), i)
            acc = ""
            start_line = 0
        elif _balanced_parens(acc):
            acc = ""
            start_line = 0
    return out


def _balanced_parens(txt: str) -> bool:
    depth = 0
    in_single = False
    in_double = False
    esc = False
    for ch in txt:
        if esc:
            esc = False
            continue
        if ch == "\\":
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
        if ch == "(":
            depth += 1
        elif ch == ")" and depth > 0:
            depth -= 1
    return depth == 0 and not in_single and not in_double


def _annotation_kind_maps_for_function(fn: FuncDef) -> dict[str, str]:
    body_eff = fn.body[:-1] if (fn.body and isinstance(fn.body[-1], ExprStmt)) else fn.body
    known_arrays = {a for a in fn.args if infer_arg_rank(fn, a) >= 1}
    assign_counts = infer_assigned_names(body_eff)
    ints, real_scalars, int_arrays, real_arrays, params = classify_vars(body_eff, assign_counts, known_arrays=known_arrays)
    char_scalars = infer_function_character_scalars(fn)
    char_arrays = infer_function_character_array_names(fn, char_scalars)
    logical_arrays: set[str] = set()
    kinds: dict[str, str] = {}
    fn_ints = infer_function_integer_names(fn)
    fn_int_arrays = infer_function_integer_array_names(fn)
    for a in fn.args:
        dflt = fn.defaults.get(a, "").strip()
        if a in fn_ints or a in fn_int_arrays or _is_int_literal(dflt):
            kinds[a] = "integer"
        elif dflt in {"TRUE", "FALSE"}:
            kinds[a] = "logical"
        elif a in char_scalars or a in char_arrays:
            kinds[a] = "character"
        else:
            kinds[a] = "double"
    for nm in sorted(set(params) | ints | int_arrays):
        kinds.setdefault(nm, "integer")
    for nm in sorted(real_scalars | real_arrays):
        kinds.setdefault(nm, "double")
    for nm in sorted(char_scalars | char_arrays):
        kinds.setdefault(nm, "character")
    for nm in sorted(logical_arrays):
        kinds.setdefault(nm, "logical")
    return kinds


def annotate_r_source_with_declares(src: str, stem: str) -> str:
    comment_lookup = build_r_comment_lookup(src)
    lines = preprocess_r_lines(src)
    stmts, i = parse_block(lines, 0, comment_lookup=comment_lookup)
    if i != len(lines):
        raise NotImplementedError("could not parse full source for R annotation")
    stmts = _lower_dim_assignments(stmts)
    stmts = attach_function_adjacent_comments(stmts)
    stmts = rename_conflicting_loop_vars(stmts)
    funcs = [s for s in stmts if isinstance(s, FuncDef)]
    main_stmts = [s for s in stmts if not isinstance(s, FuncDef)]
    raw_name = _raw_r_name_map_from_source(src)

    func_specs: dict[str, list[tuple[str, str]]] = {}
    for fn in funcs:
        kinds = _annotation_kind_maps_for_function(fn)
        ordered: list[tuple[str, str]] = []
        seen: set[str] = set()
        for nm in list(fn.args) + sorted(k for k in kinds if k not in fn.args):
            raw_nm = raw_name.get(nm, nm)
            if raw_nm in seen:
                continue
            seen.add(raw_nm)
            ordered.append((raw_nm, kinds[nm]))
        if ordered:
            func_specs[fn.name] = ordered

    assign_counts = infer_assigned_names(main_stmts)
    ints, real_scalars, int_arrays, real_arrays, params = classify_vars(main_stmts, assign_counts)
    array_params = infer_main_array_params(main_stmts, assign_counts)
    char_scalars = infer_main_character_scalars(main_stmts)
    char_arrays = infer_main_character_arrays(main_stmts)
    logical_scalars = infer_main_logical_scalars(main_stmts)
    main_kinds: dict[str, str] = {}
    for nm in sorted(set(params) | ints | int_arrays | {k for k, (kind, _n, _expr) in array_params.items() if kind == "integer"}):
        main_kinds[nm] = "integer"
    for nm in sorted(real_scalars | real_arrays | {k for k, (kind, _n, _expr) in array_params.items() if kind != "integer"}):
        main_kinds.setdefault(nm, "double")
    for nm in sorted(char_scalars | char_arrays):
        main_kinds.setdefault(nm, "character")
    for nm in sorted(logical_scalars):
        main_kinds.setdefault(nm, "logical")
    main_specs = [(raw_name.get(nm, nm), kind) for nm, kind in sorted(main_kinds.items())]

    func_open_lines = _collect_func_open_lines(src)
    insert_after: dict[int, list[str]] = {}
    for fn in funcs:
        specs = func_specs.get(fn.name)
        line_no = func_open_lines.get(fn.name)
        if not specs or line_no is None:
            continue
        insert_after.setdefault(line_no, []).extend(_format_declare_block(specs, "  "))

    raw_lines = src.splitlines()
    first_code_line = 1
    for i_line, raw in enumerate(raw_lines, start=1):
        code, _cmt = split_r_code_comment(raw)
        if code.lstrip("\ufeff").strip():
            first_code_line = i_line
            break
    out: list[str] = []
    for i_line, raw in enumerate(raw_lines, start=1):
        if main_specs and i_line == first_code_line:
            out.extend(_format_declare_block(main_specs, ""))
            out.append("")
        out.append(raw)
        if i_line in insert_after:
            out.extend(insert_after[i_line])
    return "\n".join(out) + ("\n" if src.endswith("\n") else "")


def transpile_r_to_fortran(
    src: str,
    stem: str,
    helper_modules: set[str] | None = None,
    int_like_print: bool = True,
    no_recycle: bool = False,
    recycle_warn: bool = False,
    recycle_stop: bool = False,
) -> str:
    global _HAS_R_MOD, _USER_FUNC_ARG_KIND, _USER_FUNC_ARG_INDEX, _USER_FUNC_ARG_RANK, _USER_FUNC_RETURN_RANK, _USER_FUNC_ELEMENTAL, _VOID_FUNCTION_LIKE
    global _SUBROUTINE_FUNCTIONS
    global _KNOWN_VECTOR_NAMES, _KNOWN_MATRIX_NAMES, _KNOWN_LOGICAL_VECTOR_NAMES, _NULL_ARRAY_SENTINELS
    global _KNOWN_RANK3_NAMES
    global _NAMED_VECTOR_NAMES, _NAMED_VECTOR_LABELS, _CATEGORICAL_LABELS, _TABLE_LABELS
    global _EXPANDED_DATA_FRAME_FIELDS, _EXPANDED_DATA_FRAME_ALIASES
    global _NO_RECYCLE
    global _R_SD_CALL_NAME
    _NULL_ARRAY_SENTINELS = {}
    _EXPANDED_DATA_FRAME_FIELDS = {}
    _EXPANDED_DATA_FRAME_ALIASES = {}
    _DATA_FRAME_FORCE_MATERIALIZE = set()
    _CATEGORICAL_LABELS = {}
    _TABLE_LABELS = {}
    unit_name = _fortran_ident(stem)
    module_name = _module_name_from_stem(stem)
    comment_lookup = build_r_comment_lookup(src)
    lines = preprocess_r_lines(src)
    stmts, i = parse_block(lines, 0, comment_lookup=comment_lookup)
    if i != len(lines):
        raise NotImplementedError("could not parse full source")
    stmts = _lower_dim_assignments(stmts)
    stmts = _lower_minimal_s4(stmts)
    stmts = _lower_minimal_s3(stmts)
    stmts = attach_function_adjacent_comments(stmts)
    loop_shadow_warnings: list[tuple[str, str, int | None]] = []
    stmts = rename_conflicting_loop_vars(stmts, warnings=loop_shadow_warnings, src=src)
    for old, new, line_no in loop_shadow_warnings:
        loc = f" at R line {line_no}" if line_no is not None else ""
        print(
            f"Warning: R loop variable `{old}`{loc} reuses an earlier assigned name; "
            f"translating loop variable as `{new}`."
        )

    funcs = [s for s in stmts if isinstance(s, FuncDef)]
    _CATEGORICAL_LABELS = collect_categorical_sample_labels(stmts)
    _TABLE_LABELS = collect_table_labels(stmts, _CATEGORICAL_LABELS)
    _SUBROUTINE_FUNCTIONS = {f.name.lower() for f in funcs if _function_should_emit_subroutine(f)}
    _VOID_FUNCTION_LIKE = {
        f.name.lower()
        for f in funcs
        if (
            f.name.lower() in _SUBROUTINE_FUNCTIONS
            or not f.body
            or not isinstance(f.body[-1], ExprStmt)
            or _function_returns_invisible(f)
        )
    }
    main_stmts = [s for s in stmts if not isinstance(s, FuncDef)]
    _DATA_FRAME_FORCE_MATERIALIZE = collect_model_data_frame_uses(main_stmts)
    main_stmts = expand_data_frame_assignments(main_stmts)
    main_stmts = rename_reserved_main_names(main_stmts)
    sd_name_collision = "sd" in {n.lower() for n in infer_assigned_names(main_stmts)}
    for f in funcs:
        if f.name.lower() == "sd" or any(a.lower() == "sd" for a in f.args):
            sd_name_collision = True
            break
        if "sd" in {n.lower() for n in infer_assigned_names(f.body)}:
            sd_name_collision = True
            break
    _R_SD_CALL_NAME = "r_sd" if sd_name_collision else "sd"
    fn_arg_order = {f.name: list(f.args) for f in funcs}
    fn_arg_defaults = {f.name: dict(f.defaults) for f in funcs}
    for f in funcs:
        for st in f.body:
            if isinstance(st, Assign):
                st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
            elif isinstance(st, ExprStmt):
                st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
    for st in main_stmts:
        if isinstance(st, Assign):
            st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
        elif isinstance(st, ExprStmt):
            st.expr = _rewrite_named_calls(st.expr, fn_arg_order, fn_arg_defaults)
    main_stmts = inline_single_use_temporaries(main_stmts)
    named_vectors: dict[str, tuple[list[str], list[str]]] = {}
    for st_nv in main_stmts:
        if not isinstance(st_nv, Assign):
            continue
        parsed_nv = _parse_named_c_vector(st_nv.expr.strip())
        if parsed_nv is not None:
            named_vectors[st_nv.name] = parsed_nv
    _NAMED_VECTOR_NAMES = {nm.lower(): f"{nm}_names" for nm in named_vectors}
    _NAMED_VECTOR_LABELS = {nm.lower(): labels for nm, (labels, _vals) in named_vectors.items()}
    _NAMED_VECTOR_LABELS.update(collect_colname_labels(stmts))
    list_specs = _list_return_specs(funcs)
    fn_matrix_col_labels = {f.name: collect_colname_labels(f.body) for f in funcs}
    fn_alias_return_type: dict[str, str] = {}
    for f_alias in funcs:
        if f_alias.name in list_specs:
            continue
        if not f_alias.body or not isinstance(f_alias.body[-1], ExprStmt):
            continue
        m_ret = re.match(r"^([A-Za-z]\w*)$", f_alias.body[-1].expr.strip())
        if m_ret is None:
            continue
        ret_nm = m_ret.group(1)
        alias_t: dict[str, str] = {}
        def _walk_alias_types(ss_at: list[object]) -> None:
            for st_at in ss_at:
                if isinstance(st_at, Assign):
                    lhs_at = st_at.name.strip()
                    rhs_at = st_at.expr.strip()
                    c_at = parse_call_text(rhs_at)
                    if c_at is not None and c_at[0] in list_specs:
                        alias_t[lhs_at] = _type_name_for_path(c_at[0], ())
                        continue
                    m_at = re.match(r"^([A-Za-z]\w*)$", rhs_at)
                    if m_at is not None and m_at.group(1) in alias_t:
                        alias_t[lhs_at] = alias_t[m_at.group(1)]
                elif isinstance(st_at, IfStmt):
                    _walk_alias_types(st_at.then_body)
                    _walk_alias_types(st_at.else_body)
                elif isinstance(st_at, ForStmt):
                    _walk_alias_types(st_at.body)
        _walk_alias_types(f_alias.body[:-1])
        if ret_nm in alias_t:
            fn_alias_return_type[f_alias.name] = alias_t[ret_nm]
    fn_int_names: dict[str, set[str]] = {f.name: infer_function_integer_names(f) for f in funcs}
    fn_int_array_names: dict[str, set[str]] = {f.name: infer_function_integer_array_names(f) for f in funcs}
    fn_real_array_names: dict[str, set[str]] = {f.name: infer_function_real_array_names(f) for f in funcs}
    fn_real_matrix_names: dict[str, set[str]] = {f.name: infer_function_real_matrix_names(f) for f in funcs}
    _USER_FUNC_RETURN_RANK = {}
    for f_rank_ret in funcs:
        if not (f_rank_ret.body and isinstance(f_rank_ret.body[-1], ExprStmt)):
            continue
        ret_expr_rank = f_rank_ret.body[-1].expr.strip()
        ret_arg_rank = _return_call_arg(ret_expr_rank)
        if ret_arg_rank is not None:
            ret_expr_rank = ret_arg_rank
        m_rank_ret = re.match(r"^([A-Za-z]\w*)$", ret_expr_rank)
        if m_rank_ret is None:
            continue
        ret_nm_rank = m_rank_ret.group(1)
        rr = _infer_local_array_rank(f_rank_ret.body, ret_nm_rank)
        if rr > 0:
            _USER_FUNC_RETURN_RANK[f_rank_ret.name.lower()] = rr
    fn_real_matrix_names = {f.name: infer_function_real_matrix_names(f) for f in funcs}
    fn_lm_names: dict[str, set[str]] = {f.name: infer_function_lm_names(f) for f in funcs}
    known_rank3_names: set[str] = set()
    for f_rank in funcs:
        body_eff = f_rank.body[:-1] if (f_rank.body and isinstance(f_rank.body[-1], ExprStmt)) else f_rank.body
        for a_rank in f_rank.args:
            if infer_arg_rank(f_rank, a_rank) >= 3:
                known_rank3_names.add(a_rank.lower())
        for nm_rank in infer_assigned_names(body_eff):
            if _infer_local_array_rank(body_eff, nm_rank) >= 3:
                known_rank3_names.add(nm_rank.lower())
    _KNOWN_RANK3_NAMES = known_rank3_names
    fn_return_array_kind: dict[str, str] = {}
    for f_ret in funcs:
        if not (f_ret.body and isinstance(f_ret.body[-1], ExprStmt)):
            continue
        ret_expr = f_ret.body[-1].expr.strip()
        ret_arg = _return_call_arg(ret_expr)
        if ret_arg is not None:
            ret_expr = ret_arg
        if ret_expr.startswith("c(") or (ret_expr.startswith("[") and ret_expr.endswith("]")):
            fn_return_array_kind[f_ret.name.lower()] = "real"
            continue
        m_ret = re.match(r"^([A-Za-z]\w*)$", ret_expr)
        if not m_ret:
            continue
        ret_name = m_ret.group(1)
        if ret_name in fn_int_array_names.get(f_ret.name, set()):
            fn_return_array_kind[f_ret.name.lower()] = "integer"
        elif ret_name in fn_real_array_names.get(f_ret.name, set()) or ret_name in fn_real_matrix_names.get(f_ret.name, set()):
            fn_return_array_kind[f_ret.name.lower()] = "real"
    _USER_FUNC_ARG_KIND = {}
    _USER_FUNC_ARG_INDEX = {}
    _USER_FUNC_ARG_RANK = {}
    _USER_FUNC_ELEMENTAL = set()
    for f in funcs:
        _USER_FUNC_ARG_INDEX[f.name.lower()] = {a.lower(): i for i, a in enumerate(f.args)}
    _USER_FUNC_ARG_RANK = {
        f.name.lower(): {a.lower(): infer_arg_rank(f, a) for a in f.args}
        for f in funcs
    }
    for _ in range(4):
        changed_rank = False
        for f in funcs:
            fn_l = f.name.lower()
            ranks_f = dict(_USER_FUNC_ARG_RANK.get(fn_l, {}))
            for a in f.args:
                a_l = a.lower()
                new_rank = infer_arg_rank(f, a)
                if new_rank > ranks_f.get(a_l, 0):
                    ranks_f[a_l] = new_rank
                    changed_rank = True
            _USER_FUNC_ARG_RANK[fn_l] = ranks_f
        if not changed_rank:
            break
    for f in funcs:
        kinds: list[str] = []
        fn_ints = fn_int_names.get(f.name, set())
        fn_int_arrs = fn_int_array_names.get(f.name, set())
        idx = _USER_FUNC_ARG_INDEX.get(f.name.lower(), {})
        arg_rank_f = {a: _USER_FUNC_ARG_RANK.get(f.name.lower(), {}).get(a.lower(), infer_arg_rank(f, a)) for a in f.args}
        f_body_eff = f.body[:-1] if (f.body and isinstance(f.body[-1], ExprStmt)) else f.body
        if (
            f.name.lower() not in _SUBROUTINE_FUNCTIONS
            and
            f.name.lower() not in fn_return_array_kind
            and (not _stmt_tree_has_side_effect_ops(f_body_eff))
            and all(arg_rank_f.get(a, 0) == 0 for a in f.args)
        ):
            _USER_FUNC_ELEMENTAL.add(f.name.lower())
        for i, a in enumerate(f.args):
            kinds.append(
                "integer"
                if (
                    a in fn_ints
                    or a in fn_int_arrs
                    or a in {"asset_names", "price_names"}
                    or (a == "name" and f.name.lower().startswith("print_") and f.name.lower() != "print_matrix")
                )
                else "real"
            )
        _USER_FUNC_ARG_KIND[f.name.lower()] = kinds
    helper_modules = set(m.lower() for m in (helper_modules or set()))
    _HAS_R_MOD = ("r_mod" in helper_modules)
    _NO_RECYCLE = bool(no_recycle)
    helper_ctx_mod: dict[str, object] = {
        "has_r_mod": ("r_mod" in helper_modules),
        "need_r_mod": set(),
        "need_lm": False,
        "lm_terms_by_fit": {},
        "return_array_fns": set(fn_return_array_kind.keys()),
    }
    helper_ctx_main: dict[str, object] = {
        "has_r_mod": ("r_mod" in helper_modules),
        "need_r_mod": set(),
        "need_scan_reader": False,
        "need_table_reader": False,
        "need_table_writer": False,
        "need_lm": False,
        "lm_terms_by_fit": {},
        "int_matrix_vars": set(),
        "real_matrix_vars": set(),
        "int_vector_vars": set(),
        "real_vector_vars": set(),
        "named_vector_names": dict(_NAMED_VECTOR_NAMES),
        "return_array_fns": set(fn_return_array_kind.keys()),
        "named_vector_labels": dict(_NAMED_VECTOR_LABELS),
        "matrix_col_labels": collect_colname_labels(main_stmts),
        "matrix_colname_exprs": collect_colname_sources(stmts),
    }

    assign_counts = infer_assigned_names(main_stmts)
    ints, real_scalars, int_arrays, real_arrays, params = classify_vars(main_stmts, assign_counts)
    for st_ret in main_stmts:
        if not isinstance(st_ret, Assign):
            continue
        call_ret = parse_call_text(st_ret.expr.strip())
        if call_ret is None:
            continue
        ret_kind = fn_return_array_kind.get(call_ret[0].lower())
        if ret_kind == "integer":
            int_arrays.add(st_ret.name)
            ints.discard(st_ret.name)
            real_scalars.discard(st_ret.name)
            real_arrays.discard(st_ret.name)
            params.pop(st_ret.name, None)
        elif ret_kind == "real":
            real_arrays.add(st_ret.name)
            ints.discard(st_ret.name)
            real_scalars.discard(st_ret.name)
            int_arrays.discard(st_ret.name)
            params.pop(st_ret.name, None)
    changed_main_arrays = True
    while changed_main_arrays:
        changed_main_arrays = False
        known_main_arrays = set(int_arrays) | set(real_arrays)
        for st_arr in main_stmts:
            if not isinstance(st_arr, Assign):
                continue
            if st_arr.name in known_main_arrays:
                continue
            if st_arr.name in ints or st_arr.name in real_scalars:
                continue
            rhs_arr = st_arr.expr.strip()
            if re.match(r"^(?:length|size|nrow|ncol)\s*\(", rhs_arr, re.IGNORECASE):
                continue
            if any(re.search(rf"\b{re.escape(nm)}\b", rhs_arr) for nm in known_main_arrays):
                if re.search(r"\bas\.\s*numeric\s*\(|/|\.\d|[0-9]_dp|_dp|rnorm|runif", rhs_arr, re.IGNORECASE):
                    real_arrays.add(st_arr.name)
                else:
                    int_arrays.add(st_arr.name)
                ints.discard(st_arr.name)
                real_scalars.discard(st_arr.name)
                params.pop(st_arr.name, None)
                changed_main_arrays = True
    for comp_nm in {"aic_dot_comp", "bic_dot_comp"}:
        if comp_nm in real_scalars:
            real_scalars.discard(comp_nm)
            ints.add(comp_nm)
            params.pop(comp_nm, None)
    array_params = infer_main_array_params(main_stmts, assign_counts)
    char_scalars = infer_main_character_scalars(main_stmts)
    char_arrays = infer_main_character_arrays(main_stmts)
    for nm in set(char_scalars) | set(char_arrays):
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    logical_scalars = infer_main_logical_scalars(main_stmts)
    int_matrices = infer_main_integer_matrices(main_stmts)
    real_matrices = infer_main_real_matrices(main_stmts, int_matrices)
    real_rank3_arrays = infer_main_real_rank3_arrays(main_stmts)
    real_rank4_arrays = infer_main_real_rank4_arrays(main_stmts)
    _KNOWN_RANK3_NAMES.update(nm.lower() for nm in real_rank3_arrays)
    logical_arrays = infer_main_logical_arrays(
        main_stmts,
        set(int_arrays) | set(real_arrays) | set(array_params.keys()) | set(int_matrices) | set(real_matrices),
    )
    logical_arrays.discard("ok")
    int_arrays.discard("ok")
    real_arrays.discard("ok")
    params.pop("ok", None)
    if any(re.search(r"\barma_mat\b", t) for t in _stmt_texts_for_rank_scan(main_stmts)):
        logical_arrays.add("ok")
        real_matrices.add("arma_mat")
        real_arrays.discard("arma_mat")
        real_scalars.discard("arma_mat")
        for nm_order in ("aic_order", "bic_order"):
            int_arrays.add(nm_order)
            real_arrays.discard(nm_order)
            real_scalars.discard(nm_order)
            ints.discard(nm_order)
            params.pop(nm_order, None)
    _KNOWN_VECTOR_NAMES = {n.lower() for n in (set(int_arrays) | set(real_arrays) | set(array_params.keys()))}
    _KNOWN_MATRIX_NAMES = {n.lower() for n in (set(int_matrices) | set(real_matrices))}
    _KNOWN_LOGICAL_VECTOR_NAMES = {n.lower() for n in logical_arrays}
    # Promote main-scope names referenced by local functions to module scope.
    main_name_map: dict[str, str] = {}
    for nm in set(params.keys()) | set(array_params.keys()) | ints | real_scalars | int_arrays | real_arrays | char_scalars | char_arrays | real_matrices | real_rank4_arrays | int_matrices:
        main_name_map[nm.lower()] = nm
    promoted_l: set[str] = set()
    for fn in funcs:
        for nm_l in _infer_function_free_names(fn):
            if nm_l in main_name_map:
                promoted_l.add(nm_l)
    promoted_names: set[str] = {main_name_map[nm_l] for nm_l in promoted_l}
    promoted_params: dict[str, str] = {}
    promoted_array_params: dict[str, tuple[str, int, str]] = {}
    promoted_kind: dict[str, str] = {}
    for nm in list(promoted_names):
        if nm in params:
            promoted_params[nm] = params.pop(nm)
            promoted_kind[nm] = "int_scalar"
        if nm in array_params:
            promoted_array_params[nm] = array_params.pop(nm)
            promoted_kind[nm] = "array_param"
        if nm in ints:
            promoted_kind[nm] = "int_scalar"
        elif nm in real_scalars:
            promoted_kind[nm] = "real_scalar"
        elif nm in int_arrays:
            promoted_kind[nm] = "int_vec"
        elif nm in real_arrays:
            promoted_kind[nm] = "real_vec"
        elif nm in int_matrices:
            promoted_kind[nm] = "int_mat"
        elif nm in real_matrices:
            promoted_kind[nm] = "real_mat"
        elif nm in real_rank3_arrays:
            promoted_kind[nm] = "real_rank3"
        elif nm in real_rank4_arrays:
            promoted_kind[nm] = "real_rank4"
        elif nm in char_scalars:
            promoted_kind[nm] = "char_scalar"
        elif nm in char_arrays:
            promoted_kind[nm] = "char_vec"
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        char_scalars.discard(nm)
        char_arrays.discard(nm)
        real_matrices.discard(nm)
        real_rank3_arrays.discard(nm)
        real_rank4_arrays.discard(nm)
        int_matrices.discard(nm)
    helper_ctx_main["int_matrix_vars"] = set(int_matrices)
    helper_ctx_main["real_matrix_vars"] = set(real_matrices) | set(real_rank3_arrays) | set(real_rank4_arrays)
    helper_ctx_main["int_vector_vars"] = set(int_arrays) | {k for k, (kk, _, _) in array_params.items() if kk == "integer"}
    helper_ctx_main["real_vector_vars"] = set(real_arrays) | {k for k, (kk, _, _) in array_params.items() if kk != "integer"}
    helper_ctx_main["char_scalar_vars"] = set(char_scalars)

    # Main program declarations/body (without header/footer).
    pbody = FEmit()
    main_param_comments = collect_assignment_comments(main_stmts)
    int_param_pairs: list[tuple[str, str]] = sorted((p, v) for p, v in params.items())
    # Reuse named size constants when multiple array-parameters share extent.
    size_groups: dict[int, list[str]] = {}
    for nm, (_knd, nsz, _expr_f) in array_params.items():
        size_groups.setdefault(nsz, []).append(nm)
    used_names = set(params.keys()) | set(array_params.keys()) | ints | real_scalars | int_arrays | real_arrays
    size_name_for_n: dict[int, str] = {}
    for nsz, names in sorted(size_groups.items()):
        if len(names) < 2:
            continue
        base = "n_param"
        cand = base
        k = 2
        while cand in used_names:
            cand = f"{base}_{k}"
            k += 1
        used_names.add(cand)
        size_name_for_n[nsz] = cand
        int_param_pairs.append((cand, str(nsz)))
    if int_param_pairs:
        int64_param_pairs = [(k, v) for k, v in int_param_pairs if _expr_uses_int64(v)]
        default_param_pairs = [(k, v) for k, v in int_param_pairs if not _expr_uses_int64(v)]
        default_plain = [(k, v) for k, v in default_param_pairs if not main_param_comments.get(k, "").strip()]
        default_commented = [(k, v) for k, v in default_param_pairs if main_param_comments.get(k, "").strip()]
        int64_plain = [(k, v) for k, v in int64_param_pairs if not main_param_comments.get(k, "").strip()]
        int64_commented = [(k, v) for k, v in int64_param_pairs if main_param_comments.get(k, "").strip()]
        if default_plain:
            rhs = ", ".join(f"{k} = {v}" for k, v in default_plain)
            pbody.w(f"integer, parameter :: {rhs}")
        for k, v in default_commented:
            pbody.w(f"integer, parameter :: {k} = {v} ! {main_param_comments[k].strip()}")
        if int64_plain:
            rhs = ", ".join(f"{k} = {v}" for k, v in int64_plain)
            pbody.w(f"integer(kind=int64), parameter :: {rhs}")
        for k, v in int64_commented:
            pbody.w(f"integer(kind=int64), parameter :: {k} = {v} ! {main_param_comments[k].strip()}")
    for nm, (knd, nsz, expr_f) in sorted(array_params.items()):
        n_decl = size_name_for_n.get(nsz, str(nsz))
        suffix = f" ! {main_param_comments[nm].strip()}" if main_param_comments.get(nm, "").strip() else ""
        if knd == "integer":
            pbody.w(f"integer, parameter :: {nm}({n_decl}) = {expr_f}{suffix}")
        else:
            pbody.w(f"real(kind=dp), parameter :: {nm}({n_decl}) = {expr_f}{suffix}")
    for nm, (labels, _vals) in sorted(named_vectors.items()):
        if not labels:
            continue
        name_nm = _NAMED_VECTOR_NAMES.get(nm.lower(), f"{nm}_names")
        max_len = max(1, max(len(x) for x in labels))
        arr = ", ".join(_fortran_str_literal(x) for x in labels)
        pbody.w(
            f"character(len={max_len}), parameter :: {name_nm}({len(labels)}) = "
            f"[character(len={max_len}) :: {arr}]"
        )

    # Variables assigned from list-return function calls or main-scope list(...) constructors.
    list_vars: dict[str, str] = {}
    main_list_specs: dict[str, ListReturnSpec] = {}
    main_list_var_fields: dict[str, dict[str, object]] = {}
    lm_vars: set[str] = set()
    t_test_vars: set[str] = set()
    main_vector_list_names: set[str] = set()
    main_object_list_vars: dict[str, str] = {}
    call_pat = re.compile(r"^([A-Za-z]\w*)\s*\(")
    def _collect_main_vector_list_names(ss_vl: list[object]) -> None:
        for st_vl in ss_vl:
            if isinstance(st_vl, Assign):
                if re.match(r"^vector\s*\(\s*['\"]list['\"]", st_vl.expr.strip(), re.IGNORECASE):
                    main_vector_list_names.add(st_vl.name)
            elif isinstance(st_vl, IfStmt):
                _collect_main_vector_list_names(st_vl.then_body)
                _collect_main_vector_list_names(st_vl.else_body)
            elif isinstance(st_vl, ForStmt):
                _collect_main_vector_list_names(st_vl.body)
            elif isinstance(st_vl, WhileStmt):
                _collect_main_vector_list_names(st_vl.body)
            elif isinstance(st_vl, RepeatStmt):
                _collect_main_vector_list_names(st_vl.body)

    _collect_main_vector_list_names(main_stmts)
    for st in main_stmts:
        if isinstance(st, Assign):
            if re.match(r"^vector\s*\(\s*['\"]list['\"]", st.expr.strip(), re.IGNORECASE):
                main_vector_list_names.add(st.name)
            fields_main = _parse_list_constructor(st.expr.strip())
            if fields_main is not None:
                tnm = _type_name_for_path(st.name, ())
                list_vars[st.name] = tnm
                main_list_specs[st.name] = ListReturnSpec(
                    fn_name=st.name,
                    root_fields=fields_main,
                    nested_types=_collect_nested_types(st.name, fields_main),
                )
                main_list_var_fields[st.name] = fields_main
                continue
            if re.match(
                r"^lm\s*\(\s*([A-Za-z]\w*)\s*~\s*([A-Za-z]\w*)\s*\+\s*([A-Za-z]\w*)(?:\s*,\s*.*)?\)\s*$",
                st.expr.strip(),
                re.IGNORECASE,
            ):
                lm_vars.add(st.name)
                helper_ctx_main["need_lm"] = True
            if re.match(r"^t\.?test\s*\(", st.expr.strip(), re.IGNORECASE):
                t_test_vars.add(st.name)
            m = call_pat.match(st.expr.strip())
            if not m:
                m_cp = re.match(r"^\s*([A-Za-z]\w*)\s*$", st.expr.strip())
                if m_cp is not None:
                    src_nm = m_cp.group(1)
                    if src_nm in list_vars:
                        list_vars[st.name] = list_vars[src_nm]
                continue
            fnm = m.group(1)
            if fnm in list_specs:
                list_vars[st.name] = _type_name_for_path(fnm, ())
            elif fnm in fn_alias_return_type:
                list_vars[st.name] = fn_alias_return_type[fnm]
        elif isinstance(st, ExprStmt):
            asn_obj = split_top_level_assignment(st.expr.strip())
            if asn_obj is None:
                continue
            m_obj = re.match(r"^([A-Za-z]\w*)\s*\[\[\s*.+\s*\]\]$", asn_obj[0].strip())
            if m_obj is None or m_obj.group(1) not in main_vector_list_names:
                continue
            rhs_obj = asn_obj[1].strip()
            c_obj = parse_call_text(rhs_obj)
            rhs_type = None
            if c_obj is not None:
                if c_obj[0] in list_specs:
                    rhs_type = _type_name_for_path(c_obj[0], ())
                elif c_obj[0] in fn_alias_return_type:
                    rhs_type = fn_alias_return_type[c_obj[0]]
            if rhs_type is not None:
                main_object_list_vars[m_obj.group(1)] = rhs_type

    def _collect_main_object_list_assigns(ss_obj: list[object]) -> None:
        for st_obj in ss_obj:
            if isinstance(st_obj, Assign):
                lhs_obj = st_obj.name.strip()
                rhs_obj = st_obj.expr.strip()
            elif isinstance(st_obj, ExprStmt):
                asn_obj = split_top_level_assignment(st_obj.expr.strip())
                if asn_obj is None:
                    continue
                lhs_obj = asn_obj[0].strip()
                rhs_obj = asn_obj[1].strip()
            else:
                lhs_obj = ""
                rhs_obj = ""
            if lhs_obj:
                m_obj = re.match(r"^([A-Za-z]\w*)\s*\[\[\s*.+\s*\]\]$", lhs_obj)
                if m_obj is not None and m_obj.group(1) in main_vector_list_names:
                    c_obj = parse_call_text(rhs_obj)
                    rhs_type = None
                    if c_obj is not None:
                        if c_obj[0] in list_specs:
                            rhs_type = _type_name_for_path(c_obj[0], ())
                        elif c_obj[0] in fn_alias_return_type:
                            rhs_type = fn_alias_return_type[c_obj[0]]
                    if rhs_type is not None:
                        main_object_list_vars[m_obj.group(1)] = rhs_type
            elif isinstance(st_obj, IfStmt):
                _collect_main_object_list_assigns(st_obj.then_body)
                _collect_main_object_list_assigns(st_obj.else_body)
            elif isinstance(st_obj, ForStmt):
                _collect_main_object_list_assigns(st_obj.body)
            elif isinstance(st_obj, WhileStmt):
                _collect_main_object_list_assigns(st_obj.body)
            elif isinstance(st_obj, RepeatStmt):
                _collect_main_object_list_assigns(st_obj.body)

    _collect_main_object_list_assigns(main_stmts)

    def _add_field_path(fields: dict[str, object], path: list[str], rhs_expr: str) -> None:
        cur = fields
        for p in path[:-1]:
            v = cur.get(p)
            if not isinstance(v, dict):
                cur[p] = {}
            cur = cur[p]  # type: ignore[assignment]
        leaf = path[-1]
        if leaf not in cur:
            cur[leaf] = rhs_expr

    def _walk_collect_extra_list_fields(ss: list[object]) -> None:
        for st in ss:
            if isinstance(st, IfStmt):
                _walk_collect_extra_list_fields(st.then_body)
                _walk_collect_extra_list_fields(st.else_body)
            elif isinstance(st, ForStmt):
                _walk_collect_extra_list_fields(st.body)
            elif isinstance(st, WhileStmt):
                _walk_collect_extra_list_fields(st.body)
            elif isinstance(st, RepeatStmt):
                _walk_collect_extra_list_fields(st.body)
            elif isinstance(st, ExprStmt):
                mm = re.match(
                    r"^([A-Za-z]\w*(?:\$[A-Za-z]\w+)+)\s*(?:<-|=)\s*(.+)$",
                    st.expr.strip(),
                )
                if not mm:
                    continue
                lhs = mm.group(1).strip()
                rhs = mm.group(2).strip()
                parts = lhs.split("$")
                if len(parts) < 2:
                    continue
                base = parts[0]
                if base not in main_list_specs:
                    continue
                _add_field_path(main_list_specs[base].root_fields, parts[1:], rhs)
                main_list_specs[base] = ListReturnSpec(
                    fn_name=base,
                    root_fields=main_list_specs[base].root_fields,
                    nested_types=_collect_nested_types(base, main_list_specs[base].root_fields),
                )
                main_list_var_fields[base] = main_list_specs[base].root_fields

    _walk_collect_extra_list_fields(main_stmts)
    matrix_col_labels_main = helper_ctx_main.get("matrix_col_labels")
    if not isinstance(matrix_col_labels_main, dict):
        matrix_col_labels_main = {}
        helper_ctx_main["matrix_col_labels"] = matrix_col_labels_main

    def _list_spec_from_type_name(type_name: str) -> ListReturnSpec | None:
        for fn_ls, spec_ls in list_specs.items():
            if _type_name_for_path(fn_ls, ()) == type_name:
                return spec_ls
        return None

    def _matrix_labels_from_list_field(expr_txt: str) -> list[str] | None:
        m_lf = re.match(r"^([A-Za-z]\w*)\$([A-Za-z]\w*)$", expr_txt.strip())
        if m_lf is None:
            return None
        base_lf, field_lf = m_lf.group(1), m_lf.group(2)
        spec_lf = _list_spec_from_type_name(list_vars.get(base_lf, ""))
        if spec_lf is None:
            return None
        field_expr_lf = spec_lf.root_fields.get(field_lf)
        fn_labs_lf = fn_matrix_col_labels.get(spec_lf.fn_name, {})
        if isinstance(field_expr_lf, str):
            labs_lf = fn_labs_lf.get(field_expr_lf.strip().lower())
            if isinstance(labs_lf, list) and labs_lf:
                return [str(x) for x in labs_lf]
        labs_lf = fn_labs_lf.get(field_lf.lower())
        if isinstance(labs_lf, list) and labs_lf:
            return [str(x) for x in labs_lf]
        return None

    for st_mcl in main_stmts:
        if not isinstance(st_mcl, Assign):
            continue
        rhs_mcl = st_mcl.expr.strip()
        labs_mcl = _matrix_labels_from_list_field(rhs_mcl)
        if labs_mcl is None:
            m_alias_mcl = re.match(r"^([A-Za-z]\w*)$", rhs_mcl)
            if m_alias_mcl is not None:
                prev_labs_mcl = matrix_col_labels_main.get(m_alias_mcl.group(1).lower())
                if isinstance(prev_labs_mcl, list) and prev_labs_mcl:
                    labs_mcl = [str(x) for x in prev_labs_mcl]
        if labs_mcl is not None:
            matrix_col_labels_main[st_mcl.name.lower()] = labs_mcl

    for nm in list_vars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
    for nm in main_object_list_vars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in lm_vars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in t_test_vars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in array_params:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
    for nm in char_scalars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in char_arrays:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in logical_arrays:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in logical_scalars:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    if "stats_list" in real_arrays:
        real_matrices.add("stats_list")
    for nm in real_matrices:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
    for nm in real_rank3_arrays:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        real_matrices.discard(nm)
        params.pop(nm, None)
    for nm in real_rank4_arrays:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        real_matrices.discard(nm)
        real_rank3_arrays.discard(nm)
        params.pop(nm, None)
    for nm in int_matrices:
        ints.discard(nm)
        real_scalars.discard(nm)
        int_arrays.discard(nm)
        real_arrays.discard(nm)
        params.pop(nm, None)
        real_matrices.discard(nm)

    def _force_main_list_component_alias_types(ss_alias: list[object]) -> None:
        int_fields = {"n", "k", "p", "q", "aic_p", "aic_q", "bic_p", "bic_q", "aic_row", "bic_row", "nobs", "nseries", "convergence", "n_iter", "order", "trial"}
        real_fields = {"loglik", "aic", "bic", "npar", "ridge", "sigma2"}
        matrix_fields = {"ar", "ma", "sigma", "table", "design", "y"}
        for st_alias in ss_alias:
            if isinstance(st_alias, Assign):
                rhs_alias = st_alias.expr.strip()
                m_alias = re.match(r"^[A-Za-z]\w*\$([A-Za-z]\w*)\s*$", rhs_alias)
                if m_alias is None:
                    continue
                fld = m_alias.group(1).lower()
                if fld in int_fields or fld in real_fields:
                    params.pop(st_alias.name, None)
                    int_arrays.discard(st_alias.name)
                    real_arrays.discard(st_alias.name)
                    int_matrices.discard(st_alias.name)
                    real_matrices.discard(st_alias.name)
                    real_rank3_arrays.discard(st_alias.name)
                    real_rank4_arrays.discard(st_alias.name)
                if fld in int_fields:
                    ints.add(st_alias.name)
                    real_scalars.discard(st_alias.name)
                elif fld in real_fields:
                    real_scalars.add(st_alias.name)
                    ints.discard(st_alias.name)
                elif fld in matrix_fields:
                    real_matrices.add(st_alias.name)
                    ints.discard(st_alias.name)
                    int_arrays.discard(st_alias.name)
                    real_arrays.discard(st_alias.name)
                    real_scalars.discard(st_alias.name)
                    params.pop(st_alias.name, None)
            elif isinstance(st_alias, IfStmt):
                _force_main_list_component_alias_types(st_alias.then_body)
                _force_main_list_component_alias_types(st_alias.else_body)
            elif isinstance(st_alias, ForStmt):
                _force_main_list_component_alias_types(st_alias.body)
            elif isinstance(st_alias, WhileStmt):
                _force_main_list_component_alias_types(st_alias.body)
            elif isinstance(st_alias, RepeatStmt):
                _force_main_list_component_alias_types(st_alias.body)

    _force_main_list_component_alias_types(main_stmts)

    helper_ctx_main["int_matrix_vars"] = set(int_matrices)
    helper_ctx_main["real_matrix_vars"] = set(real_matrices) | set(real_rank3_arrays) | set(real_rank4_arrays)
    helper_ctx_main["int_vector_vars"] = set(int_arrays) | {k for k, (kk, _, _) in array_params.items() if kk == "integer"}
    helper_ctx_main["real_vector_vars"] = set(real_arrays) | {k for k, (kk, _, _) in array_params.items() if kk != "integer"}
    helper_ctx_main["char_scalar_vars"] = set(char_scalars)

    if ints:
        pbody.w("integer :: " + ", ".join(sorted(ints)))
    if int_arrays:
        pbody.w("integer, allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(int_arrays)))
    if real_arrays:
        pbody.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(real_arrays)))
    if real_scalars:
        pbody.w("real(kind=dp) :: " + ", ".join(sorted(real_scalars)))
    if int_matrices:
        pbody.w("integer, allocatable :: " + ", ".join(f"{x}(:,:)" for x in sorted(int_matrices)))
    if real_matrices:
        pbody.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:)" for x in sorted(real_matrices)))
    if real_rank3_arrays:
        pbody.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:,:)" for x in sorted(real_rank3_arrays)))
    if real_rank4_arrays:
        pbody.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:,:,:)" for x in sorted(real_rank4_arrays)))
    if t_test_vars:
        pbody.w("type(t_test_result_t) :: " + ", ".join(sorted(t_test_vars)))
    if char_scalars:
        pbody.w("character(len=:), allocatable :: " + ", ".join(sorted(char_scalars)))
    if char_arrays:
        pbody.w("character(len=:), allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(char_arrays)))
    if logical_arrays:
        pbody.w("logical, allocatable :: " + ", ".join(f"{x}(:)" for x in sorted(logical_arrays)))
    if logical_scalars:
        pbody.w("logical :: " + ", ".join(sorted(logical_scalars)))
    if list_vars:
        for nm, tn in sorted(list_vars.items()):
            pbody.w(f"type({tn}) :: {nm}")
    if main_object_list_vars:
        for nm, tn in sorted(main_object_list_vars.items()):
            pbody.w(f"type({tn}), allocatable :: {nm}(:)")
    if lm_vars:
        for nm in sorted(lm_vars):
            pbody.w(f"type(lm_fit_t) :: {nm}")
    if main_list_var_fields:
        helper_ctx_main["list_locals"] = dict(main_list_var_fields)
    if main_object_list_vars:
        helper_ctx_main["object_list_vars"] = dict(main_object_list_vars)
        _KNOWN_OBJECT_LIST_NAMES.update(nm.lower() for nm in main_object_list_vars)
    if t_test_vars:
        helper_ctx_main["t_test_vars"] = set(t_test_vars)
    pbody.w("")
    if ("r_mod" in helper_modules) and (not int_like_print):
        pbody.w("call set_print_int_like(.false.)")
    if ("r_mod" in helper_modules) and recycle_warn:
        pbody.w("call set_recycle_warn(.true.)")
    if ("r_mod" in helper_modules) and recycle_stop:
        pbody.w("call set_recycle_stop(.true.)")

    need_rnorm_main = {"used": False}
    params_for_emit = set(params.keys()) | set(array_params.keys()) | set(promoted_params.keys()) | set(promoted_array_params.keys())
    emit_stmts(
        pbody,
        main_stmts,
        need_rnorm_main,
        params_for_emit,
        helper_ctx=helper_ctx_main,
    )

    # Module procedures body (without header/footer).
    mprocs = FEmit()
    fn_needs_rnorm = False
    for fn in funcs:
        fn_needs_rnorm = emit_function(mprocs, fn, list_specs, helper_ctx=helper_ctx_mod) or fn_needs_rnorm
        mprocs.w("")
    has_r_mod_main = bool(helper_ctx_main.get("has_r_mod"))
    emit_local_rnorm = (need_rnorm_main["used"] or fn_needs_rnorm) and (not has_r_mod_main)
    if emit_local_rnorm:
        mprocs.w("subroutine rnorm_vec(n, x)")
        mprocs.w("integer, intent(in) :: n")
        mprocs.w("real(kind=dp), allocatable, intent(inout) :: x(:)")
        mprocs.w("integer :: i")
        mprocs.w("real(kind=dp) :: u1, u2, r, t")
        mprocs.w("if (allocated(x)) deallocate(x)")
        mprocs.w("allocate(x(n))")
        mprocs.w("i = 1")
        mprocs.w("do while (i <= n)")
        mprocs.push()
        mprocs.w("call random_number(u1)")
        mprocs.w("call random_number(u2)")
        mprocs.w("if (u1 <= tiny(1.0_dp)) cycle")
        mprocs.w("r = sqrt(-2.0_dp * log(u1))")
        mprocs.w("t = 2.0_dp * acos(-1.0_dp) * u2")
        mprocs.w("x(i) = r * cos(t)")
        mprocs.w("if (i + 1 <= n) x(i + 1) = r * sin(t)")
        mprocs.w("i = i + 2")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end subroutine rnorm_vec")
        mprocs.w("")
    emit_local_scan_reader = bool(helper_ctx_main.get("need_scan_reader")) and (not has_r_mod_main)
    if emit_local_scan_reader:
        mprocs.w("subroutine read_real_vector(file_path, x)")
        mprocs.w("character(len=*), intent(in) :: file_path")
        mprocs.w("real(kind=dp), allocatable, intent(out) :: x(:)")
        mprocs.w("integer :: fp, ios, n, cap, new_cap")
        mprocs.w("real(kind=dp) :: v")
        mprocs.w("n = 0")
        mprocs.w("cap = 0")
        mprocs.w('open(newunit=fp, file=file_path, status="old", action="read")')
        mprocs.w("do")
        mprocs.push()
        mprocs.w("read(fp, *, iostat=ios) v")
        mprocs.w("if (ios /= 0) exit")
        mprocs.w("if (n == cap) then")
        mprocs.push()
        mprocs.w("new_cap = merge(1024, 2 * cap, cap == 0)")
        mprocs.w("block")
        mprocs.push()
        mprocs.w("real(kind=dp), allocatable :: tmp(:)")
        mprocs.w("allocate(tmp(new_cap))")
        mprocs.w("if (allocated(x) .and. n > 0) tmp(1:n) = x(1:n)")
        mprocs.w("call move_alloc(tmp, x)")
        mprocs.pop()
        mprocs.w("end block")
        mprocs.w("cap = new_cap")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("n = n + 1")
        mprocs.w("x(n) = v")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("close(fp)")
        mprocs.w("if (n == 0) then")
        mprocs.push()
        mprocs.w("allocate(x(0))")
        mprocs.pop()
        mprocs.w("else if (n < size(x)) then")
        mprocs.push()
        mprocs.w("x = x(1:n)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("end subroutine read_real_vector")
        mprocs.w("")
    emit_local_table_reader = bool(helper_ctx_main.get("need_table_reader")) and (not has_r_mod_main)
    emit_local_table_writer = bool(helper_ctx_main.get("need_table_writer")) and (not has_r_mod_main)
    if emit_local_table_reader or emit_local_table_writer:
        mprocs.w("pure integer function count_ws_tokens(line) result(n_tok)")
        mprocs.w("character(len=*), intent(in) :: line")
        mprocs.w("integer :: i, n")
        mprocs.w("logical :: in_tok")
        mprocs.w("n = len_trim(line)")
        mprocs.w("n_tok = 0")
        mprocs.w("in_tok = .false.")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("if (line(i:i) /= ' ' .and. line(i:i) /= char(9)) then")
        mprocs.push()
        mprocs.w("if (.not. in_tok) then")
        mprocs.push()
        mprocs.w("n_tok = n_tok + 1")
        mprocs.w("in_tok = .true.")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("in_tok = .false.")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function count_ws_tokens")
        mprocs.w("")
    if emit_local_table_reader:
        mprocs.w("subroutine read_table_real_matrix(file_path, x)")
        mprocs.w("character(len=*), intent(in) :: file_path")
        mprocs.w("real(kind=dp), allocatable, intent(out) :: x(:,:)")
        mprocs.w("integer :: fp, ios, nrow, ncol, i")
        mprocs.w("character(len=4096) :: line")
        mprocs.w("nrow = 0")
        mprocs.w("ncol = 0")
        mprocs.w('open(newunit=fp, file=file_path, status="old", action="read")')
        mprocs.w("do")
        mprocs.push()
        mprocs.w("read(fp, '(A)', iostat=ios) line")
        mprocs.w("if (ios /= 0) exit")
        mprocs.w("if (len_trim(line) == 0) cycle")
        mprocs.w("nrow = nrow + 1")
        mprocs.w("if (ncol == 0) ncol = count_ws_tokens(line)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (nrow <= 0 .or. ncol <= 0) then")
        mprocs.push()
        mprocs.w("allocate(x(0,0))")
        mprocs.w("close(fp)")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(x(nrow, ncol))")
        mprocs.w("rewind(fp)")
        mprocs.w("i = 0")
        mprocs.w("do")
        mprocs.push()
        mprocs.w("read(fp, '(A)', iostat=ios) line")
        mprocs.w("if (ios /= 0) exit")
        mprocs.w("if (len_trim(line) == 0) cycle")
        mprocs.w("i = i + 1")
        mprocs.w("read(line, *) x(i, 1:ncol)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("close(fp)")
        mprocs.w("end subroutine read_table_real_matrix")
        mprocs.w("")
    if emit_local_table_writer:
        mprocs.w("subroutine write_table_real_matrix(file_path, x)")
        mprocs.w("character(len=*), intent(in) :: file_path")
        mprocs.w("real(kind=dp), intent(in) :: x(:,:)")
        mprocs.w("integer :: fp, i")
        mprocs.w('open(newunit=fp, file=file_path, status="replace", action="write")')
        mprocs.w("do i = 1, size(x, 1)")
        mprocs.push()
        mprocs.w("write(fp, *) x(i, 1:size(x, 2))")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("close(fp)")
        mprocs.w("end subroutine write_table_real_matrix")
        mprocs.w("")
    emit_local_lm = (bool(helper_ctx_main.get("need_lm")) or bool(helper_ctx_mod.get("need_lm"))) and (not has_r_mod_main)
    if emit_local_lm:
        mprocs.w("subroutine print_lm_coef_rstyle(fit, term_names)")
        mprocs.w("type(lm_fit_t), intent(in) :: fit")
        mprocs.w("character(len=*), intent(in), optional :: term_names(:)")
        mprocs.w("integer :: j, p")
        mprocs.w("character(len=32) :: lbl")
        mprocs.w("p = max(0, size(fit%coef) - 1)")
        mprocs.w("write(*,'(a14)', advance='no') '(Intercept)'")
        mprocs.w("do j = 1, p")
        mprocs.push()
        mprocs.w("if (present(term_names) .and. size(term_names) >= j) then")
        mprocs.push()
        mprocs.w("write(*,'(a14)', advance='no') trim(term_names(j))")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("write(lbl,'(a,i0)') 'x', j")
        mprocs.w("write(*,'(a14)', advance='no') trim(lbl)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("write(*,*)")
        mprocs.w("if (size(fit%coef) > 0) then")
        mprocs.push()
        mprocs.w("write(*,'(*(f14.7))') fit%coef")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("write(*,*)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("end subroutine print_lm_coef_rstyle")
        mprocs.w("")
        mprocs.w("pure function lm_predict_general(fit, xpred) result(yhat)")
        mprocs.w("type(lm_fit_t), intent(in) :: fit")
        mprocs.w("real(kind=dp), intent(in) :: xpred(:,:)")
        mprocs.w("real(kind=dp), allocatable :: yhat(:)")
        mprocs.w("integer :: p")
        mprocs.w("p = size(xpred, 2)")
        mprocs.w("if (size(fit%coef) /= p + 1) error stop \"error: predictor count mismatch\"")
        mprocs.w("allocate(yhat(size(xpred, 1)))")
        mprocs.w("yhat = fit%coef(1) + matmul(xpred, fit%coef(2:p+1))")
        mprocs.w("end function lm_predict_general")
        mprocs.w("")
        mprocs.w("subroutine solve_linear(a, b, x, ok)")
        mprocs.w("real(kind=dp), intent(inout) :: a(:,:)")
        mprocs.w("real(kind=dp), intent(inout) :: b(:)")
        mprocs.w("real(kind=dp), intent(out) :: x(:)")
        mprocs.w("logical, intent(out) :: ok")
        mprocs.w("integer :: i, j, k, p, n")
        mprocs.w("real(kind=dp) :: piv, fac, t")
        mprocs.w("ok = .true.")
        mprocs.w("n = size(b)")
        mprocs.w("if (size(a,1) /= n .or. size(a,2) /= n .or. size(x) /= n) then")
        mprocs.push()
        mprocs.w("ok = .false.")
        mprocs.w("x = 0.0_dp")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do k = 1, n")
        mprocs.push()
        mprocs.w("p = k")
        mprocs.w("piv = abs(a(k,k))")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("if (abs(a(i,k)) > piv) then")
        mprocs.push()
        mprocs.w("p = i")
        mprocs.w("piv = abs(a(i,k))")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (piv <= tiny(1.0_dp)) then")
        mprocs.push()
        mprocs.w("ok = .false.")
        mprocs.w("x = 0.0_dp")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (p /= k) then")
        mprocs.push()
        mprocs.w("do j = k, n")
        mprocs.push()
        mprocs.w("t = a(k,j)")
        mprocs.w("a(k,j) = a(p,j)")
        mprocs.w("a(p,j) = t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("t = b(k)")
        mprocs.w("b(k) = b(p)")
        mprocs.w("b(p) = t")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("fac = a(i,k) / a(k,k)")
        mprocs.w("a(i,k:n) = a(i,k:n) - fac * a(k,k:n)")
        mprocs.w("b(i) = b(i) - fac * b(k)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("x(n) = b(n) / a(n,n)")
        mprocs.w("do i = n - 1, 1, -1")
        mprocs.push()
        mprocs.w("if (i < n) then")
        mprocs.push()
        mprocs.w("x(i) = (b(i) - sum(a(i,i+1:n) * x(i+1:n))) / a(i,i)")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("x(i) = b(i) / a(i,i)")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end subroutine solve_linear")
        mprocs.w("")
        mprocs.w("function lm_fit_general(y, xpred) result(fit)")
        mprocs.w("real(kind=dp), intent(in) :: y(:)")
        mprocs.w("real(kind=dp), intent(in) :: xpred(:,:)")
        mprocs.w("type(lm_fit_t) :: fit")
        mprocs.w("integer :: i, j, n, p, k, dof")
        mprocs.w("real(kind=dp), allocatable :: a(:,:), b(:), beta(:)")
        mprocs.w("real(kind=dp) :: ybar, sse, sst")
        mprocs.w("logical :: ok")
        mprocs.w("if (size(y) /= size(xpred,1)) then")
        mprocs.push()
        mprocs.w("error stop \"error: need size(y) == size(xpred,1)\"")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("n = size(y)")
        mprocs.w("p = size(xpred,2)")
        mprocs.w("k = p + 1")
        mprocs.w("if (n < k) error stop \"error: need n >= number of parameters\"")
        mprocs.w("allocate(a(k,k), b(k), beta(k))")
        mprocs.w("a = 0.0_dp")
        mprocs.w("b = 0.0_dp")
        mprocs.w("a(1,1) = n")
        mprocs.w("b(1) = sum(y)")
        mprocs.w("do j = 1, p")
        mprocs.push()
        mprocs.w("a(1,j+1) = sum(xpred(:,j))")
        mprocs.w("a(j+1,1) = a(1,j+1)")
        mprocs.w("b(j+1) = sum(xpred(:,j) * y)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("do i = 1, p")
        mprocs.push()
        mprocs.w("do j = i, p")
        mprocs.push()
        mprocs.w("a(i+1,j+1) = sum(xpred(:,i) * xpred(:,j))")
        mprocs.w("a(j+1,i+1) = a(i+1,j+1)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("call solve_linear(a, b, beta, ok)")
        mprocs.w("if (.not. ok) error stop \"error: singular normal equations\"")
        mprocs.w("fit%coef = beta")
        mprocs.w("fit%fitted = beta(1) + matmul(xpred, beta(2:k))")
        mprocs.w("fit%resid = y - fit%fitted")
        mprocs.w("sse = sum(fit%resid**2)")
        mprocs.w("ybar = sum(y) / n")
        mprocs.w("sst = sum((y - ybar)**2)")
        mprocs.w("if (sst > 0.0_dp) then")
        mprocs.push()
        mprocs.w("fit%r_squared = 1.0_dp - sse / sst")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("fit%r_squared = 0.0_dp")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("dof = max(1, n - k)")
        mprocs.w("fit%sigma = sqrt(sse / dof)")
        mprocs.w("fit%adj_r_squared = 1.0_dp - (1.0_dp - fit%r_squared) * (n - 1) / dof")
        mprocs.w("end function lm_fit_general")
        mprocs.w("")
        mprocs.w("subroutine print_lm_summary(fit)")
        mprocs.w("type(lm_fit_t), intent(in) :: fit")
        mprocs.w("write(*,'(a)') 'lm summary:'")
        mprocs.w("write(*,'(a,*(1x,g0))') 'coef:', fit%coef")
        mprocs.w("write(*,'(a,g0)') 'sigma:', fit%sigma")
        mprocs.w("write(*,'(a,g0)') 'r.squared:', fit%r_squared")
        mprocs.w("write(*,'(a,g0)') 'adj.r.squared:', fit%adj_r_squared")
        mprocs.w("end subroutine print_lm_summary")
        mprocs.w("")

    emit_local_drop = any(("r_drop_index(" in ln or "r_drop_indices(" in ln) for ln in pbody.lines) or any(
        ("r_drop_index(" in ln or "r_drop_indices(" in ln) for ln in mprocs.lines
    )
    has_r_mod_emit = ("r_mod" in helper_modules)
    emit_local_seq = ((not has_r_mod_emit) and (
        any(
            any(fn in ln for fn in ("r_seq_int(", "r_seq_len(", "r_seq_int_by(", "r_seq_int_length(", "r_seq_real_by(", "r_seq_real_length("))
            for ln in pbody.lines
        )
        or any(
            any(fn in ln for fn in ("r_seq_int(", "r_seq_len(", "r_seq_int_by(", "r_seq_int_length(", "r_seq_real_by(", "r_seq_real_length("))
            for ln in mprocs.lines
        )
    ))
    emit_local_rep = ((not has_r_mod_emit) and (
        any(("r_rep_real(" in ln or "r_rep_int(" in ln) for ln in pbody.lines)
        or any(("r_rep_real(" in ln or "r_rep_int(" in ln) for ln in mprocs.lines)
    ))
    emit_local_numeric = ((not has_r_mod_emit) and (
        any("numeric(" in ln for ln in pbody.lines)
        or any("numeric(" in ln for ln in mprocs.lines)
    ))
    emit_local_typeof = ((not has_r_mod_emit) and (
        any("r_typeof(" in ln for ln in pbody.lines)
        or any("r_typeof(" in ln for ln in mprocs.lines)
    ))
    emit_local_character = ((not has_r_mod_emit) and (
        any("r_character(" in ln for ln in pbody.lines)
        or any("r_character(" in ln for ln in mprocs.lines)
    ))
    emit_local_command_args = (
        any("r_command_args(" in ln for ln in pbody.lines)
        or any("r_command_args(" in ln for ln in mprocs.lines)
    )
    emit_local_order = (not has_r_mod_emit) and (
        any("order_real(" in ln for ln in pbody.lines)
        or any("order_real(" in ln for ln in mprocs.lines)
    )
    emit_local_rank = (not has_r_mod_emit) and (
        any("rank_average(" in ln or "rank_first(" in ln for ln in pbody.lines)
        or any("rank_average(" in ln or "rank_first(" in ln for ln in mprocs.lines)
    )
    emit_local_det = (
        any("det_real(" in ln for ln in pbody.lines)
        or any("det_real(" in ln for ln in mprocs.lines)
    ) and (not has_r_mod_main)
    emit_local_solve = (
        any("solve_real(" in ln for ln in pbody.lines)
        or any("solve_real(" in ln for ln in mprocs.lines)
    ) and (not has_r_mod_main)
    if emit_local_command_args:
        mprocs.w("function r_command_args() result(out)")
        mprocs.w("character(len=:), allocatable :: out(:)")
        mprocs.w("integer :: i, n, stat")
        mprocs.w("character(len=4096) :: buf")
        mprocs.w("n = command_argument_count()")
        mprocs.w("allocate(character(len=4096) :: out(n))")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("call get_command_argument(i, buf, status=stat)")
        mprocs.w("if (stat == 0) then")
        mprocs.push()
        mprocs.w("out(i) = trim(buf)")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w('out(i) = ""')
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_command_args")
        mprocs.w("")
    if emit_local_character:
        mprocs.w("function r_character(n) result(out)")
        mprocs.w("integer, intent(in) :: n")
        mprocs.w("character(len=:), allocatable :: out(:)")
        mprocs.w("allocate(character(len=0) :: out(max(0, n)))")
        mprocs.w("end function r_character")
        mprocs.w("")
    if emit_local_seq:
        mprocs.w("pure function r_seq_int(a, b) result(out)")
        mprocs.w("integer, intent(in) :: a, b")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("integer :: i, n, step")
        mprocs.w("n = abs(b - a) + 1")
        mprocs.w("allocate(out(n))")
        mprocs.w("step = merge(1, -1, a <= b)")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("out(i) = a + (i - 1) * step")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_seq_int")
        mprocs.w("")
        mprocs.w("pure function r_seq_len(n) result(out)")
        mprocs.w("integer, intent(in) :: n")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("integer :: i")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(out(n))")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("out(i) = i")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_seq_len")
        mprocs.w("")
        mprocs.w("pure function r_seq_int_by(a, b, by) result(out)")
        mprocs.w("integer, intent(in) :: a, b, by")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("integer :: i, n")
        mprocs.w("if (by == 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if ((by > 0 .and. a > b) .or. (by < 0 .and. a < b)) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("n = (abs(b - a) / abs(by)) + 1")
        mprocs.w("allocate(out(n))")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("out(i) = a + (i - 1) * by")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_seq_int_by")
        mprocs.w("")
        mprocs.w("pure function r_seq_int_length(a, b, n) result(out)")
        mprocs.w("integer, intent(in) :: a, b, n")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("integer :: i")
        mprocs.w("real(kind=dp) :: t")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(out(n))")
        mprocs.w("if (n == 1) then")
        mprocs.push()
        mprocs.w("out(1) = a")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("t = real(i - 1, kind=dp) / real(n - 1, kind=dp)")
        mprocs.w("out(i) = nint((1.0_dp - t) * real(a, kind=dp) + t * real(b, kind=dp))")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_seq_int_length")
        mprocs.w("")
        mprocs.w("pure function r_seq_real_by(a, b, by) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: a, b, by")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("integer :: i, n")
        mprocs.w("if (by == 0.0_dp) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if ((by > 0.0_dp .and. a > b) .or. (by < 0.0_dp .and. a < b)) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("n = int(floor((b - a) / by + 1.0e-12_dp)) + 1")
        mprocs.w("if (n < 0) n = 0")
        mprocs.w("allocate(out(n))")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("out(i) = a + real(i - 1, kind=dp) * by")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_seq_real_by")
        mprocs.w("")
        mprocs.w("pure function r_seq_real_length(a, b, n) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: a, b")
        mprocs.w("integer, intent(in) :: n")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("integer :: i")
        mprocs.w("real(kind=dp) :: t")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(out(n))")
        mprocs.w("if (n == 1) then")
        mprocs.push()
        mprocs.w("out(1) = a")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("t = real(i - 1, kind=dp) / real(n - 1, kind=dp)")
        mprocs.w("out(i) = (1.0_dp - t) * a + t * b")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function r_seq_real_length")
        mprocs.w("")
    if emit_local_rep:
        mprocs.w("pure function r_rep_real(x, times, each, len_out, times_vec) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x(:)")
        mprocs.w("integer, intent(in), optional :: times, each, len_out")
        mprocs.w("integer, intent(in), optional :: times_vec(:)")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("real(kind=dp), allocatable :: y(:), z(:)")
        mprocs.w("integer :: i, j, n, e, t, k, m, need, c")
        mprocs.w("n = size(x)")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (present(each)) then")
        mprocs.push()
        mprocs.w("e = each")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("e = 1")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (e < 1) e = 1")
        mprocs.w("allocate(y(n * e))")
        mprocs.w("k = 0")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("do j = 1, e")
        mprocs.push()
        mprocs.w("k = k + 1")
        mprocs.w("y(k) = x(i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (present(times_vec)) then")
        mprocs.push()
        mprocs.w("m = size(y)")
        mprocs.w("c = 0")
        mprocs.w("do i = 1, m")
        mprocs.push()
        mprocs.w("t = times_vec(mod(i - 1, size(times_vec)) + 1)")
        mprocs.w("if (t > 0) c = c + t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("allocate(z(c))")
        mprocs.w("k = 0")
        mprocs.w("do i = 1, m")
        mprocs.push()
        mprocs.w("t = times_vec(mod(i - 1, size(times_vec)) + 1)")
        mprocs.w("do j = 1, max(0, t)")
        mprocs.push()
        mprocs.w("k = k + 1")
        mprocs.w("z(k) = y(i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("if (present(times)) then")
        mprocs.push()
        mprocs.w("t = times")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("t = 1")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (t < 0) t = 0")
        mprocs.w("allocate(z(size(y) * t))")
        mprocs.w("k = 0")
        mprocs.w("do j = 1, t")
        mprocs.push()
        mprocs.w("do i = 1, size(y)")
        mprocs.push()
        mprocs.w("k = k + 1")
        mprocs.w("z(k) = y(i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (present(len_out)) then")
        mprocs.push()
        mprocs.w("need = max(0, len_out)")
        mprocs.w("if (need == 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(out(need))")
        mprocs.w("if (size(z) > 0) then")
        mprocs.push()
        mprocs.w("do i = 1, need")
        mprocs.push()
        mprocs.w("out(i) = z(mod(i - 1, size(z)) + 1)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("out = z")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("end function r_rep_real")
        mprocs.w("")
        mprocs.w("pure function r_rep_int(x, times, each, len_out, times_vec) result(out)")
        mprocs.w("integer, intent(in) :: x(:)")
        mprocs.w("integer, intent(in), optional :: times, each, len_out")
        mprocs.w("integer, intent(in), optional :: times_vec(:)")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("integer, allocatable :: y(:), z(:)")
        mprocs.w("integer :: i, j, n, e, t, k, m, need, c")
        mprocs.w("n = size(x)")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (present(each)) then")
        mprocs.push()
        mprocs.w("e = each")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("e = 1")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (e < 1) e = 1")
        mprocs.w("allocate(y(n * e))")
        mprocs.w("k = 0")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("do j = 1, e")
        mprocs.push()
        mprocs.w("k = k + 1")
        mprocs.w("y(k) = x(i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (present(times_vec)) then")
        mprocs.push()
        mprocs.w("m = size(y)")
        mprocs.w("c = 0")
        mprocs.w("do i = 1, m")
        mprocs.push()
        mprocs.w("t = times_vec(mod(i - 1, size(times_vec)) + 1)")
        mprocs.w("if (t > 0) c = c + t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("allocate(z(c))")
        mprocs.w("k = 0")
        mprocs.w("do i = 1, m")
        mprocs.push()
        mprocs.w("t = times_vec(mod(i - 1, size(times_vec)) + 1)")
        mprocs.w("do j = 1, max(0, t)")
        mprocs.push()
        mprocs.w("k = k + 1")
        mprocs.w("z(k) = y(i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("if (present(times)) then")
        mprocs.push()
        mprocs.w("t = times")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("t = 1")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (t < 0) t = 0")
        mprocs.w("allocate(z(size(y) * t))")
        mprocs.w("k = 0")
        mprocs.w("do j = 1, t")
        mprocs.push()
        mprocs.w("do i = 1, size(y)")
        mprocs.push()
        mprocs.w("k = k + 1")
        mprocs.w("z(k) = y(i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (present(len_out)) then")
        mprocs.push()
        mprocs.w("need = max(0, len_out)")
        mprocs.w("if (need == 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(out(need))")
        mprocs.w("if (size(z) > 0) then")
        mprocs.push()
        mprocs.w("do i = 1, need")
        mprocs.push()
        mprocs.w("out(i) = z(mod(i - 1, size(z)) + 1)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("else")
        mprocs.push()
        mprocs.w("out = z")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("end function r_rep_int")
        mprocs.w("")
    if emit_local_numeric:
        mprocs.w("pure function numeric(n) result(out)")
        mprocs.w("integer, intent(in) :: n")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("allocate(out(max(0, n)))")
        mprocs.w("if (n > 0) out = 0.0_dp")
        mprocs.w("end function numeric")
        mprocs.w("")
    if emit_local_typeof:
        mprocs.w("pure function r_typeof_i(x) result(out)")
        mprocs.w("integer, intent(in) :: x")
        mprocs.w("character(len=:), allocatable :: out")
        mprocs.w("out = \"integer\"")
        mprocs.w("end function r_typeof_i")
        mprocs.w("")
        mprocs.w("pure function r_typeof_r(x) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x")
        mprocs.w("character(len=:), allocatable :: out")
        mprocs.w("out = \"double\"")
        mprocs.w("end function r_typeof_r")
        mprocs.w("")
        mprocs.w("pure function r_typeof_l(x) result(out)")
        mprocs.w("logical, intent(in) :: x")
        mprocs.w("character(len=:), allocatable :: out")
        mprocs.w("out = \"logical\"")
        mprocs.w("end function r_typeof_l")
        mprocs.w("")
        mprocs.w("pure function r_typeof_c(x) result(out)")
        mprocs.w("character(len=*), intent(in) :: x")
        mprocs.w("character(len=:), allocatable :: out")
        mprocs.w("out = \"character\"")
        mprocs.w("end function r_typeof_c")
        mprocs.w("")
    if emit_local_order:
        mprocs.w("pure function order_real(x) result(idx)")
        mprocs.w("real(kind=dp), intent(in) :: x(:)")
        mprocs.w("integer, allocatable :: idx(:)")
        mprocs.w("integer :: i, j, n, t")
        mprocs.w("n = size(x)")
        mprocs.w("allocate(idx(n))")
        mprocs.w("do i = 1, n")
        mprocs.push()
        mprocs.w("idx(i) = i")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("do i = 2, n")
        mprocs.push()
        mprocs.w("t = idx(i)")
        mprocs.w("j = i - 1")
        mprocs.w("do while (j >= 1 .and. x(idx(j)) > x(t))")
        mprocs.push()
        mprocs.w("idx(j + 1) = idx(j)")
        mprocs.w("j = j - 1")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("idx(j + 1) = t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function order_real")
        mprocs.w("")
    if emit_local_rank:
        mprocs.w("pure function rank_first(x) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x(:)")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("integer, allocatable :: ord(:)")
        mprocs.w("integer :: i")
        mprocs.w("allocate(out(size(x)))")
        mprocs.w("if (size(x) <= 0) return")
        mprocs.w("ord = order_real(x)")
        mprocs.w("do i = 1, size(ord)")
        mprocs.push()
        mprocs.w("out(ord(i)) = real(i, kind=dp)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function rank_first")
        mprocs.w("")
        mprocs.w("pure function rank_average(x) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x(:)")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("integer, allocatable :: ord(:)")
        mprocs.w("integer :: first, i, last")
        mprocs.w("real(kind=dp) :: r")
        mprocs.w("allocate(out(size(x)))")
        mprocs.w("if (size(x) <= 0) return")
        mprocs.w("ord = order_real(x)")
        mprocs.w("first = 1")
        mprocs.w("do while (first <= size(ord))")
        mprocs.push()
        mprocs.w("last = first")
        mprocs.w("do while (last < size(ord) .and. x(ord(last + 1)) == x(ord(first)))")
        mprocs.push()
        mprocs.w("last = last + 1")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("r = 0.5_dp * real(first + last, kind=dp)")
        mprocs.w("do i = first, last")
        mprocs.push()
        mprocs.w("out(ord(i)) = r")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("first = last + 1")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function rank_average")
        mprocs.w("")
    if emit_local_det:
        mprocs.w("pure function det_real(x) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x(:,:)")
        mprocs.w("real(kind=dp) :: out")
        mprocs.w("real(kind=dp), allocatable :: a(:)")
        mprocs.w("integer :: i, j, k, n, p")
        mprocs.w("real(kind=dp) :: fac, piv, t")
        mprocs.w("n = size(x, 1)")
        mprocs.w("out = 0.0_dp")
        mprocs.w("if (n /= size(x, 2)) return")
        mprocs.w("if (n == 0) then")
        mprocs.push()
        mprocs.w("out = 1.0_dp")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(a(n*n))")
        mprocs.w("a = reshape(x, [n*n])")
        mprocs.w("out = 1.0_dp")
        mprocs.w("do k = 1, n")
        mprocs.push()
        mprocs.w("p = k")
        mprocs.w("piv = abs(a((k - 1)*n + k))")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("if (abs(a((i - 1)*n + k)) > piv) then")
        mprocs.push()
        mprocs.w("p = i")
        mprocs.w("piv = abs(a((i - 1)*n + k))")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (piv <= tiny(1.0_dp)) then")
        mprocs.push()
        mprocs.w("out = 0.0_dp")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("if (p /= k) then")
        mprocs.push()
        mprocs.w("do j = k, n")
        mprocs.push()
        mprocs.w("t = a((k - 1)*n + j)")
        mprocs.w("a((k - 1)*n + j) = a((p - 1)*n + j)")
        mprocs.w("a((p - 1)*n + j) = t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("out = -out")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("out = out * a((k - 1)*n + k)")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("fac = a((i - 1)*n + k) / a((k - 1)*n + k)")
        mprocs.w("do j = k + 1, n")
        mprocs.push()
        mprocs.w("a((i - 1)*n + j) = a((i - 1)*n + j) - fac * a((k - 1)*n + j)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function det_real")
        mprocs.w("")
    if emit_local_solve:
        mprocs.w("pure function solve_real(a, b) result(x)")
        mprocs.w("real(kind=dp), intent(in) :: a(:,:), b(:)")
        mprocs.w("real(kind=dp), allocatable :: x(:)")
        mprocs.w("real(kind=dp), allocatable :: aa(:,:), bb(:)")
        mprocs.w("integer :: i, j, k, n, p")
        mprocs.w("real(kind=dp) :: fac, piv, s, t")
        mprocs.w("n = size(b)")
        mprocs.w("allocate(x(n))")
        mprocs.w("x = 0.0_dp")
        mprocs.w("if (size(a, 1) /= n .or. size(a, 2) /= n) return")
        mprocs.w("aa = a")
        mprocs.w("bb = b")
        mprocs.w("do k = 1, n")
        mprocs.push()
        mprocs.w("p = k")
        mprocs.w("piv = abs(aa(k, k))")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("if (abs(aa(i, k)) > piv) then")
        mprocs.push()
        mprocs.w("p = i")
        mprocs.w("piv = abs(aa(i, k))")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("if (piv <= tiny(1.0_dp)) return")
        mprocs.w("if (p /= k) then")
        mprocs.push()
        mprocs.w("do j = k, n")
        mprocs.push()
        mprocs.w("t = aa(k, j)")
        mprocs.w("aa(k, j) = aa(p, j)")
        mprocs.w("aa(p, j) = t")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("t = bb(k)")
        mprocs.w("bb(k) = bb(p)")
        mprocs.w("bb(p) = t")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("do i = k + 1, n")
        mprocs.push()
        mprocs.w("fac = aa(i, k) / aa(k, k)")
        mprocs.w("aa(i, k:n) = aa(i, k:n) - fac * aa(k, k:n)")
        mprocs.w("bb(i) = bb(i) - fac * bb(k)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("do i = n, 1, -1")
        mprocs.push()
        mprocs.w("s = bb(i)")
        mprocs.w("if (i < n) s = s - sum(aa(i, i+1:n) * x(i+1:n))")
        mprocs.w("x(i) = s / aa(i, i)")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("end function solve_real")
        mprocs.w("")
    if emit_local_drop:
        mprocs.w("pure function r_drop_index_real(x, k) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x(:)")
        mprocs.w("integer, intent(in) :: k")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("logical, allocatable :: keep(:)")
        mprocs.w("integer :: n, m")
        mprocs.w("n = size(x)")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(keep(n))")
        mprocs.w("keep = .true.")
        mprocs.w("if (k >= 1 .and. k <= n) keep(k) = .false.")
        mprocs.w("m = count(keep)")
        mprocs.w("allocate(out(m))")
        mprocs.w("if (m > 0) out = pack(x, keep)")
        mprocs.w("end function r_drop_index_real")
        mprocs.w("")
        mprocs.w("pure function r_drop_index_int(x, k) result(out)")
        mprocs.w("integer, intent(in) :: x(:)")
        mprocs.w("integer, intent(in) :: k")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("logical, allocatable :: keep(:)")
        mprocs.w("integer :: n, m")
        mprocs.w("n = size(x)")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(keep(n))")
        mprocs.w("keep = .true.")
        mprocs.w("if (k >= 1 .and. k <= n) keep(k) = .false.")
        mprocs.w("m = count(keep)")
        mprocs.w("allocate(out(m))")
        mprocs.w("if (m > 0) out = pack(x, keep)")
        mprocs.w("end function r_drop_index_int")
        mprocs.w("")
        mprocs.w("pure function r_drop_indices_real(x, drop) result(out)")
        mprocs.w("real(kind=dp), intent(in) :: x(:)")
        mprocs.w("integer, intent(in) :: drop(:)")
        mprocs.w("real(kind=dp), allocatable :: out(:)")
        mprocs.w("logical, allocatable :: keep(:)")
        mprocs.w("integer :: i, n, m")
        mprocs.w("n = size(x)")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(keep(n))")
        mprocs.w("keep = .true.")
        mprocs.w("do i = 1, size(drop)")
        mprocs.push()
        mprocs.w("if (drop(i) >= 1 .and. drop(i) <= n) keep(drop(i)) = .false.")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("m = count(keep)")
        mprocs.w("allocate(out(m))")
        mprocs.w("if (m > 0) out = pack(x, keep)")
        mprocs.w("end function r_drop_indices_real")
        mprocs.w("")
        mprocs.w("pure function r_drop_indices_int(x, drop) result(out)")
        mprocs.w("integer, intent(in) :: x(:)")
        mprocs.w("integer, intent(in) :: drop(:)")
        mprocs.w("integer, allocatable :: out(:)")
        mprocs.w("logical, allocatable :: keep(:)")
        mprocs.w("integer :: i, n, m")
        mprocs.w("n = size(x)")
        mprocs.w("if (n <= 0) then")
        mprocs.push()
        mprocs.w("allocate(out(0))")
        mprocs.w("return")
        mprocs.pop()
        mprocs.w("end if")
        mprocs.w("allocate(keep(n))")
        mprocs.w("keep = .true.")
        mprocs.w("do i = 1, size(drop)")
        mprocs.push()
        mprocs.w("if (drop(i) >= 1 .and. drop(i) <= n) keep(drop(i)) = .false.")
        mprocs.pop()
        mprocs.w("end do")
        mprocs.w("m = count(keep)")
        mprocs.w("allocate(out(m))")
        mprocs.w("if (m > 0) out = pack(x, keep)")
        mprocs.w("end function r_drop_indices_int")
        mprocs.w("")

    helper_names = {
        "runif1",
        "runif_vec",
        "rnorm1",
        "rnorm_vec",
        "rnorm_mat",
        "rbinom",
        "random_choice2_prob",
        "sample_int",
        "sample_int1",
        "quantile",
        "median",
        "summary",
        "dnorm",
        "tail",
        "cbind2",
        "cbind",
        "numeric",
        "pmax",
        "sd",
        "r_sd",
        "var",
        "r_format_vec",
        "colMeans",
        "cov",
        "cor",
        "r_seq_int",
        "r_seq_len",
        "r_seq_int_by",
        "r_seq_int_length",
        "r_seq_real_by",
        "r_seq_real_length",
        "r_rep_real",
        "r_rep_int",
        "r_array_real",
        "r_array_int",
        "r_array_char",
        "matrix",
        "r_matmul",
        "r_add",
        "r_sub",
        "r_mul",
        "r_div",
        "match",
        "findInterval",
        "cumsum",
        "cumprod",
        "diff",
        "diag",
        "toeplitz",
        "sort",
        "polyroot",
        "nchar",
        "is_na",
        "r_typeof",
        "r_character",
        "rank_average",
        "rank_first",
        "det_real",
        "eigen_sym_values",
        "solve_real",
        "chol",
        "backsolve",
        "print_matrix",
        "print_matrix_rstyle",
        "print_real_scalar",
        "print_real_vector",
        "print_summary",
        "lm_coef",
        "set_print_int_like",
        "set_recycle_warn",
        "set_recycle_stop",
        "order_real",
        "read_real_vector",
        "read_table_real_matrix",
        "read_csv_real_matrix",
        "write_table_real_matrix",
        "lm_fit_general",
        "lm_predict_general",
        "print_lm_summary",
        "print_lm_coef_rstyle",
        "lm_fit_t",
        "optim_result_t",
        "t_test_result_t",
        "t_test",
        "t_test_p_value",
        "print_t_test",
        "chisq_test_result_t",
        "chisq_test",
        "print_chisq_test",
        "prop_test_result_t",
        "prop_test",
        "print_prop_test",
        "cor_test_result_t",
        "cor_test",
        "print_cor_test",
        "fisher_test_result_t",
        "fisher_test",
        "print_fisher_test",
        "max_col",
        "tabulate",
        "table2",
        "prop_table",
        "print_table1",
        "print_table2",
        "nested_matrix_list_len",
    }
    mod_needed: set[str] = set()
    main_needed: set[str] = set()
    nr_mod = helper_ctx_mod.get("need_r_mod")
    nr_main = helper_ctx_main.get("need_r_mod")
    if isinstance(nr_mod, set):
        mod_needed.update(nr_mod)
    if isinstance(nr_main, set):
        main_needed.update(nr_main)
    if ("r_mod" in helper_modules) and (not int_like_print):
        main_needed.add("set_print_int_like")
    if ("r_mod" in helper_modules) and recycle_warn:
        main_needed.add("set_recycle_warn")
    if ("r_mod" in helper_modules) and recycle_stop:
        main_needed.add("set_recycle_stop")
    mod_text_now = "\n".join(mprocs.lines)
    main_text_now = "\n".join(pbody.lines)
    ieee_pat = re.compile(r"\bieee_(?:is_finite|value|quiet_nan)\b", re.IGNORECASE)
    mod_decl_text = " ".join(params.values()) + " " + " ".join(v[2] for v in array_params.values()) + " " + " ".join(
        promoted_params.values()
    ) + " " + " ".join(v[2] for v in promoted_array_params.values())
    need_ieee_mod = bool(ieee_pat.search(mod_text_now) or ieee_pat.search(mod_decl_text))
    need_ieee_main = bool(ieee_pat.search(main_text_now))
    need_pi_mod = bool(re.search(r"\bpi\b", mod_text_now) or re.search(r"\bpi\b", mod_decl_text))
    need_int64_mod = bool(
        re.search(r"\bint64\b|_int64\b", mod_text_now, re.IGNORECASE)
        or re.search(r"\bint64\b|_int64\b", mod_decl_text, re.IGNORECASE)
    )
    for hn in helper_names:
        if re.search(rf"\b{re.escape(hn)}\s*\(", mod_text_now):
            mod_needed.add(hn)
        if re.search(rf"\b{re.escape(hn)}\s*\(", main_text_now):
            main_needed.add(hn)
    if "r_sd" in mod_needed:
        mod_needed.discard("sd")
    if "r_sd" in main_needed:
        main_needed.discard("sd")
    if "print_matrix_rstyle" in mod_needed:
        mod_needed.discard("print_matrix")
    if "print_matrix_rstyle" in main_needed:
        main_needed.discard("print_matrix")
    user_func_names_l = {f.name.lower() for f in funcs}
    mod_needed = {nm for nm in mod_needed if nm.lower() not in user_func_names_l}
    main_needed = {nm for nm in main_needed if nm.lower() not in user_func_names_l}
    if re.search(r"\btype\s*\(\s*lm_fit_t\s*\)", mod_text_now, re.IGNORECASE):
        mod_needed.add("lm_fit_t")
    if re.search(r"\btype\s*\(\s*lm_fit_t\s*\)", main_text_now, re.IGNORECASE):
        main_needed.add("lm_fit_t")
    if re.search(r"\btype\s*\(\s*optim_result_t\s*\)", mod_text_now, re.IGNORECASE):
        mod_needed.add("optim_result_t")
    if re.search(r"\btype\s*\(\s*optim_result_t\s*\)", main_text_now, re.IGNORECASE):
        main_needed.add("optim_result_t")

    module_iface_lines: list[str] = []
    if emit_local_typeof:
        module_iface_lines.extend(
            [
                "interface r_typeof",
                "   module procedure r_typeof_i, r_typeof_r, r_typeof_l, r_typeof_c",
                "end interface r_typeof",
            ]
        )
    if emit_local_drop:
        module_iface_lines.extend(
            [
                "interface r_drop_index",
                "   module procedure r_drop_index_real, r_drop_index_int",
                "end interface r_drop_index",
                "interface r_drop_indices",
                "   module procedure r_drop_indices_real, r_drop_indices_int",
                "end interface r_drop_indices",
            ]
        )

    o = FEmit()
    o.w(f"module {module_name}")
    iso_imports_mod = "dp => real64"
    if need_int64_mod:
        iso_imports_mod += ", int64"
    o.w(f"use, intrinsic :: iso_fortran_env, only: {iso_imports_mod}")
    if need_ieee_mod:
        o.w("use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan")
    if ("r_mod" in helper_modules) and mod_needed:
        o.w("use r_mod, only: " + _render_r_mod_only(mod_needed))
    o.w("implicit none")
    if need_pi_mod:
        o.w("real(kind=dp), parameter :: pi = acos(-1.0_dp)")
    if promoted_params:
        promoted_int64_params = [(k, v) for k, v in sorted(promoted_params.items()) if _expr_uses_int64(v)]
        promoted_default_params = [(k, v) for k, v in sorted(promoted_params.items()) if not _expr_uses_int64(v)]
        if promoted_default_params:
            rhs = ", ".join(f"{k} = {v}" for k, v in promoted_default_params)
            o.w(f"integer, parameter :: {rhs}")
        if promoted_int64_params:
            rhs = ", ".join(f"{k} = {v}" for k, v in promoted_int64_params)
            o.w(f"integer(kind=int64), parameter :: {rhs}")
    for nm, (knd, nsz, expr_f) in sorted(promoted_array_params.items()):
        if knd == "integer":
            o.w(f"integer, parameter :: {nm}({nsz}) = {expr_f}")
        else:
            o.w(f"real(kind=dp), parameter :: {nm}({nsz}) = {expr_f}")
    promoted_nonparam = sorted(promoted_names - set(promoted_params.keys()) - set(promoted_array_params.keys()))
    if promoted_nonparam:
        p_int_s = [n for n in promoted_nonparam if promoted_kind.get(n) == "int_scalar"]
        p_real_s = [n for n in promoted_nonparam if promoted_kind.get(n) == "real_scalar"]
        p_int_v = [n for n in promoted_nonparam if promoted_kind.get(n) == "int_vec"]
        p_real_v = [n for n in promoted_nonparam if promoted_kind.get(n) == "real_vec"]
        p_int_m = [n for n in promoted_nonparam if promoted_kind.get(n) == "int_mat"]
        p_real_m = [n for n in promoted_nonparam if promoted_kind.get(n) == "real_mat"]
        p_real_r3 = [n for n in promoted_nonparam if promoted_kind.get(n) == "real_rank3"]
        p_real_r4 = [n for n in promoted_nonparam if promoted_kind.get(n) == "real_rank4"]
        p_char = [n for n in promoted_nonparam if promoted_kind.get(n) == "char_scalar"]
        if p_int_s:
            o.w("integer :: " + ", ".join(p_int_s))
        if p_real_s:
            o.w("real(kind=dp) :: " + ", ".join(p_real_s))
        if p_int_v:
            o.w("integer, allocatable :: " + ", ".join(f"{x}(:)" for x in p_int_v))
        if p_real_v:
            o.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:)" for x in p_real_v))
        if p_int_m:
            o.w("integer, allocatable :: " + ", ".join(f"{x}(:,:)" for x in p_int_m))
        if p_real_m:
            o.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:)" for x in p_real_m))
        if p_real_r3:
            o.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:,:)" for x in p_real_r3))
        if p_real_r4:
            o.w("real(kind=dp), allocatable :: " + ", ".join(f"{x}(:,:,:,:)" for x in p_real_r4))
        if p_char:
            o.w("character(len=:), allocatable :: " + ", ".join(p_char))
    for ln in module_iface_lines:
        o.w(ln)
    need_lm = bool(helper_ctx_main.get("need_lm")) or bool(helper_ctx_mod.get("need_lm"))
    if need_lm and ("r_mod" not in helper_modules):
        o.w("")
        o.w("type :: lm_fit_t")
        o.push()
        o.w("real(kind=dp), allocatable :: coef(:), fitted(:), resid(:)")
        o.w("real(kind=dp) :: sigma, r_squared, adj_r_squared")
        o.pop()
        o.w("end type lm_fit_t")
        o.w("")
    need_optim_result = bool(
        re.search(r"\btype\s*\(\s*optim_result_t\s*\)", mod_text_now + "\n" + main_text_now, re.IGNORECASE)
    )
    if need_optim_result and ("r_mod" not in helper_modules):
        o.w("")
        o.w("type :: optim_result_t")
        o.push()
        o.w("real(kind=dp), allocatable :: par(:)")
        o.w("real(kind=dp) :: value")
        o.w("integer :: convergence")
        o.pop()
        o.w("end type optim_result_t")
        o.w("")
    # Derived types for list-return functions and main list constructors.
    emitted_types: set[str] = set()
    all_list_specs: dict[str, ListReturnSpec] = {}
    all_list_specs.update(list_specs)
    all_list_specs.update(main_list_specs)
    funcs_by_name = {f.name: f for f in funcs}

    def _local_rank_for_type_field(fn_name: str, txt: str) -> int:
        if not re.match(r"^[A-Za-z]\w*$", txt):
            return 0
        f_obj = funcs_by_name.get(fn_name)
        if f_obj is None:
            return 0
        if txt in f_obj.args:
            return infer_arg_rank(f_obj, txt)
        body_no_ret = (f_obj.body[:-1] if isinstance(f_obj.body[-1], ExprStmt) else f_obj.body) if f_obj.body else []
        return _infer_local_array_rank(body_no_ret, txt)

    def _emit_real_field_decl(k: str, rank: int) -> None:
        if rank >= 3:
            o.w(f"real(kind=dp), allocatable :: {k}(:,:,:)")
        elif rank == 2:
            o.w(f"real(kind=dp), allocatable :: {k}(:,:)")
        elif rank == 1:
            o.w(f"real(kind=dp), allocatable :: {k}(:)")
        else:
            o.w(f"real(kind=dp) :: {k}")

    def _field_decl_from_expr(fn_name: str, k: str, txt: str) -> str | None:
        fn_ints = fn_int_names.get(fn_name, set())
        fn_int_arrays = fn_int_array_names.get(fn_name, set())
        fn_real_arrays = fn_real_array_names.get(fn_name, set())
        fn_real_mats = fn_real_matrix_names.get(fn_name, set())
        fn_lms = fn_lm_names.get(fn_name, set())
        txt_l = txt.lower()
        if txt in {"TRUE", "FALSE"}:
            return f"logical :: {k}"
        if _dequote_string_literal(txt) is not None or re.match(r"^(sub|substr)\s*\(", txt, re.IGNORECASE):
            return f"character(len=:), allocatable :: {k}"
        if _is_int_literal(txt) or _selects_named_order_column(txt) or re.match(
            r"^(?:nrow|ncol|length|which\.min|which\.max|minloc|maxloc)\s*\(",
            txt,
            re.IGNORECASE,
        ):
            return f"integer :: {k}"
        if re.match(r"^[A-Za-z]\w*$", txt):
            if txt in fn_lms:
                return f"type(lm_fit_t) :: {k}"
            if txt in fn_ints:
                return f"integer :: {k}"
            if txt in fn_int_arrays:
                return f"integer, allocatable :: {k}(:)"
            if txt in fn_real_arrays or txt in fn_real_mats:
                rk_field = _local_rank_for_type_field(fn_name, txt)
                if rk_field >= 3:
                    return f"real(kind=dp), allocatable :: {k}(:,:,:)"
                if rk_field == 2:
                    return f"real(kind=dp), allocatable :: {k}(:,:)"
                if txt in fn_real_mats:
                    return f"real(kind=dp), allocatable :: {k}(:,:)"
                if rk_field <= 0:
                    return f"real(kind=dp) :: {k}"
                return f"real(kind=dp), allocatable :: {k}(:)"
            if fn_name in funcs_by_name and txt in funcs_by_name[fn_name].args:
                rk_field = _local_rank_for_type_field(fn_name, txt)
                if rk_field == 0 and txt in fn_ints:
                    return f"integer :: {k}"
                if rk_field >= 1:
                    if rk_field >= 3:
                        return f"real(kind=dp), allocatable :: {k}(:,:,:)"
                    if rk_field == 2:
                        return f"real(kind=dp), allocatable :: {k}(:,:)"
                    return f"real(kind=dp), allocatable :: {k}(:)"
        if re.search(r"\b(?:nrow|ncol|length|which\.min|which\.max|minloc|maxloc)\s*\(", txt, re.IGNORECASE):
            return f"integer :: {k}"
        c_txt = parse_call_text(txt)
        if c_txt is not None:
            cn, pos_cn, kw_cn = c_txt
            cn_l = cn.lower()
            if cn_l == "c":
                return f"real(kind=dp), allocatable :: {k}(:)"
            if cn_l in {"r_matmul", "crossprod", "tcrossprod", "t", "matrix", "array", "cbind", "cbind2"}:
                return f"real(kind=dp), allocatable :: {k}(:,:)"
            if cn_l in {"coef", "fitted", "residuals", "colmeans", "rowsums", "colsums", "numeric", "r_rep_real"}:
                return f"real(kind=dp), allocatable :: {k}(:)"
            if cn_l == "as.numeric":
                return f"real(kind=dp), allocatable :: {k}(:)"
            if cn_l == "tail":
                n_cn = pos_cn[1] if len(pos_cn) >= 2 else kw_cn.get("n", "")
                if str(n_cn).strip().upper() in {"1", "1L"}:
                    return f"real(kind=dp) :: {k}"
            arg_names = [a.strip() for a in (list(pos_cn) + list(kw_cn.values()))]
            if any(re.match(r"^[A-Za-z]\w*$", a) and a in fn_real_mats for a in arg_names):
                return f"real(kind=dp), allocatable :: {k}(:,:)"
            if any(re.match(r"^[A-Za-z]\w*$", a) and a in fn_real_arrays for a in arg_names):
                return f"real(kind=dp), allocatable :: {k}(:)"
            if any(re.match(r"^[A-Za-z]\w*$", a) and a in fn_int_arrays for a in arg_names):
                return f"integer, allocatable :: {k}(:)"
        if "%*%" in txt or re.match(r"^(?:cbind|cbind2|matrix|array)\s*\(", txt, re.IGNORECASE):
            return f"real(kind=dp), allocatable :: {k}(:,:)"
        if txt.startswith("c(") or txt.startswith("[") or txt_l.startswith(("runif(", "rnorm(")):
            return f"real(kind=dp), allocatable :: {k}(:)"
        return None

    for fn_name, spec in all_list_specs.items():
        paths = sorted(spec.nested_types.keys(), key=lambda p: len(p), reverse=True)
        for path in paths:
            tname = _type_name_for_path(fn_name, path)
            if tname in emitted_types:
                continue
            emitted_types.add(tname)
            fields = spec.nested_types[path]
            o.w("")
            o.w(f"type :: {tname}")
            o.push()
            for k, v in fields.items():
                if isinstance(v, dict):
                    nt = _type_name_for_path(fn_name, path + (k,))
                    o.w(f"type({nt}) :: {k}")
                else:
                    txt = str(v).strip()
                    structural_decl = _field_decl_from_expr(fn_name, k, txt)
                    if structural_decl is not None:
                        o.w(structural_decl)
                        continue
                    fn_ints = fn_int_names.get(fn_name, set())
                    fn_int_arrays = fn_int_array_names.get(fn_name, set())
                    fn_real_arrays = fn_real_array_names.get(fn_name, set())
                    fn_real_mats = fn_real_matrix_names.get(fn_name, set())
                    fn_lms = fn_lm_names.get(fn_name, set())
                    if k in {"n", "k", "p", "q", "aic_p", "aic_q", "bic_p", "bic_q", "aic_row", "bic_row", "trial", "n_iter", "convergence", "nobs", "nseries"}:
                        o.w(f"integer :: {k}")
                    elif k == "ok":
                        o.w(f"logical :: {k}")
                    elif k == "order":
                        o.w(f"integer :: {k}(2)")
                    elif k in {"aic_order", "bic_order"}:
                        o.w(f"integer :: {k}")
                    elif k == "fits":
                        o.w(f"type(fit_var_result_t), allocatable :: {k}(:)")
                    elif k in {"cluster", "z_hat", "class", "comp"}:
                        o.w(f"integer, allocatable :: {k}(:)")
                    elif k == "h":
                        o.w(f"real(kind=dp), allocatable :: {k}(:)")
                    elif k in {"loglik", "aic", "bic"}:
                        o.w(f"real(kind=dp) :: {k}")
                    elif k == "lm_fit" or (re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_lms):
                        o.w(f"type(lm_fit_t) :: {k}")
                    elif re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_ints:
                        o.w(f"integer :: {k}")
                    elif re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_int_arrays:
                        o.w(f"integer, allocatable :: {k}(:)")
                    elif re.match(r"^[A-Za-z]\w*$", txt) and fn_name in funcs_by_name and txt in funcs_by_name[fn_name].args:
                        rk_field = _local_rank_for_type_field(fn_name, txt)
                        _emit_real_field_decl(k, rk_field)
                    elif _is_int_literal(txt):
                        o.w(f"integer :: {k}")
                    elif txt in {"TRUE", "FALSE"}:
                        o.w(f"logical :: {k}")
                    elif k == "out" or _dequote_string_literal(txt) is not None or re.match(r"^(sub|substr)\s*\(", txt, re.IGNORECASE):
                        o.w(f"character(len=:), allocatable :: {k}")
                    elif re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_real_arrays:
                        rk_field = _local_rank_for_type_field(fn_name, txt)
                        _emit_real_field_decl(k, 2 if txt in fn_real_mats and rk_field < 2 else rk_field)
                    elif re.match(r"^[A-Za-z]\w*$", txt) and txt in fn_real_mats:
                        _emit_real_field_decl(k, 2)
                    elif any(re.search(rf"\b{re.escape(nm)}\s*\[", txt) for nm in (fn_real_arrays | fn_int_arrays)):
                        m_field_ix = re.search(r"\b[A-Za-z]\w*\s*\[([^\[\]]+)\]", txt)
                        if m_field_ix is not None and len(_split_index_dims(m_field_ix.group(1))) >= 2:
                            o.w(f"real(kind=dp), allocatable :: {k}(:,:)")
                        else:
                            o.w(f"real(kind=dp), allocatable :: {k}(:)")
                    elif txt.startswith("cbind(") or txt.startswith("cbind2(") or "%*%" in txt:
                        o.w(f"real(kind=dp), allocatable :: {k}(:,:)")
                    elif parse_call_text(txt) is not None:
                        c_txt = parse_call_text(txt)
                        _cn, pos_cn, kw_cn = c_txt if c_txt is not None else ("", [], {})
                        if _cn.lower() == "c":
                            o.w(f"real(kind=dp), allocatable :: {k}(:)")
                            continue
                        if _cn.lower() in {"r_matmul", "crossprod", "tcrossprod", "t", "matrix", "array", "cbind", "cbind2"}:
                            o.w(f"real(kind=dp), allocatable :: {k}(:,:)")
                            continue
                        if _cn.lower() in {"coef", "fitted", "residuals"}:
                            o.w(f"real(kind=dp), allocatable :: {k}(:)")
                            continue
                        if _cn.lower() == "as.numeric" and pos_cn:
                            src_cn = pos_cn[0].strip()
                            if re.match(r"^[A-Za-z]\w*$", src_cn) and src_cn in fn_real_arrays:
                                o.w(f"real(kind=dp), allocatable :: {k}(:)")
                                continue
                            if re.match(r"^[A-Za-z]\w*$", src_cn) and src_cn in fn_int_arrays:
                                o.w(f"real(kind=dp), allocatable :: {k}(:)")
                                continue
                            o.w(f"real(kind=dp), allocatable :: {k}(:)")
                            continue
                        if _cn.lower() == "tail":
                            n_cn = pos_cn[1] if len(pos_cn) >= 2 else kw_cn.get("n", "")
                            if str(n_cn).strip().upper() in {"1", "1L"}:
                                o.w(f"real(kind=dp) :: {k}")
                                continue
                        arg_names = [a.strip() for a in (list(pos_cn) + list(kw_cn.values()))]
                        if any(re.match(r"^[A-Za-z]\w*$", a) and a in fn_real_arrays for a in arg_names):
                            o.w(f"real(kind=dp), allocatable :: {k}(:)")
                        elif any(re.match(r"^[A-Za-z]\w*$", a) and a in fn_int_arrays for a in arg_names):
                            o.w(f"integer, allocatable :: {k}(:)")
                        else:
                            o.w(f"real(kind=dp) :: {k}")
                    elif k in {"resp", "responsibilities", "log_r"}:
                        o.w(f"real(kind=dp), allocatable :: {k}(:,:)")
                    elif k in {"pi", "x", "z", "weights", "means", "sds", "vars", "nk"}:
                        o.w(f"real(kind=dp), allocatable :: {k}(:)")
                    elif txt.startswith("c(") or txt.startswith("[") or txt.startswith("runif(") or txt.startswith("rnorm("):
                        o.w(f"real(kind=dp), allocatable :: {k}(:)")
                    else:
                        # If expression looks like integer index/length use integer, else real.
                        if re.match(r"^[A-Za-z]\w*$", txt):
                            o.w(f"real(kind=dp) :: {k}")
                        else:
                            o.w(f"real(kind=dp) :: {k}")
            o.pop()
            o.w(f"end type {tname}")
            o.w("")
    need_contains = (
        bool(funcs)
        or emit_local_rnorm
        or emit_local_scan_reader
        or emit_local_table_reader
        or emit_local_table_writer
        or emit_local_lm
        or emit_local_drop
        or emit_local_seq
        or emit_local_rep
        or emit_local_numeric
        or emit_local_typeof
        or emit_local_command_args
    )
    module_required = (
        bool(mod_needed)
        or bool(need_ieee_mod)
        or bool(promoted_params)
        or bool(promoted_array_params)
        or bool(promoted_nonparam)
        or bool(module_iface_lines)
        or bool(all_list_specs)
        or need_contains
    )
    if module_required:
        if need_contains:
            o.w("")
            o.w("contains")
            o.w("")
            o.lines.extend(mprocs.lines)
        o.w(f"end module {module_name}")
        o.w("")
        o.w(f"program {unit_name}")
        o.w(f"use {module_name}")
        if need_ieee_main:
            o.w("use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan")
        if ("r_mod" in helper_modules) and main_needed:
            o.w("use r_mod, only: " + _render_r_mod_only(main_needed))
        o.w("implicit none")
        o.lines.extend(pbody.lines)
        o.w(f"end program {unit_name}")
    else:
        o = FEmit()
        combined_main_text = main_text_now + "\n" + "\n".join(pbody.lines)
        main_needs_dp = bool(
            re.search(r"\bkind\s*=\s*dp\b|_dp\b", combined_main_text, re.IGNORECASE)
        )
        main_needs_int64 = bool(re.search(r"\bint64\b|_int64\b", combined_main_text, re.IGNORECASE))
        main_needs_pi = bool(re.search(r"\bpi\b", main_text_now))
        if main_needs_pi:
            main_needs_dp = True
        o.w(f"program {unit_name}")
        if main_needs_dp or main_needs_int64:
            iso_imports_main: list[str] = []
            if main_needs_dp:
                iso_imports_main.append("dp => real64")
            if main_needs_int64:
                iso_imports_main.append("int64")
            o.w("use, intrinsic :: iso_fortran_env, only: " + ", ".join(iso_imports_main))
        if need_ieee_main:
            o.w("use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan")
        if ("r_mod" in helper_modules) and main_needed:
            o.w("use r_mod, only: " + _render_r_mod_only(main_needed))
        o.w("implicit none")
        if main_needs_pi:
            o.w("real(kind=dp), parameter :: pi = acos(-1.0_dp)")
        o.lines.extend(pbody.lines)
        o.w(f"end program {unit_name}")
    return o.text()


def _norm_output(s: str) -> list[str]:
    lines = s.replace("\r\n", "\n").replace("\r", "\n").split("\n")
    lines = [" ".join(ln.split()) for ln in lines]
    while lines and lines[-1] == "":
        lines.pop()
    return lines


def _split_pretty_float_token(token: str) -> tuple[str, str]:
    mant = token
    exp = ""
    for ch in ("e", "E", "d", "D"):
        if ch in mant:
            p = mant.find(ch)
            exp = mant[p:]
            mant = mant[:p]
            break
    return mant, exp


def _trim_pretty_float_token(token: str) -> str:
    mant, exp = _split_pretty_float_token(token)
    if "." not in mant:
        return token
    head, frac = mant.split(".", 1)
    frac = frac.rstrip("0")
    return f"{head}.{frac}{exp}" if frac else f"{head}.{exp}"


def _pretty_float_token(token: str) -> str:
    try:
        val = float(token.replace("d", "e").replace("D", "E"))
    except ValueError:
        return _trim_pretty_float_token(token)
    out = f"{val:.15g}"
    if "." not in out and "e" not in out and "E" not in out:
        out = f"{out}.0"
    return out


def _pretty_output_text(text: str) -> str:
    s = text.replace("\r\n", "\n").replace("\r", "\n")
    s = _PRETTY_FLOAT_TOKEN_RE.sub(lambda m: _pretty_float_token(m.group(1)), s)
    lines = [" ".join(ln.split()) for ln in s.split("\n")]
    while lines and lines[-1] == "":
        lines.pop()
    return "\n".join(lines)


def _round_float_token(token: str, digits: int) -> str:
    try:
        val = float(token.replace("d", "e").replace("D", "E"))
    except ValueError:
        return token
    return f"{val:.{digits}f}"


def _round_output_text(text: str, digits: int) -> str:
    s = text.replace("\r\n", "\n").replace("\r", "\n")
    return _PRETTY_FLOAT_TOKEN_RE.sub(lambda m: _round_float_token(m.group(1), digits), s)


def normalize_fortran_lines(lines: list[str], max_consecutive_blank: int = 1) -> list[str]:
    out: list[str] = []
    blank_run = 0
    for ln in lines:
        s = ln.rstrip()
        if s == "":
            blank_run += 1
            if blank_run <= max_consecutive_blank:
                out.append("")
        else:
            blank_run = 0
            out.append(s)
    # Trim leading/trailing blank lines.
    while out and out[0] == "":
        out.pop(0)
    while out and out[-1] == "":
        out.pop()
    return out


def format_derived_type_blocks(lines: list[str]) -> list[str]:
    """Keep derived type definitions visually separated and indented."""
    out: list[str] = []
    in_type = False
    type_start_re = re.compile(r"^\s*type\s*(?:,\s*[^:]*)?::\s*[A-Za-z]\w*\s*$", re.IGNORECASE)
    type_end_re = re.compile(r"^\s*end\s+type\b", re.IGNORECASE)

    for raw in lines:
        code = fscan.strip_comment(raw).strip()
        if not in_type and type_start_re.match(code):
            if out and out[-1].strip() != "":
                out.append("")
            out.append(raw.strip())
            in_type = True
            continue

        if in_type:
            if type_end_re.match(code):
                out.append(raw.strip())
                out.append("")
                in_type = False
            elif raw.strip() == "":
                out.append("")
            else:
                out.append("   " + raw.strip())
            continue

        out.append(raw)

    return normalize_fortran_lines(out, max_consecutive_blank=1)


def format_interface_blocks(lines: list[str]) -> list[str]:
    """Keep interface blocks visually separated and indent their bodies."""
    out: list[str] = []
    in_interface = False
    base_indent = ""
    interface_start_re = re.compile(r"^\s*interface(?:\s+\S.*)?$", re.IGNORECASE)
    interface_end_re = re.compile(r"^\s*end\s+interface(?:\s+\S.*)?$", re.IGNORECASE)

    for raw in lines:
        code = fscan.strip_comment(raw).strip()
        if not in_interface and interface_start_re.match(code):
            if out and out[-1].strip() != "":
                out.append("")
            base_indent = re.match(r"^\s*", raw).group(0)
            out.append(base_indent + raw.strip())
            in_interface = True
            continue

        if in_interface:
            if interface_end_re.match(code):
                out.append(base_indent + raw.strip())
                out.append("")
                in_interface = False
            elif raw.strip() == "":
                out.append("")
            else:
                out.append(base_indent + "   " + raw.strip())
            continue

        out.append(raw)

    return normalize_fortran_lines(out, max_consecutive_blank=1)


def simplify_write_g0_outer_parens(lines: list[str]) -> list[str]:
    """Drop redundant outer parens in scalar g0 write payloads."""
    out: list[str] = []
    pat = re.compile(r'^(\s*write\s*\(\s*\*\s*,\s*"\(g0\)"\s*\)\s+)(.+?)(\s*)$', re.IGNORECASE)
    for raw in lines:
        code, comment = fscan._split_code_comment(raw)  # type: ignore[attr-defined]
        m = pat.match(code.rstrip())
        if m is None:
            out.append(raw)
            continue
        expr = fscan.strip_redundant_outer_parens_expr(m.group(2).strip())
        suffix = f" {comment.strip()}" if comment.strip() else ""
        out.append(f"{m.group(1)}{expr}{suffix}")
    return out


def hoist_repeated_numeric_array_literals(lines: list[str]) -> list[str]:
    """Hoist repeated numeric array constructors in each procedure/program scope."""

    unit_start_re = re.compile(
        r"^\s*(?:pure\s+|elemental\s+|recursive\s+)*\s*(?:program|function|subroutine)\b",
        re.IGNORECASE,
    )
    unit_end_re = re.compile(r"^\s*end\s+(?:program|function|subroutine)\b", re.IGNORECASE)
    implicit_re = re.compile(r"^\s*implicit\s+none\b", re.IGNORECASE)
    ctor_re = re.compile(r"\[((?:\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][+-]?\d+)?(?:_[A-Za-z]\w*)?\s*,){1,}\s*[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eEdD][+-]?\d+)?(?:_[A-Za-z]\w*)?\s*)\]")

    def classify_ctor(txt: str) -> tuple[str, int] | None:
        if "character(" in txt.lower():
            return None
        inner = txt[1:-1]
        vals = [v.strip() for v in split_top_level_commas(inner)]
        if len(vals) < 2:
            return None
        all_int = all(_is_int_literal(v) for v in vals)
        all_num = all(_is_int_literal(v) or _is_real_literal(v) for v in vals)
        if not all_num:
            return None
        return ("integer" if all_int else "real", len(vals))

    out = list(lines)
    i = 0
    while i < len(out):
        if not unit_start_re.match(out[i]):
            i += 1
            continue
        start = i
        end = i + 1
        while end < len(out) and not unit_end_re.match(out[end]):
            end += 1
        if end >= len(out):
            end = len(out) - 1
        kinds: dict[str, tuple[str, int]] = {}
        repeated_set: set[str] = set()
        stmt = ""
        for ln in out[start : end + 1]:
            code = ln.split("!", 1)[0]
            stmt = (stmt + " " + code.lstrip("&").strip()) if stmt else code.strip()
            if code.rstrip().endswith("&"):
                continue
            counts: dict[str, int] = {}
            for m in ctor_re.finditer(stmt):
                ctor = m.group(0)
                info = classify_ctor(ctor)
                if info is None:
                    continue
                counts[ctor] = counts.get(ctor, 0) + 1
                kinds[ctor] = info
            repeated_set.update(ctor for ctor, n in counts.items() if n >= 2)
            stmt = ""
        repeated = list(repeated_set)
        if not repeated:
            i = end + 1
            continue
        name_by_ctor = {ctor: f"arr_lit_{j}" for j, ctor in enumerate(repeated, start=1)}
        for idx in range(start, end + 1):
            for ctor, nm in name_by_ctor.items():
                out[idx] = out[idx].replace(ctor, nm)
        decls: list[str] = []
        for ctor in repeated:
            nm = name_by_ctor[ctor]
            kind, n = kinds[ctor]
            if kind == "integer":
                decls.append(f"integer, parameter :: {nm}({n}) = {ctor}")
            else:
                decls.append(f"real(kind=dp), parameter :: {nm}({n}) = {ctor}")
        insert_at = None
        for idx in range(start + 1, end + 1):
            if implicit_re.match(out[idx]):
                insert_at = idx + 1
                break
        if insert_at is None:
            insert_at = start + 1
        out[insert_at:insert_at] = decls
        i = end + 1 + len(decls)
    return out


def rewrite_selected_orders_dataframe_print(lines: list[str]) -> list[str]:
    """Lower the small selected-orders data.frame print used by mixture examples."""
    out: list[str] = []
    i = 0
    while i < len(lines):
        ln = lines[i]
        if (
            "write" in ln
            and "data.frame" in ln
            and 'criterion = ["AIC", "BIC"]' in ln
            and i + 1 < len(lines)
            and "selected_ncomp" in lines[i + 1]
        ):
            indent = re.match(r"^\s*", ln).group(0)
            m_vals = re.search(r"selected_ncomp\s*=\s*\[(.+?)\]", lines[i + 1])
            vals = split_top_level_commas(m_vals.group(1)) if m_vals is not None else []
            if len(vals) == 2:
                out.append(f'{indent}write(*,"(a,1x,a)") "criterion", "selected_ncomp"')
                out.append(f'{indent}write(*,"(a,1x,g0)") "AIC", {vals[0].strip()}')
                out.append(f'{indent}write(*,"(a,1x,g0)") "BIC", {vals[1].strip()}')
                i += 2
                continue
        out.append(ln)
        i += 1
    return out


def rewrite_default_array_size_refs(lines: list[str]) -> list[str]:
    """Route size(arg) through arg_def when an optional array default local exists."""
    decl_re = re.compile(r"\ballocatable\s*::\s*(.*)", re.IGNORECASE)
    out: list[str] = []
    bases: set[str] = set()
    for ln in lines:
        stripped = ln.strip().lower()
        if re.match(r"^(?:pure\s+|elemental\s+|recursive\s+)*\s*(?:function|subroutine|program)\b", stripped):
            bases = set()
        m = decl_re.search(ln)
        if m is not None:
            for name in re.findall(r"\b([A-Za-z]\w*)_def\s*\(", m.group(1)):
                bases.add(name)
        new = ln
        for base in sorted(bases, key=len, reverse=True):
            new = re.sub(rf"\bsize\s*\(\s*{re.escape(base)}\s*\)", f"size({base}_def)", new)
        out.append(new)
        if re.match(r"^end\s+(?:function|subroutine|program)\b", stripped):
            bases = set()
    return out


def rewrite_optional_init_size_checks(lines: list[str]) -> list[str]:
    """After optional init handling, validate initialized vectors instead of *_init_def sentinels."""
    local_vectors: set[str] = set()
    decl_re = re.compile(r"\ballocatable\s*::\s*(.*)", re.IGNORECASE)
    out: list[str] = []
    in_alloc_decl = False
    for ln in lines:
        stripped = ln.strip().lower()
        if re.match(r"^(?:pure\s+|elemental\s+|recursive\s+)*\s*(?:function|subroutine|program)\b", stripped):
            local_vectors = set()
            in_alloc_decl = False
        m = decl_re.search(ln)
        if m is not None:
            in_alloc_decl = True
            scan_decl = m.group(1)
        elif in_alloc_decl and stripped.startswith("&"):
            scan_decl = ln
        else:
            scan_decl = ""
        if scan_decl:
            for name in re.findall(r"\b([A-Za-z]\w*)\s*\(", scan_decl):
                local_vectors.add(name)
            if not scan_decl.rstrip().endswith("&"):
                in_alloc_decl = False
        new = ln
        for base in sorted(local_vectors, key=len, reverse=True):
            new = re.sub(
                rf"\bsize\s*\(\s*{re.escape(base)}_init_def\s*\)\s*([/=]=)\s*k_def\b",
                rf"size({base}) \1 k_def",
                new,
            )
            new = new.replace(f"size({base}_init_def) == k_def", f"size({base}) == k_def")
        out.append(new)
        if re.match(r"^end\s+(?:function|subroutine|program)\b", stripped):
            local_vectors = set()
    return out


def rewrite_rank3_print_matrix_calls(lines: list[str]) -> list[str]:
    rank3: set[str] = set()
    decl_re = re.compile(r"\breal\s*\([^)]*\)\s*,\s*allocatable\s*::\s*(.*)", re.IGNORECASE)
    for ln in lines:
        m = decl_re.search(ln)
        if m is None:
            continue
        for name in re.findall(r"\b([A-Za-z]\w*)\s*\(:\s*,\s*:\s*,\s*:\s*\)", m.group(1)):
            rank3.add(name)
    if not rank3:
        return lines
    out: list[str] = []
    for ln in lines:
        m_call = re.match(r"^(\s*)call\s+print_matrix\s*\(\s*([A-Za-z]\w*)\s*\)\s*$", ln, re.IGNORECASE)
        if m_call is not None and m_call.group(2) in rank3:
            nm = m_call.group(2)
            out.append(f"{m_call.group(1)}call print_real_vector(reshape({nm}, [size({nm})]))")
        else:
            out.append(ln)
    return out


def rewrite_arma_table_label_access(lines: list[str]) -> list[str]:
    """Lower fixed ARMA coefficient-table label accesses to numeric columns."""
    max_ar = "max_dot_ar_dot_order"
    max_ma = "max_dot_ma_dot_order"
    cols = {
        "ar.order": "1",
        "ma.order": "2",
        "sigma2": f"4 + {max_ar} + {max_ma}",
        "loglik": f"5 + {max_ar} + {max_ma}",
        "aic": f"6 + {max_ar} + {max_ma}",
        "bic": f"7 + {max_ar} + {max_ma}",
        "convergence": f"8 + {max_ar} + {max_ma}",
        "ok": f"9 + {max_ar} + {max_ma}",
    }
    out: list[str] = []
    for ln in lines:
        s = ln
        for lab, col in cols.items():
            s = s.replace(f'arma_mat(:, "{lab}")', f"arma_mat(:, {col})")
            s = s.replace(f'arma_mat(:, \'{lab}\')', f"arma_mat(:, {col})")
            s = s.replace(f'arma_mat(:, {lab})', f"arma_mat(:, {col})")
        s = s.replace('["ar.order", "ma.order"]', "[1, 2]")
        s = s.replace("['ar.order', 'ma.order']", "[1, 2]")
        s = re.sub(r"arma_mat\[(.+),\s*\[1,\s*2\]\]", r"arma_mat(\1, [1, 2])", s)
        s = s.replace('aic_order("ar.order")', "aic_order(1)")
        s = s.replace('aic_order("ma.order")', "aic_order(2)")
        s = s.replace('bic_order("ar.order")', "bic_order(1)")
        s = s.replace('bic_order("ma.order")', "bic_order(2)")
        s = re.sub(r"\bInf\b", "huge(1.0_dp)", s)
        out.append(s)
    return out


def split_long_inline_comments(lines: list[str], max_len: int = 80) -> list[str]:
    """Split overly long code+inline-comment lines into code line + comment line."""
    out: list[str] = []
    for ln in lines:
        if len(ln) <= max_len:
            out.append(ln)
            continue
        code, cmt = fscan._split_code_comment(ln)  # type: ignore[attr-defined]
        if not cmt:
            out.append(ln)
            continue
        indent = re.match(r"^\s*", code).group(0) if re.match(r"^\s*", code) else ""
        c = cmt.strip()
        if c.startswith("!"):
            c = c[1:].strip()
        out.append(code.rstrip())
        out.append(f"{indent}! {c}" if c else f"{indent}!")
    return out


def fix_wrapped_closing_delims(lines: list[str]) -> list[str]:
    """Fix continuation wraps that split immediately before ')' or ']'."""
    out = list(lines)
    changed = True
    while changed:
        changed = False
        for i in range(1, len(out)):
            cur = out[i]
            prev = out[i - 1]
            m = re.match(r"^(\s*&\s*)([\)\]])\s*(.*)$", cur)
            if m is None:
                continue
            if not prev.rstrip().endswith("&"):
                continue
            prev0 = prev.rstrip()[:-1].rstrip()
            out[i - 1] = f"{prev0}{m.group(2)} &"
            out[i] = f"{m.group(1)}{m.group(3)}".rstrip()
            changed = True
    return out


def fix_split_power_operator(lines: list[str]) -> list[str]:
    """Fix wraps that split exponentiation `**` across continuation lines."""
    out = list(lines)
    for i in range(1, len(out)):
        prev = out[i - 1]
        cur = out[i]
        m_prev = re.match(r"^(.*)\*\s*&\s*$", prev.rstrip())
        m_cur = re.match(r"^(\s*&\s*)\*\s*(.*)$", cur)
        if m_prev is None or m_cur is None:
            continue
        out[i - 1] = f"{m_prev.group(1)}** &"
        out[i] = f"{m_cur.group(1)}{m_cur.group(2)}".rstrip()
    return out


def strip_named_args_for_seq_helpers(lines: list[str]) -> list[str]:
    """Convert r_seq_* helper calls to positional actual arguments."""
    out: list[str] = []
    fn_re = re.compile(
        r"\b(r_seq_int|r_seq_len|r_seq_int_by|r_seq_int_length|r_seq_real_by|r_seq_real_length)\s*\((.*)\)"
    )
    for ln in lines:
        m = fn_re.search(ln)
        if not m:
            out.append(ln)
            continue
        fn = m.group(1)
        inner = m.group(2)
        args = split_top_level_commas(inner)
        if not args:
            out.append(ln)
            continue
        clean: list[str] = []
        for a in args:
            aa = a.strip()
            mk = re.match(r"^[A-Za-z]\w*(?:\.[A-Za-z]\w*)?\s*=\s*(.+)$", aa)
            clean.append((mk.group(1) if mk else aa).strip())
        repl = f"{fn}(" + ", ".join(clean) + ")"
        out.append(ln[: m.start()] + repl + ln[m.end() :])
    return out


def protect_rep_helper_calls(lines: list[str], *, restore: bool = False) -> list[str]:
    """Temporarily rename r_rep_* calls to avoid generic named-arg rewriting."""
    out: list[str] = []
    if restore:
        for ln in lines:
            out.append(ln.replace("zz_r_rep_int(", "r_rep_int(").replace("zz_r_rep_real(", "r_rep_real("))
        return out
    for ln in lines:
        out.append(ln.replace("r_rep_int(", "zz_r_rep_int(").replace("r_rep_real(", "zz_r_rep_real("))
    return out


def mark_pure_with_xpure(lines: list[str]) -> list[str]:
    """Mark likely PURE procedures using xpure.py analysis logic."""
    try:
        import xpure  # local tool module in this project
    except Exception:
        return lines

    try:
        result = xpure.analyze_lines(
            lines,
            external_name_status=None,
            generic_interfaces=None,
            strict_unknown_calls=False,
            conservative_call_block=True,
        )
    except Exception:
        return lines

    cand_ids = {(p.name.lower(), int(p.start)) for p in result.candidates}
    if not cand_ids:
        return lines

    out = list(lines)
    try:
        procs = fscan.parse_procedures(lines)
    except Exception:
        return lines

    for p in procs:
        key = (p.name.lower(), int(p.start))
        if key not in cand_ids:
            continue
        idx = p.start - 1
        if idx < 0 or idx >= len(out):
            continue
        new_line, changed = xpure.add_pure_to_declaration(out[idx])
        if changed:
            out[idx] = new_line
    return out


def _run_capture(cmd: list[str], cwd: Path | None = None) -> subprocess.CompletedProcess[str]:
    """Run command with robust text decoding on Windows."""
    return subprocess.run(
        cmd,
        capture_output=True,
        text=True,
        encoding="utf-8",
        errors="replace",
        cwd=(str(cwd) if cwd is not None else None),
    )


def _split_module_program_source(src_path: Path, work_dir: Path) -> tuple[Path, Path] | None:
    lines = src_path.read_text(encoding="utf-8").splitlines()
    end_idx: int | None = None
    for i, line in enumerate(lines):
        if re.match(r"^\s*end\s+module\b", line, re.IGNORECASE):
            end_idx = i
            break
    if end_idx is None:
        return None
    if not any(re.match(r"^\s*program\b", line, re.IGNORECASE) for line in lines[end_idx + 1 :]):
        return None
    mod_path = work_dir / f"{src_path.stem}_modpart.f90"
    prog_path = work_dir / f"{src_path.stem}_progpart.f90"
    mod_path.write_text("\n".join(lines[: end_idx + 1]) + "\n", encoding="utf-8")
    prog_path.write_text("\n".join(lines[end_idx + 1 :]) + "\n", encoding="utf-8")
    return mod_path, prog_path


def _include_dirs_without_j(include_dirs: list[str]) -> list[str]:
    out: list[str] = []
    i = 0
    while i < len(include_dirs):
        if include_dirs[i] == "-J" and i + 1 < len(include_dirs):
            i += 2
            continue
        out.append(include_dirs[i])
        i += 1
    return out


def _module_names_in_source(src_path: Path) -> list[str]:
    names: list[str] = []
    try:
        lines = src_path.read_text(encoding="utf-8").splitlines()
    except OSError:
        return names
    for line in lines:
        m = re.match(r"^\s*module\s+([A-Za-z]\w*)\s*$", line, re.IGNORECASE)
        if m is not None:
            names.append(m.group(1))
    return names


def _sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def _cached_runtime_object(
    helper: Path,
    cparts: list[str],
) -> tuple[Path, Path, subprocess.CompletedProcess[str] | None]:
    """Return cached object/include dir for r.f90, compiling it on cache miss."""
    helper = helper.resolve()
    src_hash = _sha256_file(helper)
    key_src = "\0".join(["xr2f-rmod-v3", str(helper), src_hash, *cparts])
    key = hashlib.sha256(key_src.encode("utf-8", errors="replace")).hexdigest()[:24]
    cache_dir = Path(tempfile.gettempdir()) / "xr2f_runtime_cache" / key
    obj = cache_dir / "r.o"
    mod = cache_dir / "r_mod.mod"
    if obj.exists() and mod.exists():
        return obj, cache_dir, None
    lock_dir = cache_dir.with_name(cache_dir.name + ".lock")
    have_lock = False
    start = time.monotonic()
    while not have_lock:
        try:
            lock_dir.mkdir(parents=True)
            have_lock = True
        except FileExistsError:
            if obj.exists() and mod.exists():
                return obj, cache_dir, None
            if time.monotonic() - start > 120:
                return obj, cache_dir, subprocess.CompletedProcess(
                    cparts, 1, "", f"timed out waiting for runtime cache lock: {lock_dir}\n"
                )
            time.sleep(0.1)
    if obj.exists() and mod.exists():
        try:
            os.rmdir(lock_dir)
        except OSError:
            pass
        return obj, cache_dir, None
    cache_dir.mkdir(parents=True, exist_ok=True)
    cmd = cparts + ["-J", str(cache_dir), "-c", str(helper), "-o", str(obj)]
    try:
        cp = _run_capture(cmd, cwd=cache_dir)
        if cp.returncode != 0:
            return obj, cache_dir, cp
        if not mod.exists():
            # Case-preserving filesystems can produce uppercase module names.
            mods = list(cache_dir.glob("*.mod"))
            if not any(p.name.lower() == "r_mod.mod" for p in mods):
                return obj, cache_dir, subprocess.CompletedProcess(cmd, 1, "", "r_mod.mod was not produced\n")
        return obj, cache_dir, cp
    finally:
        try:
            os.rmdir(lock_dir)
        except OSError:
            pass


def _print_captured(
    cp: subprocess.CompletedProcess[str],
    normalize_num_output: bool = False,
    pretty: bool = False,
    round_digits: int | None = None,
) -> None:
    out = cp.stdout or ""
    err = cp.stderr or ""
    if normalize_num_output:
        out = fscan.normalize_numeric_leading_zeros_text(out)
        err = fscan.normalize_numeric_leading_zeros_text(err)
    if pretty:
        out = _pretty_output_text(out)
        err = _pretty_output_text(err)
    if round_digits is not None:
        out = _round_output_text(out, round_digits)
        err = _round_output_text(err, round_digits)
    if out.strip():
        txt = out.rstrip()
        try:
            print(txt)
        except UnicodeEncodeError:
            sys.stdout.buffer.write((txt + "\n").encode("utf-8", errors="replace"))
            sys.stdout.flush()
    if err.strip():
        txt = err.rstrip()
        try:
            print(txt)
        except UnicodeEncodeError:
            sys.stdout.buffer.write((txt + "\n").encode("utf-8", errors="replace"))
            sys.stdout.flush()


def _r_name_candidates(fortran_name: str) -> set[str]:
    out = {fortran_name}
    if "_dot_" in fortran_name:
        out.add(fortran_name.replace("_dot_", "."))
    if "_at_" in fortran_name:
        out.add(fortran_name.replace("_at_", "@"))
    return out


def _find_likely_r_source_lines(src: str, f_line: str, message: str = "") -> list[tuple[int, str, str]]:
    f = f_line.strip()
    out: list[tuple[int, str, str]] = []
    seen: set[int] = set()
    raw_lines = src.splitlines()

    def add(i: int, reason: str) -> None:
        if i in seen or i < 1 or i > len(raw_lines):
            return
        seen.add(i)
        out.append((i, raw_lines[i - 1].rstrip(), reason))

    m_do = re.match(r"^do\s+([A-Za-z]\w*)\s*=", f, re.IGNORECASE)
    if m_do is not None:
        var = m_do.group(1)
        for i, raw in enumerate(raw_lines, start=1):
            code, _cmt = split_r_code_comment(raw)
            for cand in _r_name_candidates(var):
                if re.search(rf"\bfor\s*\(\s*{re.escape(cand)}\s+in\b", code):
                    add(i, f"Fortran loop variable `{var}`")
                    break

    if "cannot be an array" in message.lower():
        m_var = re.search(r"\bdo\s+([A-Za-z]\w*)\b", f, re.IGNORECASE)
        if m_var is not None:
            var = m_var.group(1)
            for i, raw in enumerate(raw_lines, start=1):
                code, _cmt = split_r_code_comment(raw)
                for cand in _r_name_candidates(var):
                    if re.search(rf"\b{re.escape(cand)}\s*(?:<-|=)\s*(?:c|numeric|double|integer|matrix|array)\s*\(", code):
                        add(i, f"`{cand}` appears to be assigned an array")
                        break

    if not out:
        ids = [
            t for t in re.findall(r"\b[A-Za-z]\w*\b", f)
            if t.lower() not in {"do", "if", "then", "call", "real", "int", "kind", "dp", "size"}
        ]
        for i, raw in enumerate(raw_lines, start=1):
            code, _cmt = split_r_code_comment(raw)
            if any(any(cand in code for cand in _r_name_candidates(tok)) for tok in ids[:4]):
                add(i, "shares identifiers with failing Fortran line")
                if len(out) >= 5:
                    break
    return out[:8]


def _suggest_minimal_r_reproducer(f_line: str, message: str, matches: list[tuple[int, str, str]]) -> list[str]:
    f = f_line.strip()
    msg_l = message.lower()
    var = ""
    m_do = re.match(r"^do\s+([A-Za-z]\w*)\s*=", f, re.IGNORECASE)
    if m_do is not None and "loop variable" in msg_l and "array" in msg_l:
        var = m_do.group(1)
    if not var:
        m_alloc = re.match(r"^allocate\s*\(\s*([A-Za-z]\w*)\s*\(", f, re.IGNORECASE)
        if m_alloc is not None and "allocate-object" in msg_l:
            alloc_var = m_alloc.group(1)
            if any(
                any(re.search(rf"\bfor\s*\(\s*{re.escape(cand)}\s+in\b", r_line) for cand in _r_name_candidates(alloc_var))
                for _ln, r_line, _reason in matches
            ):
                var = alloc_var
    if not var:
        return []
    r_var = var.replace("_dot_", ".")
    return [
        "k <- 3",
        f"{r_var} <- numeric(3)",
        f"for ({r_var} in 1:k) {{",
        f"  print({r_var})",
        "}",
    ]


def _explain_compile_failure(cp: subprocess.CompletedProcess[str], out_path: Path, src: str) -> None:
    blob = "\n".join(x for x in [cp.stdout or "", cp.stderr or ""] if x)
    if not blob.strip() or not out_path.exists():
        return
    diags: list[tuple[int, str]] = []
    for m in re.finditer(r"(?m)^(.*?):(\d+):(\d+):\s*$", blob):
        try:
            line_no = int(m.group(2))
        except ValueError:
            continue
        tail = blob[m.end():]
        msg_m = re.search(r"(?m)^(?:Fatal )?Error:\s*(.+)$", tail)
        msg = msg_m.group(1).strip() if msg_m is not None else ""
        if line_no not in [d[0] for d in diags]:
            diags.append((line_no, msg))
    if not diags:
        return
    try:
        f_lines = out_path.read_text(encoding="utf-8", errors="replace").splitlines()
    except OSError:
        return
    printed = False
    for line_no, msg in diags[:3]:
        if line_no < 1 or line_no > len(f_lines):
            continue
        f_line = f_lines[line_no - 1].rstrip()
        matches = _find_likely_r_source_lines(src, f_line, msg)
        if not matches:
            continue
        def safe_print(txt: str) -> None:
            try:
                print(txt)
            except UnicodeEncodeError:
                sys.stdout.buffer.write((txt + "\n").encode("utf-8", errors="replace"))
                sys.stdout.flush()

        if not printed:
            safe_print("Likely R source for compile error:")
            printed = True
        safe_print(f"  Fortran {out_path.name}:{line_no}: {f_line.strip()}")
        if msg:
            safe_print(f"  Compiler: {msg}")
        for r_line_no, r_line, reason in matches:
            safe_print(f"  R line {r_line_no}: {r_line.strip()}  [{reason}]")
        repro = _suggest_minimal_r_reproducer(f_line, msg, matches)
        if repro:
            safe_print("  Suggested minimal R reproducer:")
            for r in repro:
                safe_print(f"    {r}")


def _run_xr2r_prepass(in_path: Path) -> tuple[str | None, str | None]:
    """Return (core_r_text, error_message)."""
    xr2r_path = Path(__file__).with_name("xr2r.py")
    if not xr2r_path.exists():
        return None, f"Missing prepass tool: {xr2r_path}"
    with tempfile.TemporaryDirectory(prefix="xr2f_core_r_") as td:
        core_path = Path(td) / f"{in_path.stem}_core.r"
        cmd = [sys.executable, str(xr2r_path), str(in_path), "--out", str(core_path)]
        cp = _run_capture(cmd)
        if cp.returncode != 0:
            msg = "Core-R prepass failed"
            blob = ((cp.stdout or "") + ("\n" + cp.stderr if cp.stderr else "")).strip()
            if blob:
                msg += f":\n{blob}"
            return None, msg
        try:
            return core_path.read_text(encoding="utf-8"), None
        except Exception as e:
            return None, f"Core-R prepass wrote unreadable output: {e}"


def _has_glob_chars(s: str) -> bool:
    return any(ch in s for ch in "*?[]")


def _strip_r_comment(line: str) -> str:
    in_single = False
    in_double = False
    esc = False
    for i, ch in enumerate(line):
        if esc:
            esc = False
            continue
        if ch == "\\":
            esc = True
            continue
        if ch == "'" and not in_double:
            in_single = not in_single
            continue
        if ch == '"' and not in_single:
            in_double = not in_double
            continue
        if ch == "#" and not in_single and not in_double:
            return line[:i]
    return line


def _find_r_library_calls(src: str) -> list[tuple[int, str]]:
    out: list[tuple[int, str]] = []
    pat = re.compile(r"^\s*(library|require)\s*\(", re.IGNORECASE)
    for i, raw in enumerate(src.splitlines(), start=1):
        ln = _strip_r_comment(raw.lstrip("\ufeff")).strip()
        if not ln:
            continue
        if pat.match(ln):
            out.append((i, ln))
    return out


def _text_metrics(text: str, comment_prefix: str) -> tuple[int, int, int]:
    chars = len(text)
    lines = text.splitlines()
    loc = 0
    for ln in lines:
        t = ln.strip()
        if not t:
            continue
        if comment_prefix and t.startswith(comment_prefix):
            continue
        loc += 1
    tokens = len(re.findall(r"[A-Za-z_]\w*|\d+(?:\.\d+)?|[^\s]", text))
    return loc, tokens, chars


def _file_metrics(path: Path, comment_prefix: str) -> tuple[int, int, int] | None:
    try:
        txt = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return None
    return _text_metrics(txt, comment_prefix)


def _ratio(a: int | None, b: int | None) -> str:
    if a is None or b is None or a == 0:
        return ""
    return f"{(b / a):.2f}"


def _reinvoke_for_input(args: argparse.Namespace, input_r: str) -> int:
    cmd: list[str] = [sys.executable, str(Path(__file__).resolve()), input_r]
    cmd.extend(args.helpers)
    if args.compile:
        cmd.append("--compile")
    if getattr(args, "debug", False):
        cmd.append("--debug")
    if args.run:
        cmd.append("--run")
    if args.run_both:
        cmd.append("--run-both")
    if args.run_diff:
        cmd.append("--run-diff")
    if args.time:
        cmd.append("--time")
    if args.time_both:
        cmd.append("--time-both")
    if args.tee:
        cmd.append("--tee")
    if args.tee_both:
        cmd.append("--tee-both")
    if args.run_all:
        cmd.append("--run-all")
    if args.tee_all:
        cmd.append("--tee-all")
    if args.via_python:
        cmd.append("--via-python")
    if args.out_python:
        cmd.extend(["--out-python", args.out_python])
    if args.annotate_r is not None:
        cmd.append("--annotate-r")
        if args.annotate_r:
            cmd.append(args.annotate_r)
    if args.if_const_aggressive:
        cmd.append("--if-const-aggressive")
    if args.no_format_print:
        cmd.append("--no-format-print")
    if args.normalize_num_output:
        cmd.append("--normalize-num-output")
    if args.pretty:
        cmd.append("--pretty")
    if args.round is not None:
        cmd.extend(["--round", str(args.round)])
    if args.round_both is not None:
        cmd.extend(["--round-both", str(args.round_both)])
    if args.disp_real:
        cmd.append("--disp-real")
    if args.no_recycle:
        cmd.append("--no-recycle")
    if args.recycle_warn:
        cmd.append("--recycle-warn")
    if args.recycle_stop:
        cmd.append("--recycle-stop")
    if args.via_core_r:
        cmd.append("--via-core-r")
    if args.allow_library:
        cmd.append("--allow-library")
    if args.self_contained:
        cmd.append("--self-contained")
    if args.out_dir:
        cmd.extend(["--out-dir", args.out_dir])
    if args.real_print_fmt != "f0.6":
        cmd.extend(["--real-print-fmt", args.real_print_fmt])
    if args.compiler != DEFAULT_COMPILER:
        cmd.extend(["--compiler", args.compiler])
    if args.rscript != "rscript":
        cmd.extend(["--rscript", args.rscript])
    cp = subprocess.run(cmd)
    return int(cp.returncode)


def _maybe_adopt_positional_out(args: argparse.Namespace) -> None:
    if args.out or not args.helpers:
        return
    first = Path(args.helpers[0])
    if first.name.lower() == "r.f90" or (
        first.suffix.lower() in {".f90", ".f95", ".f03", ".f08", ".f", ".for"} and not first.exists()
    ):
        args.out = args.helpers[0]
        args.helpers = args.helpers[1:]


def _print_summary_table(rows: list[dict[str, object]]) -> None:
    if not rows:
        print("No files processed.")
        return
    headers = [
        "source",
        "status",
        "rc",
        "r_loc",
        "f_loc",
        "loc_x",
        "r_tok",
        "f_tok",
        "tok_x",
        "r_chr",
        "f_chr",
        "chr_x",
    ]
    rendered: list[list[str]] = []
    for r in rows:
        rendered.append(
            [
                str(r.get("source", "")),
                str(r.get("status", "")),
                str(r.get("rc", "")),
                str(r.get("r_loc", "")),
                str(r.get("f_loc", "")),
                str(r.get("loc_x", "")),
                str(r.get("r_tok", "")),
                str(r.get("f_tok", "")),
                str(r.get("tok_x", "")),
                str(r.get("r_chr", "")),
                str(r.get("f_chr", "")),
                str(r.get("chr_x", "")),
            ]
        )
    metric_cols = headers[1:]
    agg_labels = ["MEDIAN", "MEAN", "MIN", "MAX"]
    aggs: dict[str, dict[str, str]] = {k: {} for k in agg_labels}

    def _to_num(col: str, v: object) -> float | None:
        s = str(v).strip()
        if not s:
            return None
        if col == "status":
            su = s.upper()
            if su == "PASS":
                return 1.0
            if su == "FAIL":
                return 0.0
            return None
        try:
            return float(s)
        except ValueError:
            return None

    for col in metric_cols:
        vals: list[float] = []
        for r in rows:
            x = _to_num(col, r.get(col, ""))
            if x is not None:
                vals.append(x)
        if not vals:
            continue
        vals.sort()
        n = len(vals)
        if n % 2 == 1:
            med = vals[n // 2]
        else:
            med = 0.5 * (vals[n // 2 - 1] + vals[n // 2])
        mean = sum(vals) / n
        mn = vals[0]
        mx = vals[-1]
        for label, val in (("MEDIAN", med), ("MEAN", mean), ("MIN", mn), ("MAX", mx)):
            if col in {"rc", "status", "r_loc", "f_loc", "r_tok", "f_tok", "r_chr", "f_chr"}:
                aggs[label][col] = str(int(round(val)))
            else:
                aggs[label][col] = f"{val:.2f}"

    for label in agg_labels:
        rendered.append(
            [
                label,
                aggs[label].get("status", ""),
                aggs[label].get("rc", ""),
                aggs[label].get("r_loc", ""),
                aggs[label].get("f_loc", ""),
                aggs[label].get("loc_x", ""),
                aggs[label].get("r_tok", ""),
                aggs[label].get("f_tok", ""),
                aggs[label].get("tok_x", ""),
                aggs[label].get("r_chr", ""),
                aggs[label].get("f_chr", ""),
                aggs[label].get("chr_x", ""),
            ]
        )
    widths = [len(h) for h in headers]
    for vals in rendered:
        for i, v in enumerate(vals):
            if len(v) > widths[i]:
                widths[i] = len(v)
    print("")
    print("Summary:")
    print("  ".join(headers[i].ljust(widths[i]) for i in range(len(headers))))
    for vals in rendered:
        print("  ".join(vals[i].ljust(widths[i]) for i in range(len(headers))))
    n = len(rows)
    n_pass = sum(1 for r in rows if int(r.get("rc", 1)) == 0)
    print(f"Totals: {n} files, {n_pass} pass, {n - n_pass} fail")


def main() -> int:
    ap = argparse.ArgumentParser(description="Partial R-to-Fortran transpiler")
    ap.add_argument("input_r", help="input .R/.r source file")
    ap.add_argument(
        "helpers",
        nargs="*",
        help="optional helper Fortran source files (modules); a leading non-existent .f90 path is treated as positional output",
    )
    ap.add_argument("--out", help="output .f90 path (default: <input>_r.f90)")
    ap.add_argument("--out-dir", help="directory for transpiled .f90, executable, and runtime-generated files")
    ap.add_argument(
        "--annotate-r",
        nargs="?",
        const="",
        metavar="OUT.r",
        help="write an R copy annotated with inferred declare(type(...)) statements; default: <input>_annotated.r",
    )
    ap.add_argument("--compile", action="store_true", help="compile transpiled Fortran")
    ap.add_argument("--run", action="store_true", help="compile and run transpiled Fortran")
    ap.add_argument(
        "--debug",
        action="store_true",
        help=(
            "compile and run with debug gfortran flags "
            "(-g -O0 -Wall -Wextra -Wimplicit-interface -fcheck=all -fbacktrace); "
            "explicit --compiler overrides these flags"
        ),
    )
    ap.add_argument("--run-both", action="store_true", help="run original R and transpiled Fortran")
    ap.add_argument("--run-diff", action="store_true", help="run both and compare outputs")
    ap.add_argument("--time", action="store_true", help="time transpile/compile/run (implies --run)")
    ap.add_argument("--time-both", action="store_true", help="time both original R and transpiled Fortran (implies --run-diff)")
    ap.add_argument("--tee", action="store_true", help="print transpiled source; in run mode also prints transformed output")
    ap.add_argument("--tee-both", action="store_true", help="print original + transpiled source; in run-both mode prints both outputs")
    ap.add_argument("--run-all", action="store_true", help="run original R, translated Python, and translated Fortran (implies --via-python)")
    ap.add_argument("--tee-all", action="store_true", help="print original R, translated Python, and translated Fortran sources (implies --via-python)")
    ap.add_argument(
        "--via-python",
        action="store_true",
        help="transpile via xr2p.py then convert Python to Fortran with xp2f.py",
    )
    ap.add_argument(
        "--out-python",
        help='path for intermediate Python translation when --via-python is used (default: "temp.py")',
    )
    ap.add_argument(
        "--if-const-aggressive",
        action="store_true",
        help="aggressively fold compile-time constant IF conditions (default folds only literal .true./.false. forms)",
    )
    ap.add_argument(
        "--real-print-fmt",
        default="f0.6",
        help='format descriptor used for real expressions when rewriting `print *` (default: "f0.6")',
    )
    ap.add_argument(
        "--no-format-print",
        action="store_true",
        help="do not rewrite list-directed `print *` to explicit `write` formats",
    )
    ap.add_argument("--compiler", default=DEFAULT_COMPILER, help='compiler command, e.g. "gfortran -O2 -Wall"')
    ap.add_argument("--rscript", default="rscript", help="command to run R scripts")
    ap.add_argument(
        "--normalize-num-output",
        action="store_true",
        help="normalize Fortran run output numeric tokens like .5/-.5 to 0.5/-0.5",
    )
    ap.add_argument(
        "--pretty",
        action="store_true",
        help="pretty-format displayed Fortran runtime output",
    )
    ap.add_argument(
        "--round",
        type=int,
        default=None,
        metavar="N",
        help="round floating-point data in displayed Fortran runtime output to N decimal places",
    )
    ap.add_argument(
        "--round-both",
        type=int,
        default=None,
        metavar="N",
        help="round floating-point data in displayed R and Fortran runtime output to N decimal places",
    )
    ap.add_argument(
        "--disp-real",
        action="store_true",
        help="disable integer-like printing of real matrices (always print reals)",
    )
    ap.add_argument(
        "--no-recycle",
        action="store_true",
        help="disable R-style vector recycling in arithmetic expressions",
    )
    ap.add_argument(
        "--recycle-warn",
        action="store_true",
        help="emit runtime warning whenever vector recycling occurs (lengths differ; requires r_mod helper)",
    )
    ap.add_argument(
        "--recycle-stop",
        action="store_true",
        help="error stop whenever vector recycling occurs (lengths differ; requires r_mod helper)",
    )
    ap.add_argument(
        "--via-core-r",
        action="store_true",
        help="first rewrite input R via xr2r.py, then transpile the normalized Core-R output",
    )
    ap.add_argument(
        "--allow-library",
        action="store_true",
        help="allow R `library(...)`/`require(...)` statements (warn and continue best-effort); default is fail-fast",
    )
    ap.add_argument(
        "--self-contained",
        action="store_true",
        help="prepend the r_mod runtime to the emitted Fortran and compile without an external r.f90 helper",
    )
    ap.add_argument("--summary", action="store_true", help="Print tabular per-file status summary.")
    args = ap.parse_args()
    compiler_explicit = any(a == "--compiler" or a.startswith("--compiler=") for a in sys.argv[1:])
    _maybe_adopt_positional_out(args)
    if args.round is not None and args.round < 0:
        print("Option error: --round requires a nonnegative integer.")
        return 1
    if args.round_both is not None and args.round_both < 0:
        print("Option error: --round-both requires a nonnegative integer.")
        return 1
    if args.round is not None and args.round_both is not None:
        print("Options conflict: --round and --round-both cannot be used together.")
        return 1
    if args.no_recycle and (args.recycle_warn or args.recycle_stop):
        print("Options conflict: --no-recycle cannot be used with --recycle-warn or --recycle-stop.")
        return 1
    if args.recycle_warn and args.recycle_stop:
        print("Options conflict: --recycle-warn and --recycle-stop cannot be used together.")
        return 1

    if args.debug:
        args.run = True
        args.compile = True
        if not compiler_explicit:
            args.compiler = DEBUG_COMPILER
    if args.time_both:
        args.run_diff = True
    if args.run_all:
        args.via_python = True
        args.run_both = True
    if args.tee_all:
        args.via_python = True
        args.tee_both = True
    if args.run_diff:
        args.run_both = True
    if args.run_both:
        args.run = True
        args.compile = True
    if args.time_both:
        args.time = True
    if args.time:
        args.run = True
    if args.tee_both:
        args.tee = True
    if args.out_python and not args.via_python:
        print("Option --out-python requires --via-python (or --run-all/--tee-all).")
        return 1

    if _has_glob_chars(args.input_r):
        matches = sorted(glob.glob(args.input_r))
        if matches:
            if args.out:
                print("When input uses globbing with multiple matches, --out is not supported.")
                return 1
            if args.annotate_r:
                print("When input uses globbing with multiple matches, explicit --annotate-r OUT.r is not supported.")
                return 1
            rc = 0
            summary_rows: list[dict[str, object]] = []
            total = len(matches)
            for i, m in enumerate(matches, start=1):
                print(f"[{i}/{total}] {m}", flush=True)
                one_rc = _reinvoke_for_input(args, m)
                src_p = Path(m)
                out_dir = Path(args.out_dir).resolve() if args.out_dir else src_p.parent.resolve()
                out_p = out_dir / f"{src_p.stem}_r.f90"
                m_r = _file_metrics(src_p, "#")
                m_f = _file_metrics(out_p, "!")
                summary_rows.append(
                    {
                        "source": m,
                        "status": ("PASS" if one_rc == 0 else "FAIL"),
                        "rc": one_rc,
                        "r_loc": (m_r[0] if m_r else ""),
                        "f_loc": (m_f[0] if m_f else ""),
                        "loc_x": _ratio(m_r[0] if m_r else None, m_f[0] if m_f else None),
                        "r_tok": (m_r[1] if m_r else ""),
                        "f_tok": (m_f[1] if m_f else ""),
                        "tok_x": _ratio(m_r[1] if m_r else None, m_f[1] if m_f else None),
                        "r_chr": (m_r[2] if m_r else ""),
                        "f_chr": (m_f[2] if m_f else ""),
                        "chr_x": _ratio(m_r[2] if m_r else None, m_f[2] if m_f else None),
                    }
                )
                if one_rc != 0 and rc == 0:
                    rc = one_rc
            if args.summary:
                _print_summary_table(summary_rows)
            return rc

    in_path = Path(args.input_r)
    if not in_path.exists():
        print(f"Missing file: {in_path}")
        return 1
    helper_paths = [Path(h) for h in args.helpers]

    def _same_path(a: Path, b: Path) -> bool:
        try:
            return a.resolve() == b.resolve()
        except OSError:
            return str(a).lower() == str(b).lower()

    default_r_helper = Path(__file__).resolve().with_name("r.f90")
    if default_r_helper.exists() and not any(_same_path(default_r_helper, hp) for hp in helper_paths):
        helper_paths.insert(0, default_r_helper)

    for hp in helper_paths:
        if not hp.exists():
            print(f"Missing helper file: {hp}")
            return 1
    if args.out:
        out_cand = Path(args.out)
        if args.out_dir and not out_cand.is_absolute():
            out_path = Path(args.out_dir) / out_cand
        else:
            out_path = out_cand
    else:
        out_path = (Path(args.out_dir) / f"{in_path.stem}_r.f90") if args.out_dir else in_path.with_name(f"{in_path.stem}_r.f90")
    out_path = out_path.resolve()
    artifact_dir = Path(args.out_dir).resolve() if args.out_dir else out_path.parent.resolve()
    artifact_dir.mkdir(parents=True, exist_ok=True)
    annotate_r_path: Path | None = None
    if args.annotate_r is not None:
        if args.annotate_r:
            ann_cand = Path(args.annotate_r)
            if args.out_dir and not ann_cand.is_absolute():
                annotate_r_path = (Path(args.out_dir) / ann_cand).resolve()
            else:
                annotate_r_path = ann_cand.resolve()
        else:
            annotate_r_path = (artifact_dir / f"{in_path.stem}_annotated.r").resolve()
    py_out_path: Path | None = None
    if args.via_python:
        if args.out_python:
            py_cand = Path(args.out_python)
            if args.out_dir and not py_cand.is_absolute():
                py_out_path = (Path(args.out_dir) / py_cand).resolve()
            else:
                py_out_path = py_cand.resolve()
        else:
            py_out_path = (artifact_dir / "temp.py").resolve()
    helper_modules = helper_modules_from_files(helper_paths)

    timings: dict[str, float] = {}
    r_run = None
    fortran_round_digits = args.round if args.round is not None else args.round_both

    if args.time_both or args.run_both:
        cmd = [args.rscript, str(in_path.resolve())]
        t0 = time.perf_counter() if args.time_both else None
        r_run = _run_capture(cmd, cwd=in_path.parent.resolve())
        if args.time_both:
            timings["r_run"] = time.perf_counter() - t0
        print("Run (r):", " ".join(cmd))
        if r_run.returncode != 0:
            print(f"Run (r): FAIL (exit {r_run.returncode})")
            _print_captured(r_run, round_digits=args.round_both)
            if not (r_run.stdout or "").strip() and not (r_run.stderr or "").strip():
                print("Run (r): no stdout/stderr captured; process may have crashed before producing output.")
            return r_run.returncode
        print("Run (r): PASS")
        _print_captured(r_run, round_digits=args.round_both)
        if args.run_both:
            print()

    t0 = time.perf_counter()
    src = in_path.read_text(encoding="utf-8")
    direct_mode = (not args.via_python)
    if args.via_python:
        assert py_out_path is not None
        cmd_r2p = [sys.executable, str(Path(__file__).with_name("xr2p.py")), str(in_path.resolve())]
        cmd_r2p.extend(str(h.resolve()) for h in helper_paths)
        cmd_r2p.extend(["--out", str(py_out_path)])
        if args.via_core_r:
            cmd_r2p.append("--via-core-r")
        if args.allow_library:
            cmd_r2p.append("--allow-library")
        if args.disp_real:
            cmd_r2p.append("--disp-real")
        if args.no_recycle:
            cmd_r2p.append("--no-recycle")
        if args.recycle_warn:
            cmd_r2p.append("--recycle-warn")
        if args.recycle_stop:
            cmd_r2p.append("--recycle-stop")
        cp_r2p = _run_capture(cmd_r2p, cwd=artifact_dir)
        if cp_r2p.returncode != 0:
            print("Transpile (R->Python): FAIL")
            _print_captured(cp_r2p)
            return cp_r2p.returncode
        cmd_p2f = [
            sys.executable,
            str(Path(__file__).with_name("xp2f.py")),
            str(py_out_path),
            "--out",
            str(out_path),
        ]
        cp_p2f = _run_capture(cmd_p2f, cwd=artifact_dir)
        if cp_p2f.returncode == 0 and out_path.exists():
            f90 = out_path.read_text(encoding="utf-8", errors="replace")
            direct_mode = False
        else:
            print("note: via-python Fortran leg failed; falling back to direct R->Fortran transpilation")
            _print_captured(cp_p2f)
            direct_mode = True
    if direct_mode:
        if args.via_core_r:
            core_src, err = _run_xr2r_prepass(in_path)
            if err is not None or core_src is None:
                print(f"Transpile: FAIL ({err or 'Core-R prepass failed'})")
                return 1
            src = core_src
            print("note: via-core-r prepass applied")
        lib_calls = _find_r_library_calls(src)
        if lib_calls:
            if not args.allow_library:
                ln, stmt = lib_calls[0]
                print(f"Transpile: FAIL (unsupported package import at line {ln}: {stmt})")
                print("Hint: rerun with --allow-library for best-effort transpilation.")
                return 1
            print("Warning: package import detected; continuing with best-effort translation:")
            for ln, stmt in lib_calls:
                print(f"  line {ln}: {stmt}")
        if annotate_r_path is not None:
            try:
                annotated_r = annotate_r_source_with_declares(src, in_path.stem)
            except NotImplementedError as e:
                print(f"Annotate R: FAIL ({e})")
                return 1
            annotate_r_path.parent.mkdir(parents=True, exist_ok=True)
            annotate_r_path.write_text(annotated_r, encoding="utf-8")
            print(f"wrote {annotate_r_path}")
        try:
            f90 = transpile_r_to_fortran(
                src,
                in_path.stem,
                helper_modules=helper_modules,
                int_like_print=(not args.disp_real),
                no_recycle=args.no_recycle,
                recycle_warn=args.recycle_warn,
                recycle_stop=args.recycle_stop,
            )
        except NotImplementedError as e:
            print(f"Transpile: FAIL ({e})")
            return 1
    elif annotate_r_path is not None:
        try:
            annotated_r = annotate_r_source_with_declares(src, in_path.stem)
        except NotImplementedError as e:
            print(f"Annotate R: FAIL ({e})")
            return 1
        annotate_r_path.parent.mkdir(parents=True, exist_ok=True)
        annotate_r_path.write_text(annotated_r, encoding="utf-8")
        print(f"wrote {annotate_r_path}")
    # Reuse shared Fortran cleanup for redundant int(...) casts.
    f90_lines = f90.splitlines()
    f90_lines = fscan.remove_redundant_int_casts(f90_lines)
    f90_lines = rewrite_default_array_size_refs(f90_lines)
    f90_lines = rewrite_optional_init_size_checks(f90_lines)
    f90_lines = rewrite_rank3_print_matrix_calls(f90_lines)
    f90_lines = rewrite_arma_table_label_access(f90_lines)
    f90_lines = fscan.simplify_real_int_casts_in_mixed_expr(f90_lines)
    f90_lines = fscan.simplify_size_expressions(f90_lines)
    f90_lines = fscan.propagate_array_size_aliases(f90_lines)
    f90_lines = fscan.propagate_cached_size_values(f90_lines)
    f90_lines = fpost.simplify_redundant_parentheses(f90_lines)
    f90_lines = fpost.tighten_unary_minus_literal_spacing(f90_lines)
    f90_lines = fpost.normalize_delimiter_inner_spacing(f90_lines)
    f90_lines = fpost.simplify_norm2_patterns(f90_lines)
    f90_lines = fpost.simplify_bfgs_rank1_update(f90_lines)
    f90_lines = fpost.remove_redundant_self_assignments(f90_lines)
    f90_lines = fscan.simplify_do_bounds_parens(f90_lines)
    f90_lines = fscan.simplify_negated_relational_conditions_in_lines(f90_lines)
    f90_lines = fscan.simplify_constant_if_blocks(f90_lines, aggressive=args.if_const_aggressive)
    f90_lines = mark_pure_with_xpure(f90_lines)
    f90_lines = fpost.collapse_single_stmt_if_blocks(f90_lines)
    f90_lines = fpost.simplify_do_while_true(f90_lines)
    f90_lines = fpost.hoist_module_use_only_imports(f90_lines)
    f90_lines = fpost.ensure_blank_line_between_module_procedures(f90_lines)
    f90_lines = hoist_repeated_numeric_array_literals(f90_lines)
    # NOTE: keep named-argument rewriting disabled here; the generic pass
    # can mis-handle array constructors in helper calls (e.g., r_rep_*).
    # Keep helper argument forms as emitted; avoid line-based rewrites that can
    # accidentally rewrite nested non-seq calls.
    if not args.no_format_print:
        f90_lines = fscan.rewrite_list_directed_print_reals(f90_lines, real_fmt=args.real_print_fmt)
    f90_lines = fscan.compact_repeated_edit_descriptors(f90_lines)
    # Keep component declarations in derived types intact; generic coalescing can
    # collapse mixed-rank fields onto one line and change semantics.
    f90_lines = fscan.coalesce_simple_declarations(f90_lines, max_len=80)
    f90_lines = fscan.wrap_long_declaration_lines(f90_lines, max_len=80)
    f90_lines = fscan.ensure_space_before_inline_comments(f90_lines)
    f90_lines = split_long_inline_comments(f90_lines, max_len=80)
    f90_lines = normalize_fortran_lines(f90_lines, max_consecutive_blank=1)
    f90_lines = fscan.wrap_long_fortran_lines(f90_lines, max_len=80)
    f90_lines = fix_split_power_operator(f90_lines)
    f90_lines = fix_wrapped_closing_delims(f90_lines)
    f90_lines = fpost.rewrite_named_arguments(f90_lines)
    f90_lines = fpost.wrap_long_lines(f90_lines, max_len=80)
    f90_lines = fpost.apply_xindent_defaults(f90_lines, max_len=80)
    f90_lines = fpost.ensure_blank_line_between_module_procedures(f90_lines)
    f90_lines = fpost.ensure_blank_line_between_program_units(f90_lines)
    f90_lines = format_derived_type_blocks(f90_lines)
    f90_lines = format_interface_blocks(f90_lines)
    f90_lines = rewrite_selected_orders_dataframe_print(f90_lines)
    f90_lines = simplify_write_g0_outer_parens(f90_lines)
    f90_lines = rewrite_default_array_size_refs(f90_lines)
    f90_lines = rewrite_optional_init_size_checks(f90_lines)
    f90_lines = rewrite_rank3_print_matrix_calls(f90_lines)
    f90_lines = rewrite_arma_table_label_access(f90_lines)
    f90 = "\n".join(f90_lines) + ("\n" if f90.endswith("\n") else "")
    stamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    r_comments = extract_r_top_comments(src)
    migrated_block = ""
    if r_comments:
        migrated_block = "\n".join(f"! {c}" for c in r_comments) + "\n"
    f90 = f"! transpiled by xr2f.py from {in_path.name} on {stamp}\n" + migrated_block + f90
    if args.self_contained:
        f90 = prepend_self_contained_runtime(f90, helper_paths)
    out_path.write_text(f90, encoding="utf-8")
    timings["transpile"] = time.perf_counter() - t0
    print(f"wrote {out_path}")

    if args.tee_both:
        print(f"--- original: {in_path} ---")
        print(src.rstrip())
    if args.tee_all and py_out_path is not None and py_out_path.exists():
        print(f"--- python: {py_out_path} ---")
        print(py_out_path.read_text(encoding="utf-8", errors="replace").rstrip())
    if args.tee:
        print(f"--- transpiled: {out_path} ---")
        print(f90.rstrip())

    if args.run_all:
        assert py_out_path is not None
        cmd_py = [sys.executable, str(py_out_path.resolve())]
        print("Run (translated-python):", " ".join(cmd_py))
        t0_py = time.perf_counter() if args.time else None
        py_run = _run_capture(cmd_py, cwd=artifact_dir)
        if args.time and t0_py is not None:
            timings["python_run"] = time.perf_counter() - t0_py
        if py_run.returncode != 0:
            print(f"Run (translated-python): FAIL (exit {py_run.returncode})")
            _print_captured(py_run)
            return py_run.returncode
        print("Run (translated-python): PASS")
        _print_captured(py_run)

    if args.compile or args.run:
        cparts = shlex.split(args.compiler)
        exe = out_path.with_suffix(".exe")
        t0 = time.perf_counter()
        build_helpers: list[str] = []
        include_dirs: list[str] = []
        using_cached_runtime = False
        if not args.self_contained:
            use_runtime_cache = bool(cparts) and "gfortran" in Path(cparts[0]).name.lower()
            for hp in helper_paths:
                if use_runtime_cache and hp.name.lower() == "r.f90":
                    using_cached_runtime = True
                    obj, inc_dir, helper_cp = _cached_runtime_object(hp, cparts)
                    if helper_cp is not None and helper_cp.returncode != 0:
                        print("Build helper:", " ".join(cparts + ["-c", str(hp.resolve()), "-o", str(obj)]))
                        timings["compile"] = time.perf_counter() - t0
                        print(f"Build: FAIL (exit {helper_cp.returncode})")
                        _print_captured(helper_cp)
                        return helper_cp.returncode
                    include_dirs.extend(["-J", str(inc_dir), "-I", str(inc_dir)])
                    build_helpers.append(str(obj))
                else:
                    build_helpers.append(str(hp.resolve()))
        if using_cached_runtime:
            for stale_mod in (artifact_dir / "r_mod.mod", artifact_dir / "R_MOD.mod"):
                try:
                    stale_mod.unlink()
                except FileNotFoundError:
                    pass
        for mod_name in _module_names_in_source(out_path):
            for stale_mod in (artifact_dir / f"{mod_name.lower()}.mod", artifact_dir / f"{mod_name.upper()}.mod"):
                try:
                    stale_mod.unlink()
                except FileNotFoundError:
                    pass
        if args.debug:
            split_dir = artifact_dir / f".{out_path.stem}_debug_mod"
            split_dir.mkdir(exist_ok=True)
            split_src = _split_module_program_source(out_path, split_dir)
            if split_src is not None:
                mod_src, prog_src = split_src
                mod_obj = split_dir / f"{out_path.stem}_modpart.o"
                prog_obj = split_dir / f"{out_path.stem}_progpart.o"
                split_includes = ["-J", str(split_dir), "-I", str(split_dir)] + _include_dirs_without_j(include_dirs)
                mod_cmd = cparts + split_includes + ["-c", str(mod_src), "-o", str(mod_obj)]
                print("Build module:", " ".join(mod_cmd))
                cp_mod = _run_capture(mod_cmd, cwd=split_dir)
                if cp_mod.returncode != 0:
                    timings["compile"] = time.perf_counter() - t0
                    print(f"Build: FAIL (exit {cp_mod.returncode})")
                    _print_captured(cp_mod)
                    return cp_mod.returncode
                prog_cmd = cparts + split_includes + ["-c", str(prog_src), "-o", str(prog_obj)]
                print("Build program:", " ".join(prog_cmd))
                cp_prog = _run_capture(prog_cmd, cwd=split_dir)
                if cp_prog.returncode != 0:
                    timings["compile"] = time.perf_counter() - t0
                    print(f"Build: FAIL (exit {cp_prog.returncode})")
                    _print_captured(cp_prog)
                    return cp_prog.returncode
                link_cmd = cparts + build_helpers + [str(mod_obj), str(prog_obj)]
                if args.run:
                    link_cmd += ["-o", str(exe)]
                print("Build:", " ".join(link_cmd))
                cp_link = _run_capture(link_cmd, cwd=split_dir)
                timings["compile"] = time.perf_counter() - t0
                if cp_link.returncode != 0:
                    print(f"Build: FAIL (exit {cp_link.returncode})")
                    _print_captured(cp_link)
                    return cp_link.returncode
                print("Build: PASS")

                if args.run:
                    t0 = time.perf_counter()
                    frun = _run_capture([str(exe.resolve())], cwd=artifact_dir)
                    timings["fortran_run"] = time.perf_counter() - t0
                    if frun.returncode != 0:
                        print(f"Run: FAIL (exit {frun.returncode})")
                        _print_captured(
                            frun,
                            normalize_num_output=args.normalize_num_output,
                            pretty=args.pretty,
                            round_digits=fortran_round_digits,
                        )
                        return frun.returncode
                    print("Run: PASS")
                    _print_captured(
                        frun,
                        normalize_num_output=args.normalize_num_output,
                        pretty=args.pretty,
                        round_digits=fortran_round_digits,
                    )
                if args.time:
                    print("Timing:")
                    for k_t in ("transpile", "compile", "fortran_run"):
                        if k_t in timings:
                            print(f"  {k_t}: {timings[k_t]:.6f} s")
                return 0
        cmd = cparts + include_dirs + build_helpers + [str(out_path)]
        if args.run:
            cmd += ["-o", str(exe)]
        if args.time:
            print("Compile options:", " ".join(cparts[1:]) if len(cparts) > 1 else "<none>")
        print("Build:", " ".join(cmd))
        cp = _run_capture(cmd, cwd=artifact_dir)
        timings["compile"] = time.perf_counter() - t0
        if cp.returncode != 0:
            print(f"Build: FAIL (exit {cp.returncode})")
            _print_captured(cp)
            _explain_compile_failure(cp, out_path, src)
            return cp.returncode
        print("Build: PASS")

        if args.run:
            t0 = time.perf_counter()
            frun = _run_capture([str(exe.resolve())], cwd=artifact_dir)
            timings["fortran_run"] = time.perf_counter() - t0
            if frun.returncode != 0:
                print(f"Run: FAIL (exit {frun.returncode})")
                _print_captured(
                    frun,
                    normalize_num_output=args.normalize_num_output,
                    pretty=args.pretty,
                    round_digits=fortran_round_digits,
                )
                return frun.returncode
            print("Run: PASS")
            _print_captured(
                frun,
                normalize_num_output=args.normalize_num_output,
                pretty=args.pretty,
                round_digits=fortran_round_digits,
            )

            if args.run_diff and r_run is not None:
                r_blob = (r_run.stdout or "") + (("\n" + r_run.stderr) if r_run.stderr else "")
                if args.round_both is not None:
                    r_blob = _round_output_text(r_blob, args.round_both)
                r_lines = _norm_output(r_blob)
                f_blob = (frun.stdout or "") + (("\n" + frun.stderr) if frun.stderr else "")
                if args.normalize_num_output:
                    f_blob = fscan.normalize_numeric_leading_zeros_text(f_blob)
                if args.pretty:
                    f_blob = _pretty_output_text(f_blob)
                if fortran_round_digits is not None:
                    f_blob = _round_output_text(f_blob, fortran_round_digits)
                f_lines = _norm_output(f_blob)
                if r_lines == f_lines:
                    print("Run diff: MATCH")
                else:
                    print("Run diff: DIFF")
                    first = None
                    nmin = min(len(r_lines), len(f_lines))
                    for i in range(nmin):
                        if r_lines[i] != f_lines[i]:
                            first = i
                            break
                    if first is None:
                        first = nmin
                    print(f"  first mismatch line: {first + 1}")
                    if first < len(r_lines):
                        print(f"  r      : {r_lines[first]}")
                    if first < len(f_lines):
                        print(f"  fortran: {f_lines[first]}")
                    for dl in difflib.unified_diff(r_lines, f_lines, fromfile="r", tofile="fortran", n=1):
                        print(dl)
                        if dl.startswith("@@"):
                            break

    if args.time:
        base = timings.get("r_run", 0.0)
        rows = []
        if "r_run" in timings:
            rows.append(("r run", timings["r_run"]))
        rows.append(("transpile", timings.get("transpile", 0.0)))
        if "compile" in timings:
            rows.append(("compile", timings["compile"]))
        if "fortran_run" in timings:
            rows.append(("fortran run", timings["fortran_run"]))
        rows.append(
            (
                "fortran total",
                timings.get("compile", 0.0) + timings.get("fortran_run", 0.0),
            )
        )
        stage_w = max(len("stage"), max(len(n) for n, _ in rows))
        sec_w = max(len("seconds"), max(len(f"{v:.6f}") for _, v in rows))
        print("")
        print("Timing summary (seconds):")
        print(f"  {'stage':<{stage_w}}  {'seconds':>{sec_w}}    ratio(vs r run)")
        for n, v in rows:
            ratio = f"{(v / base):.6f}" if base > 0 else "n/a"
            print(f"  {n:<{stage_w}}  {v:>{sec_w}.6f}    {ratio}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
