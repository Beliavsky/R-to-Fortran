#!/usr/bin/env python3
"""Rename R identifiers into a more Fortran-friendly spelling.

By default this script:
- renames user-defined dotted identifiers such as `risk.aversion` to
  `risk_aversion`
- resolves case-only conflicts such as `x` and `X`, because Fortran is
  case-insensitive

It intentionally does not change string literals or comments.  It also avoids
known dotted R built-ins such as `read.csv`, `as.numeric`, and `na.rm`.
"""

from __future__ import annotations

import argparse
import difflib
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path


IDENT_RE = re.compile(r"[A-Za-z.][A-Za-z0-9_.]*")
ASSIGN_RE = re.compile(r"(?<![<>=!])(?:<-|=)(?![=])")
FORMALS_RE = re.compile(r"\bfunction\s*\(([^)]*)\)")


KNOWN_DOTTED_BUILTINS = {
    "all.equal",
    "as.character",
    "as.complex",
    "as.data.frame",
    "as.double",
    "as.factor",
    "as.integer",
    "as.list",
    "as.logical",
    "as.matrix",
    "as.numeric",
    "as.vector",
    "complete.cases",
    "data.frame",
    "is.finite",
    "is.infinite",
    "is.na",
    "is.nan",
    "is.null",
    "is.numeric",
    "isTRUE",
    "na.omit",
    "na.rm",
    "read.csv",
    "read.table",
    "row.names",
    "seq_along",  # harmless if encountered; included for symmetry.
    "write.csv",
    "write.table",
}


@dataclass(frozen=True)
class Token:
    text: str
    start: int
    end: int


def split_code_comment(line: str) -> tuple[str, str]:
    quote: str | None = None
    escape = False
    for i, ch in enumerate(line):
        if quote is not None:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            continue
        if ch in {"'", '"'}:
            quote = ch
        elif ch == "#":
            return line[:i], line[i:]
    return line, ""


def code_spans(line: str) -> list[tuple[int, int]]:
    """Return spans outside string literals and comments."""
    code, _comment = split_code_comment(line)
    spans: list[tuple[int, int]] = []
    quote: str | None = None
    escape = False
    start = 0
    i = 0
    while i < len(code):
        ch = code[i]
        if quote is not None:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
                start = i + 1
            i += 1
            continue
        if ch in {"'", '"'}:
            if start < i:
                spans.append((start, i))
            quote = ch
            i += 1
            continue
        i += 1
    if quote is None and start < len(code):
        spans.append((start, len(code)))
    return spans


def iter_code_tokens(line: str) -> list[Token]:
    out: list[Token] = []
    for a, b in code_spans(line):
        for m in IDENT_RE.finditer(line[a:b]):
            text = m.group(0)
            start = a + m.start()
            end = a + m.end()
            if text == "." or text.startswith("..."):
                continue
            out.append(Token(text=text, start=start, end=end))
    return out


def split_top_level_commas(text: str) -> list[str]:
    parts: list[str] = []
    start = 0
    depth = 0
    quote: str | None = None
    escape = False
    for i, ch in enumerate(text):
        if quote is not None:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            continue
        if ch in {"'", '"'}:
            quote = ch
        elif ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth = max(0, depth - 1)
        elif ch == "," and depth == 0:
            parts.append(text[start:i].strip())
            start = i + 1
    parts.append(text[start:].strip())
    return parts


def sanitize_dotted_name(name: str) -> str:
    out = name.replace(".", "_")
    out = re.sub(r"[^A-Za-z0-9_]", "_", out)
    if out and out[0].isdigit():
        out = "_" + out
    return out


def unique_name(base: str, used_lower: set[str]) -> str:
    cand = base
    if cand.lower() not in used_lower:
        used_lower.add(cand.lower())
        return cand
    i = 2
    while True:
        cand = f"{base}_{i}"
        if cand.lower() not in used_lower:
            used_lower.add(cand.lower())
            return cand
        i += 1


def is_builtin_dotted(name: str) -> bool:
    return name in KNOWN_DOTTED_BUILTINS


def collect_defined_names(lines: list[str]) -> list[str]:
    names: list[str] = []
    seen: set[str] = set()

    def add(name: str) -> None:
        if name and name not in seen:
            seen.add(name)
            names.append(name)

    for line in lines:
        code, _comment = split_code_comment(line)
        # LHS assignments.
        m_assign = ASSIGN_RE.search(code)
        if m_assign is not None:
            lhs = code[: m_assign.start()].strip()
            m_lhs = re.match(r"^([A-Za-z.][A-Za-z0-9_.]*)\s*(?:$|\[|\$)", lhs)
            if m_lhs is not None:
                add(m_lhs.group(1))
        # Function formals.
        for m_fun in FORMALS_RE.finditer(code):
            for part in split_top_level_commas(m_fun.group(1)):
                if not part or part == "...":
                    continue
                name = part.split("=", 1)[0].strip()
                if re.fullmatch(r"[A-Za-z.][A-Za-z0-9_.]*", name):
                    add(name)
    return names


def build_rename_map(
    defined_names: list[str],
    *,
    rename_dots: bool = True,
    rename_case_conflicts: bool = True,
) -> dict[str, str]:
    used_lower: set[str] = set()
    mapping: dict[str, str] = {}
    first_for_lower: dict[str, str] = {}

    for name in defined_names:
        proposed = name
        if rename_dots and "." in proposed and not is_builtin_dotted(proposed):
            proposed = sanitize_dotted_name(proposed)

        key = proposed.lower()
        original_case_key = name.lower()
        case_conflict = (
            rename_case_conflicts
            and original_case_key in first_for_lower
            and first_for_lower[original_case_key] != name
        )
        output_conflict = key in used_lower and mapping.get(name, proposed).lower() not in used_lower

        if case_conflict or key in used_lower:
            proposed = unique_name(proposed + "_case", used_lower) if case_conflict else unique_name(proposed, used_lower)
        else:
            used_lower.add(key)

        first_for_lower.setdefault(original_case_key, name)
        if proposed != name:
            mapping[name] = proposed

    return mapping


def previous_nonspace(code: str, pos: int) -> str:
    i = pos - 1
    while i >= 0 and code[i].isspace():
        i -= 1
    return code[i] if i >= 0 else ""


def should_rewrite_token(line: str, tok: Token, *, rename_fields: bool) -> bool:
    if not rename_fields and previous_nonspace(line, tok.start) == "$":
        return False
    return True


def apply_renames(src: str, mapping: dict[str, str], *, rename_fields: bool = False) -> str:
    out_lines: list[str] = []
    for line in src.splitlines():
        tokens = iter_code_tokens(line)
        if not tokens:
            out_lines.append(line.rstrip())
            continue
        pieces: list[str] = []
        last = 0
        for tok in tokens:
            pieces.append(line[last:tok.start])
            repl = mapping.get(tok.text)
            if repl is not None and should_rewrite_token(line, tok, rename_fields=rename_fields):
                pieces.append(repl)
            else:
                pieces.append(tok.text)
            last = tok.end
        pieces.append(line[last:])
        out_lines.append("".join(pieces).rstrip())
    return "\n".join(out_lines) + ("\n" if src.endswith("\n") else "")


def normalize_for_diff(text: str) -> list[str]:
    return [ln.rstrip() for ln in text.replace("\r\n", "\n").replace("\r", "\n").split("\n") if ln.rstrip()]


def run_rscript(path: Path, rscript: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [rscript, str(path)],
        cwd=path.parent,
        capture_output=True,
        text=True,
        check=False,
    )


def run_diff(original: Path, renamed_src: str, *, rscript: str) -> int:
    with tempfile.TemporaryDirectory(prefix="xrename_r_") as td:
        tmp = Path(td) / (original.stem + "_renamed" + original.suffix)
        tmp.write_text(renamed_src, encoding="utf-8")
        r1 = run_rscript(original, rscript)
        r2 = run_rscript(tmp, rscript)
        old = normalize_for_diff(r1.stdout + r1.stderr)
        new = normalize_for_diff(r2.stdout + r2.stderr)
        if r1.returncode == r2.returncode and old == new:
            print("Run diff: MATCH")
            return 0
        print("Run diff: DIFF")
        print(f"  original exit: {r1.returncode}")
        print(f"  renamed  exit: {r2.returncode}")
        for ln in difflib.unified_diff(old, new, fromfile="original", tofile="renamed", n=2):
            print(ln)
        return 1


def default_out_path(path: Path) -> Path:
    return path.with_name(path.stem + "_renamed" + path.suffix)


def format_mapping(mapping: dict[str, str]) -> str:
    return "\n".join(f"{k} -> {v}" for k, v in sorted(mapping.items())) + ("\n" if mapping else "")


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Rename R identifiers for Fortran-friendly translation.")
    ap.add_argument("input", type=Path, help="input R file")
    ap.add_argument("--out", type=Path, help="output R file; defaults to stdout")
    ap.add_argument("--default-out", action="store_true", help="write INPUT_stem_renamed.R")
    ap.add_argument("--in-place", action="store_true", help="overwrite the input file")
    ap.add_argument("--no-dots", action="store_true", help="do not rename dotted user identifiers")
    ap.add_argument("--no-case-conflicts", action="store_true", help="do not resolve case-only conflicts")
    ap.add_argument("--rename-fields", action="store_true", help="also rename $field occurrences")
    ap.add_argument("--dry-run", action="store_true", help="print the rename map but do not write renamed source")
    ap.add_argument("--map-out", type=Path, help="write rename map to this file")
    ap.add_argument("--run-diff", action="store_true", help="run original and renamed scripts with Rscript and compare output")
    ap.add_argument("--rscript", default=shutil.which("Rscript") or "Rscript", help="Rscript executable")
    args = ap.parse_args(argv)

    if args.in_place and (args.out or args.default_out):
        ap.error("--in-place cannot be combined with --out or --default-out")

    src = args.input.read_text(encoding="utf-8-sig")
    defined = collect_defined_names(src.splitlines())
    mapping = build_rename_map(
        defined,
        rename_dots=not args.no_dots,
        rename_case_conflicts=not args.no_case_conflicts,
    )
    renamed = apply_renames(src, mapping, rename_fields=args.rename_fields)

    map_text = format_mapping(mapping)
    if args.map_out:
        args.map_out.write_text(map_text, encoding="utf-8")
    elif mapping:
        sys.stderr.write(map_text)

    if args.dry_run:
        return 0

    out_path: Path | None = args.out
    if args.default_out:
        out_path = default_out_path(args.input)
    if args.in_place:
        out_path = args.input

    if out_path is None:
        sys.stdout.write(renamed)
    else:
        out_path.write_text(renamed, encoding="utf-8")
        print(f"wrote {out_path}")

    if args.run_diff:
        return run_diff(args.input, renamed, rscript=args.rscript)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
