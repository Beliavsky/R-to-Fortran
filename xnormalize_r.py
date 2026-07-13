#!/usr/bin/env python3
"""Normalize R source into a simpler, more transpiler-friendly form.

The normalizer is intentionally conservative. It keeps the output as valid R
and avoids target-language-specific rewrites. By default it writes to stdout;
use --out or --in-place to write a file.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

from r_assignment import expand_top_level_assignment_chain


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
        if ch in ("'", '"'):
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


def find_top_level_operator(text: str, ops: tuple[str, ...]) -> tuple[int, str] | None:
    depth = 0
    quote: str | None = None
    escape = False
    i = len(text) - 1
    while i >= 0:
        ch = text[i]
        if quote is not None:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            i -= 1
            continue
        if ch in ("'", '"'):
            quote = ch
            i -= 1
            continue
        if ch in ")]}":
            depth += 1
            i -= 1
            continue
        if ch in "([{":
            depth = max(0, depth - 1)
            i -= 1
            continue
        if depth == 0:
            for op in ops:
                j = i - len(op) + 1
                if j >= 0 and text[j : i + 1] == op:
                    return j, op
        i -= 1
    return None


def strip_comment(line: str) -> tuple[str, str]:
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
        if ch in ("'", '"'):
            quote = ch
        elif ch == "#":
            return line[:i].rstrip(), line[i:]
    return line.rstrip(), ""


def assignment_parts(line: str) -> tuple[str, str, str] | None:
    code, comment = strip_comment(line)
    m = re.match(r"^(\s*)([A-Za-z.]\w*)\s*(<-|=)\s*(.+?)\s*$", code)
    if m is None:
        return None
    return m.group(2), m.group(4).strip(), comment


def has_chained_subset(rhs: str) -> bool:
    return re.search(r"\]\s*\[", rhs) is not None


def split_chained_subset(lhs: str, rhs: str, tmp: str) -> list[str] | None:
    match = re.search(r"\]\s*\[", rhs)
    if match is None:
        return None
    first = rhs[: match.start() + 1].strip()
    rest = rhs[match.start() + 1 :].strip()
    if not first or not rest.startswith("["):
        return None
    return [f"{tmp} <- {first}", f"{lhs} <- {tmp}{rest}"]


def normalize_matrix_product(lhs: str, rhs: str, tmp: str) -> list[str] | None:
    op = find_top_level_operator(rhs, ("%*%",))
    if op is None:
        return None
    pos, oper = op
    left = rhs[:pos].strip()
    right = rhs[pos + len(oper) :].strip()
    if not left or not right:
        return None
    if find_top_level_operator(left, ("%*%",)) is None:
        return None
    return [f"{tmp} <- {left}", f"{lhs} <- {tmp} %*% {right}"]


def normalize_as_numeric_matmul(lhs: str, rhs: str, tmp: str) -> list[str] | None:
    m = re.match(r"^as\.numeric\s*\((.*%[*]%.+)\)\s*$", rhs)
    if m is None:
        return None
    inner = m.group(1).strip()
    return [f"{tmp} <- {inner}", f"{lhs} <- as.numeric({tmp})"]


def normalize_print_nested(line: str, tmp: str) -> list[str] | None:
    code, comment = strip_comment(line)
    m = re.match(r"^(\s*)print\s*\((.+)\)\s*$", code)
    if m is None:
        return None
    expr = m.group(2).strip()
    if re.fullmatch(r"[A-Za-z.]\w*(?:\[[^\]]+\])?", expr):
        return None
    if not re.search(r"\w+\s*\(|%[*]%|\]\s*\[", expr):
        return None
    suffix = f" {comment}" if comment else ""
    return [f"{tmp} <- {expr}", f"print({tmp}){suffix}"]


def normalize_if_in_operator(line: str, tmp: str) -> list[str] | None:
    code, comment = strip_comment(line)
    m = re.match(r"^(\s*)if\s*\(\s*!\s*\((.+%in%.+)\)\s*\)\s*(.+)$", code)
    if m is None:
        return None
    indent, cond, action = m.group(1), m.group(2).strip(), m.group(3).strip()
    suffix = f" {comment}" if comment else ""
    return [f"{indent}{tmp} <- {cond}", f"{indent}if (!{tmp}) {action}{suffix}"]


def add_explicit_drop(line: str) -> str:
    code, comment = strip_comment(line)

    def repl(m: re.Match[str]) -> str:
        inner = m.group(1)
        parts = split_top_level_commas(inner)
        if len(parts) == 2 and "drop" not in inner:
            return "[" + inner + ", drop = TRUE]"
        return m.group(0)

    new_code = re.sub(r"\[([^\[\]]*,[^\[\]]*)\]", repl, code)
    return new_code + ((" " + comment) if comment else "")


def split_cat_newlines(line: str) -> list[str] | None:
    code, comment = strip_comment(line)
    m = re.match(r"^(\s*)cat\s*\((.+)\)\s*$", code)
    if m is None:
        return None
    indent, inner = m.group(1), m.group(2)
    args = split_top_level_commas(inner)
    out: list[str] = []
    current: list[str] = []
    changed = False
    for arg in args:
        if re.fullmatch(r'["\']\\n["\']', arg.strip()):
            if current:
                out.append(f"{indent}cat({', '.join(current)})")
                current = []
            out.append(f"{indent}cat(\"\\n\")")
            changed = True
        else:
            current.append(arg)
    if current:
        out.append(f"{indent}cat({', '.join(current)})")
    if not changed:
        return None
    if comment and out:
        out[-1] += " " + comment
    return out


class TempNames:
    def __init__(self, prefix: str) -> None:
        self.prefix = prefix
        self.count = 0

    def next(self) -> str:
        self.count += 1
        return f"{self.prefix}{self.count}"


def normalize_r_source(src: str, *, explicit_drop: bool = False, temp_prefix: str = ".xr_norm_") -> str:
    temps = TempNames(temp_prefix)
    out: list[str] = []
    for line in src.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            out.append(line.rstrip())
            continue

        code, comment = strip_comment(line)
        indent = code[: len(code) - len(code.lstrip())]
        if (rewritten := expand_top_level_assignment_chain(code.strip())) is not None:
            rewritten = [indent + item for item in rewritten]
            if comment:
                rewritten[-1] += " " + comment
            out.extend(rewritten)
            continue

        if (rewritten := normalize_if_in_operator(line, temps.next())) is not None:
            out.extend(rewritten)
            continue

        if (rewritten := split_cat_newlines(line)) is not None:
            out.extend(rewritten)
            continue

        if (rewritten := normalize_print_nested(line, temps.next())) is not None:
            out.extend(rewritten)
            continue

        assign = assignment_parts(line)
        if assign is not None:
            lhs, rhs, comment = assign
            tmp = temps.next()
            rewritten = (
                split_chained_subset(lhs, rhs, tmp)
                or normalize_as_numeric_matmul(lhs, rhs, tmp)
                or normalize_matrix_product(lhs, rhs, tmp)
            )
            if rewritten is not None:
                if comment:
                    rewritten[-1] += " " + comment
                out.extend(rewritten)
                continue

        out.append(add_explicit_drop(line) if explicit_drop else line.rstrip())

    return "\n".join(out) + ("\n" if src.endswith("\n") else "")


def default_out_path(path: Path) -> Path:
    return path.with_name(path.stem + "_normalized" + path.suffix)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Normalize R source for easier transpilation.")
    parser.add_argument("input", type=Path, help="input R file")
    parser.add_argument("--out", type=Path, help="output R file; defaults to stdout")
    parser.add_argument("--in-place", action="store_true", help="overwrite the input file")
    parser.add_argument("--default-out", action="store_true", help="write INPUT_stem_normalized.R")
    parser.add_argument("--explicit-drop", action="store_true", help="add drop = TRUE to simple two-dimensional subsets")
    parser.add_argument("--temp-prefix", default=".xr_norm_", help="prefix for generated temporary variables")
    args = parser.parse_args(argv)

    if args.in_place and (args.out or args.default_out):
        parser.error("--in-place cannot be combined with --out or --default-out")

    src = args.input.read_text(encoding="utf-8-sig")
    normalized = normalize_r_source(src, explicit_drop=args.explicit_drop, temp_prefix=args.temp_prefix)

    out_path: Path | None = args.out
    if args.default_out:
        out_path = default_out_path(args.input)
    if args.in_place:
        out_path = args.input

    if out_path is None:
        sys.stdout.write(normalized)
    else:
        out_path.write_text(normalized, encoding="utf-8")
        print(f"wrote {out_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
