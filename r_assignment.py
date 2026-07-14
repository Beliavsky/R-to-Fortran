"""Shared parsing helpers for conservative R assignment normalization."""

from __future__ import annotations

import re


_R_IDENTIFIER_RE = re.compile(r"(?:[A-Za-z]|\.(?!\d))[A-Za-z0-9._]*\Z")


def split_top_level_assignment_chain(text: str) -> tuple[list[str], str] | None:
    """Return simple LHS names and the terminal RHS of a top-level R assignment chain."""
    operators: list[tuple[int, int]] = []
    paren_depth = 0
    bracket_depth = 0
    brace_depth = 0
    quote: str | None = None
    escape = False
    i = 0
    while i < len(text):
        ch = text[i]
        if quote is not None:
            if escape:
                escape = False
            elif ch == "\\":
                escape = True
            elif ch == quote:
                quote = None
            i += 1
            continue
        if ch in {"'", '"'}:
            quote = ch
            i += 1
            continue
        if ch == "#":
            break
        if ch == "(":
            paren_depth += 1
        elif ch == ")":
            paren_depth = max(0, paren_depth - 1)
        elif ch == "[":
            bracket_depth += 1
        elif ch == "]":
            bracket_depth = max(0, bracket_depth - 1)
        elif ch == "{":
            brace_depth += 1
        elif ch == "}":
            brace_depth = max(0, brace_depth - 1)
        elif paren_depth == 0 and bracket_depth == 0 and brace_depth == 0:
            if text.startswith("<-", i) and (i == 0 or text[i - 1] != "<"):
                operators.append((i, 2))
                i += 2
                continue
            if ch == "=":
                prev = text[i - 1] if i else ""
                nxt = text[i + 1] if i + 1 < len(text) else ""
                if prev not in {"=", "!", "<", ">"} and nxt != "=":
                    operators.append((i, 1))
        i += 1

    if len(operators) < 2:
        return None

    names: list[str] = []
    start = 0
    for pos, width in operators:
        lhs = text[start:pos].strip()
        if _R_IDENTIFIER_RE.fullmatch(lhs) is None:
            return None
        names.append(lhs)
        start = pos + width
    rhs = text[start:].strip()
    if not rhs:
        return None
    return names, rhs


def expand_top_level_assignment_chain(text: str) -> list[str] | None:
    """Expand `a <- b <- expr` in right-to-left evaluation order."""
    chain = split_top_level_assignment_chain(text)
    if chain is None:
        return None
    names, rhs = chain
    out = [f"{names[-1]} <- {rhs}"]
    out.extend(f"{names[i]} <- {names[i + 1]}" for i in range(len(names) - 2, -1, -1))
    return out
