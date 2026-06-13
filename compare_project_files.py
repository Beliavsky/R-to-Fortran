#!/usr/bin/env python3
"""Compare selected files from one project tree against another.

By default this reports files with common source/example suffixes that are
present in the GitHub checkout but missing or different in the working tree.
"""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path


DEFAULT_SOURCE = Path(r"C:\python\public_domain\github\r-to-fortran")
DEFAULT_TARGET = Path(r"C:\python\R-to-Fortran")
DEFAULT_PATTERNS = ["*.py", "*.r", "*.R", "*.f90"]
SKIP_DIRS = {".git", "__pycache__", ".pytest_cache"}
TEXT_SUFFIXES = {".py", ".r", ".f90", ".txt", ".md"}


def file_hash(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as f:
        for chunk in iter(lambda: f.read(1024 * 1024), b""):
            h.update(chunk)
    return h.hexdigest()


def normalized_text(path: Path) -> str:
    data = path.read_bytes()
    try:
        text = data.decode("utf-8-sig")
    except UnicodeDecodeError:
        text = data.decode("latin-1")
    return text.replace("\r\n", "\n").replace("\r", "\n")


def files_differ(src: Path, dst: Path, *, binary: bool) -> bool:
    if not binary and src.suffix.lower() in TEXT_SUFFIXES:
        return normalized_text(src) != normalized_text(dst)
    return src.stat().st_size != dst.stat().st_size or file_hash(src) != file_hash(dst)


def iter_matching_files(root: Path, patterns: list[str], *, root_only: bool = False) -> set[Path]:
    out: set[Path] = set()
    for pattern in patterns:
        paths = root.glob(pattern) if root_only else root.rglob(pattern)
        for path in paths:
            if not path.is_file():
                continue
            rel = path.relative_to(root)
            if any(part in SKIP_DIRS for part in rel.parts):
                continue
            out.add(rel)
    return out


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "List files present in SOURCE but missing or content-different in "
            "TARGET for selected glob patterns."
        )
    )
    parser.add_argument(
        "patterns",
        nargs="*",
        default=DEFAULT_PATTERNS,
        help='glob patterns to compare, for example "*.py" "*.r" "*.f90"',
    )
    parser.add_argument("--source", type=Path, default=DEFAULT_SOURCE)
    parser.add_argument("--target", type=Path, default=DEFAULT_TARGET)
    parser.add_argument(
        "--binary",
        action="store_true",
        help="compare raw bytes instead of normalizing text line endings",
    )
    parser.add_argument(
        "--root-only",
        action="store_true",
        help="compare only files directly under SOURCE, not subdirectories",
    )
    parser.add_argument(
        "--verbose",
        action="store_true",
        help="list files in every non-empty category, including SAME",
    )
    args = parser.parse_args()

    source = args.source.resolve()
    target = args.target.resolve()
    if not source.is_dir():
        raise SystemExit(f"source directory not found: {source}")
    if not target.is_dir():
        raise SystemExit(f"target directory not found: {target}")

    missing: list[Path] = []
    different: list[Path] = []
    same: list[Path] = []

    for rel in sorted(
        iter_matching_files(source, args.patterns, root_only=args.root_only),
        key=lambda p: p.as_posix().lower(),
    ):
        src = source / rel
        dst = target / rel
        if not dst.exists():
            missing.append(rel)
            continue
        if files_differ(src, dst, binary=args.binary):
            different.append(rel)
        else:
            same.append(rel)

    if args.verbose:
        for label, paths in (
            ("MISSING", missing),
            ("DIFFERENT", different),
            ("SAME", same),
        ):
            if paths:
                print(f"{label}:")
                for rel in paths:
                    print(f"  {rel}")
    else:
        for rel in missing:
            print(f"MISSING   {rel}")
        for rel in different:
            print(f"DIFFERENT {rel}")

    print(f"Summary: {len(missing)} missing, {len(different)} different, {len(same)} same")
    return 1 if missing or different else 0


if __name__ == "__main__":
    raise SystemExit(main())
