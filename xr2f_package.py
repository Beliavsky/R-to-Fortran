#!/usr/bin/env python3
"""Translate a simple, dependency-free R package into one Fortran module."""

from __future__ import annotations

import argparse
import json
import re
import shlex
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


_R_FUNCTION_RE = re.compile(
    r"(?m)^(?:[\"'](?P<quoted>[A-Za-z][A-Za-z0-9._]*)[\"']|"
    r"(?P<plain>[A-Za-z][A-Za-z0-9._]*))\s*(?:<-|=)\s*function\s*\("
)
_QUOTED_FUNCTION_ASSIGN_RE = re.compile(
    r"(?m)^(?P<indent>[^\S\r\n]*)[\"'](?P<name>[A-Za-z][A-Za-z0-9._]*)[\"']"
    r"\s*(?:<-|=)\s*(?:\r?\n[^\S\r\n]*)?function\b"
)
_COMPILED_SUFFIXES = {
    ".c", ".cc", ".cpp", ".cxx", ".f", ".f90", ".f95", ".f03", ".f08",
    ".for", ".ftn", ".h", ".hpp", ".o", ".obj", ".a", ".so", ".dll",
}


@dataclass(frozen=True)
class NamespaceInfo:
    export_all: bool
    exports: tuple[str, ...]
    unsupported: tuple[str, ...]


def _parse_dcf(path: Path) -> dict[str, str]:
    fields: dict[str, str] = {}
    current: str | None = None
    for line in path.read_text(encoding="utf-8-sig", errors="replace").splitlines():
        if not line.strip():
            current = None
            continue
        if line[:1].isspace():
            if current is None:
                raise ValueError(f"malformed continuation line in {path.name}: {line.strip()}")
            fields[current] += "\n" + line.strip()
            continue
        if ":" not in line:
            raise ValueError(f"malformed field in {path.name}: {line.strip()}")
        key, value = line.split(":", 1)
        current = key.strip()
        fields[current] = value.strip()
    return fields


def _dependency_names(value: str) -> list[str]:
    names: list[str] = []
    for item in value.replace("\n", " ").split(","):
        name = re.sub(r"\s*\(.*?\)\s*$", "", item).strip()
        if name and name.lower() != "r":
            names.append(name)
    return names


def _parse_namespace(path: Path) -> NamespaceInfo:
    if not path.exists():
        return NamespaceInfo(export_all=False, exports=(), unsupported=("missing NAMESPACE",))
    text = re.sub(r"(?m)#.*$", "", path.read_text(encoding="utf-8-sig", errors="replace"))
    export_all = False
    exports: list[str] = []
    unsupported: list[str] = []
    for match in re.finditer(r"(?is)\b([A-Za-z][A-Za-z0-9.]*)\s*\((.*?)\)", text):
        directive = match.group(1)
        body = match.group(2).strip()
        directive_l = directive.lower()
        if directive_l == "exportpattern":
            pattern = body.strip().strip("\"'")
            if pattern == ".":
                export_all = True
            else:
                unsupported.append(f"exportPattern({body})")
        elif directive_l == "export":
            exports.extend(
                token.strip().strip("\"'")
                for token in body.split(",")
                if token.strip()
            )
        elif directive_l not in {"s3method"}:
            unsupported.append(f"{directive}({body})")
    return NamespaceInfo(
        export_all=export_all,
        exports=tuple(dict.fromkeys(exports)),
        unsupported=tuple(unsupported),
    )


def _collated_r_files(package_dir: Path, description: dict[str, str]) -> list[Path]:
    r_dir = package_dir / "R"
    if not r_dir.is_dir():
        raise ValueError("package has no R directory")
    available = {p.name.lower(): p for p in r_dir.iterdir() if p.is_file() and p.suffix.lower() == ".r"}
    collate = description.get("Collate", "").replace("\n", " ").strip()
    if not collate:
        return sorted(available.values(), key=lambda p: (p.name.lower(), p.name))
    try:
        names = shlex.split(collate, posix=True)
    except ValueError as exc:
        raise ValueError(f"invalid DESCRIPTION Collate field: {exc}") from exc
    files: list[Path] = []
    for name in names:
        path = available.get(name.lower())
        if path is None:
            raise ValueError(f"Collate names missing R source: {name}")
        files.append(path)
    unlisted = [p.name for key, p in available.items() if key not in {n.lower() for n in names}]
    if unlisted:
        raise ValueError("R source files missing from Collate: " + ", ".join(sorted(unlisted)))
    return files


def _find_compiled_sources(package_dir: Path) -> list[str]:
    src_dir = package_dir / "src"
    if not src_dir.is_dir():
        return []
    return sorted(
        str(path.relative_to(package_dir))
        for path in src_dir.rglob("*")
        if path.is_file() and path.suffix.lower() in _COMPILED_SUFFIXES
    )


def _function_names(source: str) -> list[str]:
    return [m.group("quoted") or m.group("plain") for m in _R_FUNCTION_RE.finditer(source)]


def _assemble_source(files: list[Path]) -> str:
    chunks: list[str] = []
    for path in files:
        source = path.read_text(encoding="utf-8-sig", errors="replace").rstrip()
        source = _QUOTED_FUNCTION_ASSIGN_RE.sub(
            lambda match: f"{match.group('indent')}{match.group('name')} <- function",
            source,
        )
        chunks.append(f"# xr2f-package: source {path.name}\n{source}\n")
    return "\n".join(chunks)


def _preflight_sources(files: list[Path]) -> tuple[list[Path], list[dict[str, str]]]:
    """Keep one malformed package file from preventing partial package translation."""
    import xr2f

    accepted: list[Path] = []
    skipped: list[dict[str, str]] = []
    for path in files:
        source = _assemble_source([path])
        names = _function_names(source)
        try:
            xr2f._parse_r_functions_for_partial(source)
        except Exception as exc:
            reason = " ".join(str(exc).split()) or exc.__class__.__name__
            for name in names:
                skipped.append({"name": name, "reason": f"source preflight failed in {path.name}: {reason}"})
            if not names:
                skipped.append({"name": path.name, "reason": f"source preflight failed: {reason}"})
            continue
        accepted.append(path)
    return accepted, skipped


def _translation_report(f90_path: Path) -> tuple[list[str], list[dict[str, str]]]:
    if not f90_path.exists():
        return [], []
    translated: list[str] = []
    skipped: list[dict[str, str]] = []
    for line in f90_path.read_text(encoding="utf-8", errors="replace").splitlines():
        match = re.match(r"\s*!\s*translated:\s*(.*)$", line, re.IGNORECASE)
        if match and match.group(1).strip() != "<none>":
            translated = [name.strip() for name in match.group(1).split(",") if name.strip()]
            continue
        match = re.match(r"\s*!\s*skipped:\s*([^\s]+)\s+-\s+(.*)$", line, re.IGNORECASE)
        if match:
            skipped.append({"name": match.group(1), "reason": match.group(2).strip()})
    return translated, skipped


def _validate_package(package_dir: Path, description: dict[str, str], namespace: NamespaceInfo) -> list[str]:
    errors: list[str] = []
    package_name = description.get("Package", "").strip()
    if not package_name:
        errors.append("DESCRIPTION has no Package field")
    required = _dependency_names(description.get("Depends", ""))
    required += _dependency_names(description.get("Imports", ""))
    linking = _dependency_names(description.get("LinkingTo", ""))
    if required:
        errors.append("required package dependencies are not supported in package mode: " + ", ".join(required))
    if linking:
        errors.append("LinkingTo dependencies are not supported in pure-R package mode: " + ", ".join(linking))
    if description.get("NeedsCompilation", "").strip().lower() == "yes":
        errors.append("DESCRIPTION declares NeedsCompilation: yes")
    compiled = _find_compiled_sources(package_dir)
    if compiled:
        errors.append("compiled package sources are not supported: " + ", ".join(compiled))
    if namespace.unsupported:
        errors.append("unsupported NAMESPACE directives: " + "; ".join(namespace.unsupported))
    return errors


def translate_package(args: argparse.Namespace) -> int:
    package_dir = Path(args.package_dir).resolve()
    description_path = package_dir / "DESCRIPTION"
    if not description_path.is_file():
        print(f"Package: FAIL (DESCRIPTION not found in {package_dir})")
        return 1
    try:
        description = _parse_dcf(description_path)
        namespace = _parse_namespace(package_dir / "NAMESPACE")
        errors = _validate_package(package_dir, description, namespace)
        files = _collated_r_files(package_dir, description)
    except (OSError, ValueError) as exc:
        print(f"Package: FAIL ({exc})")
        return 1
    if errors:
        print("Package: FAIL (package is outside first-version package-mode scope)")
        for error in errors:
            print(f"  {error}")
        return 1

    package_name = description["Package"].strip()
    stem = re.sub(r"[^A-Za-z0-9_]+", "_", package_name).strip("_") or "r_package"
    out_dir = Path(args.out_dir).resolve() if args.out_dir else package_dir / "xr2f"
    out_dir.mkdir(parents=True, exist_ok=True)
    combined_path = out_dir / f"{stem}_package.R"
    f90_path = out_dir / f"{stem}.f90"
    report_path = out_dir / f"{stem}_package_report.json"

    all_source = _assemble_source(files)
    discovered = _function_names(all_source)
    accepted_files, preflight_skipped = _preflight_sources(files)
    if not accepted_files:
        print("Package: FAIL (no package R source files passed parser preflight)")
        return 1
    source = _assemble_source(accepted_files)
    combined_path.write_text(source, encoding="utf-8")
    declared_exports = discovered if namespace.export_all else list(namespace.exports)

    xr2f_path = Path(__file__).with_name("xr2f.py")
    cmd = [sys.executable, str(xr2f_path), str(combined_path), "--out", str(f90_path), "--partial"]
    if args.compile:
        cmd.append("--compile")
    if args.self_contained:
        cmd.append("--self-contained")
    if args.verbose:
        cmd.append("--verbose")
        print("Package command:", subprocess.list2cmdline(cmd))
    proc = subprocess.run(cmd, cwd=out_dir, capture_output=True, text=True, check=False)
    if proc.stdout:
        print(proc.stdout, end="" if proc.stdout.endswith("\n") else "\n")
    if proc.stderr:
        print(proc.stderr, file=sys.stderr, end="" if proc.stderr.endswith("\n") else "\n")

    translated, skipped = _translation_report(f90_path)
    skipped = preflight_skipped + skipped
    translated_l = {name.lower() for name in translated}
    report_missing = [name for name in declared_exports if name.lower() not in translated_l]
    report = {
        "schema_version": 1,
        "package": package_name,
        "version": description.get("Version", ""),
        "package_dir": str(package_dir),
        "status": "pass" if proc.returncode == 0 else "fail",
        "coverage": (
            "failed" if proc.returncode != 0 else "complete" if not report_missing else "partial"
        ),
        "source_files": [path.relative_to(package_dir).as_posix() for path in files],
        "translated_source_files": [path.relative_to(package_dir).as_posix() for path in accepted_files],
        "combined_source": str(combined_path),
        "fortran_source": str(f90_path),
        "namespace_export_all": namespace.export_all,
        "declared_exports": declared_exports,
        "discovered_functions": discovered,
        "translated_functions": translated,
        "skipped_functions": skipped,
        "translated_exports": [name for name in declared_exports if name.lower() in translated_l],
        "missing_exports": report_missing,
        "suggested_packages_ignored": _dependency_names(description.get("Suggests", "")),
        "command": cmd,
        "returncode": proc.returncode,
    }
    report_path.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"wrote {report_path}")
    print(
        f"Package translation: {len(translated)} translated, {len(skipped)} skipped, "
        f"{len(report['missing_exports'])} exported functions missing"
    )
    if proc.returncode == 0 and args.require_all_exports and report["missing_exports"]:
        print("Package: FAIL (--require-all-exports requested and some exports were not translated)")
        return 1
    return proc.returncode


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        description="Translate a dependency-free, pure-R package into one Fortran module"
    )
    parser.add_argument("package_dir", help="R package directory containing DESCRIPTION, NAMESPACE, and R/")
    parser.add_argument("--out-dir", help="output directory (default: <package>/xr2f)")
    parser.add_argument("--compile", action="store_true", help="compile the generated Fortran module")
    parser.add_argument(
        "--require-all-exports",
        action="store_true",
        help="return failure unless every declared package export is translated",
    )
    parser.add_argument(
        "--self-contained",
        action="store_true",
        help="embed required runtime helpers in the generated Fortran source",
    )
    parser.add_argument("--verbose", action="store_true", help="show package and translator progress")
    return translate_package(parser.parse_args(argv))


if __name__ == "__main__":
    raise SystemExit(main())
