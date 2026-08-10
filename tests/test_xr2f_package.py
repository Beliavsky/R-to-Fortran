from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

import xr2f_package


REPO_ROOT = Path(__file__).resolve().parents[1]
XR2F_PACKAGE_PATH = REPO_ROOT / "xr2f_package.py"


def _write_package(root: Path, *, imports: str = "", collate: str = "") -> None:
    (root / "R").mkdir(parents=True)
    description = [
        "Package: tiny",
        "Version: 1.0.0",
        "Title: Tiny test package",
        "Description: Package translator fixture.",
        "License: MIT",
        "NeedsCompilation: no",
    ]
    if imports:
        description.append(f"Imports: {imports}")
    if collate:
        description.append(f"Collate: {collate}")
    (root / "DESCRIPTION").write_text("\n".join(description) + "\n", encoding="utf-8")
    (root / "NAMESPACE").write_text('exportPattern(".")\n', encoding="utf-8")
    (root / "R" / "b.R").write_text("cube <- function(x) x * square(x)\n", encoding="utf-8")
    (root / "R" / "a.R").write_text("square <- function(x) x * x\n", encoding="utf-8")


def test_package_helpers_parse_dependencies_and_collate(tmp_path: Path) -> None:
    package_dir = tmp_path / "tiny"
    _write_package(package_dir, imports="other (>= 1.0)", collate="'b.R' 'a.R'")

    description = xr2f_package._parse_dcf(package_dir / "DESCRIPTION")
    files = xr2f_package._collated_r_files(package_dir, description)

    assert xr2f_package._dependency_names(description["Imports"]) == ["other"]
    assert [path.name for path in files] == ["b.R", "a.R"]


def test_package_cli_translates_dependency_free_package(tmp_path: Path) -> None:
    package_dir = tmp_path / "tiny"
    out_dir = tmp_path / "out"
    _write_package(package_dir)

    proc = subprocess.run(
        [
            sys.executable,
            str(XR2F_PACKAGE_PATH),
            str(package_dir),
            "--out-dir",
            str(out_dir),
            "--require-all-exports",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert (out_dir / "tiny.f90").is_file()
    report = json.loads((out_dir / "tiny_package_report.json").read_text(encoding="utf-8"))
    assert report["status"] == "pass"
    assert report["source_files"] == ["R/a.R", "R/b.R"]
    assert report["translated_functions"] == ["square", "cube"]
    assert report["missing_exports"] == []


def test_package_source_normalizes_old_quoted_function_assignment(tmp_path: Path) -> None:
    source_path = tmp_path / "old.R"
    source_path.write_text('"fact"<-\nfunction(x)\ngamma(x + 1)\n', encoding="utf-8")

    source = xr2f_package._assemble_source([source_path])

    assert "fact <- function(x)" in source
    assert xr2f_package._function_names(source) == ["fact"]


def test_package_cli_rejects_required_package_dependencies(tmp_path: Path) -> None:
    package_dir = tmp_path / "tiny"
    _write_package(package_dir, imports="other")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PACKAGE_PATH), str(package_dir)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 1
    assert "required package dependencies are not supported in package mode: other" in proc.stdout
