from __future__ import annotations

import subprocess
import sys
from pathlib import Path

import pytest


REPO_ROOT = Path(__file__).resolve().parents[1]
XR2F_PATH = REPO_ROOT / "xr2f.py"

# Refreshed from a local corpus sweep using:
#   python xr2f.py <file.r> r.f90 --compile
SUPPORTED_R_COMPILE_CASES = [
    "xarray.r",
    "xbare.r",
    "xc.r",
    "xfunc.r",
    "xhello.r",
    "xlist.r",
    "xlist_core.r",
    "xlm.r",
    "xloop.r",
    "xmatrix.r",
    "xna.r",
    "xnumeric.r",
    "xouter.r",
    "xpaste.r",
    "xr2f_smoke.R",
    "xreg_fit.r",
    "xrunif.r",
    "xseq.r",
    "xt.r",
    "xtf.r",
]


def _run_xr2f_compile(tmp_path: Path, example_name: str) -> subprocess.CompletedProcess[str]:
    local_input = tmp_path / example_name
    local_input.write_text((REPO_ROOT / example_name).read_text(encoding="utf-8-sig"), encoding="utf-8")
    out_path = tmp_path / "r.f90"
    return subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )


def test_xr2f_accepts_positional_output_path_for_compile(tmp_path: Path) -> None:
    proc = _run_xr2f_compile(tmp_path, "xhello.r")

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert (tmp_path / "r.f90").exists()


@pytest.mark.parametrize("example_name", SUPPORTED_R_COMPILE_CASES)
def test_xr2f_compiles_supported_local_r_examples(tmp_path: Path, example_name: str) -> None:
    proc = _run_xr2f_compile(tmp_path, example_name)

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert (tmp_path / "r.f90").exists()
