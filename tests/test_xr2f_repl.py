from __future__ import annotations

import shutil
import subprocess
import sys
from pathlib import Path

import pytest

from xr2f_repl import InteractiveRWorkspace, clean_input_line


REPO_ROOT = Path(__file__).resolve().parents[1]
REPL_PATH = REPO_ROOT / "xr2f_repl.py"
RSCRIPT = shutil.which("Rscript")


def test_clean_input_line_removes_windows_pipeline_bom() -> None:
    assert clean_input_line("\u00ef\u00bb\u00bfx = 1") == "x = 1"
    assert clean_input_line("\ufeffx = 1") == "x = 1"


@pytest.mark.skipif(RSCRIPT is None, reason="Rscript is required")
def test_interactive_r_workspace_preserves_random_assignment(tmp_path: Path) -> None:
    workspace = InteractiveRWorkspace(RSCRIPT or "Rscript", 10.0)
    try:
        assigned = workspace.evaluate("set.seed(123); x <- rnorm(5)")
        first = workspace.evaluate("x")
        second = workspace.evaluate("x")
    finally:
        workspace.close()

    assert assigned.ok, assigned.message
    assert first.ok, first.message
    assert second.ok, second.message
    assert first.stdout == second.stdout
    assert len(first.stdout.split()) >= 6


@pytest.mark.skipif(RSCRIPT is None, reason="Rscript is required")
def test_repl_bare_symbol_prints_saved_value_without_fortran_build() -> None:
    proc = subprocess.run(
        [sys.executable, str(REPL_PATH), "--no-save", "--timeout", "10"],
        cwd=REPO_ROOT,
        input="x = c(1, 2, 3)\nx\nquit\n",
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "[1] 1 2 3" in proc.stdout
    assert "Build:" not in proc.stdout
    assert "wrote " not in proc.stdout


@pytest.mark.skipif(RSCRIPT is None, reason="Rscript is required")
def test_repl_output_calls_execute_immediately_and_are_saved() -> None:
    proc = subprocess.run(
        [sys.executable, str(REPL_PATH), "--no-save", "--timeout", "10"],
        cwd=REPO_ROOT,
        input='x = c(1, 2, 3)\nprint(sum(x))\ncat(sum(x), "\\n")\nlist\nquit\n',
        capture_output=True,
        text=True,
        timeout=15,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "[1] 6" in proc.stdout
    assert "6\n" in proc.stdout
    assert "1: x = c(1, 2, 3)" in proc.stdout
    assert "2: print(sum(x))" in proc.stdout
    assert '3: cat(sum(x), "\\n")' in proc.stdout
    assert "Build:" not in proc.stdout
