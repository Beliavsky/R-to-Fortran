from __future__ import annotations

import subprocess
import sys
from pathlib import Path


XR2F_PATH = Path(__file__).resolve().parents[1] / "xr2f.py"


def _run_source(tmp_path: Path, name: str, source: str) -> subprocess.CompletedProcess[str]:
    input_path = tmp_path / f"{name}.R"
    output_path = tmp_path / f"{name}.f90"
    input_path.write_text(source, encoding="utf-8")
    return subprocess.run(
        [sys.executable, str(XR2F_PATH), str(input_path), "--out", str(output_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )


def test_filter_scalar_user_callback_runs(tmp_path: Path) -> None:
    proc = _run_source(
        tmp_path,
        "filter_callback",
        """is_equilibrium <- function(v, n) {
  sum(v[seq_along(v) < n]) == sum(v[seq_along(v) > n])
}
all_equilibriums <- function(v) {
  Filter(function(n) is_equilibrium(v, n), seq_along(v))
}
print(all_equilibriums(c(-7, 1, 5, 2, -4, 3, 0)))
""",
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "4 7" in proc.stdout


def test_file_extension_regmatches_idiom_runs(tmp_path: Path) -> None:
    proc = _run_source(
        tmp_path,
        "file_extension",
        """get_file_ext <- function(path) {
  match <- regmatches(path, regexec("\\\\.[A-Za-z0-9]+$", path))
  ifelse(length(match[[1]]) == 0, "", match[[1]])
}
print(get_file_ext("archive.tar.gz"))
""",
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert ".gz" in proc.stdout


def test_indexed_vector_reduce_runs(tmp_path: Path) -> None:
    proc = _run_source(
        tmp_path,
        "vector_reduce",
        """generator <- function(f, ...) c(f[2], sum(f))
print(Reduce(generator, 2:8, c(0, 1))[2])
""",
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "21" in proc.stdout


def test_literal_list_sapply_write_lines_runs(tmp_path: Path) -> None:
    proc = _run_source(
        tmp_path,
        "list_sapply",
        """first_missing <- function(values) {
  n <- 1
  repeat {
    if (!(n %in% values)) return(paste("first missing:", n))
    n <- n + 1
  }
}
sapply(list(c(1, 2, 0), c(3, 4, -1, 1)), first_missing) |> writeLines()
""",
    )
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "first missing: 3" in proc.stdout
    assert "first missing: 2" in proc.stdout
