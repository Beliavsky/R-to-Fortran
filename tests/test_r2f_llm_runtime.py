from __future__ import annotations

import subprocess
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
RUNTIME_PATH = REPO_ROOT / "r2f_llm_runtime.f90"


def test_r2f_llm_runtime_compiles_and_prints_named_table(tmp_path: Path) -> None:
    runtime_copy = tmp_path / "r2f_llm_runtime.f90"
    runtime_copy.write_text(RUNTIME_PATH.read_text(encoding="utf-8"), encoding="utf-8")
    probe = tmp_path / "probe.f90"
    probe.write_text(
        "\n".join(
            [
                "program probe",
                "  use r2f_llm_runtime_mod, only: dp, named_real_matrix_t, init_matrix, col_index, print_matrix, &",
                "       NA_dp, is_na, nanmean, nansd, na_omit, which_true, first_finite, &",
                "       split_tokens, parse_int_list, parse_real_list, itoa, ftoa, cumsum_dp, cumprod_dp, cummax_dp",
                "  implicit none",
                "  type(named_real_matrix_t) :: mat",
                "  real(dp) :: x(2,2)",
                "  real(dp) :: v(4), rvals(4)",
                "  real(dp), allocatable :: vf(:), cs(:), cp(:), cm(:)",
                "  integer :: ivals(4), n, idx(4)",
                "  integer, allocatable :: wh(:)",
                "  character(len=16) :: toks(4)",
                "  x = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2,2])",
                '  call init_matrix(mat, x, row_names=[character(len=64) :: "r1", "r2"], &',
                '       col_names=[character(len=64) :: "alpha", "beta"])',
                '  print *, "beta_col", col_index(mat, "beta")',
                '  call print_matrix(mat, "demo", digits=2)',
                "  v = [1.0_dp, NA_dp(), 3.0_dp, 5.0_dp]",
                "  vf = na_omit(v)",
                '  print *, "is_na2", is_na(v(2))',
                '  print *, "nanmean", nanmean(v)',
                '  print *, "nansd", nansd(v)',
                '  print *, "omit_n", size(vf)',
                "  wh = which_true([.true., .false., .true., .false.])",
                '  print *, "which", wh',
                '  print *, "first_finite", first_finite(v)',
                '  call split_tokens("SPY TLT GLD", toks, n)',
                '  print *, "tokens", n, trim(toks(2))',
                '  call parse_int_list("1 5 21", ivals, n)',
                '  print *, "ints", n, ivals(3)',
                '  call parse_real_list("0.1 0.2", rvals, n)',
                '  print *, "reals", n, rvals(2)',
                '  print *, "itoa", itoa(42)',
                '  print *, "ftoa", ftoa(1.25_dp, 2)',
                "  cs = cumsum_dp([1.0_dp, 2.0_dp, 3.0_dp])",
                "  cp = cumprod_dp([2.0_dp, 3.0_dp, 4.0_dp])",
                "  cm = cummax_dp([1.0_dp, 3.0_dp, 2.0_dp])",
                '  print *, "cum", cs(3), cp(3), cm(3)',
                "end program probe",
            ]
        )
        + "\n",
        encoding="utf-8",
    )

    build = subprocess.run(
        ["gfortran", "r2f_llm_runtime.f90", "probe.f90", "-o", "probe.exe"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    assert build.returncode == 0, build.stdout + build.stderr

    run = subprocess.run(
        [str(tmp_path / "probe.exe")],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    assert run.returncode == 0, run.stdout + run.stderr
    assert "beta_col" in run.stdout
    assert "2" in run.stdout
    assert "demo" in run.stdout
    assert "alpha" in run.stdout
    assert "beta" in run.stdout
    assert "is_na2" in run.stdout
    assert "omit_n" in run.stdout
    assert "tokens" in run.stdout
    assert "ints" in run.stdout
    assert "cum" in run.stdout
