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
    source_path = REPO_ROOT / example_name
    if not source_path.exists():
        source_path = REPO_ROOT / "r_examples" / example_name
    local_input.write_text(source_path.read_text(encoding="utf-8-sig"), encoding="utf-8")
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


def test_xr2f_carries_r_comments_to_fortran(tmp_path: Path) -> None:
    local_input = tmp_path / "xcomments.r"
    local_input.write_text(
        "\n".join(
            [
                "# top comment",
                "x <- 1 # inline assignment",
                "# before function",
                "twice <- function(a) {",
                "  # inside function",
                "  y <- 2 * a # inline function body",
                "  return(y)",
                "}",
                "# before print",
                'cat("twice:", twice(x), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xcomments.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "! top comment" in out_text
    assert "x = 1 ! inline assignment" in out_text
    assert "! before function" in out_text
    assert "! inside function" in out_text
    assert "twice_result = 2 * a ! inline function body" in out_text
    assert "twice_result = y" not in out_text
    assert "! before print" in out_text
    before_idx = out_text.index("! before function")
    fn_idx = out_text.index("function twice(")
    inside_idx = out_text.index("! inside function")
    arg_idx = out_text.index("intent(in) :: a")
    main_idx = out_text.index("program xcomments")
    assert before_idx < fn_idx < inside_idx < arg_idx < main_idx


def test_xr2f_annotate_r_writes_inferred_declares(tmp_path: Path) -> None:
    local_input = tmp_path / "xannotate_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "# top comment",
                "f <- function(x, max.iter = 3) {",
                "  y <- numeric(length(x))",
                "  for (i in 1:max.iter) y[i] <- x[i]",
                "  return(y)",
                "}",
                "n <- 2",
                "z <- f(c(1.0, 2.0), max.iter = n)",
                'cat("ok", length(z), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xannotate_probe.f90"
    ann_path = tmp_path / "xannotate_probe_typed.r"

    proc = subprocess.run(
        [
            sys.executable,
            str(XR2F_PATH),
            str(local_input),
            "--out",
            str(out_path),
            "--annotate-r",
            str(ann_path),
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    ann_text = ann_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert f"wrote {ann_path}" in proc.stdout
    assert ann_text.index("# top comment") < ann_text.index("declare(type(")
    assert "n = integer()" in ann_text
    assert "z = double()" in ann_text
    assert "max.iter = integer()" in ann_text
    assert "max_iter = integer()" not in ann_text
    assert ann_text.index("f <- function") < ann_text.index("  declare(type(") < ann_text.index("  y <- numeric")


def test_xr2f_dotted_for_loop_variable_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xdotted_variable.r"
    local_input.write_text(
        "\n".join(
            [
                "max.iter = 3",
                "for (i.loop in 1:max.iter) {",
                '  cat(i.loop, i.loop^2, "\\n")',
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdotted_variable.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "do i_dot_loop = 1, max_dot_iter" in out_text
    assert "i.loop" not in out_text


def test_xr2f_printing_r_function_emits_subroutine(tmp_path: Path) -> None:
    local_input = tmp_path / "xprint_subroutine.r"
    local_input.write_text(
        "\n".join(
            [
                "show_x <- function(x) {",
                '  cat("x:", x, "\\n")',
                "}",
                "show_x(3.0)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprint_subroutine.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "subroutine show_x(x)" in out_text
    assert "end subroutine show_x" in out_text
    assert "call show_x(x=3.0_dp)" in out_text
    assert "function show_x" not in out_text
    assert "ignore_val = show_x" not in out_text


def test_xr2f_keeps_reducer_temp_to_avoid_recomputing_size_expr(tmp_path: Path) -> None:
    local_input = tmp_path / "xmean_temp_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_vec <- function(x) {",
                "  y <- double(length(x))",
                "  for (i in seq_along(x)) y[i] <- x[i] * 2.0",
                "  y",
                "}",
                "v <- make_vec((1:4) * 1.0)",
                "print(mean(v))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmean_temp_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert out_text.count("make_vec(") == 2  # function definition plus one main-scope call
    assert "allocate(make_vec_result(max(0, size(x))), source=0.0_dp)" in out_text
    assert "make_vec_result = numeric(size(x))" not in out_text
    assert "make_vec_result(i) = x(i) * 2.0_dp" in out_text
    assert "make_vec_result = y" not in out_text
    assert "v = make_vec(" in out_text
    assert "sum(v)/real(size(v), kind=dp)" in out_text
    assert "size(make_vec(" not in out_text


def test_xr2f_folds_literal_max_in_double_allocation(tmp_path: Path) -> None:
    local_input = tmp_path / "xdouble_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x = double(3)",
                "x(2) = 10.0",
                "print(x)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdouble_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "allocate(x(3), source=0.0_dp)" in out_text
    assert "max(0, 3)" not in out_text
    assert "call print_real_vector(x)" in out_text
    assert "call print_real_vector(real(x, kind=dp))" not in out_text


def test_xr2f_set_functions_for_integer_real_and_character_vectors(tmp_path: Path) -> None:
    local_input = tmp_path / "xset_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "xi = c(2L,4L,6L)",
                "yi = c(1L,4L,9L)",
                "print(union(xi, yi))",
                "print(intersect(xi, yi))",
                "print(setdiff(xi, yi))",
                "print(setequal(xi, yi))",
                "xr = c(2,4,6)",
                "yr = c(1,4,9)",
                "print(union(xr, yr))",
                "print(intersect(xr, yr))",
                "print(setdiff(xr, yr))",
                "print(setequal(xr, yr))",
                'xc = c("two", "four", "six")',
                'yc = c("one", "four", "nine")',
                "print(union(xc, yc))",
                "print(intersect(xc, yc))",
                "print(setdiff(xc, yc))",
                "print(setequal(xc, yc))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xset_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "integer, parameter :: xi" in out_text
    assert "real(kind=dp), parameter :: xr" in out_text
    assert 'write(*,"(*(1x,i0))") union(xi, yi)' in out_text
    assert "call print_real_vector(union(xr, yr))" in out_text
    assert "call print_char_vector(union(xc, yc))" in out_text
    assert "two four six one nine" in proc.stdout


def test_xr2f_in_operator_for_integer_real_character_and_logical_vectors(tmp_path: Path) -> None:
    local_input = tmp_path / "xin_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "xi = c(4L, 9L)",
                "yi = 1:5",
                "tf = xi %in% yi",
                "print(tf)",
                "xr = c(4, 9)",
                "yr = as.double(1:5)",
                "tf = xr %in% yr",
                "print(tf)",
                'xc = c("one", "two", "three")',
                'yc = c("two", "four")',
                "tf = xc %in% yc",
                "print(tf)",
                "xb = c(TRUE, FALSE)",
                "yb = c(TRUE)",
                "tf = xb %in% yb",
                "print(tf)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xin_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use r_mod, only:" in out_text and "r_in" in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xi, yi)' in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xr, r_seq_int(1, 5))' in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xc, yc)' in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xb, yb)' in out_text
    assert "T F" in proc.stdout
    assert "F T F" in proc.stdout


def test_xr2f_is_element_lowers_to_membership_helper(tmp_path: Path) -> None:
    local_input = tmp_path / "xis_element_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "xi = c(4L, 9L)",
                "yi = 1:5",
                "tf = is.element(xi, yi)",
                "print(tf)",
                "xr = c(4, 9)",
                "yr = as.double(1:5)",
                "tf = is.element(xr, yr)",
                "print(tf)",
                'xc = c("one", "two", "three")',
                'yc = c("two", "four")',
                "tf = is.element(xc, yc)",
                "print(tf)",
                "xb = c(TRUE, FALSE)",
                "yb = c(TRUE)",
                "tf = is.element(xb, yb)",
                "print(tf)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xis_element_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "is.element" not in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xi, yi)' in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xr, r_seq_int(1, 5))' in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xc, yc)' in out_text
    assert 'write(*,"(*(g0,1x))") r_in(xb, yb)' in out_text
    assert "T F" in proc.stdout
    assert "F T F" in proc.stdout


def test_xr2f_unique_for_numeric_character_logical_and_mixed_vectors(tmp_path: Path) -> None:
    local_input = tmp_path / "xunique_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(3, 1, 2, 3, 2, 4, 1)",
                "unique(x)",
                'names <- c("Ann", "Bob", "Ann", "Carl", "Bob")',
                "unique(names)",
                "b <- c(TRUE, FALSE, TRUE, TRUE, FALSE)",
                "unique(b)",
                "i <- c(1L, 2L, 1L, 3L, 2L)",
                "unique(i)",
                "typeof(unique(i))",
                "d <- c(1, 2, 1, 3, 2)",
                "unique(d)",
                "typeof(unique(d))",
                'x <- c(1, "1", 2, "2", 1)',
                "x",
                "unique(x)",
                'x <- c("red", "blue", "red", "green", "blue")',
                "u <- unique(x)",
                "u",
                "length(u)",
                "x <- c(4, 2, 3, 2, 1, 4)",
                "unique(x)",
                "sort(unique(x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xunique_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert (
        'Warning: R expression `c(1, "1", 2, "2", 1)` mixes character and non-character values; '
        "translating by coercing all elements to character."
    ) in proc.stdout
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use r_mod, only:" in out_text and "unique" in out_text
    assert "call print_real_vector(unique(x))" in out_text
    assert "call print_char_vector(unique(names))" in out_text
    assert 'write(*,"(*(g0,1x))") unique(b)' in out_text
    assert 'write(*,"(*(1x,i0))") unique(i)' in out_text
    assert "x_2 = [character(len=1) :: \"1\", \"1\", \"2\", \"2\", \"1\"]" in out_text
    assert "call print_char_vector(unique(x_2))" in out_text
    assert "u = unique(x_2)" in out_text
    assert "call print_char_vector(u)" in out_text
    assert "call print_real_vector(sort(unique(" in out_text
    assert "3 1 2 4" in proc.stdout
    assert "Ann Bob Carl" in proc.stdout
    assert "T F" in proc.stdout
    assert "integer" in proc.stdout
    assert "double" in proc.stdout
    assert "red blue green" in proc.stdout


def test_xr2f_duplicated_and_anyduplicated_vectors(tmp_path: Path) -> None:
    local_input = tmp_path / "xduplicated_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "xd <- c(10, 20, 30, 20, 40, 30)",
                "print(xd)",
                "print(duplicated(xd))",
                "print(duplicated(xd, fromLast = TRUE))",
                "print(anyDuplicated(xd))",
                "print(anyDuplicated(xd, fromLast = TRUE))",
                "xi <- as.integer(xd)",
                "print(xi)",
                "print(duplicated(xi))",
                "print(duplicated(xi, fromLast = TRUE))",
                "print(anyDuplicated(xi))",
                'xc <- c("one", "two", "three", "two", "four", "three")',
                "print(duplicated(xc))",
                "print(duplicated(xc, fromLast = TRUE))",
                "print(anyDuplicated(xc))",
                "xl <- c(TRUE, FALSE, TRUE, TRUE, FALSE)",
                "print(duplicated(xl))",
                "print(duplicated(xl, fromLast = TRUE))",
                "print(anyDuplicated(xl))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xduplicated_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use r_mod, only:" in out_text and "duplicated" in out_text and "anyDuplicated" in out_text
    assert 'write(*,"(*(g0,1x))") duplicated(xd)' in out_text
    assert 'write(*,"(*(g0,1x))") duplicated(xd, fromLast = .true.)' in out_text
    assert 'write(*,"(g0)") anyDuplicated(xd)' in out_text
    assert 'write(*,"(g0)") anyDuplicated(xd, fromLast = .true.)' in out_text
    assert 'write(*,"(*(g0,1x))") duplicated(xc)' in out_text
    assert 'write(*,"(*(g0,1x))") duplicated(xl)' in out_text
    assert "F F F T F T" in proc.stdout
    assert "F T T F F F" in proc.stdout
    assert "4" in proc.stdout


def test_xr2f_replace_vectors_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xreplace_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 20, 30, 40, 50)",
                "print(replace(x, 3, 999))",
                "print(replace(x, c(2, 5), c(200, 500)))",
                "print(replace(x, c(2, 5), 0))",
                "x <- c(3, -1, 5, -7, 2)",
                "print(replace(x, x < 0, 0))",
                "x <- c(1, NaN, 3, NaN, 5)",
                "print(replace(x, is.nan(x), 0))",
                'xc <- c("a", "b", "a", "c", "b", "a")',
                'print(replace(xc, duplicated(xc), "dup"))',
                'print(replace(xc, !duplicated(xc), "first"))',
                "x <- c(5, 8, 12, 3, 20)",
                "print(replace(x, which(x > 10), 999))",
                "x <- c(10, 20, 30, 40, 50, 60)",
                "print(replace(x, c(2, 4, 6), c(100, 200)))",
                "xi <- 1:6",
                "yi <- replace(xi, c(2, 4), c(20L, 40L))",
                "print(typeof(yi))",
                "print(yi)",
                "xl <- c(TRUE, FALSE, TRUE, FALSE)",
                "print(replace(xl, xl == FALSE, TRUE))",
                "z <- c(10, 20, 30, 40, 50)",
                "w <- z",
                "w[c(2, 5)] <- c(200, 500)",
                "print(w)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreplace_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use r_mod, only:" in out_text and "replace" in out_text and "which" in out_text
    assert "is.nan" not in out_text
    assert "call print_char_vector(replace(xc, duplicated(xc)" in out_text
    assert '[character(len=3) :: "dup"]' in out_text and "value_len=3" in out_text
    assert "call print_char_vector(replace(xc, .not. duplicated(xc)" in out_text
    assert '[character(len=5) :: &' in out_text and '"first"]' in out_text and "value_len=5" in out_text
    assert 'w = replace(w, int([2.0_dp, 5.0_dp]), [200.0_dp, 500.0_dp])' in out_text
    assert "10 20 999 40 50" in proc.stdout
    assert "10 200 30 40 500" in proc.stdout
    assert "1 0 3 0 5" in proc.stdout
    assert "a b dup c dup dup" in proc.stdout
    assert "first first a first b a" in proc.stdout
    assert "10 100 30 200 50 100" in proc.stdout
    assert "integer" in proc.stdout
    assert " 1 20 3 40 5 6" in proc.stdout
    assert "T T T T" in proc.stdout


def test_xr2f_sweep_matrix_scale_apply_and_rank3_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xsweep_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "my_fun <- function(a, b) {",
                "  a - 2 * b",
                "}",
                "x <- matrix(c(10, 20, 30, 40, 50, 60), nrow = 2, byrow = TRUE)",
                "col_means <- colMeans(x)",
                "z <- sweep(x, 2, col_means, \"-\")",
                "print(z)",
                "print(colMeans(z))",
                "print(apply(x, 2, sd))",
                "zs <- scale(x)",
                "print(zs)",
                "stats <- c(1, 2, 3)",
                "print(sweep(x, 2, stats, my_fun))",
                "a <- array(1:24, dim = c(2, 3, 4))",
                "stats1 <- c(100, 200)",
                "print(sweep(a, 1, stats1, \"-\") )",
                "stats2 <- c(10, 20, 30)",
                "print(sweep(a, 2, stats2, \"-\"))",
                "stats3 <- c(1, 2, 3, 4)",
                "print(sweep(a, 3, stats3, \"-\"))",
                "stats12 <- matrix(c(100, 200, 300, 400, 500, 600), nrow = 2)",
                "print(sweep(a, c(1, 2), stats12, \"-\"))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsweep_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert "real(kind=dp), allocatable :: z(:,:)" in out_text
    assert "z = x - spread((sum(x, dim=1)/real(size(x, 1), kind=dp)), dim=1, ncopies=size(x,1))" in flat_out
    assert "apply_col_sd(real(x, kind=dp))" in out_text
    assert "call print_matrix(scale(real(x, kind=dp)))" in out_text
    assert "my_fun(real(x, kind=dp), real(spread(stats, dim=1, ncopies=size(x,1)), kind=dp))" in flat_out
    assert "spread(spread(stats1, dim=2, ncopies=size(a,2)), dim=3, ncopies=size(a,3))" in flat_out
    assert "dim=3" in flat_out and "ncopies=size(a,3)" in flat_out
    assert "-1.50000E+01" in proc.stdout
    assert "21.213203435596427" in proc.stdout
    assert "2.40000E+01" in proc.stdout
    assert "-99 -198" in proc.stdout
    assert "-594" in proc.stdout


def test_xr2f_acf_plot_false_and_plot_true_warning(tmp_path: Path) -> None:
    local_input = tmp_path / "xacf_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(2.0, 3.1, 2.8, 4.0, 3.7, 5.2, 4.9, 6.1)",
                "a <- acf(x, lag.max = 3, plot = FALSE)",
                "print(a)",
                "print(as.vector(a$lag))",
                "print(as.vector(a$acf))",
                "b <- acf(x, lag.max = 2, type = \"covariance\", plot = TRUE)",
                "print(as.vector(b$acf))",
                "x1 <- c(1, 2, 3, 4, 5, 6)",
                "x2 <- c(2, 1, 2, 1, 2, 1)",
                "xm <- cbind(x1 = x1, x2 = x2)",
                "m <- acf(xm, lag.max = 2, plot = FALSE)",
                "print(dim(m$acf))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xacf_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use r_mod, only:" in out_text and "r_acf" in out_text and "acf_fit_t" in out_text
    assert "use r_mod, only: acf," not in out_text
    assert "type(acf_fit_t) :: a" in out_text
    assert "a = r_acf(x, lag_max=3, plot=.false.)" in out_text
    assert 'b = r_acf(x, lag_max=2, type="covariance", plot=.true.)' in out_text
    assert "call print_acf(a)" in out_text
    assert "shape(m%acf)" in out_text
    assert "Warning: acf plot = TRUE requested; plots are not supported." in proc.stdout
    assert "Autocorrelations of series" in proc.stdout
    assert "1.0000000000000000" in proc.stdout
    assert "3 2 2" in proc.stdout


def test_xr2f_rle_inverse_and_fields_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xrle_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1, 1, 1, 2, 2, 5, 5, 5, 5, 1)",
                "r <- rle(x)",
                "r",
                "r$lengths",
                "r$values",
                "inverse.rle(r)",
                "identical(x, inverse.rle(r))",
                'xc <- c("a", "a", "b", "b", "b", "a", "c", "c")',
                "rle(xc)",
                "xl <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)",
                "rl <- rle(xl)",
                "rl$lengths",
                "rl$values",
                'xs <- c("up", "up", "down", "down", "down", "up", "up", "up", "up")',
                "rs <- rle(xs)",
                "max(rs$lengths)",
                "rs$values[which.max(rs$lengths)]",
                "x2 <- c(1, 1, 1, 2, 2, 3, 3, 3, 3, 2)",
                "r2 <- rle(x2)",
                "r2$values[r2$lengths >= 3]",
                "r2$lengths[r2$lengths >= 3]",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xrle_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use r_mod, only:" in out_text and "rle" in out_text and "inverse_rle" in out_text
    assert "type(rle_real_t) :: r" in out_text
    assert "type(rle_logical_t) :: rl" in out_text
    assert "type(rle_char_t) :: rs" in out_text
    assert "call print_rle(r)" in out_text
    assert 'write(*,"(*(1x,i0))") r%lengths' in out_text
    assert "call print_real_vector(inverse_rle(r))" in out_text
    assert "all_equal(x, inverse_rle(r))" in out_text
    assert 'write(*,"(*(1x,i0))") pack(r2%lengths, r2%lengths >= 3)' in out_text
    assert "Run Length Encoding" in proc.stdout
    assert "3 2 4 1" in proc.stdout
    assert "1 1 1 2 2 5 5 5 5 1" in proc.stdout
    assert "T" in proc.stdout
    assert "a b a c" in proc.stdout
    assert "T F T" in proc.stdout
    assert "up" in proc.stdout
    assert "1 3" in proc.stdout
    assert "3 4" in proc.stdout


def test_xr2f_compile_failure_reports_likely_r_source_line(tmp_path: Path) -> None:
    local_input = tmp_path / "xcompile_hint.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- 1",
                'cat(y, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xcompile_hint.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode != 0
    assert "Likely R source for compile error:" in proc.stdout
    assert 'R line 2: cat(y, "\\n")' in proc.stdout


def test_xr2f_renames_loop_variable_conflicting_with_prior_array(tmp_path: Path) -> None:
    local_input = tmp_path / "xloop_conflict.r"
    local_input.write_text(
        "\n".join(
            [
                "k <- 3",
                "j <- numeric(3)",
                "print(j)",
                "for (j in 1:k) {",
                "  print(j)",
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xloop_conflict.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "Warning: R loop variable `j` at R line 4 reuses an earlier assigned name" in proc.stdout
    assert "translating loop variable as `j_loop`" in proc.stdout
    assert "j_loop" in out_text
    assert "do j = 1" not in out_text


def test_xr2f_pretty_formats_fortran_runtime_output(tmp_path: Path) -> None:
    local_input = tmp_path / "xpretty_probe.r"
    local_input.write_text('cat("x:", 1 / 3, "\\n")\n', encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out-dir", str(tmp_path), "--run", "--pretty"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "x: 0.333333333333333" in proc.stdout


def test_xr2f_pretty_strips_r_vector_indices_in_run_both(tmp_path: Path) -> None:
    local_input = tmp_path / "xpretty_r_probe.r"
    local_input.write_text("print(c(1, 2, 3))\n", encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out-dir", str(tmp_path), "--run-both", "--pretty"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run (r): PASS" in proc.stdout
    assert "\n[1] 1 2 3" not in proc.stdout
    assert "\n1 2 3" in proc.stdout


def test_xr2f_round_formats_fortran_runtime_output(tmp_path: Path) -> None:
    local_input = tmp_path / "xround_probe.r"
    local_input.write_text('cat("x:", 1 / 3, 2.34567, -0.0049, "\\n")\n', encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out-dir", str(tmp_path), "--run", "--round", "2"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "x: 0.33 2.35 -0.00" in " ".join(proc.stdout.split())


def test_xr2f_round_both_formats_r_and_fortran_runtime_output(tmp_path: Path) -> None:
    local_input = tmp_path / "xround_both_probe.r"
    local_input.write_text('cat("x:", 1 / 3, 2.34567, -0.0049, "\\n")\n', encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out-dir", str(tmp_path), "--run-both", "--round-both", "2"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    normalized = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run (r): PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert normalized.count("x: 0.33 2.35 -0.00") == 2


def test_xr2f_round_and_round_both_conflict(tmp_path: Path) -> None:
    local_input = tmp_path / "xround_conflict.r"
    local_input.write_text('cat("x:", 1 / 3, "\\n")\n', encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--round", "2", "--round-both", "2"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 1
    assert "Options conflict: --round and --round-both cannot be used together." in proc.stdout


def test_xr2f_var_order_matrix_list_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xvar_order_matrix_list.r"
    local_input.write_text(
        "\n".join(
            [
                "make_a <- function(x, order) {",
                "  p <- ncol(x)",
                '  a <- vector("list", order)',
                "  for (j in 1:order) {",
                "    row1 <- (j - 1) * p + 1",
                "    row2 <- j * p",
                "    a[[j]] <- t(x[row1:row2, , drop = FALSE])",
                "  }",
                "  out <- list(a = a)",
                "  return(out)",
                "}",
                "use_a <- function(a) {",
                "  p <- nrow(a[[1]])",
                "  s <- diag(p)",
                "  for (j in 1:length(a)) {",
                "    s <- s - a[[j]]",
                "  }",
                "  s",
                "}",
                "intercept_from_mu_a <- function(mu, a) {",
                "  p <- length(mu)",
                "  amat <- diag(p)",
                "  for (j in 1:length(a)) {",
                "    amat <- amat - a[[j]]",
                "  }",
                "  intercept <- as.numeric(amat %*% mu)",
                "  return(intercept)",
                "}",
                "make_fit <- function(a) {",
                "  out <- list(intercept = c(0.1, 0.2), mu = c(1.0, -2.0), a = a, sigma = matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2))",
                "  return(out)",
                "}",
                "compare_fit <- function(fit, mu_true, a_true, sigma_true) {",
                "  order <- length(a_true)",
                "  intercept_true <- intercept_from_mu_a(mu_true, a_true)",
                "  print(fit$intercept - intercept_true)",
                "  print(fit$mu - mu_true)",
                "  for (j in 1:order) print(fit$a[[j]] - a_true[[j]])",
                "  print(fit$sigma - sigma_true)",
                "  return(invisible(NULL))",
                "}",
                "x <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), nrow = 4, ncol = 2)",
                "fit <- make_a(x, 2)",
                'cat("len:", length(fit$a), "\\n")',
                "print(intercept_from_mu_a(c(1.0, -2.0), fit$a))",
                'cat("det:", det(use_a(fit$a)), "\\n")',
                "compare_fit(",
                "  make_fit(fit$a),",
                "  c(1.0, -2.0), fit$a, matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2)",
                ")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xvar_order_matrix_list.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "a(:,:,:)" in out_text
    assert "a(:,:,j)" in out_text
    assert "size(a, 3)" in out_text


def test_xr2f_named_matrix_column_which_min_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xnamed_matrix_column.r"
    local_input.write_text(
        "\n".join(
            [
                "table <- matrix(c(",
                "  0.0, 10.0, 100.0,",
                "  1.0,  5.0, 200.0,",
                "  2.0,  7.0, 150.0",
                "), nrow = 3, byrow = TRUE)",
                'colnames(table) <- c("order", "aic", "bic")',
                'aic_order <- table[which.min(table[, "aic"]), "order"]',
                'bic_order <- table[which.min(table[, "bic"]), "order"]',
                'cat("aic:", aic_order, "\\n")',
                'cat("bic:", bic_order, "\\n")',
                'cat("orders:", aic_order, bic_order, "\\n")',
                "print(round(table, 4))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnamed_matrix_column.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "table(minloc(table(:, 2), dim=1), 1)" in flat_out
    assert "table(minloc(table(:, 3), dim=1), 1)" in flat_out
    assert "integer :: aic_order, bic_order" in out_text
    assert "call print_matrix_rstyle_named(" in out_text
    assert '"order"' in out_text
    assert '"aic"' in out_text
    assert '"bic"' in out_text


def test_xr2f_round_matrix_print_uses_matrix_printer(tmp_path: Path) -> None:
    local_input = tmp_path / "xround_matrix_print.r"
    local_input.write_text(
        "\n".join(
            [
                "m <- matrix(c(1.23456, 2.34567, 3.45678, 4.56789), nrow = 2)",
                'cat("rows:", nrow(m), "\\n")',
                "print(round(m, 4))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xround_matrix_print.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "call print_matrix(" in out_text
    assert "call print_real_vector" not in out_text


def test_xr2f_ignores_declare_and_lowers_double_constructor(tmp_path: Path) -> None:
    local_input = tmp_path / "xdeclare_double.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(a) {",
                "  declare(type(a = double(NA)))",
                "  out <- double(length(a))",
                "  out[1] <- a[1]",
                "  out",
                "}",
                "print(sum(f(c(2, 3))))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out-dir", str(tmp_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout


def test_xr2f_s3_scalar_method_receiver_can_be_elemental(tmp_path: Path) -> None:
    local_input = tmp_path / "xs3_elemental.r"
    local_input.write_text(
        "\n".join(
            [
                "stat_point <- function(x) {",
                "  obj <- list(x = as.numeric(x))",
                '  class(obj) <- "stat_point"',
                "  obj",
                "}",
                "center <- function(object) UseMethod(\"center\")",
                "center.default <- function(object) mean(object)",
                "center.stat_point <- function(object) mean(object$x)",
                "a <- stat_point(c(2, 4, 6, 8))",
                'cat("s3 center:", center(a), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xs3_elemental.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "pure elemental function center_stat_point" in out_path.read_text(encoding="utf-8")


def test_xr2f_for_array_prints_element_directly(tmp_path: Path) -> None:
    local_input = tmp_path / "xprint_loop.r"
    local_input.write_text(
        "\n".join(
            [
                "y <- c(1, 4, 9)",
                "for (v in y) print(v)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprint_loop.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "v = y(i_v)" not in out_text
    assert "do i_v = 1, size(y)" not in out_text
    assert 'write(*,"(f0.6)") y' in out_text


def test_xr2f_for_range_prints_implied_do(tmp_path: Path) -> None:
    local_input = tmp_path / "xprint_range_loop.r"
    local_input.write_text(
        "\n".join(
            [
                "for (i in 1:3) print(i)",
                "for (j in 1:3) print(j + 1)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprint_range_loop.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "do i = int(1), int(3)" not in out_text
    assert 'write(*,"(i0)") (i,i=1,3)' in out_text
    assert 'write(*,"(i0)") (j + 1,j=1,3)' in out_text


def test_xr2f_s3_class_vector_inherits_parent_method(tmp_path: Path) -> None:
    local_input = tmp_path / "xs3_more.r"
    local_input.write_text(
        "\n".join(
            [
                "stat_point <- function(x) {",
                "  obj <- list(x = as.numeric(x))",
                '  class(obj) <- "stat_point"',
                "  obj",
                "}",
                "weighted_point <- function(x, weight) {",
                "  obj <- list(x = as.numeric(x), weight = as.numeric(weight))",
                '  class(obj) <- c("weighted_point", "stat_point")',
                "  obj",
                "}",
                "center <- function(object) UseMethod(\"center\")",
                "center.default <- function(object) mean(object)",
                "center.stat_point <- function(object) mean(object$x)",
                "center.weighted_point <- function(object) sum(object$x * object$weight) / sum(object$weight)",
                "spread <- function(object) UseMethod(\"spread\")",
                "spread.default <- function(object) max(object) - min(object)",
                "spread.stat_point <- function(object) max(object$x) - min(object$x)",
                "w <- weighted_point(c(2, 4, 6, 8), c(1, 1, 2, 4))",
                'cat("weighted class:", class(w), "\\n")',
                'cat("weighted center:", center(w), "\\n")',
                'cat("inherited spread:", spread(w), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xs3_more.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert '"weighted_point stat_point"' in out_text
    assert "pure elemental function spread_weighted_point" in out_text
    assert "spread_weighted_point(w)" in out_text


def test_xr2f_s4_static_class_and_method_dispatch(tmp_path: Path) -> None:
    local_input = tmp_path / "xs4_static.r"
    local_input.write_text(
        "\n".join(
            [
                'setClass("stat_point", slots = c(x = "numeric"))',
                'setGeneric("center", function(object) standardGeneric("center"))',
                'setMethod("center", "stat_point", function(object) mean(object@x))',
                'setGeneric("spread", function(object) standardGeneric("spread"))',
                'setMethod("spread", "stat_point", function(object) max(object@x) - min(object@x))',
                'a <- new("stat_point", x = c(2, 4, 6, 8))',
                'cat("s4 center:", center(a), "\\n")',
                'cat("s4 spread:", spread(a), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xs4_static.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "type :: stat_point_result_t" in out_text
    assert "pure elemental function center_stat_point" in out_text
    assert "object%x" in out_text
    assert "center_stat_point(a)" in out_text


def test_xr2f_s4_inheritance_slots_and_inherited_methods(tmp_path: Path) -> None:
    local_input = tmp_path / "xs4_more.r"
    local_input.write_text(
        "\n".join(
            [
                'setClass("stat_point", slots = c(x = "numeric"))',
                'setClass("weighted_point", contains = "stat_point", slots = c(weight = "numeric"))',
                'invisible(setGeneric("center", function(object) standardGeneric("center")))',
                'setMethod("center", "stat_point", function(object) mean(object@x))',
                'setMethod("center", "weighted_point", function(object) sum(object@x * object@weight) / sum(object@weight))',
                'invisible(setGeneric("spread", function(object) standardGeneric("spread")))',
                'setMethod("spread", "stat_point", function(object) max(object@x) - min(object@x))',
                'invisible(setGeneric("total_weight", function(object) standardGeneric("total_weight")))',
                'setMethod("total_weight", "weighted_point", function(object) sum(object@weight))',
                'w <- new("weighted_point", x = c(2, 4, 6, 8), weight = c(1, 1, 2, 4))',
                'cat("weighted class:", class(w), "\\n")',
                'cat("weighted is stat:", is(w, "stat_point"), "\\n")',
                'cat("weighted center:", center(w), "\\n")',
                'cat("inherited spread:", spread(w), "\\n")',
                'cat("total weight:", total_weight(w), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xs4_more.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "type :: weighted_point_result_t" in out_text
    assert "real(kind=dp), allocatable :: x(:), weight(:)" in out_text
    assert "pure elemental function spread_weighted_point" in out_text
    assert "type(weighted_point_result_t), intent(in) :: object" in out_text
    assert 'write(*,*) "weighted class: ", "weighted_point"' in out_text
    assert 'write(*,*) "weighted is stat: ", .true.' in out_text
    assert "center_weighted_point(w)" in out_text
    assert "spread_weighted_point(w)" in out_text


def test_xr2f_quickr_gap_helpers_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xquickr_gap.r"
    local_input.write_text(
        "\n".join(
            [
                "v <- c(3, 1, 4, 2)",
                "vna <- c(3, NA, 4, 2)",
                "d <- as.double(v)",
                "m <- rbind(c(1, 2, 3), c(4, 5, 6), c(7, 8, 9), c(10, 11, 12))",
                "ch <- character(3)",
                "rw <- raw(4)",
                "shaped <- 1:6",
                "dim(shaped) <- c(2, 3)",
                'cat("prod:", prod(v), "\\n")',
                'cat("prod na:", prod(vna, na.rm = TRUE), "\\n")',
                'cat("as double:", d, "\\n")',
                'cat("rev:", rev(v), "\\n")',
                'cat("which max:", which.max(v), "\\n")',
                'cat("which min:", which.min(v), "\\n")',
                'cat("rep int:", rep.int(2, 4), "\\n")',
                'cat("rep int vec:", rep.int(c(1, 2), c(2, 3)), "\\n")',
                'cat("dim:", dim(m), "\\n")',
                'cat("dim assign:", shaped[2, 3], "\\n")',
                'cat("character len:", length(ch), "\\n")',
                'cat("raw len:", length(rw), "\\n")',
                'cat("drop row:", drop(m[1, ]), "\\n")',
                'cat("drop col:", drop(m[, 2]), "\\n")',
                'cat("mod:", Mod(-3), "\\n")',
                'cat("re:", Re(3), "\\n")',
                'cat("im:", Im(3), "\\n")',
                'cat("conj:", Conj(3), "\\n")',
                'cat("arg neg:", Arg(-3), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xquickr_gap.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "product(v)" in out_text
    assert "product(pack(vna, .not. is_na(vna)))" in out_text
    assert "v(size(v):1:-1)" in out_text
    assert "maxloc(v, dim=1)" in out_text
    assert "minloc(v, dim=1)" in out_text
    assert "r_rep_int([2], times=4)" in out_text
    assert "r_rep_int([1, 2], times_vec=[2, 3])" in out_text
    assert "shape(m)" in out_text
    assert "transpose(reshape(" in out_text
    assert "[3, 4]" in out_text
    assert "r_character(3)" in out_text
    assert "r_rep_int([0], times=4)" in out_text
    assert "shaped(2, 3)" in out_text
    assert "m(:, 2)" in out_text
    assert "abs(-3)" in out_text
    assert "0.0_dp * real(3, kind=dp)" in out_text
    assert "merge(acos(-1.0_dp), 0.0_dp, real(-3, kind=dp) < 0.0_dp)" in out_text


def test_xr2f_colnames_variable_drives_integer_table_columns(tmp_path: Path) -> None:
    local_input = tmp_path / "xlabel_table.r"
    local_input.write_text(
        "\n".join(
            [
                'labels <- c("p", "q", "nobs", "loglik", "convergence")',
                "tab <- matrix(c(0, 1, 100, 12.5, 0, 1, 0, 99, 11.25, 1), nrow = 2, byrow = TRUE)",
                "colnames(tab) <- labels",
                "print(tab)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xlabel_table.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    compact_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert 'call print_matrix_rstyle_named(tab, [character(len=11) :: "p", "q", "nobs"' in out_text
    assert "int_cols=[.true., .true., .true., .false., .true.]" in compact_out
    assert "Run: PASS" in proc.stdout
    assert "[1,]            0            1          100      12.5000            0" in proc.stdout


def test_xr2f_optional_null_vectors_and_matrix_row_broadcast_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xmix_regression.r"
    local_input.write_text(
        "\n".join(
            [
                "choose_len <- function(x, a = NULL) {",
                "  out <- length(x)",
                "  if (!is.null(a)) out <- length(a)",
                "  out",
                "}",
                "m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)",
                "r <- apply(m, 1, max)",
                "d <- exp(m - r)",
                "p <- d / rowSums(d)",
                "z <- findInterval(c(0.1, 0.8, 0.95), cumsum(c(0.7, 0.2, 0.1))) + 1L",
                "k <- 3",
                "z <- pmin(z, k)",
                'cat("n:", choose_len(c(1, 2, 3)), "\\n")',
                'cat("r:", sum(r), "\\n")',
                'cat("p:", sum(p), "\\n")',
                'cat("z:", sum(z), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmix_regression.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "allocate(a_def(0))" in out_text
    assert "spread(r, dim=2" in out_text
    assert "spread(sum(d, dim=2), dim=2" in out_text
    assert "findInterval" in out_text
    assert "pmin=-1" not in out_text


def test_xr2f_optional_init_stopifnot_checks_initialized_vector(tmp_path: Path) -> None:
    local_input = tmp_path / "xoptional_init_check.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(x, mu_init = NULL, k = NULL) {",
                "  if (!is.null(mu_init)) {",
                "    k <- length(mu_init)",
                "  } else if (is.null(k)) {",
                "    k <- length(x)",
                "  }",
                "  if (is.null(mu_init)) {",
                "    mu <- x",
                "  } else {",
                "    mu <- mu_init",
                "  }",
                "  stopifnot(length(mu) == k)",
                "  sum(mu)",
                "}",
                "x <- c(1, 2, 3)",
                'cat("s:", f(x, k = 3), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xoptional_init_check.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "size(mu) /= k_def" in out_text
    assert "size(mu_init_def) /= k_def" not in out_text


def test_xr2f_compiles_numeric_list_arrays_and_return_alias(tmp_path: Path) -> None:
    local_input = tmp_path / "xmv_list_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_obj <- function(n, mu_list, sigma_list) {",
                "  p <- length(mu_list[[1]])",
                "  x <- matrix(NA_real_, nrow = n, ncol = p)",
                "  r <- chol(sigma_list[[1]])",
                "  z <- backsolve(r, t(sweep(x, 2, mu_list[[1]], \"-\")), transpose = TRUE)",
                "  x[1, ] <- mu_list[[1]]",
                "  y <- list(x = x, z = z)",
                "  return(y)",
                "}",
                "mu_true <- list(c(-2.0, 0.0), c(2.0, 1.0))",
                "sigma_true <- list(",
                "  matrix(c(1.0, 0.5, 0.5, 1.5), nrow = 2, byrow = TRUE),",
                "  matrix(c(1.2, -0.4, -0.4, 0.8), nrow = 2, byrow = TRUE)",
                ")",
                "obj <- make_obj(1, mu_true, sigma_true)",
                'cat("ok", "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmv_list_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "type :: make_obj_result_t" in out_text
    assert "mu_list(:,1)" in out_text
    assert "sigma_list(:,:,1)" in out_text
    assert "sigma_list(:,:,:)" in out_text


def test_xr2f_scalar_loglik_and_invisible_side_effect_call_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xinvisible_fit_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_fit <- function() {",
                "  tau <- matrix(c(0.1, 0.9, 0.8, 0.2), nrow = 2, byrow = TRUE)",
                "  class <- max.col(tau)",
                "  loglik <- 1.25",
                "  y <- list(loglik = loglik, class = class)",
                "  return(y)",
                "}",
                "show_fit <- function(fit) {",
                '  cat("ll:", fit$loglik, "\\n")',
                "  return(invisible(NULL))",
                "}",
                "fit <- make_fit()",
                "show_fit(fit)",
                'cat("class:", sum(fit$class), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xinvisible_fit_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run: PASS" in proc.stdout
    assert "real(kind=dp) :: loglik" in out_text
    assert "integer, allocatable :: class(:)" in out_text
    assert "subroutine show_fit(fit)" in out_text
    assert "call show_fit(fit)" in out_text
    assert "ignore_val = show_fit(fit)" not in out_text
    assert "print *, show_fit(fit)" not in out_text


def test_xr2f_lm_fit_list_return_alias_reused_name_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xar_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "fit_ar1 <- function(x) {",
                "  n <- length(x)",
                "  y <- x[2:n]",
                "  lagx <- x[1:(n - 1)]",
                "  fit <- lm(y ~ lagx)",
                "  intercept <- coef(fit)[1]",
                "  phi <- coef(fit)[2]",
                "  resid <- residuals(fit)",
                "  sigma <- sqrt(sum(resid^2) / length(resid))",
                "  y <- list(intercept = intercept, phi = phi, sigma = sigma, fitted = fitted(fit), resid = resid, lm_fit = fit)",
                "  return(y)",
                "}",
                "set.seed(1)",
                "x <- runif(5)",
                "fit <- fit_ar1(x)",
                'cat("phi:", fit$phi, "\\n")',
                "print(summary(fit$lm_fit))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xar_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "type :: fit_ar1_result_t" in out_text
    assert "type(lm_fit_t) :: fit" in out_text
    assert "type(lm_fit_t) :: lm_fit" in out_text
    assert "real(kind=dp), allocatable :: lagx(:), resid(:), y(:)" in out_text
    assert "type(fit_ar1_result_t) :: y_list" in out_text
    assert "call print_lm_summary(fit%lm_fit)" in out_text


def test_xr2f_list_return_alias_uses_base_name_for_renamed_symbol(tmp_path: Path) -> None:
    local_input = tmp_path / "xshape_alias.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(x) {",
                "  y <- x[1:3]",
                "  y <- mean(y)",
                "  y <- list(value = y, n = length(x))",
                "  return(y)",
                "}",
                "out <- f(c(1, 2, 3, 4))",
                'cat("v:", out$value, "\\n")',
                'cat("n:", out$n, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xshape_alias.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "type(f_result_t) :: f_result, y_list" in out_text
    assert "y_2_list" not in out_text


def test_xr2f_lm_dot_data_frame_matrix_predictors_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xar_order_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "simulate_ar <- function(n, phi, x0 = NULL) {",
                "  p <- length(phi)",
                "  x <- numeric(n)",
                "  if (is.null(x0)) {",
                "    x[1:p] <- 0",
                "  } else {",
                "    if (length(x0) != p) stop(\"bad x0\")",
                "    x[1:p] <- x0",
                "  }",
                "  x",
                "}",
                "fit_ar <- function(x, p) {",
                "  n <- length(x)",
                "  y <- x[(p + 1):n]",
                "  xlag <- matrix(NA_real_, nrow = n - p, ncol = p)",
                "  for (j in 1:p) xlag[, j] <- x[(p + 1 - j):(n - j)]",
                "  colnames(xlag) <- paste0(\"lag\", 1:p)",
                "  df <- data.frame(y = y, xlag)",
                "  fit <- lm(y ~ ., data = df)",
                "  phi <- as.numeric(coef(fit)[-1])",
                "  names(phi) <- paste0(\"phi\", 1:p)",
                "  y <- list(phi = phi, lm_fit = fit)",
                "  return(y)",
                "}",
                "x <- c(1, 2, 3, 4, 5, 6)",
                "fit <- fit_ar(x, p = 2)",
                'cat("ncoef:", length(fit$lm_fit$coef), "\\n")',
                'cat("phi:", fit$phi, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xar_order_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "data.frame" not in out_text
    assert "fit = lm_fit_general(y, xlag)" in out_text
    assert "names(phi)" not in out_text
    assert "real(kind=dp), intent(in), optional :: x0(:)" in out_text
    assert "integer, intent(in) :: p" in out_text


def test_xr2f_manual_design_matrix_ar_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xxar_order_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "fit_ar <- function(x, p) {",
                "  n <- length(x)",
                "  y <- x[(p + 1):n]",
                "  xlag <- matrix(NA_real_, nrow = n - p, ncol = p)",
                "  for (j in 1:p) xlag[, j] <- x[(p + 1 - j):(n - j)]",
                "  design <- cbind(1.0, xlag)",
                "  coef_vec <- solve(crossprod(design), crossprod(design, y))",
                "  fitted <- as.numeric(design %*% coef_vec)",
                "  resid <- y - fitted",
                "  y <- list(fitted = fitted, resid = resid, coef = coef_vec)",
                "  return(y)",
                "}",
                "x <- c(1, 2, 3, 4, 5, 6)",
                "fit <- fit_ar(x, p = 2)",
                'cat("nfitted:", length(fit$fitted), "\\n")',
                'cat("coef:", fit$coef, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xxar_order_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "design = cbind(1.0_dp, xlag)" not in out_text
    assert "allocate(design(size(xlag, 1), size(xlag, 2) + 1))" in out_text
    assert "fitted = r_matmul(design, coef_vec)" in out_text
    assert "pure function solve_real" not in out_text


def test_xr2f_var_drop_false_and_matrix_solve_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xvar_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "rmvnorm_chol <- function(n, mu, sigma) {",
                "  z <- matrix(rnorm(n * length(mu)), nrow = n)",
                "  y <- sweep(z %*% chol(sigma), 2, mu, \"+\")",
                "  return(y)",
                "}",
                "simulate_var1 <- function(n, mu, a, sigma) {",
                "  p <- length(mu)",
                "  if (!all(dim(a) == c(p, p))) stop(\"bad a\")",
                "  if (!all(dim(sigma) == c(p, p))) stop(\"bad sigma\")",
                "  x <- matrix(NA_real_, nrow = n, ncol = p)",
                "  x[1, ] <- mu",
                "  for (t in 2:n) {",
                "    mean_t <- mu + as.numeric(a %*% (x[t - 1, ] - mu))",
                "    x[t, ] <- rmvnorm_chol(1, mean_t, sigma)",
                "  }",
                "  return(x)",
                "}",
                "fit_var1 <- function(x) {",
                "  n <- nrow(x)",
                "  y <- x[2:n, , drop = FALSE]",
                "  xlag <- x[1:(n - 1), , drop = FALSE]",
                "  design <- cbind(1.0, xlag)",
                "  coef_mat <- solve(crossprod(design), crossprod(design, y))",
                "  a <- t(coef_mat[-1, , drop = FALSE])",
                "  resid <- y - design %*% coef_mat",
                "  sigma <- crossprod(resid) / nrow(resid)",
                "  amat <- diag(ncol(x)) - a",
                "  mu <- as.numeric(solve(amat, as.numeric(coef_mat[1, ])))",
                "  y <- list(mu = mu, a = a, sigma = sigma, resid = resid, fitted = design %*% coef_mat)",
                "  return(y)",
                "}",
                "mu <- c(1, -2)",
                "a <- matrix(c(0.6, 0.2, -0.1, 0.5), nrow = 2, byrow = TRUE)",
                "sigma <- matrix(c(1, 0.4, 0.4, 2), nrow = 2, byrow = TRUE)",
                "x <- simulate_var1(8, mu, a, sigma)",
                "fit <- fit_var1(x)",
                'cat("na:", length(fit$a), "\\n")',
                'cat("ok:", length(fit$mu), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xvar_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "drop =" not in out_text
    assert "solve_real(r_matmul(transpose(design), design)" in out_text
    assert "reshape(rmvnorm_chol(" in out_text
    assert "real(kind=dp), allocatable :: a(:,:), amat(:,:), coef_mat(:,:)" in out_text


def test_xr2f_row_index_matrix_assignment_keeps_matrix_rhs_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xrow_matrix_assign.r"
    local_input.write_text(
        "\n".join(
            [
                "rmvnorm_chol <- function(n, mu, sigma) {",
                "  z <- matrix(rnorm(n * length(mu)), nrow = n)",
                "  y <- sweep(z %*% chol(sigma), 2, mu, \"+\")",
                "  return(y)",
                "}",
                "fill_rows <- function(n, mu) {",
                "  p <- length(mu)",
                "  comp <- sample.int(2, size = n, replace = TRUE)",
                "  sigma <- matrix(c(1.0, 0.0, 0.0, 1.0), nrow = p)",
                "  x <- matrix(NA_real_, nrow = n, ncol = p)",
                "  idx <- which(comp == 1)",
                "  if (length(idx) > 0) {",
                "    x[idx, ] <- rmvnorm_chol(length(idx), mu, sigma)",
                "  }",
                "  return(x)",
                "}",
                "x <- fill_rows(4, c(0.0, 1.0))",
                'cat("n:", length(x), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xrow_matrix_assign.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "fill_rows_result(idx, :) = rmvnorm_chol(size(idx), mu, sigma)" in out_text
    assert "fill_rows_result(idx, :) = reshape(rmvnorm_chol" not in out_text


def test_xr2f_roc_auc_manual_logical_subsets_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "39_roc_auc_manual.R"
    local_input.write_text(
        (REPO_ROOT / "r_stat_examples" / "39_roc_auc_manual.R").read_text(encoding="utf-8-sig"),
        encoding="utf-8",
    )
    out_path = tmp_path / "39_roc_auc_manual.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "real(kind=dp) :: count, neg, pos" not in out_text
    assert "real(kind=dp), allocatable :: neg(:), pos(:)" in out_text


def test_xr2f_logical_vector_subset_inference_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xlogical_subset_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(0.1, 0.8, 0.3, 0.9, 0.5)",
                "g <- c(1, 0, 1, 0, 1)",
                "y <- x[g == 1]",
                "z <- x[g == 0]",
                "print(sum(y[1] > z))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xlogical_subset_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "allocatable :: x(:), y(:), z(:)" in out_text or "allocatable :: y(:), z(:)" in out_text
    assert "y = pack(x, g == 1)" in out_text
    assert "z = pack(x, g == 0)" in out_text


def test_xr2f_data_frame_ordered_rows_do_not_emit_rank2_subset_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "26_survival_analysis_simple_life_table.R"
    local_input.write_text(
        (REPO_ROOT / "r_stat_examples" / "26_survival_analysis_simple_life_table.R").read_text(
            encoding="utf-8-sig"
        ),
        encoding="utf-8",
    )
    out_path = tmp_path / "26_survival_analysis_simple_life_table.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "dat(order_real(time), :)" not in out_text
    assert "data.frame order subset lowered to sorted column vectors" in out_text


def test_xr2f_preserves_named_vector_and_matrix_prints_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xnamed_output_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "v <- c(1.0, 2.0)",
                'names(v) <- c("alpha", "beta")',
                "print(v)",
                "m <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2)",
                'rownames(m) <- c("r1", "r2")',
                'colnames(m) <- c("c1", "c2")',
                "print(m)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnamed_output_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "print_named_real_vector" in out_text
    assert "print_table2" in out_text
    assert '"alpha"' in out_text
    assert '"c1"' in out_text
    assert '"r1"' in out_text


def test_xr2f_paste0_numeric_vector_names_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xpaste0_numeric_names_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "strikes <- c(80, 90, 100)",
                "price <- c(20.5, 12.25, 7.0)",
                'names(price) <- paste0("K=", strikes)',
                "print(price)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xpaste0_numeric_names_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "r_paste0_real" in out_text
    assert "print_named_real_vector" in out_text


@pytest.mark.parametrize("example_name", SUPPORTED_R_COMPILE_CASES)
def test_xr2f_compiles_supported_local_r_examples(tmp_path: Path, example_name: str) -> None:
    proc = _run_xr2f_compile(tmp_path, example_name)

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert (tmp_path / "r.f90").exists()
