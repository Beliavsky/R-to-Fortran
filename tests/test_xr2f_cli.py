from __future__ import annotations

import importlib.util
import os
import re
import subprocess
import sys
from pathlib import Path

import pytest


REPO_ROOT = Path(__file__).resolve().parents[1]
XR2F_PATH = REPO_ROOT / "xr2f.py"


def _load_xr2f_module():
    spec = importlib.util.spec_from_file_location("xr2f_for_test", XR2F_PATH)
    assert spec is not None and spec.loader is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module

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

STRESS_R_COMPILE_CASES = [
    "xarma.r",
    "xsolve.r",
    "xvar.r",
    "xvar_ic.r",
    "xxarray.r",
    "xgarch_dcc_t.r",
    "xreturns_mv_mix.r",
    "xreturns_var_yw.r",
    "xvarma_returns.r",
]

FULL_EXAMPLE_DIRS = [
    "r_examples",
    "r_stat_examples",
]


def _full_example_sources() -> list[Path]:
    seen: set[Path] = set()
    out: list[Path] = []
    for dirname in FULL_EXAMPLE_DIRS:
        for pattern in ("*.r", "*.R"):
            for path in sorted((REPO_ROOT / dirname).glob(pattern)):
                key = path.resolve()
                if key in seen:
                    continue
                seen.add(key)
                out.append(path)
    return out


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


def _split_top_level_commas_for_test(src: str) -> list[str]:
    parts: list[str] = []
    start = 0
    paren = bracket = 0
    for idx, ch in enumerate(src):
        if ch == "(":
            paren += 1
        elif ch == ")" and paren:
            paren -= 1
        elif ch == "[":
            bracket += 1
        elif ch == "]" and bracket:
            bracket -= 1
        elif ch == "," and paren == 0 and bracket == 0:
            parts.append(src[start:idx].strip())
            start = idx + 1
    tail = src[start:].strip()
    if tail:
        parts.append(tail)
    return parts


def _logical_fortran_lines_for_test(src: str) -> list[str]:
    logical: list[str] = []
    current = ""
    for physical in src.splitlines():
        line = physical.rstrip()
        if current:
            stripped = line.lstrip()
            if stripped.startswith("&"):
                stripped = stripped[1:].lstrip()
            current += " " + stripped
        else:
            current = line
        if current.rstrip().endswith("&"):
            current = current.rstrip()[:-1].rstrip()
            continue
        logical.append(current)
        current = ""
    if current:
        logical.append(current)
    return logical


def _has_fortran_decl(src: str, spec: str, entity: str) -> bool:
    spec_re = re.escape(spec.strip()).replace(r"\ ", r"\s+")
    entity_norm = re.sub(r"\s+", "", entity)
    for line in _logical_fortran_lines_for_test(src):
        code = line.split("!", 1)[0].strip()
        m_decl = re.match(rf"^{spec_re}\s*::\s*(.+)$", code, re.IGNORECASE)
        if m_decl is None:
            continue
        for part in _split_top_level_commas_for_test(m_decl.group(1)):
            if re.sub(r"\s+", "", part) == entity_norm:
                return True
    return False


def _has_fortran_decl_fragment(src: str, fragment: str) -> bool:
    m_decl = re.match(r"\s*(.+?)\s*::\s*(.+?)\s*$", fragment)
    if m_decl is None:
        return fragment in " ".join(src.replace("&", " ").split())
    return _has_fortran_decl(src, m_decl.group(1), m_decl.group(2))


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
    assert "n = integer(1)" in ann_text
    assert "z = double(1)" in ann_text
    assert "max.iter = integer(1)" in ann_text
    assert "max_iter = integer()" not in ann_text
    assert ann_text.index("f <- function") < ann_text.index("  declare(type(") < ann_text.index("  y <- numeric")


def test_xr2f_annotate_r_args_writes_only_argument_declares(tmp_path: Path) -> None:
    local_input = tmp_path / "xannotate_args_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(x, max.iter = 3) {",
                "  y <- numeric(length(x))",
                "  for (i in 1:max.iter) y[i] <- x[i]",
                "  return(y)",
                "}",
                "n <- 2",
                "z <- f(c(1.0, 2.0), max.iter = n)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xannotate_args_probe.f90"
    ann_path = tmp_path / "xannotate_args_probe_typed.r"

    proc = subprocess.run(
        [
            sys.executable,
            str(XR2F_PATH),
            str(local_input),
            "--out",
            str(out_path),
            "--annotate-r-args",
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
    assert "x = double(NA)" in ann_text
    assert "max.iter = integer(1)" in ann_text
    assert "y = double()" not in ann_text
    assert "i = integer()" not in ann_text
    assert "n = integer(1)" not in ann_text
    assert "z = double(1)" not in ann_text


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


def test_xr2f_leading_dot_variable_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xleading_dot_variable.r"
    local_input.write_text(
        "\n".join(
            [
                ".n = 3",
                "dot_n = 4",
                "n = 5",
                "cat(.n, dot_n, n)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xleading_dot_variable.f90"

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
    assert "3 4 5" in proc.stdout
    assert "dot_n_2 = 3" in out_text
    assert "dot_n = 4" in out_text
    assert "n = 5" in out_text
    assert ".n" not in out_text


def test_xr2f_dotted_function_direct_logical_return_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xis_prime_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "is.prime <- function(n) {",
                "  n == 2L || all(n %% 2L:max(2, floor(sqrt(n))) != 0)",
                "}",
                "for (i in 1:7) cat(i, is.prime(i), \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xis_prime_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "1 T" in proc.stdout
    assert "2 T" in proc.stdout
    assert "4 F" in proc.stdout
    assert "logical :: is_prime_result" in out_text
    assert "= =" not in out_text
    assert "logical :: n_2" not in out_text


def test_xr2f_sapply_shorthand_lambda_logical_mask_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xsapply_lambda_mask_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "digitsum <- function(x) sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)",
                "is.prime <- function(n) n == 2L || all(n %% 2L:max(2, floor(sqrt(n))) != 0)",
                "range_int <- 2:50",
                "v <- sapply(range_int, \\(x) is.prime(x) && is.prime(digitsum(x)))",
                'cat(paste("Found", length(range_int[v]), "additive primes less than 50"))',
                "print(range_int[v])",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsapply_lambda_mask_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "Found 10 additive primes less than 50" in proc.stdout
    assert "less than 50 2" in proc.stdout
    assert "2 3 5 7 11 23 29 41 43 47" in proc.stdout
    assert "logical, allocatable :: v(:)" in out_text
    assert "sapply(" not in out_text
    assert "pack(range_int, v)" in out_text


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


def test_xr2f_dotted_function_names_are_sanitized_consistently(tmp_path: Path) -> None:
    local_input = tmp_path / "xdotted_func_repro.r"
    local_input.write_text(
        "\n".join(
            [
                "is.prime <- function(n) {",
                "  if (n < 2L) return(FALSE)",
                "  if (n == 2L) return(TRUE)",
                "  all(n %% 2L:max(2L, floor(sqrt(n))) != 0L)",
                "}",
                "",
                "print.puz <- function(m) {",
                '  cat("puz:", m, "\\n")',
                "}",
                "",
                "for (i in 1:7) {",
                '  cat(i, is.prime(i), "\\n")',
                "}",
                "",
                "print.puz(15)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdotted_func_repro.f90"

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
    assert "is_prime" in out_text
    assert "print_puz" in out_text
    assert "is.prime" not in out_text
    assert "print.puz" not in out_text


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


def test_xr2f_scalar_reduction_function_result_keeps_dim_reduction_vector(tmp_path: Path) -> None:
    local_input = tmp_path / "xscalar_reduction_result_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "col_energy <- function(x) {",
                "  z <- matrix(x, nrow = 2)",
                "  q <- colSums(z * z)",
                "  p <- ncol(z)",
                "  base <- sum(diag(matrix(c(1.0, 0.0, 0.0, 1.0), nrow = 2)))",
                "  y <- -0.5 * (p + base + q)",
                "  return(y)",
                "}",
                "log_sum_exp <- function(x) {",
                "  xmax <- max(x)",
                "  if (!is.finite(xmax)) {",
                "    return(-Inf)",
                "  }",
                "  y <- xmax + log(sum(exp(x - xmax)))",
                "  return(y)",
                "}",
                "x <- c(1.0, 2.0, 3.0, 4.0)",
                "print(col_energy(x))",
                "print(log_sum_exp(x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xscalar_reduction_result_probe.f90"

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
    assert "real(kind=dp) :: log_sum_exp_result" in out_text
    assert "log_sum_exp_result(:)" not in out_text
    assert "real(kind=dp) :: xmax" in out_text
    assert "col_energy_result(:)" in out_text
    assert "real(kind=dp), allocatable :: q(:)" in out_text
    assert "q = sum(z**2, dim=1)" in out_text


def test_xr2f_scalar_reduction_result_and_vector_constructor_result_ranks(tmp_path: Path) -> None:
    local_input = tmp_path / "xreduction_rank_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "excess_kurtosis <- function(x) {",
                "  xc <- x - mean(x)",
                "  mean(xc^4) / mean(xc^2)^2 - 3.0",
                "}",
                "return_stats <- function(x) {",
                "  c(mean(x), sd(x), min(x), max(x))",
                "}",
                "x <- c(1.0, 2.0, 3.0, 4.0)",
                "print(excess_kurtosis(x))",
                "print(return_stats(x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreduction_rank_probe.f90"

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
    assert "real(kind=dp) :: excess_kurtosis_result" in out_text
    assert "excess_kurtosis_result(:)" not in out_text
    assert "real(kind=dp), allocatable :: xc(:)" in out_text
    assert "real(kind=dp), allocatable :: return_stats_result(:)" in out_text
    assert "return_stats_result = [" in out_text


def test_xr2f_filtered_renamed_argument_keeps_dummy_declaration_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xfiltered_arg_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "return_stats <- function(x) {",
                "  x <- x[is.finite(x)]",
                "  c(x[1], sd(x))",
                "}",
                "print(return_stats(c(1.0, 2.0, NA, 4.0)))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfiltered_arg_probe.f90"

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
    assert "function return_stats(x) result(return_stats_result)" in out_text
    assert "real(kind=dp), intent(in) :: x(:)" in out_text
    assert "real(kind=dp), allocatable :: x_2(:)" in out_text
    assert "x_2 = pack(x," in out_text.replace("&\n&", "")


def test_xr2f_function_returning_numeric_dataframe_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xdataframe_return_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_stats <- function(x) {",
                "  y <- x * x",
                "  data.frame(",
                "    time = seq_along(x),",
                "    value = y,",
                "    se = sqrt(y)",
                "  )",
                "}",
                "df <- make_stats(c(1.0, 2.0, 3.0))",
                "print(head(df))",
                "v <- df$value",
                "print(v)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdataframe_return_probe.f90"

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
    assert "type :: make_stats_result_t" in out_text
    assert "type(make_stats_result_t) :: make_stats_result" in out_text
    assert "call print_make_stats_result_head(df, 6)" in out_text
    assert "v = df%value" in out_text


def test_xr2f_parenthesized_power_base_preserves_grouping(tmp_path: Path) -> None:
    local_input = tmp_path / "xpower_group_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "nag_term <- function(e, h, theta, alpha) {",
                "  alpha * (e - theta * sqrt(h))^2",
                "}",
                "print(nag_term(3.0, 4.0, 0.5, 0.1))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xpower_group_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "0.4" in proc.stdout
    assert "(e - theta * sqrt(h))**2" in flat_out


def test_xr2f_scalar_power_sequence_result_is_vector(tmp_path: Path) -> None:
    local_input = tmp_path / "xpower_sequence_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "digitsum <- function(x) {",
                "  ndig <- nchar(x)",
                "  powers <- 0:(ndig - 1)",
                "  divisors <- 10^powers",
                "  digits <- floor(x / divisors) %% 10",
                "  sum(digits)",
                "}",
                "print(digitsum(438))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xpower_sequence_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "15" in proc.stdout
    assert "real(kind=dp), allocatable :: divisors(:)" in flat_out
    assert "divisors = 10**powers" in flat_out


def test_xr2f_numeric_nchar_digit_sum_and_character_nchar_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnchar_numeric_character_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "digitsum <- function(x) {",
                "  sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)",
                "}",
                "title_width <- function(title) {",
                "  nchar(title)",
                "}",
                "print(digitsum(123))",
                'print(title_width("abc"))',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnchar_numeric_character_probe.f90"

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
    assert "6" in proc.stdout
    assert "3" in proc.stdout
    assert "real(kind=dp), intent(in) :: x" in out_text
    assert "character(len=*), intent(in) :: title" in out_text
    assert "r_seq_int(0," in out_text
    assert "nchar(title)" in out_text


def test_xr2f_numeric_nchar_named_digit_sum_inline_reduction_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xdigitsum_named_inline_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "digitsum <- function(x) {",
                "  ndig <- nchar(x)",
                "  powers <- 0:(ndig - 1)",
                "  divisors <- 10^powers",
                "  sum(floor(x / divisors) %% 10)",
                "}",
                "print(digitsum(438))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdigitsum_named_inline_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "15" in proc.stdout
    assert "real(kind=dp), intent(in) :: x" in flat_out
    assert "real(kind=dp) :: ndig" in flat_out


def test_xr2f_length_null_named_value_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnull_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- NULL",
                "print(length(x))",
                "print(length(NULL))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnull_probe.f90"

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
    assert proc.stdout.count("0") >= 2
    assert "size(-1)" not in out_text
    assert 'write(*,"(i0)") 0' in out_text


def test_xr2f_static_get_literal_alias_and_loop_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xget_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- 10",
                "y <- 25",
                'name <- "x"',
                "print(get(name))",
                'name <- "y"',
                "print(get(name))",
                "a <- 1:3",
                "b <- 4:6",
                'names_to_get <- c("a", "b")',
                "for (nm in names_to_get) {",
                "  print(get(nm))",
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xget_probe.f90"

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
    assert "10" in proc.stdout
    assert "25" in proc.stdout
    assert "1 2 3" in proc.stdout
    assert "4 5 6" in proc.stdout
    assert " get(" not in out_text
    assert "get(name)" not in out_text
    assert "get(nm)" not in out_text
    assert 'if (nm == "a") then' in out_text


def test_xr2f_vector_filter_arithmetic_predicate_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfilter_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "z <- c(5, 2, -3, 8)",
                "w <- z[z*z > 8]",
                "print(w)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfilter_probe.f90"

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
    assert "5" in proc.stdout
    assert "-3" in proc.stdout
    assert "8" in proc.stdout
    assert "z * (z > 8)" not in out_text
    assert "pack(z, r_mul(z, z) > 8)" in out_text


def test_xr2f_filter_anonymous_predicate_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfilter_anonymous_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1, 2, 3, 4, 5, 6)",
                "y <- Filter(function(z) z %% 2 == 0, x)",
                "cat(y)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfilter_anonymous_probe.f90"

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
    assert "2.0000000000000000" in proc.stdout
    assert "4.0000000000000000" in proc.stdout
    assert "6.0000000000000000" in proc.stdout
    assert "Filter(" not in out_text
    assert "pack(x, mod(x, real(2, kind=dp)) == 0)" in out_text


def test_xr2f_find_anonymous_named_and_assignment_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfind_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(3, 7, 10, 15, 20)",
                "print(Find(function(z) z %% 5 == 0, x))",
                "a <- Find(function(z) z > 12, x)",
                "print(a)",
                'words <- c("cat", "dog", "elephant", "fox")',
                "first_word <- Find(function(s) nchar(s) > 5, words)",
                "print(first_word)",
                "x2 <- c(1, 3, 5, 7)",
                "print(Find(function(z) z %% 2 == 0, x2, nomatch = NA))",
                "is_large_even <- function(z) {",
                "  z %% 2 == 0 && z > 10",
                "}",
                "print(Find(is_large_even, x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfind_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "10" in proc.stdout
    assert "15" in proc.stdout
    assert "elephant" in proc.stdout
    assert "NA" in proc.stdout
    assert "20" in proc.stdout
    assert "Find(" not in out_text
    assert "first_word =" in out_text
    assert "is_large_even(int(x(" in out_text


def test_xr2f_position_anonymous_named_right_and_assignment_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xposition_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(3, 7, 10, 15, 20)",
                "print(Position(function(z) z %% 2 == 0, x))",
                "print(Position(function(z) z %% 5 == 0, x, right = TRUE))",
                "p <- Position(function(z) z > 12, x)",
                "print(p)",
                'words <- c("cat", "dog", "elephant", "fox")',
                "print(Position(function(s) nchar(s) > 5, words))",
                "x2 <- c(1, 3, 5, 7)",
                "print(Position(function(z) z %% 2 == 0, x2))",
                "print(Position(function(z) z %% 2 == 0, x2, nomatch = 0))",
                "is_large_even <- function(z) {",
                "  z %% 2 == 0 && z > 10",
                "}",
                "print(Position(is_large_even, x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xposition_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    for expected in ["3", "5", "4", "NA", "0"]:
        assert expected in proc.stdout
    assert "Position(" not in out_text
    assert "p =" in out_text
    assert "do i_pos_" in out_text
    assert "size(x), 1, -1" in out_text
    assert "is_large_even(int(x(" in out_text


def test_xr2f_negate_predicates_in_filter_find_and_position_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnegate_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "is_even <- function(x) x %% 2 == 0",
                "x <- c(1, 2, 3, 4, 5, 6)",
                "print(Filter(Negate(is_even), x))",
                "x <- c(10, NA, 20, NA, 30)",
                "print(Filter(Negate(is.na), x))",
                'words <- c("cat", "", "dog", "", "fox")',
                'print(Filter(Negate(function(s) s == ""), words))',
                "is_positive <- function(x) x > 0",
                "x <- c(3, 2, 1, 0, -1)",
                "print(Find(Negate(is_positive), x))",
                "is_short <- function(s) nchar(s) <= 3",
                'words <- c("cat", "dog", "elephant", "fox")',
                "print(Position(Negate(is_short), words))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnegate_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "1.0000000000000000 3.0000000000000000 5.0000000000000000" in proc.stdout
    assert "10.000000000000000 20.000000000000000 30.000000000000000" in proc.stdout
    assert "cat dog fox" in proc.stdout
    assert "0" in proc.stdout
    assert "3" in proc.stdout
    assert "Negate(" not in out_text
    assert ".not." in out_text


def test_xr2f_simple_vectorize_numeric_and_logical_wrappers_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xvectorize_supported_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "square_plus_one <- function(x) {",
                "  x^2 + 1",
                "}",
                "vsquare_plus_one <- Vectorize(square_plus_one)",
                "x <- c(-2, 0, 4, 7)",
                "print(vsquare_plus_one(x))",
                "between <- function(x, lo, hi) {",
                "  x >= lo && x <= hi",
                "}",
                "vbetween <- Vectorize(between)",
                "x <- c(1, 5, 10, 15)",
                "lo <- c(0, 4, 8, 20)",
                "hi <- c(2, 6, 12, 25)",
                "print(vbetween(x, lo, hi))",
                "payoff_call <- function(price, strike) {",
                "  max(price - strike, 0)",
                "}",
                "vpayoff_call <- Vectorize(payoff_call)",
                "prices <- c(80, 90, 100, 110, 120)",
                "strike <- 100",
                "print(vpayoff_call(prices, strike))",
                "grid_value <- function(x, y) {",
                "  x^2 + y^2",
                "}",
                "vgrid_value <- Vectorize(grid_value)",
                "x <- 1:3",
                "y <- 10:12",
                "print(vgrid_value(x, y))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xvectorize_supported_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "5 1 17 50" in proc.stdout
    assert "T T T F" in proc.stdout
    assert "0 0 0 10 20" in proc.stdout
    assert "101 125 153" in proc.stdout
    assert "Vectorize(" not in out_text
    assert "do i_vec_" in out_text
    assert "payoff_call(prices(i_vec_" in out_text


def test_xr2f_literal_vector_with_later_logical_assignment_not_parameter_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xset_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 20, 30, 40)",
                "x[x > 25] <- 0",
                "print(x)",
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
    assert "10 20 0 0" in proc.stdout
    assert "parameter :: x" not in out_text
    assert "where (x > 25)" in out_text


def test_xr2f_print_digits_vector_and_scalar_reductions_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xprint_sum_vec_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "print(sqrt(1:3), 3)",
                "print(sum(sqrt(1:3)))",
                "print(sum(sqrt(1:3)), 3)",
                "print(prod(sqrt(1:3)), 3)",
                "print(mean(sqrt(1:3)), 3)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprint_sum_vec_probe.f90"

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
    assert "4.15" in proc.stdout
    assert "2.45" in proc.stdout
    assert "1.38" in proc.stdout
    assert "call print_real_vector([real(sum(" in out_text
    assert "call print_real_vector([real(product(" in out_text
    assert "call print_real_vector(real(product(" not in out_text


def test_xr2f_show_alias_uses_print_translation_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xshow_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- 1:3",
                "show(x)",
                "show(sqrt(x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xshow_probe.f90"

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
    assert "1 2 3" in proc.stdout
    assert "call print_real_vector" in out_text
    assert "show(" not in out_text


def test_xr2f_el_vector_extraction_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xel_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 20)",
                "print(el(x, 1))",
                "print(el(x, 2))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xel_probe.f90"

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
    assert "10" in proc.stdout
    assert "20" in proc.stdout
    assert "x(1)" in out_text
    assert "x(2)" in out_text
    assert "el(" not in out_text


def test_xr2f_quoted_operator_calls_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xquoted_operator_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "y <- c(10, 20, 30)",
                "z <- c(TRUE, FALSE)",
                "print('+'(y, 4))",
                "print(`*`(y, 2))",
                "print('%%'(y, 6))",
                "print('>'(y, 15))",
                "print('!'(z))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xquoted_operator_probe.f90"

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
    assert "14" in proc.stdout
    assert "24" in proc.stdout
    assert "34" in proc.stdout
    assert "20" in proc.stdout
    assert "40" in proc.stdout
    assert "60" in proc.stdout
    assert "4 2 0" in proc.stdout
    assert ".not." in out_text
    assert "'+'" not in out_text
    assert "`*`" not in out_text


def test_xr2f_user_defined_binary_operator_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xuser_binary_operator_probe.r"
    local_input.write_text(
        "\n".join(
            [
                '"%a2b%" <- function(a, b) return(a + 2*b)',
                "print(3 %a2b% 5)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xuser_binary_operator_probe.f90"

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
    assert "13" in proc.stdout
    assert "function op_a2b" in out_text
    assert "op_a2b(" in out_text
    assert "%a2b%" not in out_text


def test_xr2f_function_vector_formal_flattens_matrix_actual_for_c_return_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xprint_matrix_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "z12 <- function(z) return(c(z, z^2))",
                "x <- c(10, 20)",
                "print(z12(x))",
                "y <- z12(matrix(x))",
                "print(length(y))",
                "print(y)",
                "print(z12(100))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprint_matrix_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "10 20 100 400" in proc.stdout
    assert "100 10000" in proc.stdout
    assert "4" in proc.stdout
    assert "intent(in) :: z(:)" in flat_out
    assert "z12(reshape(matrix(x, size(x), 1), [size(matrix(x, size(x), 1))]))" in flat_out
    assert "z12([100.0_dp])" in flat_out


def test_xr2f_transposed_sapply_named_c_fields_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xsapply_named_c_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_fit <- function(x) {",
                "  list(",
                "    mu = x,",
                "    omega = x + 1,",
                "    alpha = x + 2,",
                "    beta = x + 3,",
                "    loglik = x + 4,",
                "    convergence = 0",
                "  )",
                "}",
                "fits <- vector(\"list\", 2)",
                "for (i in seq_along(fits)) {",
                "  fits[[i]] <- make_fit(i)",
                "}",
                "garch_parameters <- t(sapply(fits, function(f) {",
                "  c(",
                "    mu = f$mu,",
                "    omega = f$omega,",
                "    alpha = f$alpha,",
                "    beta = f$beta,",
                "    persistence = f$alpha + f$beta,",
                "    loglik = f$loglik,",
                "    convergence = f$convergence",
                "  )",
                "}))",
                "print(garch_parameters)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsapply_named_c_probe.f90"

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
    assert "1" in proc.stdout
    assert "9" in proc.stdout
    assert "t(sapply" not in out_text
    assert "function(f)" not in out_text


def test_xr2f_obfuscates_transposed_sapply_named_c_fields_as_valid_r(tmp_path: Path) -> None:
    local_input = tmp_path / "xsapply_named_c_obf_probe.r"
    obf_path = tmp_path / "xsapply_named_c_obf_probe_obfuscated.r"
    local_input.write_text(
        "\n".join(
            [
                "make_fit <- function(x) {",
                "  list(mu = x, omega = x + 1, alpha = x + 2, beta = x + 3)",
                "}",
                "fits <- vector(\"list\", 2)",
                "for (i in seq_along(fits)) {",
                "  fits[[i]] <- make_fit(i)",
                "}",
                "garch_parameters <- t(sapply(fits, function(f) {",
                "  c(",
                "    mu = f$mu,",
                "    omega = f$omega,",
                "    persistence = f$alpha + f$beta",
                "  )",
                "}))",
                "print(garch_parameters)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )

    proc = subprocess.run(
        [
            sys.executable,
            str(XR2F_PATH),
            str(local_input),
            "--obfuscate-r",
            str(obf_path),
            "--check-obfuscated-r",
            "--compile",
        ],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    obf_text = obf_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run (obfuscated r): PASS" in proc.stdout
    assert "function(f)" not in obf_text
    assert "sapply" not in obf_text


def test_xr2f_keeps_column_reshape_matrix_print_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xreshape_print_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "A <- matrix(c(2, 0, 0, 3), nrow = 2, byrow = TRUE)",
                "ea <- eigen(A)",
                "lambda <- ea$values[1]",
                "v_pr <- ea$vectors[, 1]",
                "print(A %*% v_pr)",
                "print(lambda * v_pr)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreshape_print_probe.f90"

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
    assert "call print_matrix(reshape(v_pr, [size(v_pr), 1]))" in flat_out
    assert "call print_real_vector(reshape(v_pr, [size(v_pr), 1]))" not in flat_out


def test_xr2f_vector_constructor_assignment_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xvector_probe.r"
    local_input.write_text(
        "\n".join(
            [
                'y <- vector("numeric", 3)',
                "y[1] <- 10",
                "y[2] <- 20",
                "print(y)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xvector_probe.f90"

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
    assert "y(1) = 10" in out_text
    assert "call print_real_vector(y)" in out_text


def test_xr2f_mode_function_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xmode_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1, 2, 3)",
                "print(mode(x))",
                "print(mode(TRUE))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmode_probe.f90"

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
    assert "numeric" in proc.stdout
    assert "logical" in proc.stdout
    assert " mode(" not in out_text


def test_xr2f_hist_explicit_breaks_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xhist_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.5, 2.5, 3.5, 4.5)",
                "h <- hist(x, breaks = c(1, 2, 3, 4, 5), plot = FALSE)",
                "print(h$breaks)",
                "print(h$counts)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xhist_probe.f90"

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
    assert "1 2 3 4 5" in proc.stdout
    assert "1 1 1 1" in proc.stdout
    assert "type(hist_result_t)" in out_text
    assert "%breaks" in out_text
    assert "%counts" in out_text


def test_xr2f_file_exists_and_file_create_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfile_exists_create_repro.r"
    local_input.write_text(
        "\n".join(
            [
                'fname <- "xr2f_file_exists_create_probe.tmp"',
                "if (file.exists(fname)) {",
                "  file.remove(fname)",
                "}",
                'cat("before:", file.exists(fname), "\\n")',
                "ok <- file.create(fname)",
                'cat("create:", ok, "\\n")',
                'cat("after:", file.exists(fname), "\\n")',
                "if (file.exists(fname)) {",
                "  file.remove(fname)",
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfile_exists_create_repro.f90"

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
    flat_stdout = " ".join(proc.stdout.split())
    assert "before: F" in flat_stdout
    assert "create: T" in flat_stdout
    assert "after: T" in flat_stdout
    assert "file_exists(fname)" in out_text
    assert "file_create(fname)" in out_text


def test_xr2f_optional_file_arg_does_not_rewrite_file_exists_name(tmp_path: Path) -> None:
    local_input = tmp_path / "xoptional_file_exists_repro.r"
    local_input.write_text(
        "\n".join(
            [
                'check_file <- function(file = "xr2f_optional_file_exists_probe.tmp") {',
                '  if (!file.exists(file)) stop("missing: ", file)',
                "  file.exists(file)",
                "}",
                'fname <- "xr2f_optional_file_exists_probe.tmp"',
                "file.create(fname)",
                "print(check_file())",
                "file.remove(fname)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xoptional_file_exists_repro.f90"

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
    assert "T" in proc.stdout
    assert "file_def.exists" not in out_text
    assert "file_exists(file_def)" in out_text


def test_xr2f_getwd_and_dir_create_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xdir_repro.r"
    local_input.write_text(
        "\n".join(
            [
                "root <- getwd()",
                'd <- "xr2f_dir_create_probe"',
                "if (file.exists(d)) {",
                "  file.remove(d)",
                "}",
                'cat("cwd nonempty:", nchar(root) > 0, "\\n")',
                'cat("exists before:", file.exists(d), "\\n")',
                "ok <- dir.create(d)",
                'cat("created:", ok, "\\n")',
                'cat("exists after:", file.exists(d), "\\n")',
                "if (file.exists(d)) {",
                "  file.remove(d)",
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdir_repro.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_stdout = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "cwd nonempty: T" in flat_stdout
    assert "exists before: F" in flat_stdout
    assert "created: T" in flat_stdout
    assert "exists after: T" in flat_stdout
    assert "getwd()" in out_text
    assert "dir_create(d)" in out_text


def test_xr2f_file_info_print_and_fields_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfile_info_probe.r"
    local_input.write_text(
        "\n".join(
            [
                'fname <- "xr2f_file_info_probe.tmp"',
                "if (file.exists(fname)) file.remove(fname)",
                "file.create(fname)",
                "xinfo <- file.info(fname)",
                "print(xinfo)",
                'cat("size:", xinfo$size, "\\n")',
                'cat("isdir:", xinfo$isdir, "\\n")',
                "file.remove(fname)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfile_info_probe.f90"

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
    assert "size:" in proc.stdout
    assert "isdir:" in proc.stdout
    assert "type(file_info_t) :: xinfo" in out_text
    assert "xinfo = file_info(fname)" in out_text
    assert "call print_file_info(xinfo)" in out_text
    assert "xinfo%size" in out_text
    assert "xinfo%isdir" in out_text


def test_xr2f_ave_group_mean_sum_length_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xave_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 12, 14, 20, 25, 30)",
                'g <- c("A", "A", "A", "B", "B", "B")',
                "print(ave(x, g, FUN = mean))",
                "x_centered <- x - ave(x, g, FUN = mean)",
                "print(x_centered)",
                "print(ave(x, g, FUN = sum))",
                "print(ave(x, g, FUN = length))",
                "ratio_to_group_mean <- x / ave(x, g, FUN = mean)",
                "print(ratio_to_group_mean)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xave_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_stdout = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "12 12 12 25 25 25" in flat_stdout
    assert "-2.000000" in flat_stdout
    assert "-5.000000" in flat_stdout
    assert "36 36 36 75 75 75" in flat_stdout
    assert "3 3 3 3 3 3" in flat_stdout
    assert "0.833333" in flat_stdout
    assert 'ave(real(x, kind=dp), g, "mean")' in out_text
    assert "FUN =" not in out_text


def test_xr2f_ave_multiple_groups_min_max_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xave_tier2_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 12, 14, 20, 25, 30, 40, 50)",
                'sector <- c("A", "A", "A", "B", "B", "B", "A", "B")',
                'bucket <- c("old", "old", "new", "old", "new", "new", "new", "old")',
                "print(ave(x, sector, bucket, FUN = mean))",
                "print(ave(x, sector, bucket, FUN = length))",
                "y <- c(3, 7, 2, 9, 4, 8)",
                "id <- c(1L, 1L, 2L, 2L, 2L, 1L)",
                "print(ave(y, id, FUN = max))",
                'name <- c("red", "red", "blue", "blue", "blue", "red")',
                "print(ave(y, name, FUN = min))",
                "ratio_to_cell_mean <- x / ave(x, sector, bucket, FUN = mean)",
                "print(ratio_to_cell_mean)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xave_tier2_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_stdout = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "11.000000" in flat_stdout
    assert "27.500000" in flat_stdout
    assert "2 2 2 2 2 2 2 2" in flat_stdout
    assert "8 8 9 9 9 8" in flat_stdout
    assert "3 3 2 2 2 3" in flat_stdout
    assert "0.909090" in flat_stdout
    assert "ave_group_key(sector, bucket)" in out_text
    assert 'ave(real(y, kind=dp), id, "max")' in out_text
    assert 'ave(real(y, kind=dp), name, "min")' in out_text
    assert "FUN =" not in out_text


def test_xr2f_aggregate_vector_by_one_group_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xaggregate_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 12, 14, 20, 25, 30)",
                'g <- c("A", "A", "A", "B", "B", "B")',
                "res <- aggregate(x = x, by = list(group = g), FUN = mean)",
                "print(res)",
                "print(aggregate(x = x, by = list(group = g), FUN = sum))",
                "print(aggregate(x = x, by = list(group = g), FUN = length))",
                "print(aggregate(x = x, by = list(group = g), FUN = min))",
                "print(aggregate(x = x, by = list(group = g), FUN = max))",
                "y <- c(3, 7, 2, 9, 4, 8)",
                "id <- c(1L, 1L, 2L, 2L, 2L, 1L)",
                "print(aggregate(x = y, by = list(id = id), FUN = max))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xaggregate_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_stdout = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "group x 1 A 12.000000" in flat_stdout
    assert "1 A 36.000000" in flat_stdout
    assert "1 A 3.000000" in flat_stdout
    assert "1 A 10.000000" in flat_stdout
    assert "1 A 14.000000" in flat_stdout
    assert "id x 1 1 8.000000" in flat_stdout
    assert "type(aggregate_result_t) :: res" in out_text
    assert 'res = aggregate(x, g, "group", "x", "mean")' in out_text
    assert "call print_aggregate_result(res)" in out_text
    assert "FUN =" not in out_text


def test_xr2f_by_vector_and_matrix_groups_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xby_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 12, 14, 20, 25, 30)",
                'g <- c("A", "A", "A", "B", "B", "B")',
                "res1 <- by(x, g, mean)",
                "cat(res1, \"\\n\")",
                "print(by(x, g, sum))",
                "m <- matrix(1:12, nrow = 6, ncol = 2)",
                "res2 <- by(m, g, colMeans)",
                "print(res2)",
                "print(by(m, g, colSums))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xby_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_stdout = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "12.000000" in flat_stdout
    assert "25.000000" in flat_stdout
    assert "36.000000" in flat_stdout
    assert "75.000000" in flat_stdout
    assert "INDICES: A" in proc.stdout
    assert "INDICES: B" in proc.stdout
    assert "type(by_matrix_result_t) :: res2" in out_text
    assert 'res1 = r_by(x, g, "mean")' in out_text
    assert 'res2 = r_by(real(m, kind=dp), g, "colmeans")' in out_text
    assert "call print_by_matrix_result(res2)" in out_text


def test_xr2f_as_vector_scalar_mod_and_mixed_numeric_c_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xabundant_odd_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "find_div_sum <- function(x) {",
                "  root <- sqrt(x)",
                "  vec <- as.vector(1)",
                "  for (i in seq.int(3, root - 1, by = 2)) {",
                "    if (x %% i == 0) {",
                "      vec <- c(vec, i, x / i)",
                "    }",
                "  }",
                "  sum(vec)",
                "}",
                "print(find_div_sum(45))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xabundant_odd_probe.f90"

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
    assert "33" in proc.stdout
    assert "reshape(1, [size(1)])" not in out_text
    assert "real(i, kind=dp)" in out_text


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
    assert _has_fortran_decl(out_text, "integer, parameter", "xi(n_param) = [2, 4, 6]")
    assert "real(kind=dp), parameter :: xr" in out_text
    assert 'write(*,"(*(1x,i0))") union(xi, yi)' in out_text
    assert "call print_real_vector(union(xr, yr))" in out_text
    assert "call print_char_vector(union(xc, yc))" in out_text
    assert "two four six one nine" in proc.stdout


def test_xr2f_probability_wrapper_keyword_args_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xprob_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "print(pchisq(q = 3.0, df = 2.0))",
                "print(pchisq(3.0, 2.0))",
                "print(pnorm(q = 1.0, mean = 0.0, sd = 1.0, lower.tail = FALSE, log.p = TRUE))",
                "print(pnorm(1.0, 0.0, 1.0))",
                "print(ppois(q = 2.0, lambda = 3.0))",
                "print(qpois(p = 0.5, lambda = 3.0))",
                "print(pcauchy(q = 1.0, location = 0.0, scale = 1.0))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprob_keywords.f90"

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
    assert "pchisq(real(3.0_dp, kind=dp), real(2.0_dp, kind=dp))" in out_text
    assert "1.0_dp - normal_cdf" in out_text
    assert "ppois(real(2.0_dp, kind=dp), lambda=real(3.0_dp, kind=dp))" in out_text
    assert "qpois(real(0.5_dp, kind=dp), lambda=real(3.0_dp, kind=dp))" in out_text
    assert "pcauchy(real(1.0_dp, kind=dp), location=real(0.0_dp, kind=dp), scale=real(1.0_dp, kind=dp))" in out_text


def test_xr2f_kruskal_formula_keyword_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xkruskal_keyword.r"
    local_input.write_text(
        "\n".join(
            [
                "y <- c(1.0, 2.0, 1.5, 3.5, 4.0, 3.0)",
                "g <- c(1L, 1L, 1L, 2L, 2L, 2L)",
                "print(kruskal.test(formula = y ~ g))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xkruskal_keyword.f90"

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
    assert "call print_kruskal_test(kruskal_test(y, g))" in out_text


def test_xr2f_chisq_test_keyword_x_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xchisq_keyword.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 20, 30)",
                "print(chisq.test(x = x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xchisq_keyword.f90"

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
    assert "call print_chisq_test(chisq_test(x=x))" in out_text


def test_xr2f_cor_cov_quantile_sample_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xstat_keyword_args.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 2.0, 3.0, 4.0)",
                "y <- c(2.0, 4.0, 6.0, 8.0)",
                "rho <- cor(x = x, y = y, use = \"complete.obs\")",
                "cv <- cov(x = x, y = y, use = \"complete.obs\")",
                "print(quantile(x = x, probs = c(0.25, 0.75), type = 7))",
                "idx <- sample(x = x, size = 2L, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.4))",
                "print(idx)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xstat_keyword_args.f90"

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
    assert "rho = cor(x, y)" in out_text
    assert "cv = cov(x, y)" in out_text
    assert "quantile(x, probs=[0.25_dp, 0.75_dp], type=7)" in out_text
    assert "idx = x(sample_int(size(x), size_=2, replace=.true., prob=[0.1_dp, 0.2_dp, 0.3_dp, 0.4_dp]))" in out_text


def test_xr2f_list_result_matrix_slice_by_order_keeps_field_rank2(tmp_path: Path) -> None:
    local_input = tmp_path / "xlist_result_matrix_slice_order.r"
    local_input.write_text(
        "\n".join(
            [
                "make_fit <- function() {",
                "  resp <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 3, ncol = 2)",
                "  scores <- c(2.0, 1.0)",
                "  ord <- order(scores)",
                "  list(resp = resp[, ord])",
                "}",
                "fit <- make_fit()",
                "print(fit$resp)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xlist_result_matrix_slice_order.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "real(kind=dp), allocatable :: resp(:,:)" in out_text
    assert "make_fit_result%resp = resp(:, ord)" in out_text
    assert "real(kind=dp), allocatable :: resp(:)\n" not in out_text


def test_xr2f_rng_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xrng_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "set.seed(1)",
                "u <- runif(n = 3, min = 2.0, max = 5.0)",
                "z <- rnorm(n = 3, mean = 10.0, sd = 2.0)",
                "zp <- rnorm(3, 10.0, 2.0)",
                "e <- rexp(n = 3, rate = 2.0)",
                "b <- rbinom(n = 3, size = 2L, prob = 0.25)",
                "p <- rpois(n = 3, lambda = 4.0)",
                "idx <- sample.int(n = 5L, size = 2L, replace = TRUE)",
                'cat("ok", sum(u) + sum(z) + sum(zp) + sum(e) + sum(b) + sum(p) + sum(idx), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xrng_keywords.f90"

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
    assert "runif_vec(3)" in out_text
    assert "rnorm_vec(3)" in out_text and "10.0_dp" in out_text and "2.0_dp" in out_text
    assert "rbinom(3, 2, 0.25_dp)" in out_text
    assert "rpois(3, 4.0_dp)" in out_text
    assert "sample_int(5, size_=2, replace=.true.)" in out_text


def test_xr2f_seq_rep_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xseq_rep_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10.0, 20.0, 30.0)",
                "a <- seq(from = 2, to = 8, by = 2)",
                "b <- seq.int(from = 3L, to = 7L, by = 2L)",
                "c <- seq_len(length.out = 3L)",
                "d <- seq_along(along.with = x)",
                "e <- rep(x = c(1L, 2L), times = c(2L, 3L))",
                "f <- rep(x = 4.0, each = 2L, length.out = 5L)",
                "g <- rep.int(x = 9L, times = 3L)",
                'cat("ok", sum(a) + sum(b) + sum(c) + sum(d) + sum(e) + sum(f) + sum(g), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xseq_rep_keywords.f90"

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
    assert "r_seq_real_by" in out_text
    assert "r_seq_int_by(3, 7, 2)" in out_text
    assert "r_seq_len(3)" in out_text
    assert "r_seq_len(size(x))" in out_text
    assert "times_vec=[2, 3]" in out_text
    assert "r_rep_real([4.0_dp], each=2, len_out=5)" in out_text
    assert "r_rep_int([9], times=3)" in " ".join(out_text.replace("&", " ").split())


def test_xr2f_seq_by_subscript_uses_integer_helper_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xseq_by_subscript.r"
    local_input.write_text(
        "\n".join(
            [
                "limit <- 50",
                "is_prime <- rep(TRUE, limit - 1)",
                "is_prime[1] <- FALSE",
                "for (p in 2:floor(sqrt(limit - 1))) {",
                "  if (is_prime[p]) is_prime[seq(p * p, limit - 1, by = p)] <- FALSE",
                "}",
                'cat("largest", max(which(is_prime)), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xseq_by_subscript.f90"

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
    assert "largest 47" in proc.stdout
    assert "r_seq_int_by(p * p, int(limit - 1), p)" in out_text


def test_xr2f_stat_test_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xstat_test_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 2.0, 3.0, 4.0)",
                "y <- c(1.5, 2.5, 3.5, 4.5)",
                "print(t.test(x = x, mu = 0.0))",
                "print(t.test(x = x, y = y, paired = FALSE))",
                "print(wilcox.test(x = x, y = y, paired = FALSE))",
                "print(ks.test(x = x, mean = 0.0, sd = 1.0))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xstat_test_keywords.f90"

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
    assert "call print_t_test(t_test(x, mu=0.0_dp))" in out_text
    assert "call print_t_test(t_test(x, y, paired=.false.))" in out_text
    assert "call print_wilcox_test(wilcox_test(x, y, paired=.false.))" in out_text
    assert "call print_ks_test(ks_test(x, mean=real(0.0_dp, kind=dp), sd=real(1.0_dp, kind=dp)))" in out_text


def test_xr2f_matrix_constructor_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xmatrix_constructor_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- matrix(data = c(1.0, 2.0, 3.0, 4.0), nrow = 2, ncol = 2, byrow = TRUE)",
                "a <- array(data = 1:8, dim = c(2, 2, 2))",
                "centered <- sweep(x = x, MARGIN = 2, STATS = c(1.0, 2.0), FUN = \"-\")",
                "z <- outer(X = c(1.0, 2.0), Y = c(10.0, 20.0), FUN = \"*\")",
                "print(x)",
                "print(a[, , 1])",
                "print(centered)",
                "print(z)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmatrix_constructor_keywords.f90"

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
    assert "call print_matrix(x)" in out_text
    assert "a = reshape(r_seq_int(1, 8), [2, 2, 2], pad=r_seq_int(1, 8))" in out_text
    assert "spread([1.0_dp, 2.0_dp], dim=1, ncopies=size(x,1))" in out_text
    assert "z(i_out, j_out) = ox(i_out) * oy(j_out)" in out_text


def test_xr2f_outer_direct_prints_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xouter_direct_print_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- 1:3",
                "y <- 1:3",
                'print(outer(x, y, FUN = "*"))',
                'print(outer(x, y, FUN = "+"))',
                "a <- c(10, 20)",
                "b <- c(1, 2, 3)",
                'print(abs(outer(a, b, FUN = "-")))',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xouter_direct_print_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "outer_print_tmp(i_out, j_out) = ox(i_out) * oy(j_out)" in out_text
    assert "outer_print_tmp(i_out, j_out) = ox(i_out) + oy(j_out)" in out_text
    assert "outer_print_tmp = abs(outer_print_tmp)" in out_text
    assert 'write(*,"(g0)") outer(' not in out_text


def test_xr2f_outer_named_function_fun_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xouter_named_fun_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(a, b) {",
                "  10*a + b",
                "}",
                "x <- 2:6",
                "y <- 7:11",
                "res <- outer(x, y, FUN = f)",
                "print(res)",
                "score_pair <- function(a, b) {",
                "  z <- a*a + b",
                "  ifelse(z > 30, z - 5, z + 5)",
                "}",
                "x <- c(1, 2, 3, 4)",
                "y <- c(5, 10, 15)",
                "print(outer(x, y, FUN = score_pair))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xouter_named_fun_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_stdout = " ".join(proc.stdout.split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "27" in flat_stdout
    assert "71" in flat_stdout
    assert "31" in flat_stdout
    assert "res(i_out, j_out) = f(ox(i_out), oy(j_out))" in out_text
    assert "outer_print_tmp(i_out, j_out) = score_pair(ox(i_out), oy(j_out))" in out_text
    assert "FUN =" not in out_text


def test_xr2f_data_shaping_named_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xdata_shaping_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 2.0, 3.0)",
                "y <- c(4.0, 5.0, 6.0)",
                "m1 <- cbind(alpha = x, beta = y)",
                "m2 <- rbind(first = x, second = y)",
                "df <- data.frame(alpha = x, beta = y, check.names = FALSE)",
                "m3 <- as.matrix(x = m1)",
                "print(m1)",
                "print(m2)",
                "print(m3)",
                'cat("df:", sum(df$alpha + df$beta), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdata_shaping_keywords.f90"

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
    assert "cbind(alpha=" not in out_text
    assert "rbind(first=" not in out_text
    assert "data.frame" not in out_text
    assert "df_alpha" not in out_text
    assert "x = m1" not in out_text


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
                "unique(x = x)",
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


def test_xr2f_sort_sort_list_head_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xsort_head_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(3.0, 1.0, 2.0)",
                "idx <- sort.list(x = x)",
                "print(sort(x = x, decreasing = TRUE))",
                "print(idx)",
                "print(head(x = x, n = 2))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsort_head_keywords.f90"

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
    assert "sort(x, decreasing=.true.)" in out_text
    assert "sort_list(x)" in out_text
    assert "r_head(x, 2)" in out_text


def test_xr2f_duplicated_and_anyduplicated_vectors(tmp_path: Path) -> None:
    local_input = tmp_path / "xduplicated_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "xd <- c(10, 20, 30, 20, 40, 30)",
                "print(xd)",
                "print(duplicated(xd))",
                "print(duplicated(x = xd))",
                "print(duplicated(xd, fromLast = TRUE))",
                "print(anyDuplicated(xd))",
                "print(anyDuplicated(x = xd))",
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
                "print(replace(x = x, list = c(1, 3), values = c(111, 333)))",
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
    assert "111 20 333 40 50 60" in proc.stdout
    assert "integer" in proc.stdout
    assert "1 20 3 40 5 6" in proc.stdout
    assert "T T T T" in proc.stdout


def test_xr2f_rev_which_diff_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xvector_keyword_args.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 3.0, 6.0, 10.0, 15.0)",
                "b <- x > 4.0",
                "print(rev(x = x))",
                "print(which(x = b))",
                "print(diff(x = x, lag = 1, differences = 1))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xvector_keyword_args.f90"

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
    assert "rev_real(x)" in out_text or "x(size(x):1:-1)" in out_text
    assert "which(" in out_text
    assert "diff(x" in out_text


def test_xr2f_io_and_difftime_keyword_args_compile(tmp_path: Path) -> None:
    (tmp_path / "csv_in.csv").write_text("a,b\n1,2\n3,4\n", encoding="utf-8")
    (tmp_path / "tbl_in.txt").write_text("a b\n1 2\n3 4\n", encoding="utf-8")
    local_input = tmp_path / "xio_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                'csv <- read.csv(file = "csv_in.csv", stringsAsFactors = FALSE)',
                'tbl <- read.table(file = "tbl_in.txt", header = TRUE)',
                'write.table(x = tbl, file = "tbl_out.txt")',
                "t0 <- Sys.time()",
                "t1 <- t0 + 60",
                'cat("dt:", difftime(time1 = t1, time2 = t0, units = "mins"), "\\n")',
                "print(dim(csv))",
                "print(dim(tbl))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xio_keywords.f90"

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
    assert 'call read_csv_real_matrix("csv_in.csv", csv)' in out_text
    assert 'call read_table_real_matrix("tbl_in.txt", tbl, header=.true.)' in out_text
    assert 'call write_table_real_matrix("tbl_out.txt", tbl)' in out_text
    assert "/ 60.0_dp" in out_text


def test_xr2f_string_helper_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xstring_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                'x <- c("alpha", "beta", "gamma")',
                'print(grepl(pattern = "a", x = x))',
                'print(grep(pattern = "a", x = x))',
                'print(startsWith(x = x, prefix = "a"))',
                'print(endsWith(x = x, suffix = "a"))',
                'print(strsplit(x = "a-b-c", split = "-"))',
                's <- "abcdef"',
                'cat(substr(x = s, start = 2, stop = 4), "\\n")',
                'cat(substring(text = s, first = 3, last = 5), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xstring_keywords.f90"

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
    assert "grepl(" not in out_text
    assert "grep(" not in out_text
    assert "startsWith(" not in out_text
    assert "endsWith(" not in out_text
    assert "strsplit(" not in out_text
    assert "strsplit_fixed" in out_text
    assert "(2:4)" in out_text
    assert "(3:5)" in out_text


def test_xr2f_command_args_grep_value_header_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xheader_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "print_script_header <- function() {",
                '  f <- grep("--file=", commandArgs(FALSE), value = TRUE)',
                '  if (length(f)) cat("program:", sub("--file=", "", f[[1L]], fixed = TRUE),',
                '                     "  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\\n\\n", sep = "")',
                "}",
                "print_script_header()",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xheader_probe.f90"

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
    assert "program:" in proc.stdout
    assert str(out_path) in proc.stdout
    assert "grep_value_char" in out_text
    assert "r_command_args" in out_text
    assert "function r_command_args" not in out_text
    assert "if (size(f) > 0) then" in out_text
    assert "replace_first_fixed(f(1)" in out_text
    assert "f(:, 1)" not in out_text


def test_xr2f_switch_named_string_cases_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xswitch_probe.r"
    local_input.write_text(
        "\n".join(
            [
                'infile <- "mortgage_reit_related.csv"',
                "switch(",
                "  infile,",
                '  "asset_class_etf_prices.csv" = {',
                '    signal_assets <- c("LQD", "HYG")',
                '    traded_assets <- c("SPY")',
                "  },",
                '  "mortgage_reit_related.csv" = {',
                '    signal_assets <- c("MBB", "NLY", "AGNC")',
                '    traded_assets <- c("NLY", "AGNC", "MFA", "ORC", "ARR")',
                "  }",
                ")",
                'cat("signal_assets:", signal_assets, "\\n")',
                'cat("traded_assets:", traded_assets, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xswitch_probe.f90"

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
    assert "select case (trim(infile))" in out_text
    assert 'case ("mortgage_reit_related.csv")' in out_text
    assert "switch(" not in out_text
    stdout_flat = " ".join(proc.stdout.split())
    assert "signal_assets: MBB NLY AGNC" in stdout_flat
    assert "traded_assets: NLY AGNC MFA ORC ARR" in stdout_flat


def test_xr2f_switch_integer_positional_cases_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xswitch_int_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "for (ifile in 1:2) {",
                "  switch(",
                "    ifile,",
                "    {",
                '      signal_assets <- c("LQD", "HYG")',
                '      traded_assets <- c("SPY")',
                "    },",
                "    {",
                '      signal_assets <- c("MBB", "NLY", "AGNC")',
                '      traded_assets <- c("NLY", "AGNC", "MFA", "ORC", "ARR")',
                "    }",
                "  )",
                '  cat("ifile:", ifile, "\\n")',
                '  cat("signal_assets:", signal_assets, "\\n")',
                '  cat("traded_assets:", traded_assets, "\\n\\n")',
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xswitch_int_probe.f90"

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
    assert "select case (ifile)" in out_text
    assert "case (1)" in out_text and "case (2)" in out_text
    assert "switch(" not in out_text
    stdout_flat = " ".join(proc.stdout.split())
    assert "ifile: 1 signal_assets: LQD HYG traded_assets: SPY" in stdout_flat
    assert "ifile: 2 signal_assets: MBB NLY AGNC traded_assets: NLY AGNC MFA ORC ARR" in stdout_flat


def test_xr2f_switch_integer_expression_cases_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xswitch_int_one_line_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "i <- 2",
                "result <- switch(",
                "  i,",
                '  "first",',
                '  "second",',
                '  "third"',
                ")",
                "print(result)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xswitch_int_one_line_probe.f90"

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
    assert "select case (i)" in out_text
    assert "case (2)" in out_text
    assert 'result = "second"' in out_text
    assert "switch(" not in out_text
    assert "second" in proc.stdout


def test_xr2f_reducer_keyword_x_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xreducer_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 2.0, 3.0)",
                "m <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2)",
                "cat(sum(x = x), mean(x = x), sd(x = x), prod(x = x), min(x = x), max(x = x), \"\\n\")",
                "cat(sum(x == 2.0), mean(x == 2.0), \"\\n\")",
                "print(rowSums(x = m))",
                "print(colSums(x = m))",
                "print(rowMeans(x = m))",
                "print(colMeans(x = m))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreducer_keywords.f90"

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
    assert "sum(x =" not in out_text
    assert "mean(x =" not in out_text
    assert "sd(x =" not in out_text
    assert "product(x =" not in out_text
    assert "sum(x)" in out_text
    assert "count(x == 2.0_dp)" in out_text
    assert "sum(m, dim=2)" in out_text
    assert "sum(m, dim=1)" in out_text


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
    assert "-15" in proc.stdout
    assert "21.213203435596427" in proc.stdout
    assert "24" in proc.stdout
    assert "-99 -198" in proc.stdout
    assert "-594" in proc.stdout


def test_xr2f_apply_keyword_margin_fun_cumsum_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xapply_keyword_cumsum.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 2)",
                "y <- apply(x, MARGIN = 2, FUN = cumsum)",
                "print(y)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xapply_keyword_cumsum.f90"

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
    assert "apply_col_cumsum(" in out_text
    assert "apply(x" not in out_text


def test_xr2f_scale_keyword_args_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xscale_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- matrix(c(1.0, 2.0, 3.0, 4.0), nrow = 2)",
                "z <- scale(x = x, center = TRUE, scale = TRUE)",
                "z2 <- scale(x, TRUE, TRUE)",
                "print(z)",
                "print(z2)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xscale_keywords.f90"

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
    assert out_text.count("call print_matrix(scale(x, center=.true., scale=.true.))") == 2


def test_xr2f_apply_user_function_anonymous_and_extra_args_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xapply_user_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "sum_squares <- function(x) {",
                "  sum(x^2)",
                "}",
                "summary3 <- function(x) {",
                "  c(minimum = min(x), average = mean(x), maximum = max(x))",
                "}",
                "count_greater_than <- function(x, cutoff) {",
                "  sum(x > cutoff)",
                "}",
                "m <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)",
                "print(apply(m, 1, sum_squares))",
                "print(apply(m, 2, sum_squares))",
                "print(apply(m, 1, summary3))",
                "print(apply(m, 1, function(x) {",
                "  sum(x - mean(x))",
                "}))",
                "print(apply(m, 1, count_greater_than, cutoff = 6))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xapply_user_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "write(*,\"(g0)\") )" not in out_text
    assert "apply(" not in out_text
    assert "xr2f_apply_fun_1" in out_text
    assert "call print_matrix" in out_text
    assert "sum_squares(real(m(i_apply, :), kind=dp))" in flat_out
    assert "count_greater_than(real(m(i_apply, :), kind=dp), cutoff=real(6, kind=dp))" in flat_out
    assert "30 174 446" in proc.stdout
    assert "0 2 4" in proc.stdout


def test_xr2f_rank3_apply_sum_print_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xapply_rank3_sum_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- array(1:24, dim = c(2, 3, 4))",
                "print(apply(x, 1, sum))",
                "print(apply(x, 2, sum))",
                "print(apply(x, 3, sum))",
                "print(apply(x, c(1, 2), sum))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xapply_rank3_sum_probe.f90"

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
    assert "x(:,:,:)" in out_text
    assert "call print_real_vector(real(sum(sum(x, dim=3), dim=2), kind=dp))" in flat_out
    assert "call print_real_vector(real(sum(sum(x, dim=3), dim=1), kind=dp))" in flat_out
    assert "call print_real_vector(real(sum(x, dim=2), kind=dp))" not in flat_out


def test_xr2f_captures_global_complex_vector_in_function_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xcapture_complex_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "z <- c(1+2i, 3+4i)",
                "use_complex_vec <- function(x) {",
                "  sum(Mod(z)) + x",
                "}",
                "cat(use_complex_vec(5.0), \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xcapture_complex_probe.f90"

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
    assert "complex(kind=dp), allocatable :: z(:)" in out_text
    assert "12.236067" in proc.stdout


def test_xr2f_captures_global_real_rank3_in_function_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xcapture_rank3_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "a <- array(1.0, dim = c(2, 2, 2))",
                "use_array3 <- function(x) {",
                "  sum(a[, , 1]) + x",
                "}",
                "cat(use_array3(5.0), \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xcapture_rank3_probe.f90"

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
    assert "real(kind=dp), allocatable :: a(:,:,:)" in out_text
    assert "9.000000" in proc.stdout


@pytest.mark.parametrize("method", ["BFGS", "Nelder-Mead"])
def test_xr2f_constroptim_log_barrier_methods_run(tmp_path: Path, method: str) -> None:
    local_input = tmp_path / "xconstroptim_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(theta) {",
                "  x <- theta[1]",
                "  y <- theta[2]",
                "  (x - 2)^2 + (y - 3)^2",
                "}",
                "theta0 <- c(1, 1)",
                "ui <- rbind(c(1, 0), c(0, 1))",
                "ci <- c(0, 0)",
                f'fit <- constrOptim(theta = theta0, f = f, grad = NULL, ui = ui, ci = ci, method = "{method}")',
                "print(fit$par)",
                'cat("value:", fit$value, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xconstroptim_probe.f90"

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
    assert "constr_optim_bfgs" in out_text or "constr_optim_nelder_mead" in out_text
    assert "2.000" in proc.stdout
    assert "3.000" in proc.stdout


def test_xr2f_uses_external_fortran_module_function_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xexternal_module_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "sum_squares <- function(x) {",
                "  sum(x*x)",
                "}",
                "x <- c(1.0, 2.0, 3.0)",
                'cat(sum_squares(x), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    helper = tmp_path / "fast_stats.f90"
    helper.write_text(
        "\n".join(
            [
                "module fast_stats",
                "use, intrinsic :: iso_fortran_env, only: dp => real64",
                "implicit none",
                "contains",
                "function sum_squares(x) result(out)",
                "real(kind=dp), intent(in) :: x(:)",
                "real(kind=dp) :: out",
                "out = 100.0_dp + sum(x*x)",
                "end function sum_squares",
                "end module fast_stats",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xexternal_module_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), str(helper), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "use fast_stats, only: sum_squares" in out_text
    assert "function sum_squares" not in out_text
    assert "114.000000" in proc.stdout


def test_xr2f_rejects_external_fortran_main_program(tmp_path: Path) -> None:
    local_input = tmp_path / "xexternal_bad_probe.r"
    local_input.write_text('cat("ok\\n")\n', encoding="utf-8")
    helper = tmp_path / "bad_main.f90"
    helper.write_text("program bad_main\nend program bad_main\n", encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), str(helper), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode != 0
    assert "contains a main program" in proc.stdout + proc.stderr


def test_xr2f_rejects_external_fortran_top_level_procedure(tmp_path: Path) -> None:
    local_input = tmp_path / "xexternal_bad_proc_probe.r"
    local_input.write_text('cat("ok\\n")\n', encoding="utf-8")
    helper = tmp_path / "bad_proc.f90"
    helper.write_text(
        "function foo() result(x)\nreal :: x\nx = 1.0\nend function foo\n",
        encoding="utf-8",
    )

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), str(helper), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode != 0
    assert "contains top-level procedures" in proc.stdout + proc.stderr


def test_xr2f_split_module_writes_reusable_module_and_main_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xsplit_module_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "square_sum <- function(x) {",
                "  sum(x*x)",
                "}",
                "cat(square_sum(c(1.0, 2.0, 3.0)), \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsplit_module_probe.f90"
    mod_path = tmp_path / "xsplit_module_probe_mod.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--split-module", "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    main_text = out_path.read_text(encoding="utf-8")
    mod_text = mod_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert f"wrote {mod_path}" in proc.stdout
    assert re.search(r"(?im)^\s*module\s+split_module_probe_mod\b", mod_text)
    assert "function square_sum" in mod_text
    assert re.search(r"(?im)^\s*program\s+xsplit_module_probe\b", main_text)
    assert "function square_sum" not in main_text
    assert "14.000000" in proc.stdout


def test_xr2f_generated_module_is_private_with_explicit_publics(tmp_path: Path) -> None:
    local_input = tmp_path / "xprivate_module_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "twice <- function(x) {",
                "  2 * x",
                "}",
                "print(twice(c(1.0, 2.0)))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xprivate_module_probe.f90"
    mod_path = tmp_path / "xprivate_module_probe_mod.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--split-module", "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    mod_text = mod_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert re.search(r"(?im)^\s*private\s*$", mod_text)
    assert re.search(r"(?im)^\s*public\s*::\s*twice\b", mod_text)


def test_xr2f_split_module_without_module_writes_single_main(tmp_path: Path) -> None:
    local_input = tmp_path / "xsplit_nomodule_probe.r"
    local_input.write_text('cat("ok\\n")\n', encoding="utf-8")
    out_path = tmp_path / "xsplit_nomodule_probe.f90"
    mod_path = tmp_path / "xsplit_nomodule_probe_mod.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--split-module", "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert out_path.exists()
    assert not mod_path.exists()


def test_xr2f_ls_static_top_level_names_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xls_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "find_outlier_in_row <- function(xrow) {",
                "  row_median <- median(xrow)",
                "  deviations <- abs(xrow - row_median)",
                "  which.max(deviations)",
                "}",
                "xrow <- c(1, 2, 3, 100)",
                "long <- 1:20",
                "outlier_index <- find_outlier_in_row(xrow)",
                "print(ls())",
                'cat("lsstr:\\n")',
                "ls.str()",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xls_probe.f90"

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
    assert "call print_char_vector([character(len=19)" in out_text
    assert "find_outlier_in_row long outlier_index xrow" in proc.stdout
    assert "find_outlier_in_row : function (xrow)" in proc.stdout
    assert "long : int [1:20] 1 2 3 4 5 6 7 8 ..." in proc.stdout
    assert "outlier_index : int 4" in proc.stdout
    assert "xrow : num [1:4] 1 2 3 100" in proc.stdout


def test_xr2f_nested_apply_callback_and_matrix_cbind_index_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xoutliers_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "find_outliers <- function(x) {",
                "  find_outlier_in_row <- function(xrow) {",
                "    row_median <- median(xrow)",
                "    deviations <- abs(xrow - row_median)",
                "    which.max(deviations)",
                "  }",
                "  apply(x, 1, find_outlier_in_row)",
                "}",
                "x <- matrix(",
                "  c(",
                "    1,  2,  3, 100,",
                "    4, 50,  6,   7,",
                "    9, 10, -80, 11,",
                "    5,  5,   5,  5",
                "  ),",
                "  nrow = 4,",
                "  byrow = TRUE",
                ")",
                "outlier_cols <- find_outliers(x)",
                "print(outlier_cols)",
                "outlier_values <- x[cbind(seq_len(nrow(x)), outlier_cols)]",
                "print(outlier_values)",
                "for (i in seq_len(nrow(x))) {",
                "  print(median(x[i, ]))",
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xoutliers_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "apply(" not in out_text
    assert "real(kind=dp), intent(in) :: xrow(:)" in out_text
    assert "find_outliers_result = real([(find_outliers_find_outlier_in_row" in flat_out
    assert "r_matrix_index(x, int(r_seq_len(size(x, 1)) + (outlier_cols - 1) * size(x, 1)))" in flat_out
    assert "cbind(" not in flat_out
    assert "median(real(x(i, :), kind=dp))" in flat_out
    assert "4 2 3 1" in proc.stdout
    assert "100 50 -80 5" in proc.stdout


def test_xr2f_user_function_rep_return_is_vector_not_elemental(tmp_path: Path) -> None:
    local_input = tmp_path / "xelem_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_weights <- function(k) {",
                "  rep(1.0 / k, k)",
                "}",
                "k <- 3",
                "w <- make_weights(k)",
                "print(w)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xelem_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "elemental function make_weights" not in out_text
    assert "function make_weights(k) result(make_weights_result)" in out_text
    assert "make_weights_result(:)" in out_text
    assert "times=int(k)" in flat_out
    assert "call print_real_vector(make_weights(real(k, kind=dp)))" in out_text
    assert proc.stdout.count("0.33333333333333331") == 3


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
                "rk <- rle(x = x)",
                "r",
                "r$lengths",
                "r$values",
                "inverse.rle(r)",
                "inverse.rle(x = rk)",
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
    assert "rk = rle(x)" in out_text
    assert "call print_real_vector(inverse_rle(rk))" in out_text
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


def test_xr2f_no_diagnostics_suppresses_compile_failure_source_context(tmp_path: Path) -> None:
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
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile", "--no-diagnostics"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode != 0
    assert "Build: FAIL" in proc.stdout
    assert "Likely R source for compile error:" not in proc.stdout
    assert 'R line 2: cat(y, "\\n")' not in proc.stdout


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


def test_xr2f_run_diff_accepts_numeric_format_differences(tmp_path: Path) -> None:
    local_input = tmp_path / "xrun_diff_numeric_probe.r"
    local_input.write_text("cat(0, 21.692781522776912, \"\\n\")\n", encoding="utf-8")

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out-dir", str(tmp_path), "--run-diff"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Run (r): PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "Run diff: MATCH" in proc.stdout
    assert "Run diff: DIFF" not in proc.stdout


def test_xr2f_run_diff_accepts_different_dash_separator_lengths() -> None:
    xr2f = _load_xr2f_module()

    assert xr2f._diff_output_lines_equal("-" * 160, "-" * 60)
    assert xr2f._diff_output_matches(["header", "-" * 160, "tail"], ["header", "-" * 60, "tail"]) == (
        True,
        None,
    )
    assert not xr2f._diff_output_lines_equal("-" * 60, "---------- x")


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
    assert "integer :: aic_order" in flat_out
    assert _has_fortran_decl(out_text, "integer", "bic_order")
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
    assert "call print_matrix(m, digits=4)" in out_text
    assert "r_round(" not in out_text
    assert "call print_real_vector" not in out_text


def test_xr2f_round_vector_uses_runtime_helper_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xround_vector.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.23456, -2.34567)",
                "y <- round(x, digits = 3)",
                "print(y[1])",
                "print(y)",
                "print(round(x, 3))",
                "print(round(1.23456))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xround_vector.f90"

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
    assert "r_round(x, 3)" in out_text
    assert "r_round(1.23456_dp, 0)" in out_text
    assert "call print_real_vector(x, digits=3)" in out_text
    assert "real(r_round(" not in out_text


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


def test_xr2f_for_call_iterator_materializes_vector_once(tmp_path: Path) -> None:
    local_input = tmp_path / "xfor_call_iter.r"
    local_input.write_text(
        "\n".join(
            [
                "twice <- function(v) {",
                "  v * 2",
                "}",
                "v <- 1:3",
                "for (x in twice(v)) {",
                "  print(x)",
                "}",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfor_call_iter.f90"

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
    assert "iter_for_x = twice(real(v, kind=dp))" in out_text
    assert "do i_x = 1, size(iter_for_x)" in out_text
    assert "2.000000" in proc.stdout
    assert "4.000000" in proc.stdout
    assert "6.000000" in proc.stdout


def test_xr2f_matrix_without_dims_defaults_to_one_column(tmp_path: Path) -> None:
    local_input = tmp_path / "xmatrix_1d_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10.0, 20.0, 30.0)",
                "y <- matrix(x)",
                "print(y)",
                "print(dim(y))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmatrix_1d_probe.f90"

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
    assert "y = matrix(x, size(x))" in out_text
    assert "3 1" in proc.stdout


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
    assert "real(kind=dp), allocatable :: x(:)" in out_text
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "weight(:)")
    assert "pure elemental function spread_weighted_point" in out_text
    assert "type(weighted_point_result_t), intent(in) :: object" in out_text
    assert 'write(*,"(*(g0,:,1x))") "weighted class: ", "weighted_point"' in out_text
    assert 'write(*,"(*(g0,:,1x))") "weighted is stat: ", .true.' in out_text
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
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert "merge(acos(-1.0_dp), 0.0_dp, real(-3, kind=dp) < 0.0_dp)" in flat_out


def test_xr2f_multiline_assignment_if_expression_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xmultiline_if_expr.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 2.0, 3.0)",
                "y <- if (length(x) > 1L && sd(x) > 0)",
                "       mean(x) / sd(x) else NA_real_",
                "print(y)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmultiline_if_expr.f90"

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
    assert "size(x) > 1 .and. sd(x) > 0" in out_text
    assert "else ieee_value" not in out_text


def test_xr2f_mapply_named_and_anonymous_functions_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xmapply_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "power_plus <- function(x, p) x^p + 1",
                "res1 <- mapply(power_plus, x = c(2, 3, 4), p = c(1, 2, 3))",
                "print(res1)",
                "x <- c(1, 2, 3)",
                "y <- c(10, 20, 30)",
                "res2 <- mapply(function(a, b) a + b, x, y)",
                "print(res2)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmapply_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "3 10 65" in proc.stdout
    assert "11 22 33" in proc.stdout
    assert "mapply(" not in out_text
    assert "xr2f_mapply_i_res1" in out_text
    assert "xr2f_mapply_i_res2" in out_text


def test_xr2f_tapply_grouped_mean_and_anonymous_function_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xtapply_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(10, 20, 30, 40)",
                'g <- c("A", "A", "B", "B")',
                "y1 <- tapply(x, g, mean)",
                'cat(y1, "\\n")',
                "y2 <- tapply(x, g, function(a) 10*mean(a))",
                "cat(y2)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xtapply_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "15.000000000000000 35.000000000000000" in proc.stdout
    assert "150.00000000000000 350.00000000000000" in proc.stdout
    assert "tapply(" not in out_text
    assert "xr2f_tapply_i_y1" in out_text
    assert "xr2f_tapply_i_y2" in out_text


def test_xr2f_static_new_env_and_eapply_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnew_env_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "twice <- function(x) return(2*x)",
                "e <- new.env()",
                "e$x <- 10",
                "e$y <- 20",
                "print(e$x)",
                "e$x <- 100",
                "print(e$x)",
                "res1 <- eapply(e, twice)",
                "print(res1)",
                "res2 <- eapply(e, function(a) 10*a)",
                "print(res2)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnew_env_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "200 40" in proc.stdout
    assert "1000 200" in proc.stdout
    assert "new.env" not in out_text
    assert "eapply(" not in out_text
    assert "e$x" not in out_text
    assert "e_x" in out_text and "e_y" in out_text


def test_xr2f_branch_logical_scalar_local_decl_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xbranch_logical_scalar_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "if (TRUE) {",
                "  ok <- file.exists(\"missing.txt\")",
                "} else {",
                "  ok <- FALSE",
                "}",
                "flag <- ok",
                "print(flag)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xbranch_logical_scalar_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "logical :: ok" in out_text
    assert 'write(*,"(l1)") ok' in out_text


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


def test_xr2f_list_scalar_field_demote_no_embedded_declaration_prefix(tmp_path: Path) -> None:
    local_input = tmp_path / "xscalar_field_prefix_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "summarize <- function(x) {",
                "  mu <- mean(x)",
                "  s <- sd(x)",
                "  out <- list(mu = mu, s = s)",
                "  return(out)",
                "}",
                "fit <- summarize(c(1.0, 2.0, 3.0))",
                "mu_value <- fit$mu",
                'cat("mu:", mu_value, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xscalar_field_prefix_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert ":: real(kind=dp) ::" not in out_text
    assert ":: integer ::" not in out_text
    assert "real(kind=dp) :: mu" in out_text
    assert _has_fortran_decl(out_text, "real(kind=dp)", "s")


def test_xr2f_list_character_vector_fields_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xlist_character_fields.r"
    local_input.write_text(
        "\n".join(
            [
                'config <- list(signal_assets = c("LQD", "HYG"), traded_assets = c("SPY"), v = c(10L, 20L))',
                "print(config)",
                'config$signal_assets <- c("QQQ")',
                "print(config)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xlist_character_fields.f90"

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
    assert "character(len=:), allocatable :: signal_assets(:)" in out_text
    assert "character(len=:), allocatable :: traded_assets(:)" in out_text
    assert "integer, allocatable :: v(:)" in out_text
    assert "real(kind=dp), allocatable :: signal_assets(:)" not in out_text
    assert "config%v = [10, 20]" in out_text
    assert "QQQ" in proc.stdout


def test_xr2f_main_list_alias_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xmain_list_alias_fields.r"
    local_input.write_text(
        "\n".join(
            [
                'nm <- c("aa", "bb")',
                "cnt <- c(1L, 2L)",
                "flg <- c(TRUE, FALSE)",
                "config <- list(names = nm, counts = cnt, flags = flg)",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmain_list_alias_fields.f90"

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
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_main_list_chained_alias_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xmain_list_chained_alias_fields.r"
    local_input.write_text(
        "\n".join(
            [
                'nm0 <- c("aa", "bb")',
                "cnt0 <- c(1L, 2L)",
                "ok0 <- c(TRUE, FALSE)",
                "nm <- nm0",
                "cnt <- cnt0",
                "ok <- ok0",
                "config <- list(names = nm, counts = cnt, flags = ok)",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmain_list_chained_alias_fields.f90"

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
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_branch_list_alias_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xbranch_list_alias_fields.r"
    local_input.write_text(
        "\n".join(
            [
                "if (TRUE) {",
                '  nm <- c("aa", "bb")',
                "  cnt <- c(1L, 2L)",
                "  ok <- c(TRUE, FALSE)",
                "  config <- list(names = nm, counts = cnt, flags = ok)",
                "} else {",
                '  nm <- c("cc")',
                "  cnt <- c(3L)",
                "  ok <- c(FALSE)",
                "  config <- list(names = nm, counts = cnt, flags = ok)",
                "}",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xbranch_list_alias_fields.f90"

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
    assert "type(config_result_t) :: config" in out_text
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_switch_integer_list_cases_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xswitch_list_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "ifile <- 2",
                "config <- switch(",
                "  ifile,",
                "  list(",
                '    signal_assets = c("LQD", "HYG"),',
                '    traded_assets = c("SPY")',
                "  ),",
                "  list(",
                '    signal_assets = c("MBB", "NLY", "AGNC"),',
                '    traded_assets = c("NLY", "AGNC", "MFA", "ORC", "ARR")',
                "  )",
                ")",
                "print(config$signal_assets)",
                "print(config$traded_assets)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xswitch_list_probe.f90"

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
    assert "type(config_result_t) :: config" in out_text
    assert "select case (ifile)" in out_text
    assert "character(len=:), allocatable :: signal_assets(:)" in out_text
    assert "character(len=:), allocatable :: traded_assets(:)" in out_text
    assert "call print_char_vector(config%signal_assets)" in out_text
    assert "call print_char_vector(config%traded_assets)" in out_text
    assert "MBB" in proc.stdout and "ARR" in proc.stdout


def test_xr2f_function_list_literal_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfunction_list_literal_fields.r"
    local_input.write_text(
        "\n".join(
            [
                "make_config <- function() {",
                '  list(names = c("aa", "bb"), counts = c(1L, 2L), flags = c(TRUE, FALSE))',
                "}",
                "config <- make_config()",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfunction_list_literal_fields.f90"

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
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_function_list_alias_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfunction_list_alias_fields.r"
    local_input.write_text(
        "\n".join(
            [
                "make_config <- function() {",
                '  nm <- c("aa", "bb")',
                "  cnt <- c(1L, 2L)",
                "  flg <- c(TRUE, FALSE)",
                "  list(names = nm, counts = cnt, flags = flg)",
                "}",
                "config <- make_config()",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfunction_list_alias_fields.f90"

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
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_function_branch_list_alias_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfunction_branch_list_alias_fields.r"
    local_input.write_text(
        "\n".join(
            [
                "make_config <- function() {",
                "  if (TRUE) {",
                '    nm <- c("aa", "bb")',
                "    cnt <- c(1L, 2L)",
                "    ok <- c(TRUE, FALSE)",
                "  } else {",
                '    nm <- c("cc")',
                "    cnt <- c(3L)",
                "    ok <- c(FALSE)",
                "  }",
                "  list(names = nm, counts = cnt, flags = ok)",
                "}",
                "config <- make_config()",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfunction_branch_list_alias_fields.f90"

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
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_function_list_chained_alias_vector_field_kinds_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xfunction_list_chained_alias_fields.r"
    local_input.write_text(
        "\n".join(
            [
                "make_config <- function() {",
                '  nm0 <- c("aa", "bb")',
                "  cnt0 <- c(1L, 2L)",
                "  ok0 <- c(TRUE, FALSE)",
                "  nm <- nm0",
                "  cnt <- cnt0",
                "  ok <- ok0",
                "  list(names = nm, counts = cnt, flags = ok)",
                "}",
                "config <- make_config()",
                "print(config$names)",
                "print(config$counts)",
                "print(config$flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xfunction_list_chained_alias_fields.f90"

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
    assert "character(len=:), allocatable :: names(:)" in out_text
    assert "integer, allocatable :: counts(:)" in out_text
    assert "logical, allocatable :: flags(:)" in out_text
    assert "call print_char_vector(config%names)" in out_text
    assert "call print_integer_vector(config%counts)" in out_text
    assert 'write(*,"(*(g0,1x))") config%flags' in out_text


def test_xr2f_coerces_positional_real_actual_for_integer_formal_from_decls(tmp_path: Path) -> None:
    local_input = tmp_path / "xpositional_int_actual_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "rolling_sum <- function(x, h) {",
                "  if (h < 1) stop(\"bad h\")",
                "  out <- numeric(length(x) - h + 1)",
                "  for (i in seq_along(out)) {",
                "    out[i] <- sum(x[i:(i + h - 1)])",
                "  }",
                "  out",
                "}",
                "driver <- function(x, horizons) {",
                "  h <- horizons[1]",
                "  sum(rolling_sum(x, h))",
                "}",
                "cat(driver(c(1.0, 2.0, 3.0, 4.0), c(2.0)), \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xpositional_int_actual_probe.f90"

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
    assert "rolling_sum(x, int(h))" in flat_out


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
    assert "real(kind=dp), allocatable :: lagx(:)" in out_text
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "resid(:)")
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "y(:)")
    assert "type(fit_ar1_result_t) :: y_list" in out_text
    assert "call print_lm_summary(fit%lm_fit)" in out_text


def test_xr2f_lm_accessor_keyword_object_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xlm_accessor_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(1.0, 2.0, 3.0, 4.0)",
                "y <- c(2.0, 4.1, 5.9, 8.2)",
                "fit <- lm(y ~ x)",
                "r <- resid(object = fit)",
                "r2 <- residuals(object = fit)",
                "cf <- coef(object = fit)",
                "fv <- fitted(object = fit)",
                "pv <- predict(object = fit)",
                "cd <- cooks.distance(object = fit)",
                "print(summary(object = fit))",
                "print(r)",
                "print(r2)",
                "print(cf)",
                "print(fv)",
                "print(pv)",
                "print(cd)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xlm_accessor_keywords.f90"

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
    assert "fit%resid" in out_text
    assert "fit%coef" in out_text
    assert "fit%fitted" in out_text
    assert "call print_lm_summary(fit" in out_text
    assert "lm_cooks_distance(fit)" in out_text


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
    assert "type(f_result_t) :: f_result" in out_text
    assert "type(f_result_t) :: y_list" in out_text
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


def test_xr2f_solve_rhs_keyword_rank_extraction_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xsolve_rhs_keywords.r"
    local_input.write_text(
        "\n".join(
            [
                "A <- matrix(c(2.0, 0.0, 0.0, 3.0), nrow = 2, byrow = TRUE)",
                "b <- c(4.0, 9.0)",
                "L <- chol(A)",
                "U <- t(L)",
                "x1 <- solve(A, b = b)",
                "x2 <- qr.solve(A, b = b)",
                "x3 <- forwardsolve(L, x = b)",
                "x4 <- backsolve(U, x = b)",
                "x5 <- solve(a = A, b = b)",
                "x6 <- qr.solve(a = A, b = b)",
                "x7 <- forwardsolve(l = L, x = b)",
                "x8 <- backsolve(r = U, x = b)",
                "L2 <- chol(x = A)",
                "print(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + L2[, 1])",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsolve_rhs_keywords.f90"

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
    assert "solve_real(A, b)" in out_text
    assert "qr_coef(qr(real(A, kind=dp)), b)" in out_text
    assert "forwardsolve(L, b, transpose=.false.)" in out_text
    assert "backsolve(U, b, transpose=.false.)" in out_text
    assert "L2 = chol(A)" in out_text
    assert "Build: PASS" in proc.stdout


def test_xr2f_solve_tangency_vector_rhs_return_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xsolve_tangency_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "solve_tangency <- function(cov_mat, mu_vec) {",
                "  ridge <- 1e-4 * mean(diag(cov_mat))",
                "  solve(cov_mat + diag(ridge, nrow(cov_mat)), mu_vec)",
                "}",
                "cov_mat <- matrix(c(0.04, 0.01, 0.01, 0.09), nrow = 2, byrow = TRUE)",
                "mu_vec <- c(0.08, 0.12)",
                "print(solve_tangency(cov_mat, mu_vec))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsolve_tangency_probe.f90"

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
    assert _has_fortran_decl(out_text, "real(kind=dp), intent(in)", "mu_vec(:)")
    assert "real(kind=dp), allocatable :: solve_tangency_result(:)" in out_text
    assert "solve_tangency_result(:,:)" not in out_text
    assert "solve_real(cov_mat + diag" in out_text


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
    assert "real(kind=dp), allocatable :: a(:,:)" in out_text
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "amat(:,:)")
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "coef_mat(:,:)")


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
    assert "real(kind=dp), allocatable :: neg(:)" in out_text
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "pos(:)")


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
    flat_out = " ".join(out_text.split())
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "y(:)")
    assert _has_fortran_decl(out_text, "real(kind=dp), allocatable", "z(:)")
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


def test_xr2f_switch_body_character_index_alias_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xswitch_character_index_alias_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "mode <- 1",
                'labels <- c("low", "high")',
                "switch(",
                "  mode,",
                "  {",
                "    idx <- c(1L, 2L)",
                "    grp <- labels[idx]",
                "  },",
                "  {",
                "    idx <- c(2L)",
                "    grp <- labels[idx]",
                "  }",
                ")",
                "print(grp)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xswitch_character_index_alias_probe.f90"

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
    assert '"low"' in out_text
    assert '"high"' in out_text
    assert "integer, allocatable :: idx(:)" in out_text
    assert "real(kind=dp), allocatable :: idx(:)" not in out_text
    assert "call print_char_vector(grp)" in out_text


def test_xr2f_named_c_function_return_string_subscripts_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnamed_c_return_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_par <- function(x) {",
                "  c(omega = x[1], alpha = x[2], beta = x[3])",
                "}",
                "p <- make_par(c(1, 2, 3))",
                'cat(p["omega"], p["alpha"], p["beta"], "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnamed_c_return_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "1.000000" in proc.stdout and "2.000000" in proc.stdout and "3.000000" in proc.stdout
    assert '("omega")' not in flat_out
    assert '("alpha")' not in flat_out
    assert '("beta")' not in flat_out
    assert "p(1)" in flat_out and "p(2)" in flat_out and "p(3)" in flat_out


def test_xr2f_named_c_local_alias_return_string_subscripts_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnamed_c_alias_return_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_par <- function(x) {",
                "  y <- c(omega = x[1], alpha = x[2], beta = x[3])",
                "  return(y)",
                "}",
                "p <- make_par(c(1, 2, 3))",
                'cat(p["omega"], p["alpha"], p["beta"], "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnamed_c_alias_return_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "1.000000" in proc.stdout and "2.000000" in proc.stdout and "3.000000" in proc.stdout
    assert '("omega")' not in flat_out
    assert '("alpha")' not in flat_out
    assert '("beta")' not in flat_out
    assert "p(1)" in flat_out and "p(2)" in flat_out and "p(3)" in flat_out


def test_xr2f_named_c_local_alias_chain_return_string_subscripts_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xnamed_c_alias_chain_return_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_par <- function(x) {",
                "  y <- c(omega = x[1], alpha = x[2], beta = x[3])",
                "  z <- y",
                "  z",
                "}",
                "p <- make_par(c(1, 2, 3))",
                'cat(p["omega"], p["alpha"], p["beta"], "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xnamed_c_alias_chain_return_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "1.000000" in proc.stdout and "2.000000" in proc.stdout and "3.000000" in proc.stdout
    assert '("omega")' not in flat_out
    assert '("alpha")' not in flat_out
    assert '("beta")' not in flat_out
    assert "p(1)" in flat_out and "p(2)" in flat_out and "p(3)" in flat_out


def test_xr2f_function_return_rank_through_local_alias_chain_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xreturn_rank_alias_chain_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "take3 <- function(x) {",
                "  y <- x[1:3]",
                "  z <- y",
                "  z",
                "}",
                "out <- take3(c(1, 2, 3, 4))",
                "print(out)",
                "cat(length(out), \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreturn_rank_alias_chain_probe.f90"

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
    assert "real(kind=dp), allocatable :: take3_result(:)" in out_text
    assert "real(kind=dp) :: take3_result" not in out_text
    assert "3" in proc.stdout


def test_xr2f_function_return_kind_through_local_alias_chain_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xreturn_kind_alias_chain_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_idx <- function(n) {",
                "  y <- integer(n)",
                "  y[1] <- 2L",
                "  z <- y",
                "  z",
                "}",
                "idx <- make_idx(3)",
                "print(idx)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreturn_kind_alias_chain_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "integer, allocatable :: make_idx_result(:)" in out_text
    assert "real(kind=dp), allocatable :: make_idx_result(:)" not in out_text


def test_xr2f_function_return_integer_c_kind_through_alias_chain_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xreturn_integer_c_alias_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_idx <- function() {",
                "  y <- c(1L, 2L, 3L)",
                "  z <- y",
                "  z",
                "}",
                "idx <- make_idx()",
                "print(idx)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreturn_integer_c_alias_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "integer, allocatable :: make_idx_result(:)" in out_text
    assert "real(kind=dp), allocatable :: make_idx_result(:)" not in out_text


def test_xr2f_function_return_integer_c_kind_through_local_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xreturn_integer_c_local_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_idx <- function() {",
                "  y <- c(1L, 2L, 3L)",
                "  y",
                "}",
                "idx <- make_idx()",
                "print(idx)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreturn_integer_c_local_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "integer, allocatable :: make_idx_result(:)" in out_text
    assert "real(kind=dp), allocatable :: make_idx_result(:)" not in out_text


def test_xr2f_function_return_logical_c_kind_through_alias_chain_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xreturn_logical_c_alias_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_flags <- function() {",
                "  y <- c(TRUE, FALSE, TRUE)",
                "  z <- y",
                "  z",
                "}",
                "flags <- make_flags()",
                "print(flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreturn_logical_c_alias_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "logical, allocatable :: make_flags_result(:)" in out_text
    assert "real(kind=dp), allocatable :: make_flags_result(:)" not in out_text


def test_xr2f_function_return_logical_c_kind_through_local_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xreturn_logical_c_local_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_flags <- function() {",
                "  y <- c(TRUE, FALSE, TRUE)",
                "  y",
                "}",
                "flags <- make_flags()",
                "print(flags)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xreturn_logical_c_local_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    out_text = out_path.read_text(encoding="utf-8")
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "logical, allocatable :: make_flags_result(:)" in out_text
    assert "real(kind=dp), allocatable :: make_flags_result(:)" not in out_text


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


def test_xr2f_sys_tier1_datetime_functions_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xsys_tier1_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "now <- Sys.time()",
                "today <- Sys.Date()",
                "d <- date()",
                "tz <- Sys.timezone()",
                'cat("now class:", class(now), "\\n")',
                'cat("today class:", class(today), "\\n")',
                'cat("date class:", class(d), "\\n")',
                'cat("timezone:", tz, "\\n")',
                'cat("date fmt:", format(now, "%Y-%m-%d"), "\\n")',
                'cat("time fmt:", format(now, "%H:%M:%S"), "\\n")',
                "t0 <- Sys.time()",
                "Sys.sleep(0)",
                "t1 <- Sys.time()",
                'cat("secs:", difftime(t1, t0, units = "secs"), "\\n")',
                'cat("mins:", difftime(t1, t0, units = "mins"), "\\n")',
                "pt0 <- proc.time()",
                "pt1 <- proc.time()",
                "print(pt1 - pt0)",
                'cat("tomorrow:", as.character(today + 1), "\\n")',
                'cat("last week:", as.character(today - 7), "\\n")',
                'cat("later:", format(now + 3600, "%Y-%m-%d %H:%M:%S"), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsys_tier1_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run", "--no-warn"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "now class:" in proc.stdout and "POSIXct" in proc.stdout
    assert "today class:" in proc.stdout and "Date" in proc.stdout
    assert "date class:" in proc.stdout and "character" in proc.stdout
    assert "sys_time()" in out_text
    assert "sys_date()" in out_text
    assert "sys_date_string()" in out_text
    assert "sys_timezone()" in out_text
    assert "sys_sleep" in out_text
    assert "proc_time_vec()" in out_text
    assert "date_to_char(today + 1)" in flat_out
    assert 'sys_time_format(now + 3600, "%Y-%m-%d %H:%M:%S")' in flat_out
    assert '"tomorrow:", (today + 1)' not in flat_out
    assert '"later:", now + 3600' not in flat_out


def test_xr2f_proc_time_elapsed_checkpoints_are_not_inlined(tmp_path: Path) -> None:
    local_input = tmp_path / "xtiming_probe.r"
    local_input.write_text(
        "\n".join(
            [
                't0 <- proc.time()[["elapsed"]]',
                "x <- sum(1:10)",
                't1 <- proc.time()[["elapsed"]]',
                'cat("elapsed:", t1 - t0, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xtiming_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )


def _run_xr2f_compile_source(tmp_path: Path, source_path: Path) -> subprocess.CompletedProcess[str]:
    local_input = tmp_path / source_path.name
    local_input.write_text(source_path.read_text(encoding="utf-8-sig"), encoding="utf-8")
    out_path = tmp_path / f"{source_path.stem}.f90"
    cmd = [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"]
    proc = subprocess.run(
        cmd,
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )
    if (
        os.name == "nt"
        and proc.returncode == 0xC000070A
        and not proc.stdout
        and not proc.stderr
    ):
        proc = subprocess.run(
            cmd,
            cwd=tmp_path,
            capture_output=True,
            text=True,
            check=False,
        )
    return proc

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "t0 = r_elapsed()" in out_text
    assert "t1 = r_elapsed()" in out_text
    assert "t1 - t0" in flat_out
    assert "r_elapsed() - r_elapsed()" not in flat_out


def test_xr2f_recent_r_semantics_regression_compile(tmp_path: Path) -> None:
    local_input = tmp_path / "xrecent_semantics_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "# f controls the smoothing fraction and should remain a comment.",
                "# Fortran counterpart: this should also remain a comment.",
                "x <- matrix(1:6, nrow = 3)",
                "print(x[c(1, 6)])",
                "print(x[-c(1, 3)])",
                "print(x[x > 2])",
                "x[6] <- 99",
                "x[c(4, 5)] <- -1",
                "x[c(2, 3)] <- c(10, 20)",
                "print(x)",
                "print(x[, 1, drop = FALSE])",
                "print(matrix(1:6, nrow = 3)[, 1])",
                "print(matrix(1:6, nrow = 3)[c(1, 6)])",
                "print(matrix(1:6, nrow = 3)[-c(1, 3)])",
                "print(matrix(1:6, nrow = 3) + c(10, 20, 30))",
                "a1 <- array(1:5, dim = 5)",
                "print(a1)",
                "a3 <- array(1:8, dim = c(2, 2, 2))",
                "print(a3[1, , 1, drop = FALSE])",
                "print(a3 + a3)",
                "g <- c(1, 2, 1, 2)",
                "idx <- which(g == 1)",
                    "m <- matrix(1:8, nrow = 4)",
                    "print(m[1, ])",
                    "print(colMeans(m[idx, , drop = FALSE]))",
                'names <- c("r1", "r2")',
                "print(names)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xrecent_semantics_probe.f90"

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
    assert "controls the smoothing fraction" in out_text
    assert "! Fortran counterpart" in out_text
    assert "controls the smoothing fraction" in out_text
    assert "\ncontrols the smoothing fraction." not in out_text
    assert "r_matrix_index" in out_text
    assert "print_integer_vector(a1)" in out_text
    assert "print_real_vector(real(reshape(a3 + a3" in flat_out
    assert "r_seq_int(idx, idx)" not in out_text
    assert "idx:idx" not in out_text


def test_xr2f_matrix_dimnames_vector_subsets_run_both(tmp_path: Path) -> None:
    local_input = tmp_path / "xmatrix_dimnames_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- matrix(1:4, 2, 2)",
                'rownames(x) <- c("r1", "r2")',
                'colnames(x) <- c("c1", "c2")',
                "print(x[1, ])",
                "print(x[, 1])",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xmatrix_dimnames_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run-both"],
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
    assert "call print_named_real_vector(real(x(1, :), kind=dp)" in flat_out
    assert "call print_named_real_vector(real(x(:, 1), kind=dp)" in flat_out
    assert "c1 c2" in proc.stdout
    assert "r1 r2" in proc.stdout


def test_xr2f_scalar_list_field_stays_scalar_when_used_with_vector(tmp_path: Path) -> None:
    local_input = tmp_path / "xscalar_list_field_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "make_par <- function(par) {",
                "  nu <- 2.01 + exp(par[1])",
                "  y <- list(nu = nu)",
                "  return(y)",
                "}",
                "log_density <- function(z, nu) {",
                "  z + nu",
                "}",
                "p <- make_par(c(1.0))",
                "x <- c(1.0, 2.0)",
                'cat("nu =", p$nu, "\\n")',
                "print(sum(log_density(x, p$nu)))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xscalar_list_field_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "real(kind=dp) :: nu" in flat_out
    assert "nu(:)" not in flat_out


@pytest.mark.parametrize("example_name", SUPPORTED_R_COMPILE_CASES)
def test_xr2f_compiles_supported_local_r_examples(tmp_path: Path, example_name: str) -> None:
    proc = _run_xr2f_compile(tmp_path, example_name)

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert (tmp_path / "r.f90").exists()


@pytest.mark.parametrize("example_name", STRESS_R_COMPILE_CASES)
def test_xr2f_compiles_recent_regression_stress_examples(tmp_path: Path, example_name: str) -> None:
    proc = _run_xr2f_compile(tmp_path, example_name)

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert (tmp_path / "r.f90").exists()


@pytest.mark.parametrize(
    ("example_name", "required_fragments", "forbidden_fragments"),
    [
        (
            "xgarch_dcc_t.r",
            ["call read_csv_real_matrix(price_file, dat)", "dat(:,:)"],
            [
                "dat(:), garch_fit",
                "real(kind=dp), allocatable :: opt_p(:), real",
                "real(:,:), allocatable ::",
            ],
        ),
        ("xvar.r", ["xlag(:,:)", "xlag = x(r_seq_int(1, n - 1), :)"], ["xlag(:), y(:,:)"]),
        ("xvar_ic.r", ["integer :: aic_order", "integer :: bic_order"], ["aic_order(:)", "bic_order(:)"]),
        ("xxarray.r", ["b(:,:)", "b = r_array_real([10.0_dp, 20.0_dp], [3, 4])"], ["b(:,:,:)"]),
        ("xsolve.r", ["complex(kind=dp), allocatable :: A_2(:,:)"], ["real(kind=dp), allocatable :: A_2(:,:)"]),
    ],
)
def test_xr2f_recent_regression_generated_fragments(
    tmp_path: Path,
    example_name: str,
    required_fragments: list[str],
    forbidden_fragments: list[str],
) -> None:
    proc = _run_xr2f_compile(tmp_path, example_name)

    out_text = (tmp_path / "r.f90").read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    for fragment in required_fragments:
        assert _has_fortran_decl_fragment(out_text, fragment)
    for fragment in forbidden_fragments:
        assert not _has_fortran_decl_fragment(out_text, fragment)


@pytest.mark.parametrize(
    ("fixture_name", "required_stdout", "forbidden_stdout"),
    [
        (
            "02_matrix_rownames_colnames.R",
            ["labeled matrix:", "SPY", "EFA", "EEM", "mean", "sd"],
            ["[,1]"],
        ),
        (
            "05_transpose_labels.R",
            ["transpose labels:", "mean", "sd", "SPY", "EFA", "EEM"],
            ["[1,]"],
        ),
        (
            "06_cor_cov2cor_labels.R",
            ["cor labels:", "cov labels via assignment:", "cov2cor labels:", "SPY", "EFA", "EEM"],
            ["[,1]"],
        ),
        (
            "07_apply_t_named_columns.R",
            ["apply stats labels:", "SPY", "EFA", "EEM", "mean", "sd", "min", "max"],
            ["[,1]"],
        ),
        (
            "09_matrix_no_labels_control.R",
            ["unlabeled matrix control:", "[,1]", "[1,]"],
            [],
        ),
    ],
)
def test_xr2f_preserves_matrix_and_vector_labels_in_prints(
    tmp_path: Path,
    fixture_name: str,
    required_stdout: list[str],
    forbidden_stdout: list[str],
) -> None:
    source_path = REPO_ROOT / "label_tests" / fixture_name
    local_input = tmp_path / fixture_name
    local_input.write_text(source_path.read_text(encoding="utf-8-sig"), encoding="utf-8")
    out_path = tmp_path / f"{source_path.stem}.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    for fragment in required_stdout:
        assert fragment in proc.stdout
    for fragment in forbidden_stdout:
        assert fragment not in proc.stdout


def test_xr2f_coerces_local_integer_actuals_for_real_function_formals(tmp_path: Path) -> None:
    local_input = tmp_path / "xconvolve_no_decl_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "slow_convolve <- function(a, b) {",
                "  ab <- double(length(a) + length(b) - 1)",
                "  for (i in seq_along(a)) {",
                "    for (j in seq_along(b)) {",
                "      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]",
                "    }",
                "  }",
                "  ab",
                "}",
                "na <- 10",
                "nb <- 3",
                "a <- 1:na",
                "b <- 1:nb",
                "r <- slow_convolve(a, b)",
                "nr <- length(r)",
                "cat(length(r), r[nr], \"\\n\")",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xconvolve_no_decl_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "slow_convolve(real(a, kind=dp), real(b, kind=dp))" in flat_out
    assert "12 30" in proc.stdout


def test_xr2f_lifts_nested_helpers_and_lowers_native_pipe(tmp_path: Path) -> None:
    local_input = tmp_path / "xdiffuse_no_decl_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "diffuse_heat <- function(nx, ny, dx, dy, dt, k, steps) {",
                "  temp <- matrix(0, nx, ny)",
                "  temp[nx %/% 2L, ny %/% 2L] <- 100",
                "  apply_boundary_conditions <- function(temp) {",
                "    temp[1, ] <- 0",
                "    temp[nx, ] <- 0",
                "    temp[, 1] <- 0",
                "    temp[, ny] <- 0",
                "    temp",
                "  }",
                "  update_temperature <- function(temp) {",
                "    temp_new <- temp",
                "    i <- 2:(nx - 1)",
                "    j <- 2:(ny - 1)",
                "    laplacian <-",
                "      (temp[i + 1, j] - 2 * temp[i, j] + temp[i - 1, j]) / dx ^ 2 +",
                "      (temp[i, j + 1] - 2 * temp[i, j] + temp[i, j - 1]) / dy ^ 2",
                "    temp_new[i, j] <- temp[i, j] + k * dt * laplacian",
                "    temp_new",
                "  }",
                "  for (step in seq_len(steps)) {",
                "    temp <- temp |> ",
                "      apply_boundary_conditions() |> ",
                "      update_temperature()",
                "  }",
                "  temp",
                "}",
                "nx <- 20L",
                "ny <- 20L",
                "dx <- 1L",
                "dy <- 1L",
                "dt <- 0.01",
                "k <- 0.1",
                "steps <- 20L",
                "diffuse_heat <- diffuse_heat(nx, ny, dx, dy, dt, k, steps)",
                "cat(diffuse_heat[1,1], diffuse_heat[1, ny], diffuse_heat[nx, 1], diffuse_heat[nx, ny])",
                'cat("\\n")',
                "cat(round(min(diffuse_heat), 4), round(max(diffuse_heat), 4))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xdiffuse_no_decl_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "function diffuse_heat_apply_boundary_conditions" in out_text
    assert "function diffuse_heat_update_temperature" in out_text
    assert "|>" not in out_text
    assert "diffuse_heat_r = diffuse_heat(" in flat_out
    assert "0.0000000000000000 0.0000000000000000 0.0000000000000000 0.0000000000000000" in proc.stdout
    assert "92.3675" in proc.stdout


def test_xr2f_infers_integer_formal_for_integer_modulus(tmp_path: Path) -> None:
    local_input = tmp_path / "xoddcount_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "oddcount <- function(x) {",
                "  return(sum(x%%2 != 0))",
                "}",
                "v <- 2:7",
                "print(oddcount(v))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xoddcount_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "integer, intent(in) :: x(:)" in flat_out
    assert "integer :: oddcount_result" in flat_out
    assert "iso_fortran_env" not in out_text
    assert "oddcount(v)" in flat_out
    assert "real(v, kind=dp)" not in flat_out
    assert "\n3\n" in proc.stdout


def test_xr2f_sum_logical_vector_lowers_to_count(tmp_path: Path) -> None:
    local_input = tmp_path / "xsum_logical_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- c(TRUE, FALSE, TRUE)",
                "print(sum(x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xsum_logical_probe.f90"

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
    assert 'write(*,"(g0)") count(x)' in out_text
    assert "sum(merge(1, 0, x))" not in out_text
    assert "\n2\n" in proc.stdout


def test_xr2f_integrate_result_object_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xintegrate_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(x) {",
                "  x^2",
                "}",
                "result <- integrate(f, lower = 0, upper = 10)",
                "print(result)",
                'cat("value =", result$value, "\\n")',
                'cat("absolute error =", result$abs.error, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xintegrate_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "pure function f(x) result(f_result)" in out_text
    assert "pure elemental function f" not in out_text
    assert "type(integrate_result_t) :: result" in flat_out
    assert "result = integrate(f, lower=real(0, kind=dp), upper=real(10, kind=dp))" in flat_out
    assert "call print_integrate_result(result)" in out_text
    assert "result%value" in out_text
    assert "result%abs_error" in out_text
    assert "333.333" in proc.stdout


def test_xr2f_integrate_handles_two_sided_improper_integral(tmp_path: Path) -> None:
    local_input = tmp_path / "ximproper_integral_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(x) {",
                "  exp(-x^2)",
                "}",
                "result <- integrate(f, lower = -Inf, upper = Inf)",
                'cat("value =", result$value, "\\n")',
                'cat("absolute error =", result$abs.error, "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "ximproper_integral_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--run"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    flat_out = " ".join(out_text.replace("&", " ").split())
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
    assert "Run: PASS" in proc.stdout
    assert "type(integrate_result_t) :: result" in flat_out
    assert "lower=real(-huge(1.0_dp), kind=dp)" in flat_out
    assert "upper=real(huge(1.0_dp), kind=dp)" in flat_out
    assert "NaN" not in proc.stdout
    assert "1.77245" in proc.stdout


def test_xr2f_symbolic_derivative_eval_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xderivative_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "f <- function(x) {",
                "  x^2",
                "}",
                'expr <- expression(x^2)',
                'dexpr <- D(expr, "x")',
                "fprime <- function(x) {",
                "  eval(dexpr)",
                "}",
                'cat("f(3)  =", f(3), "\\n")',
                'cat("fprime(3) =", fprime(3), "\\n")',
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xderivative_probe.f90"

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
    assert "eval(" not in out_text
    assert "D(" not in out_text
    assert "fprime_result =" in out_text
    assert "fprime(3) =" in proc.stdout
    assert "6" in proc.stdout


def test_xr2f_symbolic_derivative_reports_unsupported_expression(tmp_path: Path) -> None:
    local_input = tmp_path / "xderivative_unsupported_probe.r"
    local_input.write_text(
        "\n".join(
            [
                'expr <- expression(besselJ(x, 1))',
                'dexpr <- D(expr, "x")',
                "fprime <- function(x) {",
                "  eval(dexpr)",
                "}",
                "print(fprime(2))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xderivative_unsupported_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path), "--compile"],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    assert proc.returncode != 0
    assert "unsupported D() expression" in proc.stdout + proc.stderr
    assert "besselJ" in proc.stdout + proc.stderr


def test_xr2f_symbolic_derivative_special_functions_compile_and_run(tmp_path: Path) -> None:
    local_input = tmp_path / "xderivative_special_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "fprime <- function(x) {",
                "  d_tan <- D(expression(tan(x)), \"x\")",
                "  d_gamma <- D(expression(gamma(x)), \"x\")",
                "  d_lgamma <- D(expression(lgamma(x)), \"x\")",
                "  d_erf <- D(expression(erf(x)), \"x\")",
                "  d_erfc <- D(expression(erfc(x)), \"x\")",
                "  d_cospi <- D(expression(cospi(x)), \"x\")",
                "  d_sinpi <- D(expression(sinpi(x)), \"x\")",
                "  d_tanpi <- D(expression(tanpi(x)), \"x\")",
                "  c(eval(d_tan), eval(d_gamma), eval(d_lgamma), eval(d_erf),",
                "    eval(d_erfc), eval(d_cospi), eval(d_sinpi), eval(d_tanpi))",
                "}",
                "print(round(fprime(0.5), 4))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xderivative_special_probe.f90"

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
    assert "r_digamma" in out_text
    assert "eval(" not in out_text
    assert "D(" not in out_text
    assert "NaN" not in proc.stdout


def test_xr2f_viterbi_no_decl_benchmark_wrapper_compiles(tmp_path: Path) -> None:
    local_input = tmp_path / "xviterbi_no_decl_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "slow_viterbi <- function(observations, states, initial_probs, transition_probs, emission_probs) {",
                "  trellis <- matrix(0, nrow = length(states), ncol = length(observations))",
                "  backpointer <- matrix(0L, nrow = length(states), ncol = length(observations))",
                "  trellis[, 1] <- initial_probs * emission_probs[, observations[1]]",
                "  for (step in 2:length(observations)) {",
                "    for (current_state in 1:length(states)) {",
                "      probabilities <- trellis[, step - 1] * transition_probs[, current_state]",
                "      trellis[current_state, step] <- max(probabilities) * emission_probs[current_state, observations[step]]",
                "      backpointer[current_state, step] <- which.max(probabilities)",
                "    }",
                "  }",
                "  path <- integer(length(observations))",
                "  path[length(observations)] <- which.max(trellis[, length(observations)])",
                "  for (step in seq(length(observations) - 1, 1)) {",
                "    path[step] <- backpointer[path[step + 1], step + 1]",
                "  }",
                "  states[path]",
                "}",
                "set.seed(1234)",
                "num_steps <- 8",
                "num_states <- 4",
                "num_obs <- 5",
                "observations <- sample(1:num_obs, num_steps, replace = TRUE)",
                "states <- 1:num_states",
                "initial_probs <- runif (num_states)",
                "initial_probs <- initial_probs / sum(initial_probs)",
                "transition_probs <- matrix(runif (num_states * num_states), nrow = num_states)",
                "transition_probs <- transition_probs / rowSums(transition_probs)",
                "emission_probs <- matrix(runif (num_states * num_obs), nrow = num_states)",
                "emission_probs <- emission_probs / rowSums(emission_probs)",
                "timings <- bench::mark(",
                "  slow_viterbi = slow_viterbi(observations, states, initial_probs, transition_probs, emission_probs),",
                ")",
                "print(timings)",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xviterbi_no_decl_probe.f90"

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
    assert "integer, allocatable :: observations(:)" in out_text
    assert _has_fortran_decl(out_text, "integer, allocatable", "states(:)")
    assert "real(kind=dp), allocatable :: slow_viterbi_result(:)" in out_text
    assert 'write(*,"(g0)") mark(' not in out_text


def test_xr2f_read_csv_result_stays_matrix_rank(tmp_path: Path) -> None:
    (tmp_path / "prices.csv").write_text("Date,SPY,EFA\n1,10,20\n2,11,21\n", encoding="utf-8")
    local_input = tmp_path / "xread_csv_rank_probe.r"
    local_input.write_text(
        "\n".join(
            [
                'dat <- read.csv("prices.csv", stringsAsFactors = FALSE)',
                "x <- dat[, 2:3]",
                "print(dim(x))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xread_csv_rank_probe.f90"

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
    assert "call read_csv_real_matrix" in out_text
    assert "dat(:,:)" in " ".join(out_text.replace("&", " ").split())


def test_xr2f_as_data_frame_list_field_lowers_to_matrix_value(tmp_path: Path) -> None:
    (tmp_path / "prices.csv").write_text("Date,SPY,EFA\n1,10,20\n2,11,21\n", encoding="utf-8")
    local_input = tmp_path / "xas_data_frame_field_probe.r"
    local_input.write_text(
        "\n".join(
            [
                "read_price_file <- function() {",
                '  dat <- read.csv("prices.csv", stringsAsFactors = FALSE, check.names = FALSE)',
                "  dates <- dat[[1]]",
                "  prices <- dat[-1]",
                "  list(dates = dates, prices = as.data.frame(prices, check.names = FALSE))",
                "}",
                "dat <- read_price_file()",
                "print(dim(dat$prices))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xas_data_frame_field_probe.f90"

    proc = subprocess.run(
        [sys.executable, str(XR2F_PATH), str(local_input), "--out", str(out_path)],
        cwd=tmp_path,
        capture_output=True,
        text=True,
        check=False,
    )

    out_text = out_path.read_text(encoding="utf-8")
    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "as.data.frame" not in out_text
    assert "read_price_file_result%prices = prices" in out_text


def test_xr2f_prints_integer_vector_expressions_as_integers(tmp_path: Path) -> None:
    local_input = tmp_path / "xinteger_vector_prints.r"
    local_input.write_text(
        "\n".join(
            [
                "x <- matrix(1:6, c(2, 3))",
                "print(dim(x))",
                "print(seq_len(3))",
                "print(which(c(TRUE, FALSE, TRUE)))",
            ]
        )
        + "\n",
        encoding="utf-8",
    )
    out_path = tmp_path / "xinteger_vector_prints.f90"

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
    assert "call print_integer_vector(shape(" in out_text
    assert 'write(*,"(*(g0,1x))") r_seq_len(3)' in out_text or "call print_integer_vector(r_seq_len(3))" in out_text
    assert "call print_integer_vector(which([" in out_text
    assert "print_real_vector(real(shape(x)" not in out_text


@pytest.mark.skipif(
    os.environ.get("XR2F_FULL_EXAMPLES", "").strip() != "1",
    reason="set XR2F_FULL_EXAMPLES=1 to compile the full example corpus",
)
@pytest.mark.parametrize(
    "source_path",
    _full_example_sources(),
    ids=lambda p: str(p.relative_to(REPO_ROOT)),
)
def test_xr2f_compiles_full_example_corpus_when_enabled(tmp_path: Path, source_path: Path) -> None:
    proc = _run_xr2f_compile_source(tmp_path, source_path)

    assert proc.returncode == 0, proc.stdout + proc.stderr
    assert "Build: PASS" in proc.stdout
