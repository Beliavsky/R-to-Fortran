# R-to-Fortran

`xr2f.py` is an experimental source-to-source transpiler from a practical subset of R to modern Fortran.  The goal is to translate numeric, array-oriented, and statistical R scripts into readable Fortran that can be compiled with `gfortran`.

This is not a complete R implementation.  It is useful for scripts that mostly use base R syntax, arrays, loops, vector operations, matrix algebra, and a growing subset of base R statistical algorithms.  The project now includes substantial Fortran runtime support for common statistics, distributions, smoothing, linear models, time-series helpers, clustering, tests, and file I/O patterns used by the example corpus.

## Quick Start

Translate an R script:

```bat
python xr2f.py r_examples\xhello.r
```

Compile the generated Fortran:

```bat
python xr2f.py r_examples\xhello.r --compile
```

Compile and run the generated Fortran:

```bat
python xr2f.py r_examples\xhello.r --run
```

Run both the original R script and the generated Fortran:

```bat
python xr2f.py r_examples\xhello.r --run-both
```

Create a single self-contained Fortran file with the needed runtime support prepended:

```bat
python xr2f.py r_examples\xrunif.r --self-contained --compile
```

## Example

Input R:

```r
x <- 3
y <- c(1.0, 4.0, 9.0)
print(x)
for (v in y) {
  print(v)
}
for (i in 1:3) {
  print(i)
}
```

Generated Fortran:

```fortran
program xr2f_smoke
use, intrinsic :: iso_fortran_env, only: dp => real64
implicit none
integer, parameter :: x = 3
real(kind=dp), parameter :: y(3) = [1.0_dp, 4.0_dp, 9.0_dp]
integer :: i
real(kind=dp) :: v

write(*,"(i0)") x
block
   integer :: i_v
   do i_v = 1, size(y)
      v = y(i_v)
      write(*,"(f0.6)") v
   end do
end block
do i = 1, 3
   write(*,"(i0)") i
end do
end program xr2f_smoke
```

## Requirements

- Python 3.11 or newer is recommended.
- `gfortran` is needed for `--compile`, `--run`, `--run-both`, and batch compile/run modes.
- `Rscript` is needed for `--run-both`, `--run-diff`, and `--time-both`.
- The helper runtime `r.f90` is used by default for R-like helper functions such as `rnorm_vec`, `sd`, `quantile`, matrix printing, and vector recycling.

## Files

- `xr2f.py`: main R-to-Fortran transpiler.
- `xr2f_batch.py`: batch runner for many R files, globs, directories, or `@list` files.
- `r.f90`: Fortran runtime helper module implementing R-like vector, matrix, statistics, distribution, model, smoothing, time-series, clustering, hypothesis-test, optimization, and file-I/O helpers.
- `fortran_scan.py`, `fortran_post.py`, `xunused.py`: Fortran scanning and postprocessing helpers used by the transpiler.
- `tests/`: pytest tests for the standalone command-line tools.
- `r_examples/`: small R scripts used as examples and regression inputs.  These include both R syntax probes and statistical algorithm examples.
- `r_stat_examples/`: numbered statistical examples, including data-reading examples and base-R statistical workflows.

Generated files normally use the suffix `_r.f90`, for example `foo.r` becomes `foo_r.f90`.

## Supported R Subset

The supported subset is intentionally focused on numerical scripts:

- Scalar arithmetic, comparisons, logical expressions, `if`, `for`, `while`, and simple functions.
- Numeric vectors and matrices.
- Vector constructors such as `c(...)`, ranges, `seq`, `rep`, `numeric`, and `integer`.
- Basic indexing, negative indexing, logical indexing, and selected matrix indexing forms.
- Elementwise vector arithmetic with optional R-style recycling.
- Common math functions such as `abs`, `sqrt`, `log`, `exp`, trigonometric functions, rounding, and `sign`.
- Reductions and statistics such as `sum`, `mean`, `sd`, `var`, `min`, `max`, `quantile`, `median`, `summary`, `cumsum`, `cumprod`, and `diff`.
- Matrix helpers such as `matrix`, `array`, `t`, `%*%`, `crossprod`, `tcrossprod`, `rowSums`, `colSums`, `det`, and `solve(a, b)` for selected cases.
- Random helpers such as `runif`, `rnorm`, and `set.seed`.
- Basic named vectors: construction with names, `names(v)`, `unname(v)`, named printing, positional indexing, literal-name indexing, and name-preserving printed arithmetic.
- Selected data-frame and file-reading patterns such as `read.table(..., header = TRUE)` into numeric matrices.
- Statistical distributions and tests such as normal/exponential/gamma/beta-related helpers, `t.test`, empirical CDF/KS-style helpers, and related summaries.
- Linear-model helpers including selected `lm`, prediction, coefficients, summaries, confidence intervals, and simple stepwise model selection support.
- Smoothing and time-series helpers such as moving filters, running medians, lowess/loess-style approximations, spline/decomposition helpers, `acf`/`pacf`-style routines, AR/ARMA/ARIMA-related subsets, and VAR/VARMA example support.
- Clustering and multivariate helpers such as distance matrices, hierarchical clustering/cutting, `kmeans`, covariance/correlation helpers, Cholesky/QR helpers, and selected mixture-model routines.

Unsupported or incomplete areas include general R objects, packages, data frames beyond narrow patterns, formulas beyond simple cases, S3/S4 dispatch, closures with general lexical scoping, environments, complex string processing, and arbitrary list manipulation.  Some translated statistical routines are intentionally approximate rather than bit-for-bit implementations of R internals; use `--warn-approx` to surface known approximate translations.

## Runtime Modes

By default, compiled output is linked with `r.f90`:

```bat
python xr2f.py foo.r --compile
```

Use `--self-contained` to embed a pruned `r_mod` runtime in the generated Fortran file:

```bat
python xr2f.py foo.r --self-contained --compile
```

Use recycling diagnostics when porting R vector code:

```bat
python xr2f.py foo.r --run --recycle-warn
python xr2f.py foo.r --run --recycle-stop
```

## Batch Testing

Compile all `.R` files in a directory:

```bat
python xr2f_batch.py r_stat_examples\*.R --compile
```

Compile with a limit:

```bat
python xr2f_batch.py r_stat_examples\*.R --compile --limit 20
```

Run multiple jobs in parallel:

```bat
python xr2f_batch.py r_stat_examples\*.R --compile --jobs 4
```

Stop after the first failure and print useful failure detail:

```bat
python xr2f_batch.py r_stat_examples\*.R --compile --max-fail 1
```

Save batch output to a results file:

```bat
python xr2f_batch.py r_stat_examples\*.R --compile --tee
```

## Tests

The repository includes a focused pytest suite and R fixture scripts:

```bat
pytest -q
```

The tests compile supported R examples with `gfortran`, so `gfortran` must be on `PATH`.  Many tests use scripts from `r_examples/` and generated one-off R programs in temporary directories.

## Comparing R and Fortran Output

Run the original R and translated Fortran program:

```bat
python xr2f.py foo.r --run-both
```

Run both and compare normalized output:

```bat
python xr2f.py foo.r --run-diff --normalize-num-output
```

Differences can be legitimate when the R script uses random numbers, platform-dependent formatting, or unsupported R semantics.  For deterministic numerical scripts, `--run-diff` is the preferred regression check.

## Project Status

This project is experimental and test-driven.  The practical strategy is to add support for real scripts one feature at a time while checking that existing translated scripts still compile and run.

The current implementation is broader than a syntax translator: it includes many Fortran implementations of base-R-style statistical operations used by the example corpus.  Coverage is still selective and pragmatic.  The translator favors real regression examples over full language completeness.

Good bug reports include:

- The smallest R program that fails.
- The exact `python xr2f.py ...` command.
- The generated Fortran compile or runtime error.
- Whether the original R script runs with `Rscript`.
