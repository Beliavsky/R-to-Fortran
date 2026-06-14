# R-to-Fortran

`xr2f.py` is an experimental source-to-source transpiler from a practical subset of R to modern Fortran.  The goal is to translate numeric, array-oriented, and statistical R scripts into readable Fortran that can be compiled with `gfortran`.

This is not a complete R implementation.  It is useful for scripts that mostly use base R syntax, arrays, loops, vector operations, matrix algebra, and a growing subset of base R statistical algorithms.  The project includes substantial Fortran runtime support for common statistics, distributions, smoothing, linear models, time-series helpers, clustering, tests, random-number generation, and file I/O patterns used by the example corpus.

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

Time translation, compilation, and execution:

```bat
python xr2f.py r_examples\xrunif.r --time
python xr2f.py r_examples\xrunif.r --time-both
```

Run repeated timing trials after one transpile/build:

```bat
python xr2f.py r_examples\xrunif.r --time --run-repeat 10
python xr2f.py r_examples\xrunif.r --time --run-repeat 10 --verbose-runs
```

Create a single self-contained Fortran file with the needed runtime support prepended:

```bat
python xr2f.py r_examples\xrunif.r --self-contained --compile
```

Start the interactive REPL:

```bat
python xr2f_repl.py
python xr2f_repl.py r_examples\xrunif.r
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
- Intel `ifx` is optional and can be selected with `--ifx` or from the REPL.
- `Rscript` is needed for `--run-both`, `--run-diff`, and `--time-both`.
- The helper runtime `r.f90` is used by default for R-like helper functions such as `rnorm_vec`, `sd`, `quantile`, matrix printing, and vector recycling.

## Files

- `xr2f.py`: main R-to-Fortran transpiler.
- `xr2f_repl.py`: interactive R-to-Fortran session runner.  It can load an R file, accept more R statements, run the generated Fortran, run R, compare both, and benchmark compiler choices.
- `xr2f_batch.py`: batch runner for many R files, globs, directories, or `@list` files.
- `r.f90`: Fortran runtime helper module implementing R-like vector, matrix, statistics, distribution, model, smoothing, time-series, clustering, hypothesis-test, optimization, and file-I/O helpers.
- `fortran_scan.py`, `fortran_post.py`, `xunused.py`: Fortran scanning and postprocessing helpers used by the transpiler.
- `compare_project_files.py`: helper for comparing selected source files against another checkout.
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
- Optional use of R's RNG through an R-linked shim with `--r-rng`.
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

Choose a compiler:

```bat
python xr2f.py foo.r --run --gfortran
python xr2f.py foo.r --run --ifx
python xr2f.py foo.r --time-both --gfortran --ifx
```

When both `--gfortran` and `--ifx` are supplied, `xr2f.py` runs the job once per compiler and prints a combined timing table.

Use the R RNG shim when matching R random streams matters:

```bat
python xr2f.py foo.r --r-rng --run-both
```

On Windows this requires an R installation with headers/libraries available to the C and Fortran compilers.  `xr2f.py` caches compiled runtime objects to reduce repeat compile time where possible.

## Interactive REPL

`xr2f_repl.py` starts an interactive session.  A positional R file loads into the session instead of running and exiting:

```bat
python xr2f_repl.py r_examples\xrunif.r
```

Common commands:

```text
run        run accumulated session through Fortran
time       run Fortran and show timing
run-r      run accumulated session with Rscript
time-r     run Rscript and show timing
run-both   run R and Fortran and compare
time-both  run R and Fortran with timing
fortran    show the last generated Fortran
list       list accumulated R lines
clear      reset the session
quit       exit
```

Bare expressions are evaluated but not accumulated:

```text
xr2f> x <- c(1, 2, 3)
xr2f> mean(x)
2
xr2f> sum(x)
6
```

Run commands accept an optional repeat count, `verbose`, and compiler specifications:

```text
xr2f> time 10
xr2f> time 10 verbose
xr2f> time-both 5 gfortran -O2 gfortran -O3
xr2f> time-both gfortran -O3 -march=native ifx /O2
```

For repeated timing runs, translation and compilation happen once.  The executable or R script is then run repeatedly.  Repeated timing reports mean and sample standard deviation for the run stage.

Use `--batch` for the old run-and-exit file behavior:

```bat
python xr2f_repl.py r_examples\xrunif.r --batch --mode time --repeat 10
```

## Fortran Escape Comments

For diagnostics or unsupported cases, R comments beginning with `#f` or `#fortran` inject raw Fortran at that point:

```r
x <- rnorm(1000)
#f print*, minval(x), maxval(x)
```

The payload is treated as raw Fortran.  Formatting and indentation may change, but semantic rewrites such as changing `print *` to `write(...)` are avoided.

Disable this behavior when a script has ordinary comments that should not be treated as Fortran escapes:

```bat
python xr2f.py foo.r --no-fortran-comments
python xr2f_repl.py foo.r --no-fortran-comments
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

For prettier comparisons:

```bat
python xr2f.py foo.r --run-both --pretty --round-both 4 --wrap-out 80 --trim-zd
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
