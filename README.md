# R-to-Fortran

`xr2f.py` is an experimental source-to-source transpiler from a practical subset of R to modern Fortran.  The goal is to translate numeric, array-oriented, statistical, and file-oriented R scripts into readable Fortran that can be compiled with `gfortran` or `ifx`, or run interactively through the `ofort` Fortran interpreter from the REPL.

This is not a complete R implementation.  It is useful for scripts that mostly use base R syntax, arrays, loops, vector operations, matrix algebra, static data structures, and a growing subset of base R statistical and filesystem workflows.  The project includes substantial Fortran runtime support for common statistics, distributions, smoothing, linear models, time-series helpers, clustering, tests, random-number generation, formatted printing, and file/directory I/O patterns used by the example corpus.

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
- `ofort` is optional and can be selected from `xr2f_repl.py` to run generated Fortran source directly without compiling an executable.
- `Rscript` is needed for `--run-both`, `--run-diff`, and `--time-both`.
- The helper runtime `r.f90` is used by default for R-like helper functions such as `rnorm_vec`, `sd`, `quantile`, matrix printing, and vector recycling.

## Files

- `xr2f.py`: main R-to-Fortran transpiler.
- `xr2f_repl.py`: interactive R-to-Fortran session runner.  It can load an R file, accept more R statements, run the generated Fortran, run R, compare both, benchmark compiler choices, and run generated Fortran through `ofort`.
- `xr2f_batch.py`: batch runner for many R files, globs, directories, or `@list` files.
- `r.f90`: Fortran runtime helper module implementing R-like vector, matrix, statistics, distribution, model, smoothing, time-series, clustering, hypothesis-test, optimization, string, filesystem, and file-I/O helpers.
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
- Basic indexing, negative indexing, logical indexing, selected matrix indexing forms, and selected row/column logical filters.
- Elementwise vector arithmetic with optional R-style recycling.
- Common math functions such as `abs`, `sqrt`, `log`, `exp`, trigonometric functions, rounding, and `sign`.
- Reductions and statistics such as `sum`, `prod`, `mean`, `sd`, `var`, `min`, `max`, `quantile`, `median`, `summary`, `cumsum`, `cumprod`, `cummax`, and `diff`.
- Matrix helpers such as `matrix`, `array`, `t`, `%*%`, `crossprod`, `tcrossprod`, `rowSums`, `colSums`, `det`, `kappa`, `diag`, `chol`, `forwardsolve`, `backsolve`, and `solve(a, b)` for selected cases.
- `apply(x, 1, f)` and `apply(x, 2, f)` for selected matrix/array cases, including common reducers and some user-defined scalar callbacks.
- Ordering and ranking helpers such as `sort`, `order`, and `rank` for selected vectors.
- Random helpers such as `runif`, `rnorm`, and `set.seed`.
- Optional use of R's RNG through an R-linked shim with `--r-rng`.
- Printing helpers including `print`, `show`, selected `cat`, `sprintf` with scalar literal formats, `round(..., digits=...)`, compact vector printing, matrix printing, `mode`, and `hist` result printing.
- Basic named vectors: construction with names, `names(v)`, `unname(v)`, named printing, positional indexing, literal-name indexing, and name-preserving printed arithmetic.
- Static R lists with fixed fields for selected cases.  Named fields become Fortran derived-type components; unnamed fields use generated component names `item1`, `item2`, and so on.  Scalar, vector/array, character, logical, and nested static-list components are supported for common `$`, `[[...]]`, indexing, assignment, and printing patterns.
- Homogeneous positional numeric lists such as `list(c(...), c(...))` or `list(matrix(...), matrix(...))` are kept as numerical array/list-of-matrix structures where the numerical examples expect array semantics.
- Static S3/S4-style examples are supported for narrow, compile-time-known method dispatch patterns.  General object systems and dynamic dispatch are still out of scope.
- Selected data-frame and file-reading patterns such as `read.table(..., header = TRUE)`, CSV matrix/header readers, `scan`, and simple numeric file writes.
- Filesystem helpers including selected `getwd`, `tempfile`, `file.path`, `file.exists`, `file.create`, `file.remove`, `file.info`, `dir.exists`, `dir.create`, `dir`/`list.files`, and `ls`/`ls.str` static introspection patterns.
- Character helpers including selected `nchar`, `paste0`, `strsplit`, `toupper`, `tolower`, `casefold`, `trimws`, `chartr`, and fixed-string replacement patterns.
- User-defined binary operators of the form `"%op%" <- function(a, b) ...` for simple static definitions.
- Statistical distributions and tests such as normal/exponential/gamma/beta-related helpers, `t.test`, empirical CDF/KS-style helpers, and related summaries.
- Linear-model helpers including selected `lm`, prediction, coefficients, summaries, confidence intervals, and simple stepwise model selection support.
- Smoothing and time-series helpers such as moving filters, running medians, lowess/loess-style approximations, spline/decomposition helpers, `acf`/`pacf`-style routines, AR/ARMA/ARIMA-related subsets, and VAR/VARMA example support.
- Clustering and multivariate helpers such as distance matrices, hierarchical clustering/cutting, `kmeans`, covariance/correlation helpers, Cholesky/QR helpers, and selected mixture-model routines.

### Static list support

Static list support is intended for list shapes that can be known at translation time:

```r
x <- list(a = 10.0, b = c(20.0, 30.0), tag = "fit")
y <- list(10.0, c(4.0, 9.0, 16.0), "abc")
print(x$b)
print(y[[2]][-1])
```

The generated Fortran uses static derived types for heterogeneous lists, so field names and field kinds must be stable.  Adding a field in all branches with the same inferred kind is allowed in selected cases, but unconditional dynamic field creation or changing a field's kind after construction is rejected with a transpile error.  General R list concatenation and fully dynamic list mutation are not complete R-compatible object semantics.

Unsupported or incomplete areas include packages, data frames beyond narrow patterns, formulas beyond simple cases, closures with general lexical scoping, environments, arbitrary S3/S4 dispatch, complex regular-expression behavior, and dynamic or arbitrary list manipulation where field sets or field kinds change at runtime.  Dynamic environment mutation features such as `assign()` and superassignment `<<-` are intentionally rejected rather than translated incorrectly.  General `get()` is supported only for feasible static name lookups.  Some translated statistical routines are intentionally approximate rather than bit-for-bit implementations of R internals; use `--warn-approx` to surface known approximate translations.

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

`ofort` is handled by `xr2f_repl.py`, because it interprets Fortran source directly rather than compiling and linking an executable:

```bat
python xr2f_repl.py foo.r --ofort
```

Use the R RNG shim when matching R random streams matters:

```bat
python xr2f.py foo.r --r-rng --run-both
```

On Windows this requires an R installation with headers/libraries available to the C and Fortran compilers.  `xr2f.py` caches compiled runtime objects to reduce repeat compile time where possible.

## Partial Translation

Large R scripts often mix translatable numerical functions with top-level workflow code, reporting, formulas, anonymous functions, package calls, or dynamic R features that are outside the supported subset.  For those files, `--partial` is the recommended workflow:

```bat
python xr2f.py foo.r --partial --compile
```

`--partial` translates the R functions it can into a single Fortran module, reports which functions were translated, and reports skipped functions with dependency or compile-error reasons.  This mode is useful when you want reusable Fortran procedures from a larger R file.  If you need an executable, write a small Fortran driver program manually and call the translated module procedures you intend to validate.

`--partial-main` is available as an experimental best-effort mode:

```bat
python xr2f.py foo.r --partial-main --compile
```

It tries to keep only top-level statements that do not depend on skipped functions and then reduces the generated program until it compiles.  This can be slow, and the resulting main program is a pruned approximation of the original script rather than a semantically complete translation.  Prefer `--partial` for normal partial-conversion work.

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
xr2f> time ofort
xr2f> time 5 ofort gfortran -O3 ifx /O2
```

For repeated timing runs, translation and compilation happen once.  The executable or R script is then run repeatedly.  Repeated timing reports mean and sample standard deviation for the run stage.

For `ofort`, translation happens once and the generated Fortran source is run directly by `ofort`.  If the generated program uses `r_mod`, the REPL includes `r.f90` in the `ofort` command.

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

Search subdirectories recursively:

```bat
python xr2f_batch.py art_of_R --recursive --compile
python xr2f_batch.py art_of_R -r --compile
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

Run the full example corpus by opting in:

```bat
set XR2F_FULL_EXAMPLES=1&& pytest -q
```

On machines with `pytest-xdist`, the same corpus can be run in parallel.  Runtime cache isolation is used for worker processes, but parallel failures should still be rechecked serially before treating them as deterministic regressions:

```bat
set XR2F_FULL_EXAMPLES=1&& pytest -q -n 4
```

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

This project is experimental and test-driven.  The practical strategy is to add support for real scripts one feature at a time while checking that existing translated scripts still compile and run.  Most new behavior starts from a small reproducer script and then becomes either a pytest regression test or part of the full example corpus.

The current implementation is broader than a syntax translator: it includes many Fortran implementations of base-R-style statistical operations, object containers, printing helpers, and filesystem helpers used by the example corpus.  Coverage is still selective and pragmatic.  The translator favors real regression examples over full language completeness, and it generally prefers explicit unsupported-feature errors over silently generating misleading Fortran.

Good bug reports include:

- The smallest R program that fails.
- The exact `python xr2f.py ...` command.
- The generated Fortran compile or runtime error.
- Whether the original R script runs with `Rscript`.
