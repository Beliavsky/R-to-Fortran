# R-to-Fortran

`xr2f.py` is an experimental source-to-source transpiler from a practical subset of R to modern Fortran.  The goal is to translate simple numeric, array-oriented, and statistical R scripts into readable Fortran that can be compiled with `gfortran`.

This is not a complete R implementation.  It is useful for numeric scripts that mostly use base R arrays, loops, scalar functions, vector operations, and selected statistical helpers.

## Quick Start

Translate an R script:

```bat
python xr2f.py xrnorm_stats.r
```

Compile the generated Fortran:

```bat
python xr2f.py xrnorm_stats.r --compile
```

Compile and run the generated Fortran:

```bat
python xr2f.py xrnorm_stats.r --run
```

Run both the original R script and the generated Fortran:

```bat
python xr2f.py xrnorm_stats.r --run-both
```

Create a single self-contained Fortran file with the needed runtime support prepended:

```bat
python xr2f.py xrnorm_stats.r --self-contained --compile
```

## Example

Input R:

```r
x = rnorm(10^5)
print(mean(x))
print(sd(x))
```

Generated Fortran:

```fortran
program xrnorm_stats
use, intrinsic :: iso_fortran_env, only: dp => real64
use r_mod, only: rnorm_vec, sd
implicit none
real(kind=dp), allocatable :: x(:)

x = rnorm_vec(10**5)
write(*,"(g0)") sum(x)/real(size(x), kind=dp)
write(*,"(g0)") sd(x)
end program xrnorm_stats
```

## Requirements

- Python 3.11 or newer is recommended.
- `gfortran` is needed for `--compile`, `--run`, `--run-both`, and batch compile/run modes.
- `Rscript` is needed for `--run-both`, `--run-diff`, and `--time-both`.
- The helper runtime `r.f90` is used by default for R-like helper functions such as `rnorm_vec`, `sd`, `quantile`, matrix printing, and vector recycling.

## Files

- `xr2f.py`: main R-to-Fortran transpiler.
- `xr2f_batch.py`: batch runner for many R files, globs, directories, or `@list` files.
- `r.f90`: Fortran runtime helper module.
- `fortran_scan.py`, `fortran_post.py`: Fortran scanning and postprocessing helpers used by the transpiler.

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

Unsupported or incomplete areas include general R objects, packages, data frames beyond narrow patterns, formulas beyond simple cases, S3/S4 dispatch, closures with general lexical scoping, environments, complex string processing, and arbitrary list manipulation.

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
python xr2f_batch.py examples\*.R --compile
```

Compile with a limit:

```bat
python xr2f_batch.py examples\*.R --compile --limit 20
```

Run multiple jobs in parallel:

```bat
python xr2f_batch.py examples\*.R --compile --jobs 4
```

Stop after the first failure and print useful failure detail:

```bat
python xr2f_batch.py examples\*.R --compile --max-fail 1
```

Save batch output to a results file:

```bat
python xr2f_batch.py examples\*.R --compile --tee
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

Differences can be legitimate when the R script uses random numbers, platform-dependent formatting, or unsupported R semantics.  For deterministic numerical scripts, `--run-diff` is the preferred regression check.

## Project Status

This project is early-stage and test-driven.  The practical strategy is to add support for real scripts one feature at a time while checking that existing translated scripts still compile and run.

Good bug reports include:

- The smallest R program that fails.
- The exact `python xr2f.py ...` command.
- The generated Fortran compile or runtime error.
- Whether the original R script runs with `Rscript`.
