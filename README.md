# R-to-Fortran

`xr2f.py` is an experimental source-to-source transpiler from a practical subset of R to modern Fortran, written using Codex.  The goal is to translate numeric, array-oriented, statistical, time-series, optimization, and file-oriented R scripts into readable Fortran that can be compiled with `gfortran` or `ifx`, or run interactively through the `ofort` Fortran interpreter from the REPL.

This is not a complete R implementation.  It is useful for scripts that mostly use base R syntax, arrays, loops, vector operations, matrix algebra, static data structures, and a growing subset of base R statistical and filesystem workflows.  The project includes substantial Fortran runtime support for common statistics, distributions, smoothing, linear models, time-series helpers, clustering, tests, random-number generation, formatted printing, and file/directory I/O patterns used by the example corpus.

The current workflow is test-driven and pragmatic: translate supported R code, compile it, compare it with R where useful, reduce failures to small reproducers, and reuse generated or hand-edited Fortran modules when that is the better engineering path.  The project can also be used as an LLM-assisted translation toolkit: `xr2f.py` can emit partial modules, structured translation reports, and runtime helpers that make generated Fortran easier for a human or LLM to inspect and repair.

## Why Fortran?

Fortran is a practical compilation target for numerical R code.  It is a fast, standardized language with mature compilers available on major platforms.  Like R, it has first-class multidimensional arrays, array operations, array sections, and 1-based indexing, so many numerical and statistical kernels have a natural Fortran shape rather than needing to be rewritten around zero-based scalar loops.

Fortran also has a long history in statistics and numerical computing.  Many statistical algorithms have reference or production implementations in Fortran, parts of R itself are written in Fortran, and R has an established interface for calling compiled Fortran routines.  Translating suitable R scripts to readable Fortran can therefore produce code that is fast, portable, and close to an ecosystem that R already knows how to call.

As an example of the possible speedup, `r_examples\xgarch_dcc.r` fits univariate GARCH/NAGARCH models and a multivariate DCC-GARCH model in base R.  In one `--time-both` run where the R and Fortran results matched, the R run took 19.61 seconds and the generated Fortran run took 1.14 seconds.  Timings are workload- and machine-dependent, but this illustrates the kind of numerical script where translation can pay off.

For a practical side-by-side overview of R syntax and modern Fortran equivalents, see [R To Fortran Syntax Guide](r_to_fortran_syntax_guide.md).  It covers control flow, functions, array indexing, matrix algebra, logical indexing, printing helpers, and common R-to-Fortran pitfalls such as `x[1:n-1]`.

## Relationship To quickr

[`quickr`](https://github.com/t-kalinowski/quickr) is another R-to-Fortran project.  It is an R package focused on accelerating individual R functions from inside R: the user calls `quick()`, supplies explicit `declare(type(...))` metadata for function arguments, and gets back an accelerated R-callable function.  Its documented target is high-performance numerical kernels with atomic vectors, matrices, and arrays.

`xr2f.py` is a standalone script-oriented transpiler.  It aims to emit readable Fortran source for whole scripts or selected functions, compile and run that source from the command line, and compare output with R when useful.  It relies more heavily on inference and runtime support, which makes it useful for script, example-corpus, and standalone Fortran workflows, but also means it supports a selective and experimental subset of R rather than all dynamic R code.

The two projects are complementary.  `quickr` is a good fit when the desired result is an accelerated function that remains in an R workflow.  `xr2f.py` is a good fit when the desired result is inspectable Fortran source or a standalone Fortran executable.  The `--annotate-r-args` option in `xr2f.py` emits `declare(type(...))` argument declarations in R source form, which can be used as a starting point for the declarations required by `quickr`.

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

Emit reusable Fortran module and main program files separately:

```bat
python xr2f.py r_examples\xfunc.r --split-module --compile
```

Use an existing Fortran module that provides procedures compatible with R functions in the input:

```bat
python xr2f.py foo.r custom_helpers.f90 --compile
```

Write source-analysis artifacts without changing the original R file:

```bat
python xr2f.py foo.r --annotate-r
python xr2f.py foo.r --integerize-r
python xr2f.py foo.r --obfuscate --check-obfuscated-r
```

Write LLM-friendly translation reports as JSON and Markdown:

```bat
python xr2f.py foo.r --report
python xr2f.py foo.r --report foo_report.json --compile
```

Use strict R numeric-literal semantics as a lint mode:

```bat
python xr2f.py foo.r --run-both --r-numeric-literals
```

By default, `xr2f.py` treats many bare integer-looking literals pragmatically when they are used as lengths, indices, loop bounds, or integer arguments.  With `--r-numeric-literals`, bare literals such as `100` are treated as R numeric/double values when they are not used in an integer context; explicit `100L` remains integer.  Differences between default mode and `--r-numeric-literals` often identify R code that should be clarified with `L`, `as.integer()`, or an explicit numeric conversion.

Control wrapping of coalesced generated Fortran declarations:

```bat
python xr2f.py foo.r --compile --decl-line-length 100
```

`--decl-line-length N` controls only merged declaration lines, not general Fortran wrapping.  The default is 100, and accepted values are 60 through 132.

Start the interactive REPL:

```bat
python xr2f_repl.py
python xr2f_repl.py r_examples\xrunif.r
```

## Package Translation

`xr2f_package.py` provides an initial package-level workflow for dependency-free,
pure-R packages.  The package must contain `DESCRIPTION`, `NAMESPACE`, and `R/`;
required package dependencies, `LinkingTo`, compiled sources, and unsupported
`NAMESPACE` directives are rejected explicitly.

Translate a package into one Fortran module and a JSON coverage report:

```bat
python xr2f_package.py C:\rcode\public_domain\github\combinat
```

Compile the generated module, require every declared export, or embed the runtime:

```bat
python xr2f_package.py C:\rcode\public_domain\github\combinat --compile
python xr2f_package.py C:\rcode\public_domain\github\combinat --require-all-exports
python xr2f_package.py C:\rcode\public_domain\github\combinat --self-contained
```

Output defaults to `<package>\xr2f`.  Package mode may translate only part of a
package unless `--require-all-exports` is used; inspect the generated
`*_package_report.json` for translated, skipped, and missing exports.

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
- The helper runtime in `src/r_mod.f90` is used by default for R-like helper functions such as `rnorm_vec`, `sd`, `quantile`, matrix printing, and vector recycling.
- [fpm](https://fpm.fortran-lang.org/) is optional and can build, test, or consume the runtime as a standalone Fortran package.

## Files

- `xr2f.py`: main R-to-Fortran transpiler.
- `xr2f_package.py`: first-version translator for dependency-free, pure-R packages.  It combines package R sources, validates package metadata, invokes partial translation, and writes a coverage report.
- `xr2f_repl.py`: interactive R-to-Fortran session runner.  It can load an R file, accept more R statements, run the generated Fortran, run R, compare both, benchmark compiler choices, and run generated Fortran through `ofort`.
- `xr2f_batch.py`: batch runner for many R files, globs, directories, or `@list` files.
- `compare_xr2f_batch_results.py`: compares two timestamped batch-result files and lists scripts whose outcomes changed, optionally restricting output to regressions.
- `xr_obfuscate.py`: standalone batch obfuscator for R sources.  It renames user-defined functions and variables, can preserve directory layout under an output directory, can run the generated R with `Rscript`, and can continue through failures with a quiet summary mode.
- `xr2f_reduce.py`: reducer for R scripts that trigger a reproducible `xr2f.py` Fortran compile failure.  It can infer the first compile-error signature, reduce while preserving that signature, optionally check R validity, and backtrack to the smallest reduced R file that still runs.
- `src/r_mod.f90`: Fortran runtime and standalone fpm library implementing R-like vector, matrix, statistics, distribution, model, smoothing, time-series, clustering, hypothesis-test, optimization, string, filesystem, and file-I/O helpers.
- `fpm.toml`, `example/`, `test/`: package metadata, a direct-use Fortran example, and a small standalone runtime test.
- `xr2f_runtime_api.md`: curated guide to the stable `r_mod` helpers for generated Fortran, direct Fortran use, manual repairs, and LLM-assisted translation.
- `r2f_llm_runtime.f90`: optional standalone helper module for LLM/manual translations.  It provides named numeric matrices, printable numeric tables, model summaries, linear-model result records, finite/NA-aware statistics, token parsers, index helpers, string conversion helpers, and cumulative-vector helpers without making normal `xr2f.py` output depend on those containers.
- `fortran_scan.py`, `fortran_post.py`, `xunused.py`: Fortran scanning and postprocessing helpers used by the transpiler.
- `xr2p.py`, `xp2f.py`, `xr2r.py`: alternate and normalization pipelines used by selected modes such as `--via-python` and `--via-core-r`.
- `compare_project_files.py`: helper for comparing selected source files against another checkout.
- `tests/`: pytest tests for the standalone command-line tools.
- `r_examples/`: small R scripts used as examples and regression inputs.  These include both R syntax probes and statistical algorithm examples.
- `r_stat_examples/`: numbered statistical examples, including data-reading examples and base-R statistical workflows.

Generated files normally use the suffix `_r.f90`, for example `foo.r` becomes `foo_r.f90`.
With `--split-module`, `foo_r_mod.f90` contains the generated module and `foo_r.f90` contains the main program.

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
- Higher-order/vectorized helpers for selected static cases: `sapply`, `vapply`, `mapply`, `tapply`, `ave`, `aggregate`, `by`, `Filter`, `Find`, `Position`, `Negate`, and simple `Vectorize` aliases.
- Ordering and ranking helpers such as `sort`, `order`, and `rank` for selected vectors.
- Random helpers such as `runif`, `rnorm`, and `set.seed`.
- Optional use of R's RNG through an R-linked shim with `--r-rng`.
- Printing helpers including `print`, `show`, selected `cat`, `sprintf` with scalar literal formats, `round(..., digits=...)`, compact vector printing, aligned one-row named-vector printing for table-row matrix sections, matrix/table printing, `mode`, and `hist` result printing.
- Basic named vectors: construction with names, `names(v)`, `unname(v)`, named printing, positional indexing, literal-name indexing, and name-preserving printed arithmetic.
- Static R lists with fixed fields for selected cases.  Named fields become Fortran derived-type components; unnamed fields use generated component names `item1`, `item2`, and so on.  Scalar, vector/array, character, logical, and nested static-list components are supported for common `$`, `[[...]]`, indexing, assignment, and printing patterns.
- Homogeneous positional numeric lists such as `list(c(...), c(...))` or `list(matrix(...), matrix(...))` are kept as numerical array/list-of-matrix structures where the numerical examples expect array semantics.
- Grouped operations include selected `tapply`, `ave`, `aggregate`, and `by` forms.  Current support is intentionally first-tier: one or more static grouping vectors for `ave`, one named grouping vector for `aggregate`, and one grouping vector for `by`; supported summaries include common reducers such as `mean`, `sum`, `length`, `min`, `max`, plus `colMeans`/`colSums` for selected matrix-by-group cases.
- Static environment patterns using `new.env()` and `eapply()` are supported when bindings and callbacks are known at translation time.  General environment mutation remains out of scope.
- Static S3/S4-style examples are supported for narrow, compile-time-known method dispatch patterns.  General object systems and dynamic dispatch are still out of scope.
- Selected data-frame and file-reading patterns such as `read.table(..., header = TRUE)`, CSV matrix/header readers, `scan`, and simple numeric file writes.
- Filesystem helpers including selected `getwd`, `tempfile`, `file.path`, `file.exists`, `file.create`, `file.remove`, `file.info`, `dir.exists`, `dir.create`, `dir`/`list.files`, and `ls`/`ls.str` static introspection patterns.
- Character helpers including selected `nchar`, `paste0`, `strsplit`, `toupper`, `tolower`, `casefold`, `trimws`, `chartr`, and fixed-string replacement patterns.
- User-defined binary operators of the form `"%op%" <- function(a, b) ...` for simple static definitions.
- Statistical distributions and tests such as normal/exponential/gamma/beta-related helpers, `t.test`, empirical CDF/KS-style helpers, and related summaries.
- Linear-model helpers including selected `lm`, prediction, coefficients, summaries, confidence intervals, and simple stepwise model selection support.
- Optimization helpers including selected `optim`, `nlm`, and `constrOptim` patterns.  `optim` lowering includes runtime helpers for BFGS, Nelder-Mead, CG, SANN, and selected L-BFGS-B-like bounded workflows.  `constrOptim` support is for static linear-constraint cases and uses a log-barrier outer loop with BFGS/Nelder-Mead inner solves.
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

Unsupported or incomplete areas include packages outside the dependency-free pure-R package-mode subset, data frames beyond narrow patterns, formulas beyond simple cases, closures with general lexical scoping, arbitrary S3/S4 dispatch, complex regular-expression behavior, dynamic environments, and dynamic or arbitrary list manipulation where field sets or field kinds change at runtime.  Dynamic environment mutation features such as `assign()` and superassignment `<<-` are intentionally rejected rather than translated incorrectly.  General `get()` is supported only for feasible static name lookups.  Some translated statistical routines are intentionally approximate rather than bit-for-bit implementations of R internals; use `--warn-approx` to surface known approximate translations.

## Runtime Modes

By default, compiled output is linked with `src/r_mod.f90`:

```bat
python xr2f.py foo.r --compile
```

Use `--self-contained` to embed a pruned `r_mod` runtime in the generated Fortran file:

```bat
python xr2f.py foo.r --self-contained --compile
```

Use `--split-module` when the translated R file defines procedures you want to reuse:

```bat
python xr2f.py foo.r --split-module --compile
```

The generated module is written to `<output-stem>_mod.f90`, and the generated main program remains at the requested output path.  Generated modules are emitted `private` by default, with explicit `public :: ...` lines for procedures, result types, parameters, and module variables that the generated program must initialize or use.  This makes the module boundary closer to normal reusable Fortran while keeping the generated main program buildable.

You can also pass one or more `.f90` helper modules as positional arguments:

```bat
python xr2f.py foo.r hand_tuned_stats.f90 --compile
```

These files must contain modules only.  Source files with a main program or top-level procedures outside a module are rejected.  If a helper module exports a procedure with a compatible Fortran name for an R function, `xr2f.py` can use that module procedure instead of emitting a translated copy of the R function.  This is useful when a translated procedure has been hand-optimized or when several R scripts should share an existing Fortran implementation.

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

## Standalone Fortran Runtime

The same `r_mod` implementation is an fpm library, so Fortran programs can use its numerical and statistical routines without translating R source.  The public API is documented in [`xr2f_runtime_api.md`](xr2f_runtime_api.md).

```text
fpm build
fpm test
fpm run --example descriptive_statistics
fpm run --example distributions
fpm run --example linear_algebra
fpm run --example hypothesis_tests
fpm run --example linear_models
fpm run --example time_series
fpm run --example data_and_strings
```

The examples progress from descriptive statistics to probability distributions,
linear algebra, hypothesis tests, linear models, time-series helpers, and numeric
data frames with string utilities.  Each is self-contained and uses deterministic
inputs; the random-number example sets an explicit seed.

As an fpm dependency, add the repository to the consuming package's manifest and import only the routines needed by the program:

```toml
[dependencies]
xr2f_runtime = { git = "https://github.com/Beliavsky/R-to-Fortran.git" }
```

```fortran
use r_mod, only: dp, dnorm, sd
```

The runtime currently remains in this repository so generated code and direct Fortran users exercise the same implementation.  Semantic compatibility targets practical R behavior; some advanced statistical routines are approximations rather than bit-for-bit reproductions of R internals.

For direct use of the runtime outside fully generated programs, see [`xr2f_runtime_api.md`](xr2f_runtime_api.md).  It documents the stable `r_mod` helpers for vectors, matrices, printing, distributions, optimization, integration, clustering, models, data frames, files, and common LLM repair patterns.

## Source Annotation, Integerization, Obfuscation, and Reduction

`xr2f.py` includes source-level tools that are useful when improving the translator or preparing R code for translation.

Write an annotated R copy with inferred `declare(type(...))` hints:

```bat
python xr2f.py foo.r --annotate-r
python xr2f.py foo.r --annotate-r-args
```

`--annotate-r-args` emits only function-argument declarations.  This is useful when preparing a function for [`quickr`](https://github.com/t-kalinowski/quickr), which requires explicit `declare(type(...))` declarations for function arguments.

Write an R copy where safe numeric literal assignments used in integer contexts are rewritten with `L` suffixes:

```bat
python xr2f.py foo.r --integerize-r
```

Obfuscate user-defined R function and variable names before translation.  This is mainly a regression tool: if a translation only works because the source variable was named `weights`, `print_portfolio`, or another special name, obfuscation should expose that bug.

```bat
python xr2f.py foo.r --obfuscate
python xr2f.py foo.r --obfuscate --check-obfuscated-r
python xr2f.py foo.r --obfuscate-r foo_obf.r
```

For batches of R files, use `xr_obfuscate.py`:

```bat
python xr_obfuscate.py r_examples r_stat_examples --recursive --out-dir obfuscated_r --check --keep-going --summary --quiet
```

When a script fails during Fortran compilation, `xr2f_reduce.py` can build a smaller R reproducer that preserves the same compile-error signature:

```bat
python xr2f_reduce.py foo.r
python xr2f_reduce.py foo.r --check-r-final --backtrack
```

If `--match` is omitted, the reducer first runs `xr2f.py` and infers a signature from the first compile error.  `--check-r` validates every reduced candidate with `Rscript`; `--check-r-final` validates only the final reduced output; `--backtrack` emits the smallest earlier reduction that still runs under R if the smallest compile-error reproducer is not valid R.  Reduced files include a one-line banner naming the original source file.

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

## LLM-Assisted Translation

`xr2f.py` can be useful even when a script does not fully translate on its own.  A practical LLM-assisted workflow is:

1. Run `xr2f.py foo.r --llm-bundle foo_bundle --compile` or `--run-both`.
2. Give the LLM `foo_bundle`, which contains the R script, full generated Fortran, partial module output, translation reports, directives, runtime API guide, compile notes, and a prompt.
3. Ask the LLM to repair the generated Fortran locally rather than rewrite the whole script from scratch.
4. Validate with `--run-both`, `--run-diff`, or a separate numerical comparison harness.

`--llm-bundle DIR` writes a fixed-layout handoff directory:

- `<stem>_r.f90`: the normal full-program translation.
- `<stem>_partial.f90`: best-effort module translation of reusable R functions.
- `<stem>_report.json` and `<stem>_report.md`: structured translation reports.
- `<stem>_directed.r`: R source with suggested `# xr2f:` directive comments.
- `xr2f_runtime_api.md`: curated guide to stable `r_mod` helpers.
- `compile_log.txt` and `llm_prompt.md`: build/run diagnostics and LLM repair instructions.  When `--compile`, `--run`, or `--run-both` is used, `compile_log.txt` records the exact build command, compiler output, run output, run-diff status when available, and first-error/source-line hints for compile failures.
- `fix_targets.md`: ranked repair checklist with the first compile target, runtime/diff status, high-risk translation regions, concrete directive candidates for recognizable compiler errors, declaration context, and validation plan.

`--report` writes a JSON report and a Markdown companion report.  The report is intended to make translation failures easier to localize by recording source metadata, inferred types, generated symbols, warnings, approximations, and build/run status where available.

`xr2f_repair.py` can run a conservative automated repair pass over an LLM bundle.  It reads directive candidates from `fix_targets.md`, creates trial copies of the R source, reruns `xr2f.py`, and writes `repair_log.json` / `repair_log.md` without modifying the original source:

```bat
python xr2f_repair.py --bundle foo_bundle --mode compile
```

For larger scripts, start with:

```bat
python xr2f.py foo.r --partial --compile --report
python xr2f.py foo.r --partial-main --compile --report
```

Use [`xr2f_runtime_api.md`](xr2f_runtime_api.md) as the reference for `r_mod` helpers an LLM should prefer when repairing or extending generated Fortran.  The runtime guide is curated rather than generated: it documents recommended public helpers, result types, examples, approximation notes, and known gaps.

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

Simple assignments are evaluated once in a session workspace, so a bare symbol or expression
returns the current value immediately without transpiling and compiling the session. Explicit
`run`, `time`, and `run-both` commands still translate the accumulated R source to Fortran.
Explicit output calls such as `print(...)`, `cat(...)`, `message(...)`, and `warning(...)` also
execute immediately. They remain in the accumulated source so an explicit `run` transpiles and
reproduces their output.
Sessions containing multiline control constructs use the transpile-and-run path for bare
expressions because arbitrary compiled Fortran state cannot be kept between commands.

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

For `ofort`, translation happens once and the generated Fortran source is run directly by `ofort`.  If the generated program uses `r_mod`, the REPL includes `src/r_mod.f90` in the `ofort` command.

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

Compare two saved result files.  Their `Started` timestamps determine which run
is older, regardless of argument order:

```bat
python compare_xr2f_batch_results.py newer_results.txt older_results.txt
python compare_xr2f_batch_results.py newer_results.txt older_results.txt --worse-only
```

For obfuscation-specific batch testing, prefer `xr_obfuscate.py` rather than `xr2f_batch.py`, because it understands obfuscated output paths and optional R validity checks.

## Tests

The repository includes a focused pytest suite and R fixture scripts:

```bat
pytest -q
```

The tests compile supported R examples with `gfortran`, so `gfortran` must be on `PATH`.  Many tests use scripts from `r_examples/`, `r_stat_examples/`, local root-level regression scripts, and generated one-off R programs in temporary directories.  The exact test count changes frequently as new reproducers are added; recent local full-corpus runs have been passing with `XR2F_FULL_EXAMPLES=1`.

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

`--run-diff` compares normalized lines rather than raw text.  It tolerates common R-vs-Fortran formatting differences such as numeric spellings, logical spellings, quoted character tokens, stripped R vector indices, and different lengths of dash-only separator lines.

For prettier comparisons:

```bat
python xr2f.py foo.r --run-both --pretty --round-both 4 --wrap-out 80 --trim-zd
```

Differences can be legitimate when the R script uses random numbers, platform-dependent formatting, or unsupported R semantics.  For deterministic numerical scripts, `--run-diff` is the preferred regression check.  Use `--run-diff-all` when the first mismatch is not enough context.

## Project Status

This project is experimental and test-driven.  The practical strategy is to add support for real scripts one feature at a time while checking that existing translated scripts still compile and run.  Most new behavior starts from a small reproducer script and then becomes either a pytest regression test or part of the full example corpus.

The current implementation is broader than a syntax translator: it includes many Fortran implementations of base-R-style statistical operations, object containers, printing helpers, optimization routines, table-formatting helpers, and filesystem helpers used by the example corpus.  It can emit reusable modules, use external Fortran modules, create structured translation reports, create obfuscated R copies to detect name-specific lowering, integerize selected R literals, run strict numeric-literal linting with `--r-numeric-literals`, control declaration wrapping with `--decl-line-length`, and reduce compile failures to reproducers.  The `r_mod` runtime is documented in [`xr2f_runtime_api.md`](xr2f_runtime_api.md), packaged for direct fpm use, and available as a starting point for manual or LLM-assisted translation.

Coverage is still selective and pragmatic.  The translator favors real regression examples over full language completeness, and it generally prefers explicit unsupported-feature errors over silently generating misleading Fortran.  Some final post-codegen repairs remain intentionally pragmatic; `--no-post-repairs` and `--special-repairs` exist to separate generic translator behavior from corpus-specific compatibility repairs.

Good bug reports include:

- The smallest R program that fails.
- The exact `python xr2f.py ...` command.
- The generated Fortran compile or runtime error.
- Whether the original R script runs with `Rscript`.
