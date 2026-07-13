# R To Fortran Syntax Guide

This guide summarizes common mappings from R syntax to modern Fortran.  It is not a full Fortran tutorial or a complete specification of `xr2f.py`, but it documents the style of Fortran the transpiler tries to emit and the places where R programmers should be careful.

Both R and Fortran are good at array-oriented numerical code, use 1-based indexing by default, and have concise whole-array operations.  The main difference is that R is dynamic while Fortran usually wants explicit, stable types, ranks, and procedure interfaces.

## Scalar Values

R values are dynamically typed:

```r
x <- 1.5
n <- 10L
flag <- TRUE
name <- "SPY"
```

Modern Fortran uses `implicit none` and explicit declarations:

```fortran
real(kind=dp) :: x
integer :: n
logical :: flag
character(len=:), allocatable :: name

x = 1.5_dp
n = 10
flag = .true.
name = "SPY"
```

When a value is a compile-time constant, generated Fortran may use a parameter:

```fortran
integer, parameter :: n = 10
real(kind=dp), parameter :: x = 1.5_dp
```

R's bare numeric literal `100` is a double in R, while `100L` is integer.  `xr2f.py` defaults to pragmatic integer inference in integer contexts, and `--r-numeric-literals` can be used as a stricter lint mode.

R numeric values are double precision.  Fortran default `real` is usually single precision, so generated code uses `real(kind=dp)` where `dp` is an alias for `real64`, and real literals use the `_dp` suffix:

```fortran
use, intrinsic :: iso_fortran_env, only: dp => real64
real(kind=dp) :: x
x = 1.5_dp
```

## Identifier Names

R names are case-sensitive and may contain periods:

```r
daily.return <- 0.01
ex.kurt <- 2.5
```

Fortran names are case-insensitive.  A standard Fortran name begins with a letter and contains only letters, digits, and underscores, so an embedded period is not allowed.  `xr2f.py` normally replaces each period with `_dot_`:

```fortran
daily_dot_return = 0.01_dp
ex_dot_kurt = 2.5_dp
```

Backtick-quoted R names may require additional sanitization.  The transpiler also disambiguates names when different R spellings would otherwise produce the same Fortran identifier.

## Complex Numbers

R writes the imaginary unit as `1i`:

```r
z <- 1 + 2i
w <- exp(1i * theta)
```

Fortran complex values are usually written with `cmplx(real_part, imag_part, kind=dp)` or with a named imaginary unit:

```fortran
complex(kind=dp) :: z, w
complex(kind=dp), parameter :: iu = cmplx(0.0_dp, 1.0_dp, kind=dp)

z = cmplx(1.0_dp, 2.0_dp, kind=dp)
w = exp(iu * theta)
```

R's `Re`, `Im`, `Conj`, `Mod`, and `Arg` correspond to Fortran intrinsics or simple wrappers such as `real`, `aimag`, `conjg`, `abs`, and `atan2`.

## Assignment

R commonly uses `<-`:

```r
x <- y + 1
```

Fortran uses `=` for assignment:

```fortran
x = y + 1
```

Fortran also uses `=` in declarations for initialization:

```fortran
integer, parameter :: n = 100
```

R permits chained assignment:

```r
a <- b <- expensive_call()
```

Fortran has no chained assignment.  The equivalent statements must preserve R's right-to-left order and evaluate the final expression only once:

```fortran
b = expensive_call()
a = b
```

`xr2f.py` lowers chained assignments internally, and `xnormalize_r.py` can make the same expansion in normalized R source.

## If Statements

R:

```r
if (x > 0) {
  y <- x
} else {
  y <- -x
}
```

Fortran:

```fortran
if (x > 0) then
   y = x
else
   y = -x
end if
```

Fortran logical constants are `.true.` and `.false.`, and logical operators are `.and.`, `.or.`, and `.not.`.

## Loops

R:

```r
for (i in 1:n) {
  x[i] <- i * i
}
```

Fortran:

```fortran
do i = 1, n
   x(i) = i * i
end do
```

R's `next` maps to Fortran `cycle`, and R's `break` maps to Fortran `exit`:

```fortran
do i = 1, n
   if (x(i) < 0) cycle
   if (x(i) == 0) exit
end do
```

Fortran also supports `do while`:

```fortran
do while (err > tol)
   ...
end do
```

R's `repeat` loop maps to an unbounded Fortran `do` with an explicit `exit`:

```r
repeat {
  x <- update(x)
  if (converged(x)) break
}
```

```fortran
do
   x = update(x)
   if (converged(x)) exit
end do
```

## Functions And Subroutines

R functions can return any object:

```r
square <- function(x) {
  x * x
}
```

Fortran functions have declared argument and result types:

```fortran
pure function square(x) result(y)
real(kind=dp), intent(in) :: x
real(kind=dp) :: y

y = x * x
end function square
```

Fortran subroutines are used for procedures that primarily act through arguments or I/O:

```fortran
subroutine center(x, mu)
real(kind=dp), intent(inout) :: x(:)
real(kind=dp), intent(in) :: mu

x = x - mu
end subroutine center
```

Subroutines are also commonly used in hand-written Fortran to return multiple values that do not fit naturally in one array, by giving some dummy arguments `intent(out)` or `intent(inout)`.  `xr2f.py` usually keeps R functions as Fortran functions; when an R function returns a fixed list-like result, the generated function returns a derived type whose fields match the R result fields.

Because Fortran procedure interfaces are static, R functions that return different types or ranks on different paths are hard to translate safely.

## Optional Arguments

R function arguments can have defaults:

```r
scale_shift <- function(x, scale = 1.0, shift = 0.0) {
  scale * x + shift
}

y <- scale_shift(x, shift = 2.0)
```

Fortran supports optional dummy arguments with the `optional` attribute and tests them with `present()`:

```fortran
function scale_shift(x, scale, shift) result(y)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: scale
real(kind=dp), intent(in), optional :: shift
real(kind=dp), allocatable :: y(:)
real(kind=dp) :: scale_def, shift_def

scale_def = 1.0_dp
if (present(scale)) scale_def = scale
shift_def = 0.0_dp
if (present(shift)) shift_def = shift

y = scale_def * x + shift_def
end function scale_shift
```

`xr2f.py` generally lowers R defaults this way: each optional argument gets a local default variable such as `scale_def`, and calls can pass only the arguments that are present in the R call.

## Vectors, Matrices, And Arrays

R uses square brackets for subscripting:

```r
x[2]
m[2, ]
m[, 2]
```

Fortran uses parentheses:

```fortran
x(2)
m(2, :)
m(:, 2)
```

Important differences:

- R and normal Fortran arrays both start at 1, but Fortran can define other lower bounds.
- R uses `x[2, ]` and `x[, 2]`; Fortran uses `x(2, :)` and `x(:, 2)`.
- R negative subscripts mean exclusion, such as `x[-1]`; Fortran has no built-in negative-subscript exclusion syntax.
- R permits logical indexing, such as `x[x > 0]`; Fortran normally uses masks with `pack`, `where`, or helper procedures.
- R drops dimensions by default in many cases; Fortran array sections have explicit rank.

## The `1:n-1` Pitfall

In R, operator precedence means:

```r
x[1:n-1]
```

is parsed as:

```r
x[(1:n) - 1]
```

This includes index 0 when `n >= 1`, which is usually unintended.  Most code should write:

```r
x[1:(n - 1)]
```

The Fortran equivalent is:

```fortran
x(1:n - 1)
```

Fortran section bounds are not R expressions inside brackets, so this means the range from `1` to `n - 1`.

## Array Constructors

R:

```r
x <- c(1.0, 2.0, 3.0)
```

Fortran:

```fortran
real(kind=dp), parameter :: x(3) = [1.0_dp, 2.0_dp, 3.0_dp]
```

R vectors coerce mixed elements to a common type:

```r
x <- c(3L, 4.5)          # numeric vector
s <- c("boy", "girl")    # character vector
```

Fortran array constructors require compatible element types.  Character constructor elements must also have a consistent length, either by padding shorter strings or by declaring the constructor length:

```fortran
real(kind=dp), parameter :: x(2) = [3.0_dp, 4.5_dp]
character(len=4), parameter :: s(2) = [character(len=4) :: "boy", "girl"]
```

R matrices are column-major, and Fortran arrays are also column-major:

```r
m <- matrix(c(1, 2, 3, 4), nrow = 2)
```

Fortran:

```fortran
integer :: m(2, 2)
m = reshape([1, 2, 3, 4], [2, 2])
```

## Array Arithmetic

R:

```r
y <- a + b
z <- exp(x) / sum(exp(x))
```

Fortran:

```fortran
y = a + b
z = exp(x) / sum(exp(x))
```

Both languages support elementwise arithmetic on conforming arrays.  A key difference is recycling: R may recycle shorter vectors, while Fortran normally requires conforming shapes.  `xr2f.py` supports selected recycling patterns through generated code or runtime helpers, but explicit matching shapes are easier to translate and read.

## Reductions And Dimensions

R:

```r
sum(x)
mean(x)
rowSums(m)
colSums(m)
```

Fortran:

```fortran
sum(x)
sum(m, dim=2)  ! row sums: one result per row
sum(m, dim=1)  ! column sums: one result per column
```

The `dim` number is the dimension being reduced.  For a matrix `m(row, col)`, `sum(m, dim=1)` collapses rows and returns one value per column.

Fortran has intrinsic reducers such as

```fortran
sum(x)
product(x)
minval(x)
maxval(x)
any(mask)
all(mask)
count(mask)
```

## Matrix Algebra

R:

```r
z <- x %*% beta
xtx <- crossprod(x)
tx <- t(x)
```

Fortran:

```fortran
z = matmul(x, beta)
xtx = matmul(transpose(x), x)
tx = transpose(x)
```

Common mappings:

- R `%*%` maps to Fortran `matmul`.
- R `t(x)` maps to `transpose(x)`.
- R `crossprod(x, y)` maps to `matmul(transpose(x), y)`.
- R `tcrossprod(x, y)` maps to `matmul(x, transpose(y))`.
- R `chol`, `solve`, `qr`, and related helpers are provided through `r.f90` for selected cases.

## Logical Indexing And Masks

R:

```r
positive <- x[x > 0]
x[x < 0] <- 0
```

Fortran:

```fortran
positive = pack(x, x > 0)
where (x < 0)
   x = 0
end where
```

For matrix row filtering, generated Fortran may use runtime helpers such as `r_matrix_row_filter`.

## Missing Values

R has `NA`, `NaN`, `Inf`, and `-Inf` as common data values.

For real values, Fortran can use IEEE NaN and infinity support:

```fortran
use, intrinsic :: ieee_arithmetic

x = ieee_value(0.0_dp, ieee_quiet_nan)
if (ieee_is_nan(x)) then
   ...
end if
```

Integer, logical, and character `NA` values are less direct in Fortran.  `xr2f.py` supports selected NA patterns, but code that relies heavily on mixed missing-value semantics may need manual treatment.

## Lists And Derived Types

R lists can be dynamic:

```r
fit <- list(mu = 0.1, sigma = 0.2)
fit$aic <- 123.4
```

Fortran derived types have fixed fields:

```fortran
type :: fit_result_t
   real(kind=dp) :: mu
   real(kind=dp) :: sigma
   real(kind=dp) :: aic
end type fit_result_t

type(fit_result_t) :: fit
fit%mu = 0.1_dp
fit%sigma = 0.2_dp
fit%aic = 123.4_dp
```

This maps well when the list field set is static and field types are stable.  It does not map cleanly when fields are created dynamically or change kind/rank over time.

## Data Frames

R data frames are heterogeneous column containers.  Fortran can represent selected data-frame-like structures as:

- separate arrays, one per column;
- a derived type with allocatable fields;
- a numeric matrix when all relevant columns are numeric.

`xr2f.py` supports selected data-frame and file-reading patterns, but general data-frame programming is still outside the reliable subset.

## Printing

R printing is highly dynamic.  Fortran output is explicit:

```fortran
write(*,"(*(g0,1x))") x
```

The runtime module `r.f90` provides helpers used by generated code, including:

- `print_real_vector`
- `print_integer_vector`
- `print_char_vector`
- `print_named_real_vector`
- `print_named_real_row`
- `print_matrix`
- `print_table2`

`print_named_real_vector` prints a compact named vector.  `print_named_real_row` prints a 1D real array as an aligned one-row table, useful when a matrix row represents one observation with column labels.  `print_table2` prints a 2D matrix with row and column labels.

## Common Pitfalls For R Programmers

- `x[1:n-1]` usually should be `x[1:(n - 1)]`.
- R `100` is numeric/double, while `100L` is integer.
- R recycles vectors; Fortran generally requires conforming shapes.
- R can change a variable's type or rank; Fortran declarations are fixed in a scope.
- R functions can return different object shapes; Fortran functions need one declared result shape/kind.
- R lists and data frames are dynamic; Fortran derived types are static.
- R's `[` can drop dimensions; Fortran array sections have explicit rank.
- R logical indexing is built in; Fortran usually uses `pack`, `where`, masks, or helper procedures.

## How This Relates To `xr2f.py`

`xr2f.py` automates many of these mappings for static, numerical R scripts.  It also adds runtime helpers in `r.f90` where base Fortran lacks a direct equivalent, such as R-like printing, named tables, selected statistical routines, distributions, optimization helpers, file readers, and list-like derived-type results.

The most reliable translations are scripts where:

- variable kinds and ranks are stable;
- vector and matrix shapes are explicit or easily inferred;
- list fields are fixed;
- dynamic dispatch, environments, package-specific objects, and arbitrary data-frame programming are avoided;
- R and Fortran outputs can be compared with `--run-both` or `--run-diff`.
