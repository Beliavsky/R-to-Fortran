! helper functions for R-to-Fortran transpiler
module r_mod
use, intrinsic :: iso_fortran_env, only: real64, int64
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
   & ieee_is_finite
implicit none
private
public :: runif1, runif_vec, rnorm1, rnorm_vec, rnorm_mat, rbinom, random_choice2_prob, &
   & randint_range, sample_int, sample_int1, quantile, median, summary, dnorm, tail, cbind2, cbind, numeric, &
   & pmax, sd, r_sd, var, r_format_vec, colMeans, count_ws_tokens, &
   & read_real_vector, read_table_real_matrix, read_csv_real_matrix, &
   & write_table_real_matrix, lm_fit_t, optim_result_t, lm_fit_general, lm_predict_general, &
   & lm_coef, print_lm_summary, print_lm_coef_rstyle, cov, cor, r_seq_int, r_seq_len, &
   & r_seq_int_by, r_seq_int_length, r_seq_real_by, r_seq_real_length, &
   & r_rep_real, r_rep_int, r_array_real, r_array_int, r_array_char, matrix, &
   & r_matmul, r_add, r_sub, r_mul, r_div, print_matrix, &
   & print_matrix_rstyle, print_matrix_rstyle_named, print_real_scalar, &
   & print_real_vector, print_char_vector, &
   & print_named_real_vector, print_table1, print_table2, print_summary, set_print_int_like, &
   & set_print_int_like_tol, set_recycle_warn, set_recycle_stop, set_seed_int, &
   & kmeans_result_t, kmeans, max_col, tabulate, table2, prop_table, match, findInterval, &
   & cumsum, cumprod, diff, diag, toeplitz, chol, backsolve, sort, polyroot, &
   & nchar, is_na, r_typeof, r_character, order_real, rank_average, &
   & rank_first, det_real, eigen_sym_values, solve_real, &
   & nested_matrix_list_len, t_test_result_t, t_test, t_test_p_value, print_t_test, &
   & chisq_test_result_t, chisq_test, print_chisq_test, prop_test_result_t, &
   & prop_test, print_prop_test
integer, parameter :: dp = real64
logical :: print_int_like_default = .true.
real(kind=dp) :: print_int_like_tol = 1000.0_dp * epsilon(1.0_dp)
logical :: recycle_warn_default = .false.
logical :: recycle_stop_default = .false.
type :: lm_fit_t
   real(kind=dp), allocatable :: coef(:), fitted(:), resid(:)
   real(kind=dp) :: sigma, r_squared, adj_r_squared
end type lm_fit_t

type :: kmeans_result_t
   real(kind=dp), allocatable :: centers(:)
   integer, allocatable :: cluster(:)
end type kmeans_result_t

type :: optim_result_t
   real(kind=dp), allocatable :: par(:)
   real(kind=dp) :: value
   integer :: convergence
end type optim_result_t

type :: t_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   real(kind=dp) :: parameter = 0.0_dp
   real(kind=dp) :: p_value = 1.0_dp
   real(kind=dp) :: estimate = 0.0_dp
   real(kind=dp) :: estimate2 = 0.0_dp
   real(kind=dp) :: null_value = 0.0_dp
   real(kind=dp) :: conf_low = 0.0_dp
   real(kind=dp) :: conf_high = 0.0_dp
   real(kind=dp) :: stderr = 0.0_dp
   integer :: method = 1
end type t_test_result_t

type :: chisq_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   integer :: parameter = 0
   real(kind=dp) :: p_value = 1.0_dp
   integer :: method = 1
end type chisq_test_result_t

type :: prop_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   integer :: parameter = 0
   real(kind=dp) :: p_value = 1.0_dp
   real(kind=dp) :: estimate = 0.0_dp
   real(kind=dp) :: estimate2 = 0.0_dp
   real(kind=dp) :: null_value = 0.0_dp
   integer :: method = 1
end type prop_test_result_t

interface cov
   module procedure cov_vec
   module procedure cov_mat
end interface cov

interface var
   module procedure var_vec
   module procedure var_mat
end interface var

interface cor
   module procedure cor_vec
   module procedure cor_mat
   module procedure cor_mat_pair
end interface cor

interface kmeans
   module procedure kmeans_vec
   module procedure kmeans_mat
end interface kmeans

interface t_test
   module procedure t_test_one
   module procedure t_test_two
end interface t_test

interface t_test_p_value
   module procedure t_test_p_value_one
   module procedure t_test_p_value_two
end interface t_test_p_value

interface chisq_test
   module procedure chisq_test_int_vec
   module procedure chisq_test_real_vec
   module procedure chisq_test_int_mat
   module procedure chisq_test_real_mat
end interface chisq_test

interface prop_test
   module procedure prop_test_int_scalar
   module procedure prop_test_real_scalar
   module procedure prop_test_int_vec
   module procedure prop_test_real_vec
end interface prop_test

interface matrix
   module procedure matrix_real
   module procedure matrix_int
end interface matrix

interface summary
   module procedure summary_vec
   module procedure summary_mat
end interface summary

interface print_summary
   module procedure print_summary_vec
   module procedure print_summary_mat
end interface print_summary

interface r_matmul
   module procedure r_matmul_vv_real
   module procedure r_matmul_vv_int
   module procedure r_matmul_vv_real_int
   module procedure r_matmul_vv_int_real
   module procedure r_matmul_mv_real
   module procedure r_matmul_mv_int
   module procedure r_matmul_mv_real_int
   module procedure r_matmul_mv_int_real
   module procedure r_matmul_vm_real
   module procedure r_matmul_vm_int
   module procedure r_matmul_vm_real_int
   module procedure r_matmul_vm_int_real
   module procedure r_matmul_mm_real
   module procedure r_matmul_mm_int
   module procedure r_matmul_mm_real_int
   module procedure r_matmul_mm_int_real
end interface r_matmul

interface r_add
   module procedure r_add_vv
   module procedure r_add_vs
   module procedure r_add_sv
end interface r_add

interface r_sub
   module procedure r_sub_vv
   module procedure r_sub_vs
   module procedure r_sub_sv
end interface r_sub

interface r_mul
   module procedure r_mul_vv
   module procedure r_mul_vs
   module procedure r_mul_sv
end interface r_mul

interface r_div
   module procedure r_div_vv
   module procedure r_div_vs
   module procedure r_div_sv
end interface r_div

interface r_array
   module procedure r_array_real
   module procedure r_array_int
   module procedure r_array_char
end interface r_array

interface tabulate
   module procedure tabulate_int
   module procedure tabulate_real
end interface tabulate

interface table2
   module procedure table2_int
end interface table2

interface prop_table
   module procedure prop_table_int_vec
   module procedure prop_table_int_mat
end interface prop_table

interface print_table2
   module procedure print_table2_int
   module procedure print_table2_real
end interface print_table2

interface match
   module procedure match_int
   module procedure match_real
end interface match

interface print_matrix
   module procedure print_matrix_real
   module procedure print_matrix_int
end interface print_matrix

interface print_matrix_rstyle
   module procedure print_matrix_rstyle_real
   module procedure print_matrix_rstyle_int
end interface print_matrix_rstyle

interface cumsum
   module procedure cumsum_real
   module procedure cumsum_int
end interface cumsum

interface cumprod
   module procedure cumprod_real
   module procedure cumprod_int
end interface cumprod

interface diff
   module procedure diff_real
   module procedure diff_mat_real
   module procedure diff_int
end interface diff

interface diag
   module procedure diag_mat_real
   module procedure diag_vec_real
   module procedure diag_mat_int
   module procedure diag_vec_int
   module procedure diag_scalar_int
   module procedure diag_scalar_real_n
end interface diag

interface sort
   module procedure sort_real
   module procedure sort_int
end interface sort

interface solve_real
   module procedure solve_real_vec
   module procedure solve_real_mat
end interface solve_real

interface is_na
   module procedure is_na_real_scalar
   module procedure is_na_real_vec
   module procedure is_na_int_scalar
   module procedure is_na_int_vec
   module procedure is_na_char_scalar
   module procedure is_na_char_vec
end interface is_na

interface r_typeof
   module procedure r_typeof_real_scalar
   module procedure r_typeof_real_vec
   module procedure r_typeof_int_scalar
   module procedure r_typeof_int_vec
   module procedure r_typeof_char_scalar
   module procedure r_typeof_char_vec
   module procedure r_typeof_logical_scalar
   module procedure r_typeof_logical_vec
end interface r_typeof

contains

function r_character(n) result(out)
! Allocate an R-like character vector initialized to empty strings.
integer, intent(in) :: n
character(len=:), allocatable :: out(:)
allocate(character(len=0) :: out(max(0, n)))
end function r_character


subroutine set_print_int_like(flag)
! Enable/disable integer-like rendering for real matrix printing.
logical, intent(in) :: flag
print_int_like_default = flag
end subroutine set_print_int_like

subroutine set_print_int_like_tol(tol)
! Set tolerance used for integer-like real rendering.
real(kind=dp), intent(in) :: tol
if (tol > 0.0_dp) print_int_like_tol = tol
end subroutine set_print_int_like_tol

subroutine set_recycle_warn(flag)
! Enable/disable warnings for non-multiple recycling lengths.
logical, intent(in) :: flag
recycle_warn_default = flag
end subroutine set_recycle_warn

subroutine set_recycle_stop(flag)
! Enable/disable error stop for non-multiple recycling lengths.
logical, intent(in) :: flag
recycle_stop_default = flag
end subroutine set_recycle_stop

subroutine set_seed_int(seed)
! Set Fortran RNG seed deterministically from a single integer.
integer, intent(in) :: seed
integer :: n, i
integer, allocatable :: put(:)
integer(kind=int64) :: s, m
call random_seed(size=n)
allocate(put(n))
s = int(abs(seed), kind=int64)
if (s == 0_int64) s = 104729_int64
m = int(huge(0), kind=int64) - 1_int64
do i = 1, n
   put(i) = int(modulo(s + 104729_int64 * int(i, kind=int64), m) + 1_int64)
end do
call random_seed(put=put)
deallocate(put)
end subroutine set_seed_int

function kmeans_vec(x, centers, nstart) result(out)
! Minimal 1D k-means helper: returns centers and 1-based cluster ids.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: centers
integer, intent(in), optional :: nstart
type(kmeans_result_t) :: out
real(kind=dp), allocatable :: c(:), c_new(:), sums(:)
integer, allocatable :: cnt(:), cl(:)
integer :: i, j, k, n, it, jbest, nstart_loc
real(kind=dp) :: xmin, xmax, scale, d, dbest
n = size(x)
k = max(1, centers)
nstart_loc = 1
if (present(nstart)) nstart_loc = max(1, nstart)
allocate(c(k), c_new(k), sums(k), cnt(k), cl(n))
if (n <= 0) then
   c = 0.0_dp
   out%centers = c
   out%cluster = cl
   return
end if
xmin = minval(x)
xmax = maxval(x)
if (k == 1) then
   c(1) = sum(x) / real(n, kind=dp)
else
   scale = (xmax - xmin) / real(k - 1, kind=dp)
   do j = 1, k
      c(j) = xmin + real(j - 1, kind=dp) * scale
   end do
end if
do it = 1, 50 * nstart_loc
   do i = 1, n
      jbest = 1
      dbest = abs(x(i) - c(1))
      do j = 2, k
         d = abs(x(i) - c(j))
         if (d < dbest) then
            dbest = d
            jbest = j
         end if
      end do
      cl(i) = jbest
   end do
   sums = 0.0_dp
   cnt = 0
   do i = 1, n
      j = cl(i)
      sums(j) = sums(j) + x(i)
      cnt(j) = cnt(j) + 1
   end do
   c_new = c
   do j = 1, k
      if (cnt(j) > 0) c_new(j) = sums(j) / real(cnt(j), kind=dp)
   end do
   if (maxval(abs(c_new - c)) <= 1.0e-12_dp * max(1.0_dp, maxval(abs(c)))) exit
   c = c_new
end do
out%centers = c
out%cluster = cl
end function kmeans_vec

function kmeans_mat(x, centers, nstart) result(out)
! Minimal row-wise k-means helper for matrix observations.
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in) :: centers
integer, intent(in), optional :: nstart
type(kmeans_result_t) :: out
real(kind=dp), allocatable :: c(:,:), c_new(:,:), sums(:,:), dist(:)
integer, allocatable :: cnt(:), cl(:)
integer :: i, j, k, n, p, it, jbest, nstart_loc
real(kind=dp) :: d, dbest, shift
n = size(x, 1)
p = size(x, 2)
k = max(1, centers)
nstart_loc = 1
if (present(nstart)) nstart_loc = max(1, nstart)
allocate(c(k, p), c_new(k, p), sums(k, p), cnt(k), cl(n), dist(n))
if (n <= 0 .or. p <= 0) then
   out%centers = reshape(c, [k * max(1, p)])
   out%cluster = cl
   return
end if
do i = 1, n
   dist(i) = sum((x(i, :) - sum(x, dim=1) / real(n, kind=dp))**2)
end do
do j = 1, k
   c(j, :) = x(1 + mod(j - 1, n), :)
end do
if (k > 1) then
   do j = 1, k
      c(j, :) = x(1 + int(real(j - 1, kind=dp) * real(max(1, n - 1), kind=dp) / real(k - 1, kind=dp)), :)
   end do
end if
do it = 1, 50 * nstart_loc
   do i = 1, n
      jbest = 1
      dbest = sum((x(i, :) - c(1, :))**2)
      do j = 2, k
         d = sum((x(i, :) - c(j, :))**2)
         if (d < dbest) then
            dbest = d
            jbest = j
         end if
      end do
      cl(i) = jbest
   end do
   sums = 0.0_dp
   cnt = 0
   do i = 1, n
      j = cl(i)
      sums(j, :) = sums(j, :) + x(i, :)
      cnt(j) = cnt(j) + 1
   end do
   c_new = c
   do j = 1, k
      if (cnt(j) > 0) c_new(j, :) = sums(j, :) / real(cnt(j), kind=dp)
   end do
   shift = maxval(abs(c_new - c))
   if (shift <= 1.0e-12_dp * max(1.0_dp, maxval(abs(c)))) exit
   c = c_new
end do
out%centers = reshape(transpose(c), [k * p])
out%cluster = cl
end function kmeans_mat

pure function max_col(x, ties_method) result(idx)
! Return 1-based column index of row-wise maxima (ties -> first).
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in), optional :: ties_method
integer, allocatable :: idx(:)
integer :: i, j, n, k, jbest
real(kind=dp) :: vbest
n = size(x, 1)
k = size(x, 2)
allocate(idx(n))
if (present(ties_method)) then
   ! Only "first" tie handling is supported in this subset.
end if
if (k <= 0) then
   idx = 1
   return
end if
do i = 1, n
   jbest = 1
   vbest = x(i, 1)
   do j = 2, k
      if (x(i, j) > vbest) then
         vbest = x(i, j)
         jbest = j
      end if
   end do
   idx(i) = jbest
end do
end function max_col

function tabulate_int(x, nbins) result(out)
! Count occurrences of integer labels 1..nbins.
integer, intent(in) :: x(:)
integer, intent(in) :: nbins
integer, allocatable :: out(:)
integer :: i, b
allocate(out(max(0, nbins)))
if (size(out) > 0) out = 0
do i = 1, size(x)
   b = x(i)
   if (b >= 1 .and. b <= size(out)) out(b) = out(b) + 1
end do
end function tabulate_int

function tabulate_real(x, nbins) result(out)
! Count occurrences after integer-coding real labels.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nbins
integer, allocatable :: out(:)
integer :: i, b
allocate(out(max(0, nbins)))
if (size(out) > 0) out = 0
do i = 1, size(x)
   b = int(nint(x(i)))
   if (b >= 1 .and. b <= size(out)) out(b) = out(b) + 1
end do
end function tabulate_real

function table2_int(x, y, nx, ny) result(out)
! Count paired integer labels into an nx-by-ny contingency table.
integer, intent(in) :: x(:), y(:)
integer, intent(in) :: nx, ny
integer, allocatable :: out(:,:)
integer :: i, a, b, n
allocate(out(max(0, nx), max(0, ny)))
if (size(out) > 0) out = 0
n = min(size(x), size(y))
do i = 1, n
   a = x(i)
   b = y(i)
   if (a >= 1 .and. a <= size(out, 1) .and. b >= 1 .and. b <= size(out, 2)) then
      out(a, b) = out(a, b) + 1
   end if
end do
end function table2_int

function prop_table_int_vec(x, margin) result(out)
! Convert integer counts to proportions.
integer, intent(in) :: x(:)
integer, intent(in), optional :: margin
real(kind=dp), allocatable :: out(:)
integer :: s
allocate(out(size(x)))
s = sum(x)
if (s > 0) then
   out = real(x, kind=dp) / real(s, kind=dp)
else
   out = ieee_value(0.0_dp, ieee_quiet_nan)
end if
end function prop_table_int_vec

function prop_table_int_mat(x, margin) result(out)
! Convert integer contingency tables to overall, row, or column proportions.
integer, intent(in) :: x(:,:)
integer, intent(in), optional :: margin
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, s
allocate(out(size(x, 1), size(x, 2)))
if (present(margin)) then
   if (margin == 1) then
      do i = 1, size(x, 1)
         s = sum(x(i, :))
         if (s > 0) then
            out(i, :) = real(x(i, :), kind=dp) / real(s, kind=dp)
         else
            out(i, :) = ieee_value(0.0_dp, ieee_quiet_nan)
         end if
      end do
      return
   else if (margin == 2) then
      do j = 1, size(x, 2)
         s = sum(x(:, j))
         if (s > 0) then
            out(:, j) = real(x(:, j), kind=dp) / real(s, kind=dp)
         else
            out(:, j) = ieee_value(0.0_dp, ieee_quiet_nan)
         end if
      end do
      return
   end if
end if
s = sum(x)
if (s > 0) then
   out = real(x, kind=dp) / real(s, kind=dp)
else
   out = ieee_value(0.0_dp, ieee_quiet_nan)
end if
end function prop_table_int_mat

pure function nested_matrix_list_len(x) result(n)
! Count non-padding matrix slices in a ragged nested list lowered to rank 3.
real(kind=dp), intent(in) :: x(:,:,:)
integer :: n
integer :: j
n = 0
do j = 1, size(x, 3)
   if (any(ieee_is_finite(x(:,:,j)))) n = j
end do
end function nested_matrix_list_len

pure function match_int(x, table) result(out)
! Return first 1-based match positions, or a sentinel for NA.
integer, intent(in) :: x(:), table(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
out = -huge(0)
do i = 1, size(x)
   do j = 1, size(table)
      if (x(i) == table(j)) then
         out(i) = j
         exit
      end if
   end do
end do
end function match_int

pure function match_real(x, table) result(out)
! Return first 1-based match positions, or a sentinel for NA.
real(kind=dp), intent(in) :: x(:), table(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
out = -huge(0)
do i = 1, size(x)
   do j = 1, size(table)
      if (x(i) == table(j)) then
         out(i) = j
         exit
      end if
   end do
end do
end function match_real

function r_format_vec(x, digits, sep) result(out)
! Format a real vector like paste(sprintf("%.<digits>f", x), collapse=sep).
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: digits
character(len=*), intent(in), optional :: sep
character(len=:), allocatable :: out
character(len=64) :: fmt, buf
integer :: i, d
d = max(0, min(30, digits))
write(fmt, '("(f0.", i0, ")")') d
out = ""
do i = 1, size(x)
   write(buf, fmt) x(i)
   if (i > 1) then
      if (present(sep)) then
         out = out // sep
      else
         out = out // " "
      end if
   end if
   out = out // trim(adjustl(buf))
end do
end function r_format_vec

subroutine maybe_warn_recycle(op, na, nb)
! Warn/stop when any vector recycling occurs (lengths differ).
character(len=*), intent(in) :: op
integer, intent(in) :: na, nb
integer :: nmax, nmin
if ((.not. recycle_warn_default) .and. (.not. recycle_stop_default)) return
nmax = max(na, nb)
nmin = min(na, nb)
if (nmin <= 0) return
if (nmax /= nmin) then
   if (recycle_stop_default) then
      error stop "recycle-stop: vector recycling occurred (lengths differ)"
   end if
   if (recycle_warn_default) then
      write(*,'(a)') "Warning message:"
      write(*,'(3a)') "In ", trim(op), " : vector recycling occurred (lengths differ)"
   end if
end if
end subroutine maybe_warn_recycle

subroutine print_real_scalar(x, int_like)
! Print one real value using integer format when integer-like.
real(kind=dp), intent(in) :: x
logical, intent(in), optional :: int_like
logical :: use_int_like, as_int
integer(kind=int64) :: k
real(kind=dp) :: tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
as_int = .false.
if (use_int_like) then
   if (ieee_is_finite(x) .and. abs(x) <= real(huge(0_int64), kind=dp)) then
      k = nint(x, kind=int64)
      tol = print_int_like_tol * max(1.0_dp, abs(x))
      as_int = abs(x - real(k, kind=dp)) <= tol
   end if
end if
if (as_int) then
   write(*,"(i0)") k
else
   write(*,"(g0)") x
end if
end subroutine print_real_scalar

subroutine print_real_vector(x, int_like)
! Print one real vector; use integer format when all values are integer-like.
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: int_like
logical :: use_int_like, all_int
integer :: i
integer(kind=int64) :: k
real(kind=dp) :: r, tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
all_int = .false.
if (use_int_like) then
   all_int = .true.
   do i = 1, size(x)
      r = x(i)
      if (.not. ieee_is_finite(r)) then
         all_int = .false.
         exit
      end if
      if (abs(r) > real(huge(0_int64), kind=dp)) then
         all_int = .false.
         exit
      end if
      k = nint(r, kind=int64)
      tol = print_int_like_tol * max(1.0_dp, abs(r))
      if (abs(r - real(k, kind=dp)) > tol) then
         all_int = .false.
         exit
      end if
   end do
end if
if (all_int) then
   do i = 1, size(x)
      k = nint(x(i), kind=int64)
      write(*,"(i0)", advance="no") k
      if (i < size(x)) write(*,"(a)", advance="no") " "
   end do
   write(*,*)
else
   write(*,"(*(g0,1x))") x
end if
end subroutine print_real_vector

subroutine print_char_vector(x)
character(len=*), intent(in) :: x(:)
integer :: i
do i = 1, size(x)
   write(*,"(a)", advance="no") trim(x(i))
   if (i < size(x)) write(*,"(a)", advance="no") " "
end do
write(*,*)
end subroutine print_char_vector

subroutine print_named_real_vector(x, names)
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in) :: names(:)
call print_char_vector(names)
call print_real_vector(x)
end subroutine print_named_real_vector

subroutine print_table1(x, names)
integer, intent(in) :: x(:)
character(len=*), intent(in) :: names(:)
integer :: i
do i = 1, min(size(x), size(names))
   write(*,'(a,1x)', advance='no') trim(names(i))
end do
write(*,*)
do i = 1, size(x)
   write(*,'(i0,1x)', advance='no') x(i)
end do
write(*,*)
end subroutine print_table1

subroutine print_table2_int(x, row_names, col_names)
integer, intent(in) :: x(:,:)
character(len=*), intent(in) :: row_names(:), col_names(:)
integer :: i, j
write(*,'(12x)', advance='no')
do j = 1, size(x, 2)
   if (j <= size(col_names)) then
      write(*,'(a12,1x)', advance='no') trim(col_names(j))
   else
      write(*,'(i12,1x)', advance='no') j
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   if (i <= size(row_names)) then
      write(*,'(a12,1x)', advance='no') trim(row_names(i))
   else
      write(*,'(i12,1x)', advance='no') i
   end if
   do j = 1, size(x, 2)
      write(*,'(i12,1x)', advance='no') x(i, j)
   end do
   write(*,*)
end do
end subroutine print_table2_int

subroutine print_table2_real(x, row_names, col_names)
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in) :: row_names(:), col_names(:)
integer :: i, j
write(*,'(12x)', advance='no')
do j = 1, size(x, 2)
   if (j <= size(col_names)) then
      write(*,'(a12,1x)', advance='no') trim(col_names(j))
   else
      write(*,'(i12,1x)', advance='no') j
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   if (i <= size(row_names)) then
      write(*,'(a12,1x)', advance='no') trim(row_names(i))
   else
      write(*,'(i12,1x)', advance='no') i
   end if
   do j = 1, size(x, 2)
      write(*,'(f12.4,1x)', advance='no') x(i, j)
   end do
   write(*,*)
end do
end subroutine print_table2_real

subroutine print_summary_vec(x)
! Print an R-like summary for a numeric vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: s(:)
s = summary_vec(x)
write(*,'(a,1x,g0)') "   Min.:", s(1)
write(*,'(a,1x,g0)') "1st Qu.:", s(2)
write(*,'(a,1x,g0)') " Median:", s(3)
write(*,'(a,1x,g0)') "   Mean:", s(4)
write(*,'(a,1x,g0)') "3rd Qu.:", s(5)
write(*,'(a,1x,g0)') "   Max.:", s(6)
end subroutine print_summary_vec

subroutine print_summary_mat(x)
! Print R-like per-column summaries for a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: s(:,:)
character(len=*), parameter :: labels(6) = [character(len=8) :: "Min.   :", "1st Qu.:", "Median :", "Mean   :", "3rd Qu.:", "Max.   :"]
integer :: j
allocate(s(6, size(x, 2)))
do j = 1, size(x, 2)
   s(:, j) = summary_vec(x(:, j))
end do
write(*,'(9x)', advance='no')
do j = 1, size(x, 2)
   write(*,'(a,i0,13x)', advance='no') "V", j
end do
write(*,*)
do j = 1, 6
   write(*,'(a,1x)', advance='no') labels(j)
   write(*,'(*(es14.6,1x))') s(j, :)
end do
end subroutine print_summary_mat

pure function r_seq_int(a, b) result(out)
! Return integer sequence a, a+step, ..., b with step +/-1.
integer, intent(in) :: a, b
integer, allocatable :: out(:)
integer :: i, n, step
n = abs(b - a) + 1
allocate(out(n))
step = merge(1, -1, a <= b)
do i = 1, n
   out(i) = a + (i - 1) * step
end do
end function r_seq_int

pure function r_seq_len(n) result(out)
! Return integer sequence 1..n (empty for n<=0).
integer, intent(in) :: n
integer, allocatable :: out(:)
integer :: i
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
do i = 1, n
   out(i) = i
end do
end function r_seq_len

pure function r_seq_int_by(a, b, by) result(out)
! Return integer sequence from a to b using explicit integer step by.
integer, intent(in) :: a, b, by
integer, allocatable :: out(:)
integer :: i, n
if (by == 0) then
   allocate(out(0))
   return
end if
if ((by > 0 .and. a > b) .or. (by < 0 .and. a < b)) then
   allocate(out(0))
   return
end if
n = (abs(b - a) / abs(by)) + 1
allocate(out(n))
do i = 1, n
   out(i) = a + (i - 1) * by
end do
end function r_seq_int_by

pure function r_seq_int_length(a, b, n) result(out)
! Return n integer values linearly spaced from a to b.
integer, intent(in) :: a, b, n
integer, allocatable :: out(:)
integer :: i
real(kind=dp) :: t
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
if (n == 1) then
   out(1) = a
   return
end if
do i = 1, n
   t = real(i - 1, kind=dp) / real(n - 1, kind=dp)
   out(i) = nint((1.0_dp - t) * real(a, kind=dp) + t * real(b, kind=dp))
end do
end function r_seq_int_length

pure function r_seq_real_by(a, b, by) result(out)
! Return real sequence from a to b with real step by.
real(kind=dp), intent(in) :: a, b, by
real(kind=dp), allocatable :: out(:)
integer :: i, n
if (by == 0.0_dp) then
   allocate(out(0))
   return
end if
if ((by > 0.0_dp .and. a > b) .or. (by < 0.0_dp .and. a < b)) then
   allocate(out(0))
   return
end if
n = int(floor((b - a) / by + 1.0e-12_dp)) + 1
if (n < 0) n = 0
allocate(out(n))
do i = 1, n
   out(i) = a + real(i - 1, kind=dp) * by
end do
end function r_seq_real_by

pure function r_seq_real_length(a, b, n) result(out)
! Return n real values linearly spaced from a to b.
real(kind=dp), intent(in) :: a, b
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: i
real(kind=dp) :: t
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
if (n == 1) then
   out(1) = a
   return
end if
do i = 1, n
   t = real(i - 1, kind=dp) / real(n - 1, kind=dp)
   out(i) = (1.0_dp - t) * a + t * b
end do
end function r_seq_real_length

pure function r_rep_real(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of a real vector (R-like rep subset).
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(out(0))
      return
   end if
   allocate(out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_real

pure function r_rep_int(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of an integer vector (R-like rep subset).
integer, intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
integer, allocatable :: out(:)
integer, allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(out(0))
      return
   end if
   allocate(out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_int

function runif1() result(u)
! Return one U(0,1) variate.
real(kind=dp) :: u
call random_number(u)
end function runif1

function runif_vec(n) result(x)
! Return a length-n vector of U(0,1) variates.
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
allocate(x(n))
call random_number(x)
end function runif_vec

function rnorm1() result(x)
! Return one N(0,1) variate (Box-Muller).
real(kind=dp) :: x
real(kind=dp) :: u1, u2
do
   call random_number(u1)
   call random_number(u2)
   if (u1 > tiny(1.0_dp)) exit
end do
x = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * acos(-1.0_dp) * u2)
end function rnorm1

function rnorm_vec(n) result(x)
! Return a length-n vector of N(0,1) variates.
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
integer :: i
real(kind=dp) :: u1, u2, r, t
allocate(x(n))
i = 1
do while (i <= n)
   call random_number(u1)
   call random_number(u2)
   if (u1 <= tiny(1.0_dp)) cycle
   r = sqrt(-2.0_dp * log(u1))
   t = 2.0_dp * acos(-1.0_dp) * u2
   x(i) = r * cos(t)
   if (i + 1 <= n) x(i + 1) = r * sin(t)
   i = i + 2
end do
end function rnorm_vec

function rnorm_mat(nrow, ncol) result(x)
! Return an nrow-by-ncol matrix of N(0,1) variates.
integer, intent(in) :: nrow, ncol
real(kind=dp), allocatable :: x(:,:)
real(kind=dp), allocatable :: v(:)
v = rnorm_vec(nrow * ncol)
x = reshape(v, [nrow, ncol])
end function rnorm_mat

function rbinom(n, size_, prob) result(x)
! Return n binomial(size, prob) variates.
integer, intent(in) :: n, size_
real(kind=dp), intent(in) :: prob
integer, allocatable :: x(:)
integer :: i, j, s
real(kind=dp) :: u, p
allocate(x(max(0, n)))
p = max(0.0_dp, min(1.0_dp, prob))
do i = 1, size(x)
   s = 0
   do j = 1, max(0, size_)
      call random_number(u)
      if (u < p) s = s + 1
   end do
   x(i) = s
end do
end function rbinom

function random_choice2_prob(n, p1) result(z)
! Sample n labels in {1,2} with P(label=1)=p1.
integer, intent(in) :: n
real(kind=dp), intent(in) :: p1
integer, allocatable :: z(:)
integer :: i
real(kind=dp) :: u
allocate(z(n))
do i = 1, n
   call random_number(u)
   z(i) = merge(1, 2, u < p1)
end do
end function random_choice2_prob

function randint_range(n, lo, hi) result(out)
! Sample n integers uniformly from [lo, hi].
integer, intent(in) :: n, lo, hi
integer, allocatable :: out(:)
integer :: i, span
real(kind=dp) :: u
if (hi < lo) then
   allocate(out(0))
   return
end if
span = hi - lo + 1
allocate(out(n))
do i = 1, n
   call random_number(u)
   out(i) = lo + int(u * span)
   if (out(i) > hi) out(i) = hi
end do
end function randint_range

function sample_int(n, size_, replace, prob) result(out)
! R-like sample.int with optional replacement and probabilities.
integer, intent(in) :: n
integer, intent(in), optional :: size_
logical, intent(in), optional :: replace
real(kind=dp), intent(in), optional :: prob(:)
integer, allocatable :: out(:), pool(:)
real(kind=dp), allocatable :: w(:), cdf(:)
integer :: m, i, j, pick, tmp
logical :: rep
real(kind=dp) :: u, s, acc

m = n
if (present(size_)) m = size_
rep = .false.
if (present(replace)) rep = replace

if (n < 1) then
   if (m == 0) then
      allocate(out(0))
      return
   end if
   error stop "sample_int: n must be >= 1"
end if
if (m < 0) error stop "sample_int: size must be >= 0"
if ((.not. rep) .and. (m > n)) error stop &
   & "sample_int: size > n without replacement"
if (present(prob)) then
   if (size(prob) /= n) error stop "sample_int: prob length mismatch"
   if (any(prob < 0.0_dp)) error stop "sample_int: prob must be nonnegative"
   if (sum(prob) <= 0.0_dp) error stop "sample_int: prob sum must be positive"
end if

allocate(out(m))
if (m == 0) return

if (present(prob)) then
   if (rep) then
      allocate(cdf(n))
      s = sum(prob)
      cdf(1) = prob(1) / s
      do i = 2, n
         cdf(i) = cdf(i - 1) + prob(i) / s
      end do
      cdf(n) = 1.0_dp
      do i = 1, m
         call random_number(u)
         pick = 1
         do while (pick < n .and. u > cdf(pick))
            pick = pick + 1
         end do
         out(i) = pick
      end do
   else
      allocate(w(n))
      w = prob
      do i = 1, m
         s = sum(w)
         if (s <= 0.0_dp) error stop "sample_int: depleted probability mass"
         call random_number(u)
         u = u * s
         acc = 0.0_dp
         pick = n
         do j = 1, n
            acc = acc + w(j)
            if (u <= acc) then
               pick = j
               exit
            end if
         end do
         out(i) = pick
         w(pick) = 0.0_dp
      end do
   end if
else
   if (rep) then
      do i = 1, m
         call random_number(u)
         out(i) = 1 + int(u * real(n, kind=dp))
         if (out(i) > n) out(i) = n
      end do
   else
      allocate(pool(n))
      pool = [(j, j=1,n)]
      do i = 1, m
         call random_number(u)
         pick = i + int(u * real(n - i + 1, kind=dp))
         if (pick > n) pick = n
         tmp = pool(i)
         pool(i) = pool(pick)
         pool(pick) = tmp
         out(i) = pool(i)
      end do
   end if
end if
end function sample_int

function sample_int1(n, replace, prob) result(out)
! Scalar wrapper for sample_int(..., size_=1).
integer, intent(in) :: n
logical, intent(in), optional :: replace
real(kind=dp), intent(in), optional :: prob(:)
integer :: out
integer, allocatable :: tmp(:)
if (present(replace) .and. present(prob)) then
   tmp = sample_int(n, size_=1, replace=replace, prob=prob)
else if (present(prob)) then
   tmp = sample_int(n, size_=1, prob=prob)
else if (present(replace)) then
    tmp = sample_int(n, size_=1, replace=replace)
else
   tmp = sample_int(n, size_=1)
end if
out = tmp(1)
end function sample_int1

pure subroutine sort_increasing(x)
! Sort a real vector in increasing order (insertion sort).
real(kind=dp), intent(inout) :: x(:)
integer :: i, j
real(kind=dp) :: key
do i = 2, size(x)
   key = x(i)
   j = i - 1
   do while (j >= 1 .and. x(j) > key)
      x(j + 1) = x(j)
      j = j - 1
   end do
   x(j + 1) = key
end do
end subroutine sort_increasing

pure subroutine sort_increasing_int(x)
! Sort an integer vector in increasing order (insertion sort).
integer, intent(inout) :: x(:)
integer :: i, j, key
do i = 2, size(x)
   key = x(i)
   j = i - 1
   do while (j >= 1 .and. x(j) > key)
      x(j + 1) = x(j)
      j = j - 1
   end do
   x(j + 1) = key
end do
end subroutine sort_increasing_int

pure function sort_real(x) result(out)
! Return a sorted copy of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
out = x
call sort_increasing(out)
end function sort_real

pure function sort_int(x) result(out)
! Return a sorted copy of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
out = x
call sort_increasing_int(out)
end function sort_int

pure function order_real(x) result(idx)
! Return 1-based order indices that sort a real vector increasingly.
real(kind=dp), intent(in) :: x(:)
integer, allocatable :: idx(:)
integer :: i, j, t
allocate(idx(size(x)))
if (size(x) <= 0) return
do i = 1, size(x)
   idx(i) = i
end do
do i = 2, size(idx)
   t = idx(i)
   j = i - 1
   do while (j >= 1 .and. x(idx(j)) > x(t))
      idx(j + 1) = idx(j)
      j = j - 1
   end do
   idx(j + 1) = t
end do
end function order_real

pure function rank_first(x) result(out)
! Return R rank(x, ties.method="first") for a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer, allocatable :: ord(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
ord = order_real(x)
do i = 1, size(ord)
   out(ord(i)) = real(i, kind=dp)
end do
end function rank_first

pure function rank_average(x) result(out)
! Return R rank(x, ties.method="average") for a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer, allocatable :: ord(:)
integer :: first, i, last
real(kind=dp) :: r
allocate(out(size(x)))
if (size(x) <= 0) return
ord = order_real(x)
first = 1
do while (first <= size(ord))
   last = first
   do while (last < size(ord) .and. x(ord(last + 1)) == x(ord(first)))
      last = last + 1
   end do
   r = 0.5_dp * real(first + last, kind=dp)
   do i = first, last
      out(ord(i)) = r
   end do
   first = last + 1
end do
end function rank_average

pure function det_real(x) result(out)
! Return the determinant of a real square matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp) :: out
real(kind=dp), allocatable :: a(:)
integer :: i, j, k, n, p
real(kind=dp) :: fac, piv, t
n = size(x, 1)
out = 0.0_dp
if (n /= size(x, 2)) return
if (n == 0) then
   out = 1.0_dp
   return
end if
allocate(a(n*n))
a = reshape(x, [n*n])
out = 1.0_dp
do k = 1, n
   p = k
   piv = abs(a((k - 1)*n + k))
   do i = k + 1, n
      if (abs(a((i - 1)*n + k)) > piv) then
         p = i
         piv = abs(a((i - 1)*n + k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) then
      out = 0.0_dp
      return
   end if
   if (p /= k) then
      do j = k, n
         t = a((k - 1)*n + j)
         a((k - 1)*n + j) = a((p - 1)*n + j)
         a((p - 1)*n + j) = t
      end do
      out = -out
   end if
   out = out * a((k - 1)*n + k)
   do i = k + 1, n
      fac = a((i - 1)*n + k) / a((k - 1)*n + k)
      do j = k + 1, n
         a((i - 1)*n + j) = a((i - 1)*n + j) - fac * a((k - 1)*n + j)
      end do
   end do
end do
end function det_real

pure function eigen_sym_values(x) result(vals)
! Return eigenvalues of a real symmetric matrix using Jacobi rotations.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: vals(:)
real(kind=dp), allocatable :: a(:,:)
integer :: i, j, p, q, iter, n, max_iter
real(kind=dp) :: app, aqq, apq, c, off, phi, s, tau, t, tmp
n = size(x, 1)
allocate(vals(n))
vals = 0.0_dp
if (n <= 0 .or. n /= size(x, 2)) return
a = x
max_iter = max(1, 100 * n * n)
do iter = 1, max_iter
   off = 0.0_dp
   p = 1
   q = min(2, n)
   do i = 1, n - 1
      do j = i + 1, n
         if (abs(a(i, j)) > off) then
            off = abs(a(i, j))
            p = i
            q = j
         end if
      end do
   end do
   if (off <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, maxval(abs(a)))) exit
   app = a(p, p)
   aqq = a(q, q)
   apq = a(p, q)
   if (abs(apq) <= tiny(1.0_dp)) cycle
   tau = (aqq - app) / (2.0_dp * apq)
   t = sign(1.0_dp, tau) / (abs(tau) + sqrt(1.0_dp + tau * tau))
   c = 1.0_dp / sqrt(1.0_dp + t * t)
   s = t * c
   do j = 1, n
      if (j /= p .and. j /= q) then
         tmp = a(j, p)
         a(j, p) = c * tmp - s * a(j, q)
         a(p, j) = a(j, p)
         a(j, q) = s * tmp + c * a(j, q)
         a(q, j) = a(j, q)
      end if
   end do
   phi = t * apq
   a(p, p) = app - phi
   a(q, q) = aqq + phi
   a(p, q) = 0.0_dp
   a(q, p) = 0.0_dp
end do
do i = 1, n
   vals(i) = a(i, i)
end do
end function eigen_sym_values

pure function solve_real_vec(a, b) result(x)
! Return the solution of a square linear system a %*% x = b.
real(kind=dp), intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: aa(:,:), bb(:)
integer :: i, j, k, n, p
real(kind=dp) :: fac, piv, s, t
n = size(b)
allocate(x(n))
x = 0.0_dp
if (size(a, 1) /= n .or. size(a, 2) /= n) return
aa = a
bb = b
do k = 1, n
   p = k
   piv = abs(aa(k, k))
   do i = k + 1, n
      if (abs(aa(i, k)) > piv) then
         p = i
         piv = abs(aa(i, k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) return
   if (p /= k) then
      do j = k, n
         t = aa(k, j)
         aa(k, j) = aa(p, j)
         aa(p, j) = t
      end do
      t = bb(k)
      bb(k) = bb(p)
      bb(p) = t
   end if
   do i = k + 1, n
      fac = aa(i, k) / aa(k, k)
      aa(i, k:n) = aa(i, k:n) - fac * aa(k, k:n)
      bb(i) = bb(i) - fac * bb(k)
   end do
end do
do i = n, 1, -1
   s = bb(i)
   if (i < n) s = s - sum(aa(i, i+1:n) * x(i+1:n))
   x(i) = s / aa(i, i)
end do
end function solve_real_vec

pure function solve_real_mat(a, b) result(x)
! Return the solution of a square linear system a %*% x = b for matrix b.
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: x(:,:)
integer :: j, n, m
n = size(a, 1)
m = size(b, 2)
allocate(x(n, m))
x = 0.0_dp
if (size(a, 2) /= n .or. size(b, 1) /= n) return
do j = 1, m
   x(:, j) = solve_real_vec(a, b(:, j))
end do
end function solve_real_mat

pure function cumsum_real(x) result(out)
! Return cumulative sums of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) + x(i)
end do
end function cumsum_real

pure function cumsum_int(x) result(out)
! Return cumulative sums of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) + x(i)
end do
end function cumsum_int

pure function findInterval(x, vec) result(out)
! Return R-style interval counts for each x against sorted breakpoints vec.
real(kind=dp), intent(in) :: x(:), vec(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = 0
   do j = 1, size(vec)
      if (x(i) >= vec(j)) out(i) = j
   end do
end do
end function findInterval

pure function cumprod_real(x) result(out)
! Return cumulative products of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) * x(i)
end do
end function cumprod_real

pure function cumprod_int(x) result(out)
! Return cumulative products of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) * x(i)
end do
end function cumprod_int

pure function diff_real(x) result(out)
! First differences of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(x)
allocate(out(max(0, n - 1)))
if (n > 1) out = x(2:n) - x(1:n - 1)
end function diff_real

pure function diff_mat_real(x) result(out)
! First row differences of a real matrix, matching R diff() on matrices.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: n, p
n = size(x, 1)
p = size(x, 2)
allocate(out(max(0, n - 1), p))
if (n > 1) out = x(2:n, :) - x(1:n - 1, :)
end function diff_mat_real

pure function diff_int(x) result(out)
! First differences of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: n
n = size(x)
allocate(out(max(0, n - 1)))
if (n > 1) out = x(2:n) - x(1:n - 1)
end function diff_int

pure function diag_mat_real(a) result(out)
! Return diagonal of a real matrix.
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: out(:)
integer :: i, n
n = min(size(a, 1), size(a, 2))
allocate(out(n))
do i = 1, n
   out(i) = a(i, i)
end do
end function diag_mat_real

pure function diag_vec_real(v) result(out)
! Create diagonal real matrix from a real vector.
real(kind=dp), intent(in) :: v(:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, n
n = size(v)
allocate(out(n, n))
out = 0.0_dp
do i = 1, n
   out(i, i) = v(i)
end do
end function diag_vec_real

pure function diag_mat_int(a) result(out)
! Return diagonal of an integer matrix.
integer, intent(in) :: a(:,:)
integer, allocatable :: out(:)
integer :: i, n
n = min(size(a, 1), size(a, 2))
allocate(out(n))
do i = 1, n
   out(i) = a(i, i)
end do
end function diag_mat_int

pure function diag_vec_int(v) result(out)
! Create diagonal integer matrix from an integer vector.
integer, intent(in) :: v(:)
integer, allocatable :: out(:,:)
integer :: i, n
n = size(v)
allocate(out(n, n))
out = 0
do i = 1, n
   out(i, i) = v(i)
end do
end function diag_vec_int

pure function diag_scalar_int(n) result(out)
! Create an n by n integer identity matrix, matching R diag(n).
integer, intent(in) :: n
integer, allocatable :: out(:,:)
integer :: i
allocate(out(n, n))
out = 0
do i = 1, n
   out(i, i) = 1
end do
end function diag_scalar_int

pure function diag_scalar_real_n(x, n) result(out)
! Create an n by n real diagonal matrix with scalar value x.
real(kind=dp), intent(in) :: x
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:,:)
integer :: i
allocate(out(n, n))
out = 0.0_dp
do i = 1, n
   out(i, i) = x
end do
end function diag_scalar_real_n

pure function toeplitz(x) result(out)
! Symmetric Toeplitz matrix from first column/row vector x.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, n
n = size(x)
allocate(out(n, n))
do j = 1, n
   do i = 1, n
      out(i, j) = x(abs(i - j) + 1)
   end do
end do
end function toeplitz

pure function chol(a) result(r)
! Upper-triangular Cholesky factor for a symmetric positive-definite matrix.
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: r(:,:)
real(kind=dp) :: s
integer :: i, j, k, n
n = size(a, 1)
allocate(r(n, n))
r = 0.0_dp
do j = 1, n
   s = a(j, j)
   if (j > 1) s = s - sum(r(1:j-1, j)**2)
   r(j, j) = sqrt(max(s, 0.0_dp))
   do k = j + 1, n
      s = a(j, k)
      if (j > 1) s = s - sum(r(1:j-1, j) * r(1:j-1, k))
      if (r(j, j) > 0.0_dp) r(j, k) = s / r(j, j)
   end do
end do
end function chol

pure function backsolve(r, b, transpose) result(x)
! Solve R x = b for upper-triangular R; transpose=.true. solves R^T x = b.
real(kind=dp), intent(in) :: r(:,:), b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
logical :: tr
integer :: i, j, n, m
real(kind=dp) :: s
n = size(r, 1)
m = size(b, 2)
allocate(x(n, m))
x = 0.0_dp
tr = .false.
if (present(transpose)) tr = transpose
if (tr) then
   do j = 1, m
      do i = 1, n
         s = b(i, j)
         if (i > 1) s = s - sum(r(1:i-1, i) * x(1:i-1, j))
         x(i, j) = s / r(i, i)
      end do
   end do
else
   do j = 1, m
      do i = n, 1, -1
         s = b(i, j)
         if (i < n) s = s - sum(r(i, i+1:n) * x(i+1:n, j))
         x(i, j) = s / r(i, i)
      end do
   end do
end if
end function backsolve

pure integer function nchar(s) result(out)
! Return character length (R-like nchar scalar subset).
character(len=*), intent(in) :: s
out = len_trim(s)
end function nchar

pure elemental logical function is_na_real_scalar(x) result(out)
! True when real scalar is NA/NaN in this subset.
real(kind=dp), intent(in) :: x
out = .not. ieee_is_finite(x)
end function is_na_real_scalar

pure function is_na_real_vec(x) result(out)
! Elementwise NA test for a real vector.
real(kind=dp), intent(in) :: x(:)
logical, allocatable :: out(:)
allocate(out(size(x)))
out = .not. ieee_is_finite(x)
end function is_na_real_vec

pure elemental logical function is_na_int_scalar(x) result(out)
! True when integer scalar uses NA sentinel.
integer, intent(in) :: x
out = (x == -huge(0))
end function is_na_int_scalar

pure function is_na_int_vec(x) result(out)
! Elementwise NA test for integer vector.
integer, intent(in) :: x(:)
logical, allocatable :: out(:)
allocate(out(size(x)))
out = (x == -huge(0))
end function is_na_int_vec

pure elemental logical function is_na_char_scalar(x) result(out)
! True when character scalar uses NA sentinel in this subset.
character(len=*), intent(in) :: x
out = (x == "")
end function is_na_char_scalar

pure function is_na_char_vec(x) result(out)
! Elementwise NA test for character vector.
character(len=*), intent(in) :: x(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = (x(i) == "")
end do
end function is_na_char_vec

pure function r_typeof_real_scalar(x) result(out)
! Return R-like type label for real scalar.
real(kind=dp), intent(in) :: x
character(len=:), allocatable :: out
out = "double"
end function r_typeof_real_scalar

pure function r_typeof_real_vec(x) result(out)
! Return R-like type label for real vector.
real(kind=dp), intent(in) :: x(:)
character(len=:), allocatable :: out
out = "double"
end function r_typeof_real_vec

pure function r_typeof_int_scalar(x) result(out)
! Return R-like type label for integer scalar.
integer, intent(in) :: x
character(len=:), allocatable :: out
out = "integer"
end function r_typeof_int_scalar

pure function r_typeof_int_vec(x) result(out)
! Return R-like type label for integer vector.
integer, intent(in) :: x(:)
character(len=:), allocatable :: out
out = "integer"
end function r_typeof_int_vec

pure function r_typeof_char_scalar(x) result(out)
! Return R-like type label for character scalar.
character(len=*), intent(in) :: x
character(len=:), allocatable :: out
out = "character"
end function r_typeof_char_scalar

pure function r_typeof_char_vec(x) result(out)
! Return R-like type label for character vector.
character(len=*), intent(in) :: x(:)
character(len=:), allocatable :: out
out = "character"
end function r_typeof_char_vec

pure function r_typeof_logical_scalar(x) result(out)
! Return R-like type label for logical scalar.
logical, intent(in) :: x
character(len=:), allocatable :: out
out = "logical"
end function r_typeof_logical_scalar

pure function r_typeof_logical_vec(x) result(out)
! Return R-like type label for logical vector.
logical, intent(in) :: x(:)
character(len=:), allocatable :: out
out = "logical"
end function r_typeof_logical_vec

pure function quantile(x, probs, names, type) result(out)
! Compute Type-7 quantiles for a numeric vector.
real(kind=dp), intent(in) :: x(:), probs(:)
logical, intent(in), optional :: names
integer, intent(in), optional :: type
real(kind=dp), allocatable :: out(:), xs(:)
integer :: n, i, j
real(kind=dp) :: p, h, g
n = size(x)
allocate(out(size(probs)))
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
xs = x
call sort_increasing(xs)
do i = 1, size(probs)
   p = min(1.0_dp, max(0.0_dp, probs(i)))
   h = (n - 1) * p + 1.0_dp
   j = int(floor(h))
   g = h - j
   if (j < 1) then
      out(i) = xs(1)
   else if (j >= n) then
      out(i) = xs(n)
   else
      out(i) = (1.0_dp - g) * xs(j) + g * xs(j + 1)
   end if
end do
! names/type accepted for API compatibility in this subset.
if (present(names)) continue
if (present(type)) continue
end function quantile

pure function median(x) result(out)
! Compute the median of a numeric vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out
real(kind=dp), allocatable :: xs(:)
integer :: n, mid
n = size(x)
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
xs = x
call sort_increasing(xs)
mid = (n + 1) / 2
if (mod(n, 2) == 1) then
   out = xs(mid)
else
   out = 0.5_dp * (xs(mid) + xs(mid + 1))
end if
end function median

pure function summary_vec(x) result(out)
! Return R-like numeric summary: Min, 1st Qu., Median, Mean, 3rd Qu., Max.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:), qs(:)
integer :: n
n = size(x)
allocate(out(6))
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
qs = quantile(x, [0.0_dp, 0.25_dp, 0.5_dp, 0.75_dp, 1.0_dp], .false., 7)
out = [qs(1), qs(2), qs(3), sum(x) / real(n, kind=dp), qs(4), qs(5)]
end function summary_vec

pure function summary_mat(x) result(out)
! Return R-like numeric summary over all elements of a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:)
out = summary_vec(reshape(x, [size(x)]))
end function summary_mat

pure function dnorm(x, mean, sd, log_) result(out)
! Evaluate normal density (or log-density) elementwise.
real(kind=dp), intent(in) :: x(:), mean, sd
logical, intent(in), optional :: log_
real(kind=dp), allocatable :: out(:), z(:)
logical :: l
l = .false.
if (present(log_)) l = log_
if (sd <= 0.0_dp) then
   allocate(out(size(x)))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
z = (x - mean) / sd
out = -0.5_dp * log(2.0_dp * acos(-1.0_dp)) - log(sd) - 0.5_dp * z**2
if (.not. l) out = exp(out)
end function dnorm

pure function tail(x, n) result(out)
! Return the last n elements of a vector.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: m, n0
n0 = size(x)
m = max(0, min(n, n0))
allocate(out(m))
if (m > 0) out = x(n0 - m + 1:n0)
end function tail

pure function cbind2(a, b) result(out)
! Bind two vectors as columns of a 2D array.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
n = min(size(a), size(b))
allocate(out(n, 2))
if (n > 0) then
   out(:,1) = a(1:n)
   out(:,2) = b(1:n)
end if
end function cbind2

pure function cbind(r1, r2, r3) result(out)
! Bind two or three vectors as columns of a 2D array.
real(kind=dp), intent(in) :: r1(:), r2(:)
real(kind=dp), intent(in), optional :: r3(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
if (.not. present(r3)) then
   out = cbind2(r1, r2)
   return
end if
n = min(size(r1), min(size(r2), size(r3)))
allocate(out(n, 3))
if (n > 0) then
   out(:,1) = r1(1:n)
   out(:,2) = r2(1:n)
   out(:,3) = r3(1:n)
end if
end function cbind

pure function matrix_real(x, nrow, ncol) result(out)
! Build matrix with R-like recycling in column-major order.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nrow, ncol
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: buf(:)
integer :: i, need_n, nx
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0.0_dp
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function matrix_real

pure function matrix_int(x, nrow, ncol) result(out)
! Integer variant of matrix() with R-like recycling.
integer, intent(in) :: x(:)
integer, intent(in) :: nrow, ncol
integer, allocatable :: out(:,:)
integer, allocatable :: buf(:)
integer :: i, need_n, nx
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function matrix_int

pure function r_matmul_vv_real(a, b) result(out)
! Matrix-product helper for real vectors: dot product.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp) :: out
out = dot_product(a, b)
end function r_matmul_vv_real

pure function r_matmul_vv_int(a, b) result(out)
! Matrix-product helper for integer vectors: dot product (real result).
integer, intent(in) :: a(:), b(:)
real(kind=dp) :: out
out = dot_product(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_vv_int

pure function r_matmul_vv_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int vectors: dot product.
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: b(:)
real(kind=dp) :: out
out = dot_product(a, real(b, kind=dp))
end function r_matmul_vv_real_int

pure function r_matmul_vv_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real vectors: dot product.
integer, intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp) :: out
out = dot_product(real(a, kind=dp), b)
end function r_matmul_vv_int_real

pure function r_matmul_mv_real(a, b) result(out)
! Matrix-product helper for real matrix-vector multiplication.
real(kind=dp), intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, b)
end function r_matmul_mv_real

pure function r_matmul_mv_int(a, b) result(out)
! Matrix-product helper for integer matrix-vector multiplication.
integer, intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_mv_int

pure function r_matmul_mv_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int matrix-vector multiplication.
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_mv_real_int

pure function r_matmul_mv_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real matrix-vector multiplication.
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_mv_int_real

pure function r_matmul_vm_real(a, b) result(out)
! Matrix-product helper for real vector-matrix multiplication.
real(kind=dp), intent(in) :: a(:), b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(a, b)
end function r_matmul_vm_real

pure function r_matmul_vm_int(a, b) result(out)
! Matrix-product helper for integer vector-matrix multiplication.
integer, intent(in) :: a(:), b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_vm_int

pure function r_matmul_vm_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int vector-matrix multiplication.
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_vm_real_int

pure function r_matmul_vm_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real vector-matrix multiplication.
integer, intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_vm_int_real

pure function r_matmul_mm_real(a, b) result(out)
! Matrix-product helper for real matrix-matrix multiplication.
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, b)
end function r_matmul_mm_real

pure function r_matmul_mm_int(a, b) result(out)
! Matrix-product helper for integer matrix-matrix multiplication.
integer, intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_mm_int

pure function r_matmul_mm_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int matrix-matrix multiplication.
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_mm_real_int

pure function r_matmul_mm_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real matrix-matrix multiplication.
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_mm_int_real

function r_add_vv(a, b) result(out)
! Recycle and add two vectors (R-style recycling).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x + y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) + b(modulo(i - 1, nb) + 1)
end do
end function r_add_vv

pure function r_add_vs(a, b) result(out)
! Add scalar to vector.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a + b
end function r_add_vs

pure function r_add_sv(a, b) result(out)
! Add vector to scalar.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a + b
end function r_add_sv

function r_sub_vv(a, b) result(out)
! Recycle and subtract two vectors (a - b).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x - y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) - b(modulo(i - 1, nb) + 1)
end do
end function r_sub_vv

pure function r_sub_vs(a, b) result(out)
! Subtract scalar from vector.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a - b
end function r_sub_vs

pure function r_sub_sv(a, b) result(out)
! Subtract vector from scalar.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a - b
end function r_sub_sv

function r_mul_vv(a, b) result(out)
! Recycle and multiply two vectors.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x * y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) * b(modulo(i - 1, nb) + 1)
end do
end function r_mul_vv

pure function r_mul_vs(a, b) result(out)
! Multiply vector by scalar.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a * b
end function r_mul_vs

pure function r_mul_sv(a, b) result(out)
! Multiply scalar by vector.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a * b
end function r_mul_sv

function r_div_vv(a, b) result(out)
! Recycle and divide two vectors (a / b).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x / y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) / b(modulo(i - 1, nb) + 1)
end do
end function r_div_vv

pure function r_div_vs(a, b) result(out)
! Divide vector by scalar.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a / b
end function r_div_vs

pure function r_div_sv(a, b) result(out)
! Divide scalar by vector.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a / b
end function r_div_sv

pure function r_array_real(x, dim) result(out)
! Build 2D real array with R-like recycling.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: dim(:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol
if (size(dim) < 2) then
   allocate(out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0.0_dp
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function r_array_real

pure function r_array_int(x, dim) result(out)
! Build 2D integer array with R-like recycling.
integer, intent(in) :: x(:)
integer, intent(in) :: dim(:)
integer, allocatable :: out(:,:)
integer, allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol
if (size(dim) < 2) then
   allocate(out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function r_array_int

function r_array_char(x, dim) result(out)
! Build 2D character array with R-like recycling.
character(len=*), intent(in) :: x(:)
integer, intent(in) :: dim(:)
character(len=:), allocatable :: out(:,:)
character(len=:), allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol, lch
if (size(dim) < 2) then
   allocate(character(len=1) :: out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
lch = max(1, len(x))
if (nrow < 0 .or. ncol < 0) then
   allocate(character(len=lch) :: out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(character(len=lch) :: out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(character(len=lch) :: out(nrow, ncol))
   out = ""
   return
end if
allocate(character(len=lch) :: buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
allocate(character(len=lch) :: out(nrow, ncol))
out = reshape(buf, [nrow, ncol])
end function r_array_char

pure function numeric(n) result(out)
! Allocate a length-n real vector initialized to zero.
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
allocate(out(max(0, n)))
if (n > 0) out = 0.0_dp
end function numeric

pure elemental function pmax(a, b) result(out)
! Elementwise maximum of two real scalars.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: out
out = max(a, b)
end function pmax

pure function sd(x) result(out)
! Sample standard deviation (n-1 denominator).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out, m
integer :: n
n = size(x)
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
m = sum(x) / real(n, kind=dp)
out = sqrt(sum((x - m)**2) / real(n - 1, kind=dp))
end function sd

pure function var_vec(x) result(out)
! Sample variance (n-1 denominator).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out, m
integer :: n
n = size(x)
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
m = sum(x) / real(n, kind=dp)
out = sum((x - m)**2) / real(n - 1, kind=dp)
end function var_vec

pure function var_mat(x) result(out)
! Column covariance matrix, matching R var(matrix).
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
out = cov_mat(x)
end function var_mat

pure function r_sd(x) result(out)
! Alias for sd(x), used by transpiled code to avoid local-name collisions.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out
out = sd(x)
end function r_sd

pure function colMeans(x) result(out)
! Column means of a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(x, 1)
if (size(x, 2) <= 0) then
   allocate(out(0))
   return
end if
if (n <= 0) then
   allocate(out(size(x, 2)))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
out = sum(x, dim=1) / real(n, kind=dp)
end function colMeans

pure function cov_vec(x, y) result(out)
! Sample covariance of two vectors (n-1 denominator).
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: out, mx, my
integer :: n
n = min(size(x), size(y))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
mx = sum(x(1:n)) / real(n, kind=dp)
my = sum(y(1:n)) / real(n, kind=dp)
out = sum((x(1:n) - mx) * (y(1:n) - my)) / real(n - 1, kind=dp)
end function cov_vec

pure function cov_mat(x) result(out)
! Sample covariance matrix of columns of x (n-1 denominator).
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: xc(:,:), mu(:)
integer :: n, p
n = size(x, 1)
p = size(x, 2)
if (p <= 0) then
   allocate(out(0,0))
   return
end if
if (n <= 1) then
   allocate(out(p, p))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
mu = sum(x, dim=1) / real(n, kind=dp)
xc = x - spread(mu, dim=1, ncopies=n)
out = matmul(transpose(xc), xc) / real(n - 1, kind=dp)
end function cov_mat

pure function cor_vec(x, y) result(out)
! Sample correlation of two vectors.
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: out, sdx, sdy
integer :: n
n = min(size(x), size(y))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
sdx = sd(x(1:n))
sdy = sd(y(1:n))
if (.not. ieee_is_finite(sdx) .or. .not. ieee_is_finite(sdy) .or. sdx <= 0.0_dp .or. sdy <= 0.0_dp) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
out = cov_vec(x(1:n), y(1:n)) / (sdx * sdy)
end function cor_vec

pure function cor_mat(x) result(out)
! Sample correlation matrix of columns of x.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:), s(:)
real(kind=dp), allocatable :: c(:,:)
integer :: i, j, p
c = cov_mat(x)
p = size(c, 1)
if (p <= 0) then
   allocate(out(0,0))
   return
end if
allocate(out(p, p), s(p))
do i = 1, p
   s(i) = sqrt(c(i, i))
end do
do i = 1, p
   do j = 1, p
      if (.not. ieee_is_finite(s(i)) .or. .not. ieee_is_finite(s(j)) .or. s(i) <= 0.0_dp .or. s(j) <= 0.0_dp) then
         out(i, j) = ieee_value(0.0_dp, ieee_quiet_nan)
      else
         out(i, j) = c(i, j) / (s(i) * s(j))
      end if
   end do
end do
end function cor_mat

pure function cor_mat_pair(x, y) result(out)
! Sample column-pair correlation matrix between x and y.
real(kind=dp), intent(in) :: x(:,:), y(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, n, px, py
n = min(size(x, 1), size(y, 1))
px = size(x, 2)
py = size(y, 2)
allocate(out(px, py))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
do i = 1, px
   do j = 1, py
      out(i, j) = cor_vec(x(1:n, i), y(1:n, j))
   end do
end do
end function cor_mat_pair

pure integer function count_ws_tokens(line) result(n_tok)
! Count whitespace-separated tokens in one text line.
character(len=*), intent(in) :: line
integer :: i, n
logical :: in_tok
n = len_trim(line)
n_tok = 0
in_tok = .false.
do i = 1, n
   if (line(i:i) /= " " .and. line(i:i) /= char(9)) then
      if (.not. in_tok) then
         n_tok = n_tok + 1
         in_tok = .true.
      end if
   else
      in_tok = .false.
   end if
end do
end function count_ws_tokens

subroutine read_real_vector(file_path, x)
! Read one real value per line into a vector.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: fp, ios, n, cap, new_cap
real(kind=dp) :: v
n = 0
cap = 0
open(newunit=fp, file=file_path, status="old", action="read")
do
   read(fp, *, iostat=ios) v
   if (ios /= 0) exit
   if (n == cap) then
      new_cap = merge(1024, 2 * cap, cap == 0)
      block
         real(kind=dp), allocatable :: tmp(:)
         allocate(tmp(new_cap))
         if (allocated(x) .and. n > 0) tmp(1:n) = x(1:n)
         call move_alloc(tmp, x)
      end block
      cap = new_cap
   end if
   n = n + 1
   x(n) = v
end do
close(fp)
if (n == 0) then
   allocate(x(0))
else if (n < size(x)) then
   x = x(1:n)
end if
end subroutine read_real_vector

subroutine read_table_real_matrix(file_path, x)
! Read a whitespace-delimited numeric table into a matrix.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:,:)
integer :: fp, ios, nrow, ncol, i
character(len=4096) :: line
nrow = 0
ncol = 0
open(newunit=fp, file=file_path, status="old", action="read")
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   nrow = nrow + 1
   if (ncol == 0) ncol = count_ws_tokens(line)
end do
if (nrow <= 0 .or. ncol <= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
allocate(x(nrow, ncol))
rewind(fp)
i = 0
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   i = i + 1
   read(line, *) x(i, 1:ncol)
end do
close(fp)
end subroutine read_table_real_matrix

subroutine read_csv_real_matrix(file_path, x)
! Read a comma-delimited numeric CSV with one header row into a matrix.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:,:)
integer :: fp, ios, nrow, ncol, i, k
character(len=4096) :: line
nrow = 0
ncol = 0
open(newunit=fp, file=file_path, status="old", action="read")
read(fp, "(A)", iostat=ios) line
if (ios /= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
ncol = 1
do k = 1, len_trim(line)
   if (line(k:k) == ",") ncol = ncol + 1
end do
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   nrow = nrow + 1
end do
if (nrow <= 0 .or. ncol <= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
allocate(x(nrow, ncol))
rewind(fp)
read(fp, "(A)", iostat=ios) line
i = 0
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   do k = 1, len_trim(line)
      if (line(k:k) == ",") line(k:k) = " "
   end do
   i = i + 1
   read(line, *) x(i, 1:ncol)
end do
close(fp)
end subroutine read_csv_real_matrix

subroutine write_table_real_matrix(file_path, x)
! Write a numeric matrix as a whitespace-delimited table.
character(len=*), intent(in) :: file_path
real(kind=dp), intent(in) :: x(:,:)
integer :: fp, i
open(newunit=fp, file=file_path, status="replace", action="write")
do i = 1, size(x, 1)
   write(fp, *) x(i, 1:size(x, 2))
end do
close(fp)
end subroutine write_table_real_matrix

subroutine print_matrix_real(x, int_like)
! Print a real matrix row-by-row; use integer format when all values are integer-like.
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: int_like
logical :: use_int_like, all_int
integer :: i, j
integer(kind=int64) :: k
real(kind=dp) :: r, tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
all_int = .false.
if (use_int_like) then
   all_int = .true.
   do i = 1, size(x, 1)
      do j = 1, size(x, 2)
         r = x(i, j)
         if (.not. ieee_is_finite(r)) then
            all_int = .false.
            exit
         end if
         if (abs(r) > real(huge(0_int64), kind=dp)) then
            all_int = .false.
            exit
         end if
         k = nint(r, kind=int64)
         tol = print_int_like_tol * max(1.0_dp, abs(r))
         if (abs(r - real(k, kind=dp)) > tol) then
            all_int = .false.
            exit
         end if
      end do
      if (.not. all_int) exit
   end do
end if
if (all_int) then
   do i = 1, size(x, 1)
      do j = 1, size(x, 2)
         k = nint(x(i, j), kind=int64)
         write(*,"(i0)", advance="no") k
         if (j < size(x, 2)) write(*,"(a)", advance="no") " "
      end do
      write(*,*)
   end do
else
   do i = 1, size(x, 1)
      write(*,"(*(g0,1x))") x(i, :)
   end do
end if
end subroutine print_matrix_real

subroutine print_matrix_rstyle_real(x)
! Print a real matrix with R-like column and row labels.
real(kind=dp), intent(in) :: x(:,:)
integer :: i
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   write(*,'("[,",i0,"]",8x)', advance='no') i
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   write(*,'(*(es12.5,1x))') x(i, :)
end do
end subroutine print_matrix_rstyle_real

subroutine print_matrix_rstyle_named(x, names, int_cols)
! Print a real matrix with R-like row labels and provided column names.
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in) :: names(:)
logical, intent(in), optional :: int_cols(:)
integer :: i, j
logical :: as_int_col
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   if (i <= size(names)) then
      write(*,'(a12,1x)', advance='no') trim(names(i))
   else
      write(*,'("[,",i0,"]",8x)', advance='no') i
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   do j = 1, size(x, 2)
      as_int_col = .false.
      if (present(int_cols)) then
         if (j <= size(int_cols)) as_int_col = int_cols(j)
      end if
      if (as_int_col .and. ieee_is_finite(x(i, j))) then
         write(*,'(i12,1x)', advance='no') nint(x(i, j))
      else
         write(*,'(f12.4,1x)', advance='no') x(i, j)
      end if
   end do
   write(*,*)
end do
end subroutine print_matrix_rstyle_named

subroutine print_matrix_int(x)
! Print an integer matrix row-by-row.
integer, intent(in) :: x(:,:)
integer :: i
do i = 1, size(x, 1)
   write(*,"(*(i0,1x))") x(i, :)
end do
end subroutine print_matrix_int

subroutine print_matrix_rstyle_int(x)
! Print an integer matrix with R-like column and row labels.
integer, intent(in) :: x(:,:)
integer :: i
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   write(*,'("[,",i0,"]",8x)', advance='no') i
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   write(*,'(*(i12,1x))') x(i, :)
end do
end subroutine print_matrix_rstyle_int

pure function lm_predict_general(fit, xpred) result(yhat)
! Predict responses for a fitted linear model.
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), allocatable :: yhat(:)
integer :: p
p = size(xpred, 2)
if (size(fit%coef) /= p + 1) error stop "error: predictor count mismatch"
allocate(yhat(size(xpred, 1)))
yhat = fit%coef(1) + matmul(xpred, fit%coef(2:p+1))
end function lm_predict_general

subroutine solve_linear(a, b, x, ok)
! Solve Ax=b by Gaussian elimination with partial pivoting.
real(kind=dp), intent(inout) :: a(:,:), b(:)
real(kind=dp), intent(out) :: x(:)
logical, intent(out) :: ok
integer :: i, j, k, p, n
real(kind=dp) :: piv, fac, t
ok = .true.
n = size(b)
if (size(a,1) /= n .or. size(a,2) /= n .or. size(x) /= n) then
   ok = .false.
   x = 0.0_dp
   return
end if
do k = 1, n
   p = k
   piv = abs(a(k,k))
   do i = k + 1, n
      if (abs(a(i,k)) > piv) then
         p = i
         piv = abs(a(i,k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) then
      ok = .false.
      x = 0.0_dp
      return
   end if
   if (p /= k) then
      do j = k, n
         t = a(k,j)
         a(k,j) = a(p,j)
         a(p,j) = t
      end do
      t = b(k)
      b(k) = b(p)
      b(p) = t
   end if
   do i = k + 1, n
      fac = a(i,k) / a(k,k)
      a(i,k:n) = a(i,k:n) - fac * a(k,k:n)
      b(i) = b(i) - fac * b(k)
   end do
end do
x(n) = b(n) / a(n,n)
do i = n - 1, 1, -1
   if (i < n) then
      x(i) = (b(i) - sum(a(i,i+1:n) * x(i+1:n))) / a(i,i)
   else
      x(i) = b(i) / a(i,i)
   end if
end do
end subroutine solve_linear

function lm_fit_general(y, xpred) result(fit)
! Fit linear regression with intercept and arbitrary predictors.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
type(lm_fit_t) :: fit
integer :: i, j, n, p, k, dof
real(kind=dp), allocatable :: a(:,:), b(:), beta(:)
real(kind=dp) :: ybar, sse, sst
logical :: ok
if (size(y) /= size(xpred,1)) then
   error stop "error: need size(y) == size(xpred,1)"
end if
n = size(y)
p = size(xpred,2)
k = p + 1
if (n < k) error stop "error: need n >= number of parameters"
allocate(a(k,k), b(k), beta(k))
a = 0.0_dp
b = 0.0_dp
a(1,1) = n
b(1) = sum(y)
do j = 1, p
   a(1,j+1) = sum(xpred(:,j))
   a(j+1,1) = a(1,j+1)
   b(j+1) = sum(xpred(:,j) * y)
end do
do i = 1, p
   do j = i, p
      a(i+1,j+1) = sum(xpred(:,i) * xpred(:,j))
      a(j+1,i+1) = a(i+1,j+1)
   end do
end do
call solve_linear(a, b, beta, ok)
if (.not. ok) error stop "error: singular normal equations"
fit%coef = beta
fit%fitted = beta(1) + matmul(xpred, beta(2:k))
fit%resid = y - fit%fitted
sse = sum(fit%resid**2)
ybar = sum(y) / n
sst = sum((y - ybar)**2)
if (sst > 0.0_dp) then
   fit%r_squared = 1.0_dp - sse / sst
else
   fit%r_squared = 0.0_dp
end if
dof = max(1, n - k)
fit%sigma = sqrt(sse / dof)
fit%adj_r_squared = 1.0_dp - (1.0_dp - fit%r_squared) * (n - 1) / dof
end function lm_fit_general

function lm_coef(y, xpred) result(coef)
! Fit linear model and return only coefficient vector.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
real(kind=dp), allocatable :: coef(:)
type(lm_fit_t) :: fit
fit = lm_fit_general(y, xpred)
coef = fit%coef
end function lm_coef

pure function polyroot(coef) result(root_mod)
! Return moduli of roots for coefficients in R polyroot order.
real(kind=dp), intent(in) :: coef(:)
real(kind=dp), allocatable :: root_mod(:)
integer :: n, i, j, iter
complex(kind=dp), allocatable :: z(:), znew(:)
complex(kind=dp) :: pz, denom
real(kind=dp) :: theta, max_delta
real(kind=dp), parameter :: twopi = 6.2831853071795864769252867665590058_dp
n = size(coef) - 1
allocate(root_mod(max(0, n)))
if (n <= 0) return
if (abs(coef(n + 1)) <= tiny(1.0_dp)) then
   root_mod = huge(1.0_dp)
   return
end if
allocate(z(n), znew(n))
do i = 1, n
   theta = twopi * real(i - 1, kind=dp) / real(n, kind=dp)
   z(i) = cmplx(cos(theta), sin(theta), kind=dp)
end do
do iter = 1, 200
   max_delta = 0.0_dp
   znew = z
   do i = 1, n
      pz = cmplx(coef(n + 1), 0.0_dp, kind=dp)
      do j = n, 1, -1
         pz = pz * z(i) + cmplx(coef(j), 0.0_dp, kind=dp)
      end do
      denom = cmplx(1.0_dp, 0.0_dp, kind=dp)
      do j = 1, n
         if (j /= i) denom = denom * (z(i) - z(j))
      end do
      if (abs(denom) > tiny(1.0_dp)) znew(i) = z(i) - (pz / cmplx(coef(n + 1), 0.0_dp, kind=dp)) / denom
      max_delta = max(max_delta, abs(znew(i) - z(i)))
   end do
   z = znew
   if (max_delta <= 100.0_dp * epsilon(1.0_dp)) exit
end do
root_mod = abs(z)
end function polyroot

subroutine print_lm_summary(fit)
! Print a compact summary of fitted linear model diagnostics.
type(lm_fit_t), intent(in) :: fit
write(*,'(a)') "lm summary:"
write(*,'(a,*(1x,g0))') "coef:", fit%coef
write(*,'(a,g0)') "sigma:", fit%sigma
write(*,'(a,g0)') "r.squared:", fit%r_squared
write(*,'(a,g0)') "adj.r.squared:", fit%adj_r_squared
end subroutine print_lm_summary

subroutine print_lm_coef_rstyle(fit, term_names)
! Print coefficients with R-like aligned header/value rows.
type(lm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
integer :: j, p
character(len=32) :: lbl
p = max(0, size(fit%coef) - 1)
write(*,'(a14)', advance='no') "(Intercept)"
do j = 1, p
   if (present(term_names) .and. size(term_names) >= j) then
      write(*,'(a14)', advance='no') trim(term_names(j))
   else
      write(lbl,'(a,i0)') "x", j
      write(*,'(a14)', advance='no') trim(lbl)
   end if
end do
write(*,*)
if (size(fit%coef) > 0) then
   write(*,'(*(f14.7))') fit%coef
else
   write(*,*)
end if
end subroutine print_lm_coef_rstyle

pure function normal_cdf(x) result(p)
real(kind=dp), intent(in) :: x
real(kind=dp) :: p
p = 0.5_dp * (1.0_dp + erf(x / sqrt(2.0_dp)))
end function normal_cdf

pure function chisq_upper_tail_approx(x, df) result(p)
real(kind=dp), intent(in) :: x, df
real(kind=dp) :: p, z
if (df <= 0.0_dp) then
   p = 1.0_dp
   return
end if
if (x <= 0.0_dp) then
   p = 1.0_dp
   return
end if
if (abs(df - 1.0_dp) <= epsilon(1.0_dp)) then
   p = erfc(sqrt(0.5_dp * x))
   return
end if
z = ((x / df)**(1.0_dp / 3.0_dp) - (1.0_dp - 2.0_dp / (9.0_dp * df))) / &
   & sqrt(2.0_dp / (9.0_dp * df))
p = max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(z)))
end function chisq_upper_tail_approx

pure function chisq_test_real_vec(x, p) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: p(:)
type(chisq_test_result_t) :: out
real(kind=dp), allocatable :: expected(:), prob(:)
real(kind=dp) :: total, psum
integer :: n
n = size(x)
if (n <= 1) return
total = sum(x)
if (total <= 0.0_dp) return
if (present(p)) then
   if (size(p) /= n) return
   psum = sum(p)
   if (psum <= 0.0_dp) return
   prob = p / psum
else
   allocate(prob(n))
   prob = 1.0_dp / real(n, kind=dp)
end if
expected = total * prob
if (any(expected <= 0.0_dp)) return
out%statistic = sum((x - expected)**2 / expected)
out%parameter = n - 1
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 1
end function chisq_test_real_vec

pure function chisq_test_int_vec(x, p) result(out)
integer, intent(in) :: x(:)
real(kind=dp), intent(in), optional :: p(:)
type(chisq_test_result_t) :: out
if (present(p)) then
   out = chisq_test_real_vec(real(x, kind=dp), p)
else
   out = chisq_test_real_vec(real(x, kind=dp))
end if
end function chisq_test_int_vec

pure function chisq_test_int_mat(x) result(out)
integer, intent(in) :: x(:,:)
type(chisq_test_result_t) :: out
out = chisq_test_real_mat(real(x, kind=dp))
end function chisq_test_int_mat

pure function chisq_test_real_mat(x) result(out)
real(kind=dp), intent(in) :: x(:,:)
type(chisq_test_result_t) :: out
real(kind=dp), allocatable :: xr(:,:), row_tot(:), col_tot(:), expected(:,:)
real(kind=dp) :: total
integer :: nr, nc, i, j
nr = size(x, 1)
nc = size(x, 2)
if (nr <= 1 .or. nc <= 1) return
xr = x
total = sum(xr)
if (total <= 0.0_dp) return
allocate(row_tot(nr), col_tot(nc), expected(nr, nc))
do i = 1, nr
   row_tot(i) = sum(xr(i, :))
end do
do j = 1, nc
   col_tot(j) = sum(xr(:, j))
end do
do i = 1, nr
   do j = 1, nc
      expected(i, j) = row_tot(i) * col_tot(j) / total
   end do
end do
if (any(expected <= 0.0_dp)) return
out%statistic = sum((xr - expected)**2 / expected)
out%parameter = (nr - 1) * (nc - 1)
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 2
end function chisq_test_real_mat

pure function prop_test_int_scalar(x, n, p, correct) result(out)
integer, intent(in) :: x, n
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
out = prop_test_real_scalar(real(x, kind=dp), real(n, kind=dp), p, correct)
end function prop_test_int_scalar

pure function prop_test_real_scalar(x, n, p, correct) result(out)
real(kind=dp), intent(in) :: x, n
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
real(kind=dp) :: p0, diff, denom
logical :: use_correct
p0 = 0.5_dp
if (present(p)) p0 = p
if (n <= 0.0_dp .or. p0 <= 0.0_dp .or. p0 >= 1.0_dp) return
use_correct = .false.
if (present(correct)) use_correct = correct
out%estimate = x / n
out%null_value = p0
out%parameter = 1
diff = abs(x - n * p0)
if (use_correct) diff = max(0.0_dp, diff - 0.5_dp)
denom = n * p0 * (1.0_dp - p0)
if (denom > 0.0_dp) out%statistic = diff * diff / denom
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 1
end function prop_test_real_scalar

pure function prop_test_int_vec(x, n, p, correct) result(out)
integer, intent(in) :: x(:), n(:)
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
out = prop_test_real_vec(real(x, kind=dp), real(n, kind=dp), p, correct)
end function prop_test_int_vec

pure function prop_test_real_vec(x, n, p, correct) result(out)
real(kind=dp), intent(in) :: x(:), n(:)
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
real(kind=dp) :: p0, pooled, denom, diff
integer :: k, i
logical :: use_correct
k = size(x)
if (k /= size(n) .or. k < 1) return
if (any(n <= 0.0_dp)) return
use_correct = .false.
if (present(correct)) use_correct = correct
if (k == 1) then
   if (present(p)) then
      out = prop_test_real_scalar(x(1), n(1), p, correct)
   else
      out = prop_test_real_scalar(x(1), n(1), correct=correct)
   end if
   return
end if
out%parameter = k - 1
out%estimate = x(1) / n(1)
if (k >= 2) out%estimate2 = x(2) / n(2)
pooled = sum(x) / sum(n)
out%null_value = pooled
if (pooled <= 0.0_dp .or. pooled >= 1.0_dp) return
do i = 1, k
   p0 = x(i) / n(i)
   diff = abs(p0 - pooled)
   if (use_correct .and. k == 2) diff = max(0.0_dp, diff - 0.5_dp / n(i))
   denom = pooled * (1.0_dp - pooled) / n(i)
   if (denom > 0.0_dp) out%statistic = out%statistic + diff * diff / denom
end do
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 2
end function prop_test_real_vec

pure function t_test_one(x, mu) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: mu
type(t_test_result_t) :: out
real(kind=dp) :: xbar, s, mu0
integer :: n
n = size(x)
mu0 = 0.0_dp
if (present(mu)) mu0 = mu
if (n <= 1) return
xbar = sum(x) / real(n, kind=dp)
s = sd(x)
out%stderr = s / sqrt(real(n, kind=dp))
out%estimate = xbar
out%null_value = mu0
out%parameter = real(n - 1, kind=dp)
if (out%stderr > 0.0_dp) out%statistic = (xbar - mu0) / out%stderr
out%p_value = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(out%statistic))))
out%conf_low = xbar - 1.96_dp * out%stderr
out%conf_high = xbar + 1.96_dp * out%stderr
out%method = 1
end function t_test_one

pure function t_test_two(x, y, paired, var_equal) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
logical, intent(in), optional :: paired, var_equal
type(t_test_result_t) :: out
logical :: is_paired, is_equal
real(kind=dp) :: nx, ny, mx, my, vx, vy, sp2, denom
real(kind=dp), allocatable :: d(:)
is_paired = .false.
if (present(paired)) is_paired = paired
is_equal = .false.
if (present(var_equal)) is_equal = var_equal
if (is_paired) then
   if (size(x) /= size(y)) return
   d = x - y
   out = t_test_one(d, 0.0_dp)
   out%method = 4
   return
end if
if (size(x) <= 1 .or. size(y) <= 1) return
nx = real(size(x), kind=dp)
ny = real(size(y), kind=dp)
mx = sum(x) / nx
my = sum(y) / ny
vx = var(x)
vy = var(y)
out%estimate = mx
out%estimate2 = my
out%null_value = 0.0_dp
if (is_equal) then
   sp2 = ((nx - 1.0_dp) * vx + (ny - 1.0_dp) * vy) / (nx + ny - 2.0_dp)
   out%stderr = sqrt(sp2 * (1.0_dp / nx + 1.0_dp / ny))
   out%parameter = nx + ny - 2.0_dp
   out%method = 3
else
   out%stderr = sqrt(vx / nx + vy / ny)
   denom = (vx / nx)**2 / max(1.0_dp, nx - 1.0_dp) + (vy / ny)**2 / max(1.0_dp, ny - 1.0_dp)
   if (denom > 0.0_dp) out%parameter = (vx / nx + vy / ny)**2 / denom
   out%method = 2
end if
if (out%stderr > 0.0_dp) out%statistic = ((mx - my) - out%null_value) / out%stderr
out%p_value = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(out%statistic))))
out%conf_low = (mx - my) - 1.96_dp * out%stderr
out%conf_high = (mx - my) + 1.96_dp * out%stderr
end function t_test_two

pure function t_test_p_value_one(x, mu) result(p)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: mu
real(kind=dp) :: p
type(t_test_result_t) :: fit
fit = t_test_one(x, mu)
p = fit%p_value
end function t_test_p_value_one

pure function t_test_p_value_two(x, y, paired, var_equal) result(p)
real(kind=dp), intent(in) :: x(:), y(:)
logical, intent(in), optional :: paired, var_equal
real(kind=dp) :: p
type(t_test_result_t) :: fit
fit = t_test_two(x, y, paired, var_equal)
p = fit%p_value
end function t_test_p_value_two

subroutine print_t_test(fit)
type(t_test_result_t), intent(in) :: fit
select case (fit%method)
case (1)
   write(*,'(a)') "One Sample t-test"
case (2)
   write(*,'(a)') "Welch Two Sample t-test"
case (3)
   write(*,'(a)') "Two Sample t-test"
case (4)
   write(*,'(a)') "Paired t-test"
case default
   write(*,'(a)') "t-test"
end select
write(*,'(a,g0,a,g0,a,g0)') "t = ", fit%statistic, ", df = ", fit%parameter, ", p-value = ", fit%p_value
write(*,'(a,g0,a,g0)') "95 percent confidence interval: ", fit%conf_low, " ", fit%conf_high
if (fit%method == 2 .or. fit%method == 3) then
   write(*,'(a,g0,a,g0)') "sample estimates: ", fit%estimate, " ", fit%estimate2
else
   write(*,'(a,g0)') "sample estimate: ", fit%estimate
end if
end subroutine print_t_test

subroutine print_chisq_test(fit)
type(chisq_test_result_t), intent(in) :: fit
select case (fit%method)
case (1)
   write(*,'(a)') "Chi-squared goodness-of-fit test"
case (2)
   write(*,'(a)') "Pearson's Chi-squared test"
case default
   write(*,'(a)') "Chi-squared test"
end select
write(*,'(a,g0,a,i0,a,g0)') "X-squared = ", fit%statistic, ", df = ", fit%parameter, &
   & ", p-value = ", fit%p_value
end subroutine print_chisq_test

subroutine print_prop_test(fit)
type(prop_test_result_t), intent(in) :: fit
select case (fit%method)
case (1)
   write(*,'(a)') "1-sample proportions test"
case (2)
   write(*,'(a)') "2-sample test for equality of proportions"
case default
   write(*,'(a)') "proportions test"
end select
write(*,'(a,g0,a,i0,a,g0)') "X-squared = ", fit%statistic, ", df = ", fit%parameter, &
   & ", p-value = ", fit%p_value
if (fit%method == 2) then
   write(*,'(a,g0,a,g0)') "sample estimates: ", fit%estimate, " ", fit%estimate2
else
   write(*,'(a,g0)') "sample estimate: ", fit%estimate
   write(*,'(a,g0)') "null value: ", fit%null_value
end if
end subroutine print_prop_test

end module r_mod
