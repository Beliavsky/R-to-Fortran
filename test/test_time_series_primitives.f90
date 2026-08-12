program test_time_series_primitives
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: acf_fit_t, dp, r_acf, r_acf_values, r_ccf, r_filter_linear, runmed
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: x(4)
real(kind=dp), allocatable :: values(:)
type(acf_fit_t) :: fit

x = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
fit = r_acf(x, lag_max=2, plot=.false.)
if (fit%n_used /= 4) error stop "autocorrelation observation count failed"
if (fit%type_code /= 1) error stop "autocorrelation type code failed"
call assert_vector_close(fit%lag, [0.0_dp, 1.0_dp, 2.0_dp], "autocorrelation lags")
call assert_vector_close(fit%acf(:, 1, 1), [1.0_dp, 0.25_dp, -0.3_dp], &
   "autocorrelation values")

fit = r_acf(x, lag_max=2, type="covariance", plot=.false.)
if (fit%type_code /= 2) error stop "autocovariance type code failed"
call assert_vector_close(fit%acf(:, 1, 1), [1.25_dp, 0.3125_dp, -0.375_dp], &
   "autocovariance values")
values = r_acf_values(x, lag_max=2, plot=.false.)
call assert_vector_close(values, [1.0_dp, 0.25_dp, -0.3_dp], &
   "flattened autocorrelation values")

fit = r_ccf(x, x, lag_max=1, plot=.false.)
call assert_vector_close(fit%lag, [-1.0_dp, 0.0_dp, 1.0_dp], "cross-correlation lags")
call assert_vector_close(fit%acf(:, 1, 1), [0.25_dp, 1.0_dp, 0.25_dp], &
   "cross-correlation values")

values = r_filter_linear([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], &
   [0.5_dp, 0.5_dp], sides=1)
if (.not. ieee_is_nan(values(1))) error stop "one-sided filter leading boundary failed"
call assert_vector_close(values(2:5), [1.5_dp, 2.5_dp, 3.5_dp, 4.5_dp], &
   "one-sided moving average")

values = r_filter_linear([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], &
   [1.0_dp, 1.0_dp, 1.0_dp] / 3.0_dp, sides=2)
if (.not. ieee_is_nan(values(1)) .or. .not. ieee_is_nan(values(5))) &
   error stop "two-sided filter boundaries failed"
call assert_vector_close(values(2:4), [2.0_dp, 3.0_dp, 4.0_dp], &
   "two-sided moving average")

values = runmed([1.0_dp, 100.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], 3, endrule="keep")
call assert_vector_close(values, [1.0_dp, 2.0_dp, 3.0_dp, 3.0_dp, 4.0_dp], &
   "running median")
values = runmed([3.0_dp, 1.0_dp, 2.0_dp], 1)
call assert_vector_close(values, [3.0_dp, 1.0_dp, 2.0_dp], "unit running median")

contains

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close
end program test_time_series_primitives
