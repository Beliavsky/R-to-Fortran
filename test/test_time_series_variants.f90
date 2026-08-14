program test_time_series_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, &
   ieee_quiet_nan, ieee_value
use r_mod, only: acf_fit_t, ARMAacf, ar_fit, ar_fit_t, decompose, &
   decompose_result_t, dp, r_acf, r_acf_values, r_ccf
implicit none

real(kind=dp), parameter :: tolerance = 2.0e-10_dp
real(kind=dp) :: matrix_series(4, 2), with_missing(4), sample(8)
real(kind=dp), allocatable :: values(:)
type(acf_fit_t) :: fit
type(ar_fit_t) :: ar_model
type(decompose_result_t) :: decomposition

matrix_series(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
matrix_series(:, 2) = [4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
fit = r_acf(matrix_series, lag_max=2, plot=.false.)
if (any(shape(fit%acf) /= [3, 2, 2])) error stop "matrix ACF shape failed"
call assert_close(fit%acf(1, 1, 1), 1.0_dp, "first lag-zero autocorrelation")
call assert_close(fit%acf(1, 2, 2), 1.0_dp, "second lag-zero autocorrelation")
call assert_close(fit%acf(1, 1, 2), -1.0_dp, "lag-zero cross-correlation")
values = r_acf_values(matrix_series, lag_max=2, plot=.false.)
if (size(values) /= 12) error stop "flattened matrix ACF shape failed"
if (any(abs(values - reshape(fit%acf, [12])) > tolerance)) &
   error stop "flattened matrix ACF values failed"

fit = r_acf([5.0_dp, 5.0_dp, 5.0_dp], lag_max=99, plot=.false.)
if (size(fit%lag) /= 3) error stop "ACF lag clamping failed"
if (any(.not. ieee_is_nan(fit%acf(:, 1, 1)))) &
   error stop "constant-series ACF failed"

with_missing = [1.0_dp, ieee_value(0.0_dp, ieee_quiet_nan), 3.0_dp, 4.0_dp]
fit = r_acf(with_missing, lag_max=1, plot=.false.)
if (.not. ieee_is_finite(fit%acf(1, 1, 1))) error stop "missing-value ACF failed"

fit = r_ccf(matrix_series(:, 1), matrix_series(:, 1), lag_max=1, &
   type="covariance", plot=.false.)
if (fit%type_code /= 2) error stop "CCF covariance type failed"
call assert_vector_close(fit%acf(:, 1, 1), &
   [0.3125_dp, 1.25_dp, 0.3125_dp], "CCF covariance values")
fit = r_ccf([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [1.0_dp, 2.0_dp, 3.0_dp], &
   lag_max=99, plot=.false.)
if (fit%n_used /= 3 .or. size(fit%lag) /= 5) error stop "unequal-length CCF failed"

values = ARMAacf(ar=[0.4_dp], ma=[0.3_dp], lag_max=8)
if (size(values) /= 9 .or. abs(values(1) - 1.0_dp) > tolerance) &
   error stop "mixed ARMA ACF shape failed"
if (any(.not. ieee_is_finite(values)) .or. any(abs(values) > 1.0_dp + tolerance)) &
   error stop "mixed ARMA ACF bounds failed"
if (size(ARMAacf(ar=[0.5_dp], lag_max=-2)) /= 1) &
   error stop "negative ARMA lag clamp failed"

sample = [1.0_dp, 1.5_dp, 2.2_dp, 2.8_dp, 3.1_dp, 3.7_dp, 4.0_dp, 4.5_dp]
ar_model = ar_fit(sample, order_max=5, aic=.true., method="burg")
if (ar_model%order /= 2 .or. size(ar_model%ar) /= 2) &
   error stop "AR order clamp failed"
if (size(ar_model%aic) /= 6 .or. any(.not. ieee_is_finite(ar_model%ar))) &
   error stop "higher-order AR result failed"
ar_model = ar_fit([2.0_dp], order_max=3)
if (ar_model%order /= 0 .or. size(ar_model%ar) /= 0 .or. &
   size(ar_model%aic) /= 1) error stop "single-value AR fit failed"
ar_model = ar_fit([real(kind=dp) ::], order_max=3)
if (ar_model%order /= 0 .or. size(ar_model%ar) /= 0) error stop "empty AR fit failed"

decomposition = decompose([1.0_dp, 2.0_dp, 3.0_dp], &
   type="multiplicative", frequency=5)
if (size(decomposition%figure) /= 5) error stop "large-frequency decomposition failed"
call assert_vector_close(decomposition%trend + decomposition%seasonal + &
   decomposition%random, [1.0_dp, 2.0_dp, 3.0_dp], &
   "large-frequency decomposition reconstruction")
decomposition = decompose([real(kind=dp) ::], frequency=4)
if (size(decomposition%trend) /= 0 .or. size(decomposition%seasonal) /= 0 .or. &
   size(decomposition%random) /= 0 .or. size(decomposition%figure) /= 4) &
   error stop "empty decomposition shape failed"
if (any(decomposition%figure /= 0.0_dp)) error stop "empty decomposition figure failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > tolerance) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_vector_close
end program test_time_series_variants
