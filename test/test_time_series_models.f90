program test_time_series_models
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
use r_mod, only: ARMAacf, ar_fit, ar_fit_t, decompose, decompose_result_t, dp
implicit none

real(kind=dp), parameter :: tolerance = 2.0e-8_dp
real(kind=dp), allocatable :: acf_values(:)
real(kind=dp) :: series(12)
type(ar_fit_t) :: ar_model
type(decompose_result_t) :: components
integer :: i

acf_values = ARMAacf(ar=[0.5_dp], lag_max=5)
call assert_vector_close(acf_values, [1.0_dp, 0.5_dp, 0.25_dp, 0.125_dp, 0.0625_dp, 0.03125_dp], &
   "AR(1) theoretical autocorrelation")

acf_values = ARMAacf(ma=[0.5_dp], lag_max=4)
call assert_vector_close(acf_values, [1.0_dp, 0.4_dp, 0.0_dp, 0.0_dp, 0.0_dp], &
   "MA(1) theoretical autocorrelation")
acf_values = ARMAacf(ma=0.5_dp, lag_max=2)
call assert_vector_close(acf_values, [1.0_dp, 0.4_dp, 0.0_dp], &
   "scalar MA theoretical autocorrelation")
acf_values = ARMAacf(lag_max=3)
call assert_vector_close(acf_values, [1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], &
   "white-noise theoretical autocorrelation")

series = [11.0_dp, 9.0_dp, 11.0_dp, 9.0_dp, 11.0_dp, 9.0_dp, &
   11.0_dp, 9.0_dp, 11.0_dp, 9.0_dp, 11.0_dp, 9.0_dp]
components = decompose(series, type="additive", frequency=2)
call assert_decomposition_shape(components, size(series), 2)
call assert_vector_close(components%trend + components%seasonal + components%random, series, &
   "decomposition reconstruction")
call assert_close(sum(components%figure), 0.0_dp, "centered seasonal figure")
do i = 1, size(series)
   call assert_close(components%seasonal(i), components%figure(mod(i - 1, 2) + 1), &
      "repeated seasonal figure")
end do

components = decompose(series, frequency=1)
call assert_decomposition_shape(components, size(series), 1)
call assert_vector_close(components%trend, series, "frequency-one trend")
call assert_vector_close(components%seasonal, spread(0.0_dp, 1, size(series)), "frequency-one seasonal")
call assert_vector_close(components%random, spread(0.0_dp, 1, size(series)), "frequency-one residual")

do i = 1, size(series)
   series(i) = 2.0_dp + 0.75_dp**real(i, kind=dp)
end do
ar_model = ar_fit(series, order_max=1, aic=.false., method="yw")
if (ar_model%order /= 1) error stop "AR fit order failed"
if (size(ar_model%ar) /= 1) error stop "AR coefficient shape failed"
if (size(ar_model%aic) /= 2) error stop "AR AIC shape failed"
if (.not. all(ieee_is_finite(ar_model%ar))) error stop "AR coefficient finiteness failed"
if (.not. ieee_is_finite(ar_model%var_pred) .or. ar_model%var_pred < 0.0_dp) &
   error stop "AR prediction variance failed"

ar_model = ar_fit(series, order_max=0)
if (ar_model%order /= 0 .or. size(ar_model%ar) /= 0) error stop "zero-order AR fit failed"
call assert_close(ar_model%var_pred, 0.0_dp, "zero-order AR variance")

contains

subroutine assert_decomposition_shape(actual, n, frequency)
type(decompose_result_t), intent(in) :: actual
integer, intent(in) :: n, frequency

if (size(actual%trend) /= n) error stop "decomposition trend shape failed"
if (size(actual%seasonal) /= n) error stop "decomposition seasonal shape failed"
if (size(actual%random) /= n) error stop "decomposition residual shape failed"
if (size(actual%figure) /= frequency) error stop "decomposition figure shape failed"
end subroutine assert_decomposition_shape

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_vector_close
end program test_time_series_models
