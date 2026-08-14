program test_arima_helpers
use r_mod, only: arima_fit, arima_fit_t, arima_predict, arima_predict_result, &
   arima_predict_result_t, arima_sim, dp, set_seed_int
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
implicit none

real(kind=dp), allocatable :: first(:), second(:), prediction(:)
type(arima_fit_t) :: fit
type(arima_predict_result_t) :: forecast

call set_seed_int(501)
first = arima_sim(0.6_dp, -0.2_dp, 30)
call set_seed_int(501)
second = arima_sim(0.6_dp, -0.2_dp, 30)
call assert_reproducible(first, second, "scalar ARMA simulation")

call set_seed_int(502)
first = arima_sim([0.5_dp, -0.1_dp], 0.25_dp, 25)
call set_seed_int(502)
second = arima_sim([0.5_dp, -0.1_dp], 0.25_dp, 25)
call assert_reproducible(first, second, "vector AR simulation")

call set_seed_int(503)
first = arima_sim([0.4_dp], [0.2_dp, -0.1_dp], 20)
call set_seed_int(503)
second = arima_sim([0.4_dp], [0.2_dp, -0.1_dp], 20)
call assert_reproducible(first, second, "vector ARMA simulation")
if (size(arima_sim(0.5_dp, 0.0_dp, 0)) /= 0) error stop "empty ARMA simulation failed"

fit = arima_fit(first, [1, 0, 0], include_mean=.true.)
if (fit%p /= 1 .or. fit%d /= 0 .or. fit%q /= 0) error stop "ARIMA order fields failed"
if (size(fit%coef) /= 2) error stop "ARIMA coefficient shape failed"
if (size(fit%resid) /= size(first)) error stop "ARIMA residual shape failed"
if (.not. all(ieee_is_finite(fit%coef)) .or. .not. ieee_is_finite(fit%sigma2)) &
   error stop "ARIMA fit finiteness failed"
if (fit%sigma2 <= 0.0_dp) error stop "ARIMA residual variance failed"

prediction = arima_predict(fit, 5)
if (size(prediction) /= 5 .or. any(.not. ieee_is_finite(prediction))) &
   error stop "ARIMA prediction failed"
if (size(arima_predict(fit, 0)) /= 0) error stop "empty ARIMA prediction failed"
forecast = arima_predict_result(fit, 4)
if (size(forecast%pred) /= 4 .or. size(forecast%se) /= 4) error stop "ARIMA result shape failed"
if (any(abs(forecast%pred - arima_predict(fit, 4)) > 1.0e-12_dp)) &
   error stop "ARIMA result predictions failed"
if (any(abs(forecast%se - sqrt(fit%sigma2)) > 1.0e-12_dp)) error stop "ARIMA result errors failed"

fit = arima_fit([2.0_dp, 4.0_dp, 6.0_dp, 8.0_dp], [0, 0, 0])
prediction = arima_predict(fit, 3)
if (any(abs(prediction - 5.0_dp) > 1.0e-12_dp)) error stop "mean-only ARIMA prediction failed"

contains

subroutine assert_reproducible(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_reproducible
end program test_arima_helpers
