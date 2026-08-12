program test_generalized_linear_models
use r_mod, only: dp, glm_binomial_fit, glm_fit_t, glm_pearson_resid, &
   glm_poisson_fit, glm_predict_response
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-9_dp
real(kind=dp) :: no_predictors(4, 0), new_no_predictors(2, 0)
real(kind=dp) :: offset(4)
real(kind=dp), allocatable :: actual(:)
type(glm_fit_t) :: fit

fit = glm_binomial_fit([0, 1, 1, 0], no_predictors)
if (fit%family /= 1) error stop "binomial family code failed"
if (fit%convergence /= 0) error stop "binomial convergence failed"
if (fit%df /= 3) error stop "binomial residual degrees of freedom failed"
if (fit%iter < 1) error stop "binomial iteration count failed"
call assert_vector_close(fit%coef, [0.0_dp], "binomial intercept")
call assert_vector_close(fit%fitted, [0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp], &
   "binomial fitted probabilities")
call assert_vector_close(fit%resid, [-0.5_dp, 0.5_dp, 0.5_dp, -0.5_dp], &
   "binomial response residuals")
call assert_vector_close(fit%se, [1.0_dp], "binomial coefficient standard error")
call assert_vector_close(fit%z_value, [0.0_dp], "binomial z statistic")
call assert_vector_close(fit%p_value, [1.0_dp], "binomial coefficient p value")
actual = glm_pearson_resid(fit)
call assert_vector_close(actual, [-1.0_dp, 1.0_dp, 1.0_dp, -1.0_dp], &
   "binomial Pearson residuals")
actual = glm_predict_response(fit, new_no_predictors)
call assert_vector_close(actual, [0.5_dp, 0.5_dp], "binomial prediction")

fit = glm_poisson_fit([2, 2, 2, 2], no_predictors)
if (fit%family /= 2) error stop "Poisson family code failed"
if (fit%convergence /= 0) error stop "Poisson convergence failed"
if (fit%df /= 3) error stop "Poisson residual degrees of freedom failed"
call assert_vector_close(fit%coef, [log(2.0_dp)], "Poisson intercept")
call assert_vector_close(fit%fitted, [2.0_dp, 2.0_dp, 2.0_dp, 2.0_dp], &
   "Poisson fitted means")
call assert_vector_close(fit%resid, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], &
   "Poisson response residuals")
call assert_vector_close(fit%se, [1.0_dp / sqrt(8.0_dp)], &
   "Poisson coefficient standard error")
actual = glm_pearson_resid(fit)
call assert_vector_close(actual, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], &
   "Poisson Pearson residuals")
actual = glm_predict_response(fit, new_no_predictors)
call assert_vector_close(actual, [2.0_dp, 2.0_dp], "Poisson prediction")

offset = log(2.0_dp)
fit = glm_poisson_fit([2, 2, 2, 2], no_predictors, offset=offset)
call assert_vector_close(fit%coef, [0.0_dp], "Poisson offset intercept")
call assert_vector_close(fit%offset, offset, "Poisson stored offset")
call assert_vector_close(fit%fitted, [2.0_dp, 2.0_dp, 2.0_dp, 2.0_dp], &
   "Poisson offset fitted means")

contains

subroutine assert_vector_close(actual_value, expected, label)
real(kind=dp), intent(in) :: actual_value(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual_value) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual_value - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close
end program test_generalized_linear_models
