program test_linear_models
use r_mod, only: dp, lm_coef, lm_fit_general, lm_fit_t, lm_predict_general, lm_r_squared_general
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: predictors(5, 2), response(5), design(5, 3), xtx(3, 3)
real(kind=dp) :: new_data(2, 2), identity(3, 3), no_intercept_x(4, 1)
real(kind=dp), allocatable :: actual(:)
type(lm_fit_t) :: fit

predictors(:, 1) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
predictors(:, 2) = [1.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
response = 2.0_dp + 3.0_dp * predictors(:, 1) - 4.0_dp * predictors(:, 2)

fit = lm_fit_general(response, predictors)
if (.not. fit%has_intercept) error stop "linear-model intercept flag failed"
if (fit%df /= 2) error stop "linear-model residual degrees of freedom failed"
call assert_vector_close(fit%coef, [2.0_dp, 3.0_dp, -4.0_dp], "linear-model coefficients")
call assert_vector_close(fit%fitted, response, "linear-model fitted values")
call assert_vector_close(fit%resid, spread(0.0_dp, 1, 5), "linear-model residuals")
call assert_close(fit%r_squared, 1.0_dp, "linear-model R-squared")
call assert_close(fit%adj_r_squared, 1.0_dp, "linear-model adjusted R-squared")
call assert_close(fit%sigma, 0.0_dp, "linear-model residual standard error")

design(:, 1) = 1.0_dp
design(:, 2:3) = predictors
xtx = matmul(transpose(design), design)
identity = 0.0_dp
identity(1, 1) = 1.0_dp
identity(2, 2) = 1.0_dp
identity(3, 3) = 1.0_dp
call assert_matrix_close(matmul(xtx, fit%cov_unscaled), identity, &
   "linear-model unscaled covariance")

new_data(1, :) = [5.0_dp, 0.0_dp]
new_data(2, :) = [5.0_dp, 1.0_dp]
actual = lm_predict_general(fit, new_data)
call assert_vector_close(actual, [17.0_dp, 13.0_dp], "linear-model predictions")
actual = lm_coef(response, predictors)
call assert_vector_close(actual, [2.0_dp, 3.0_dp, -4.0_dp], "coefficient convenience API")
call assert_close(lm_r_squared_general(response, predictors), 1.0_dp, &
   "R-squared convenience API")

no_intercept_x(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
fit = lm_fit_general(2.0_dp * no_intercept_x(:, 1), no_intercept_x, intercept=.false.)
if (fit%has_intercept) error stop "no-intercept linear-model flag failed"
if (fit%df /= 3) error stop "no-intercept residual degrees of freedom failed"
call assert_vector_close(fit%coef, [2.0_dp], "no-intercept coefficient")
call assert_vector_close(fit%fitted, 2.0_dp * no_intercept_x(:, 1), &
   "no-intercept fitted values")

contains

subroutine assert_close(actual_value, expected, label)
real(kind=dp), intent(in) :: actual_value, expected
character(len=*), intent(in) :: label

if (abs(actual_value - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual_value, expected
   error stop 1
end if
end subroutine assert_close

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

subroutine assert_matrix_close(actual_value, expected, label)
real(kind=dp), intent(in) :: actual_value(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual_value) /= shape(expected))) then
   write(*, '(a)') trim(label) // " shape failed"
   error stop 1
end if
if (any(abs(actual_value - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_matrix_close
end program test_linear_models
