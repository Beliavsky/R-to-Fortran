program linear_models
use r_mod, only: dp, lm_fit_general, lm_fit_t, lm_predict_general, &
   print_lm_summary, print_real_vector
implicit none

real(kind=dp) :: predictors(6, 2), response(6), new_data(2, 2)
real(kind=dp), allocatable :: predictions(:)
type(lm_fit_t) :: fit

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: Linear models"
write(*, '(a, /)') repeat("=", 72)

predictors(:, 1) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
predictors(:, 2) = [1.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp, 0.0_dp]
response = 2.0_dp + 3.0_dp * predictors(:, 1) - 4.0_dp * predictors(:, 2)

fit = lm_fit_general(response, predictors)
call print_lm_summary(fit, [character(len=5) :: "trend", "group"])

write(*, '(/, a, f8.5)') "R-squared from result field: ", fit%r_squared
write(*, '(a)') "Residuals:"
call print_real_vector(fit%resid)

new_data(1, :) = [6.0_dp, 0.0_dp]
new_data(2, :) = [6.0_dp, 1.0_dp]
predictions = lm_predict_general(fit, new_data)
write(*, '(a)') "Predictions for two new observations:"
call print_real_vector(predictions)
end program linear_models
