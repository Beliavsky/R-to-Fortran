program test_model_result_printing
use r_mod, only: dp, glm_binomial_fit, glm_fit_t, glm_poisson_fit, &
   lm_fit_general, lm_fit_t, print_glm_summary, print_lm_anova, &
   print_lm_coef_rstyle, print_lm_confint, print_lm_cooks_top, &
   print_lm_prediction_interval, print_lm_summary
implicit none

character(len=8), parameter :: term_names(2) = ["trend   ", "group   "]
real(kind=dp) :: predictors(8, 2), response(8), new_data(2, 2)
real(kind=dp) :: glm_predictor(8, 1)
type(glm_fit_t) :: glm_fit
type(lm_fit_t) :: lm_fit

predictors(:, 1) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, &
   6.0_dp, 7.0_dp]
predictors(:, 2) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp, &
   0.0_dp, 1.0_dp]
response = [1.1_dp, 3.9_dp, 4.8_dp, 7.7_dp, 9.2_dp, 12.3_dp, &
   12.8_dp, 16.2_dp]

lm_fit = lm_fit_general(response, predictors)
call print_lm_summary(lm_fit, term_names)
call print_lm_coef_rstyle(lm_fit, term_names)
call print_lm_confint(lm_fit, term_names)
call print_lm_anova(lm_fit, term_names)
call print_lm_anova(lm_fit, [character(len=8) :: "combined"], [2])
call print_lm_cooks_top(lm_fit, 3)
call print_lm_cooks_top(lm_fit, 0)

new_data(1, :) = [2.5_dp, 0.0_dp]
new_data(2, :) = [6.5_dp, 1.0_dp]
call print_lm_prediction_interval(lm_fit, new_data)

glm_predictor(:, 1) = predictors(:, 1)
glm_fit = glm_binomial_fit([0, 0, 1, 0, 1, 1, 0, 1], glm_predictor)
call print_glm_summary(glm_fit, [character(len=8) :: "trend"])

glm_fit = glm_poisson_fit([1, 2, 1, 3, 2, 4, 3, 5], glm_predictor)
call print_glm_summary(glm_fit, [character(len=8) :: "trend"])
end program test_model_result_printing
