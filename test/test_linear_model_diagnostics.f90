program test_linear_model_diagnostics
use r_mod, only: dp, lm_confint, lm_cooks_distance, lm_fit_general, lm_fit_t, &
   lm_predict_general, lm_predict_interval
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
implicit none

real(kind=dp) :: predictors(8,1), response(8), new_data(3,1)
real(kind=dp), allocatable :: intervals(:,:), confidence(:,:), cooks(:), fitted(:)
type(lm_fit_t) :: fit
integer :: i

predictors(:, 1) = [(real(i - 1, kind=dp), i=1,8)]
response = 1.5_dp + 2.0_dp * predictors(:, 1) + &
   [0.2_dp, -0.1_dp, 0.1_dp, -0.2_dp, 0.15_dp, -0.05_dp, 0.1_dp, -0.2_dp]
fit = lm_fit_general(response, predictors)
if (fit%sigma <= 0.0_dp) error stop "diagnostic fit residual scale failed"

confidence = lm_confint(fit)
if (any(shape(confidence) /= [2, 2])) error stop "confidence interval shape failed"
do i = 1, 2
   call assert_close(0.5_dp * (confidence(i, 1) + confidence(i, 2)), fit%coef(i), &
      "confidence interval midpoint")
   if (confidence(i, 1) > fit%coef(i) .or. confidence(i, 2) < fit%coef(i)) &
      error stop "confidence interval ordering failed"
end do
if (any(abs(lm_confint(fit, level=0.90_dp) - confidence) > 1.0e-12_dp)) &
   error stop "confidence level compatibility failed"

new_data(:, 1) = [0.5_dp, 3.5_dp, 8.0_dp]
fitted = lm_predict_general(fit, new_data)
intervals = lm_predict_interval(fit, new_data)
if (any(shape(intervals) /= [3, 3])) error stop "prediction interval shape failed"
if (any(abs(intervals(:, 1) - fitted) > 1.0e-12_dp)) error stop "prediction interval fit failed"
if (any(intervals(:, 2) > intervals(:, 1) .or. intervals(:, 3) < intervals(:, 1))) &
   error stop "prediction interval ordering failed"
if (any(.not. ieee_is_finite(intervals))) error stop "prediction interval finiteness failed"

cooks = lm_cooks_distance(fit)
if (size(cooks) /= size(response)) error stop "Cook distance shape failed"
if (any(.not. ieee_is_finite(cooks)) .or. any(cooks < 0.0_dp)) error stop "Cook distance values failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > 1.0e-10_dp) error stop trim(label) // " failed"
end subroutine assert_close
end program test_linear_model_diagnostics
