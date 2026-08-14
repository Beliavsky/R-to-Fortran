program test_model_selection_and_stubs
use r_mod, only: dp, lm_fit_general, lm_fit_t, nlm_result_t, nlm_stub, step_lm
implicit none

real(kind=dp) :: predictors(12,2), response(12)
real(kind=dp), allocatable :: no_predictors(:,:)
type(lm_fit_t) :: lower, upper, selected
type(nlm_result_t) :: stub
integer :: i

stub = nlm_stub([3.0_dp, -2.0_dp], hessian=.true.)
if (any(stub%estimate /= [3.0_dp, -2.0_dp])) error stop "nlm stub estimate failed"
if (any(stub%gradient /= 0.0_dp)) error stop "nlm stub gradient failed"
if (any(shape(stub%hessian) /= [2, 2]) .or. any(stub%hessian /= 0.0_dp)) &
   error stop "nlm stub Hessian failed"
if (stub%minimum /= 0.0_dp .or. stub%code /= 1 .or. stub%iterations /= 0) &
   error stop "nlm stub metadata failed"

do i = 1, 12
   predictors(i, 1) = real(i - 1, kind=dp)
   predictors(i, 2) = merge(1.0_dp, -1.0_dp, mod(i, 2) == 0)
end do
response = 5.0_dp + 2.5_dp * predictors(:, 1) + &
   [0.1_dp, -0.1_dp, 0.05_dp, -0.05_dp, 0.08_dp, -0.08_dp, &
    0.04_dp, -0.04_dp, 0.06_dp, -0.06_dp, 0.02_dp, -0.02_dp]
allocate(no_predictors(12, 0))
lower = lm_fit_general(response, no_predictors)
upper = lm_fit_general(response, predictors)
selected = step_lm(lower, upper)
if (size(selected%xpred, 2) < 1 .or. size(selected%xpred, 2) > 2) &
   error stop "stepwise model predictor count failed"
if (selected%r_squared < 0.999_dp) error stop "stepwise model fit failed"
if (abs(selected%coef(1) - 5.0_dp) > 0.2_dp) error stop "stepwise model intercept failed"
if (abs(selected%coef(2) - 2.5_dp) > 0.05_dp) error stop "stepwise model slope failed"

selected = step_lm(lower, upper, k=log(12.0_dp))
if (selected%r_squared < 0.999_dp) error stop "BIC stepwise model fit failed"
end program test_model_selection_and_stubs
