program test_continuous_distributions
use r_mod, only: dexp, dlogis, dp, dunif, pexp, plogis, punif, qexp, qlogis, qunif
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: probabilities(3), quantiles(3)

call assert_close(dunif(2.0_dp, min=1.0_dp, max=5.0_dp), 0.25_dp, "uniform density")
call assert_close(dunif(0.0_dp, min=1.0_dp, max=5.0_dp), 0.0_dp, "uniform density outside support")
call assert_close(dunif(2.0_dp, min=1.0_dp, max=5.0_dp, log_=.true.), log(0.25_dp), &
   "uniform log density")
call assert_close(punif(0.0_dp, min=1.0_dp, max=5.0_dp), 0.0_dp, "uniform cdf below support")
call assert_close(punif(3.0_dp, min=1.0_dp, max=5.0_dp), 0.5_dp, "uniform cdf midpoint")
call assert_close(punif(6.0_dp, min=1.0_dp, max=5.0_dp), 1.0_dp, "uniform cdf above support")
call assert_close(qunif(0.25_dp, min=1.0_dp, max=5.0_dp), 2.0_dp, "uniform quantile")
call assert_close(qunif(-1.0_dp, min=1.0_dp, max=5.0_dp), 1.0_dp, "uniform lower clamp")
call assert_close(qunif(2.0_dp, min=1.0_dp, max=5.0_dp), 5.0_dp, "uniform upper clamp")

probabilities = [0.1_dp, 0.25_dp, 0.9_dp]
quantiles = qunif(probabilities, min=-2.0_dp, max=4.0_dp)
call assert_vector_close(punif(quantiles, min=-2.0_dp, max=4.0_dp), probabilities, &
   "uniform cdf-quantile round trip")

call assert_close(dexp(0.0_dp, rate=2.0_dp), 2.0_dp, "exponential density at zero")
call assert_close(dexp(1.0_dp, rate=2.0_dp), 2.0_dp * exp(-2.0_dp), "exponential density")
call assert_close(dexp(-1.0_dp, rate=2.0_dp), 0.0_dp, "exponential density below support")
call assert_close(dexp(1.0_dp, rate=2.0_dp, log_=.true.), log(2.0_dp) - 2.0_dp, &
   "exponential log density")
call assert_close(pexp(-1.0_dp, rate=2.0_dp), 0.0_dp, "exponential cdf below support")
call assert_close(pexp(0.0_dp, rate=2.0_dp), 0.0_dp, "exponential cdf at zero")
call assert_close(qexp(0.5_dp, rate=2.0_dp), log(2.0_dp) / 2.0_dp, "exponential median")

quantiles = qexp(probabilities, rate=1.7_dp)
call assert_vector_close(pexp(quantiles, rate=1.7_dp), probabilities, &
   "exponential cdf-quantile round trip")

call assert_close(dlogis(3.0_dp, location=3.0_dp, scale=2.0_dp), 0.125_dp, &
   "logistic density at location")
call assert_close(dlogis(3.0_dp, location=3.0_dp, scale=2.0_dp, log_=.true.), log(0.125_dp), &
   "logistic log density")
call assert_close(plogis(3.0_dp, location=3.0_dp, scale=2.0_dp), 0.5_dp, &
   "logistic cdf at location")
call assert_close(qlogis(0.5_dp, location=3.0_dp, scale=2.0_dp), 3.0_dp, &
   "logistic median")
call assert_close(plogis(1.0_dp, location=3.0_dp, scale=2.0_dp) + &
   plogis(5.0_dp, location=3.0_dp, scale=2.0_dp), 1.0_dp, "logistic cdf symmetry")

quantiles = qlogis(probabilities, location=-1.0_dp, scale=2.5_dp)
call assert_vector_close(plogis(quantiles, location=-1.0_dp, scale=2.5_dp), probabilities, &
   "logistic cdf-quantile round trip")

contains

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

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close
end program test_continuous_distributions
