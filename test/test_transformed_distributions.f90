program test_transformed_distributions
use r_mod, only: dcauchy, dlnorm, dp, dweibull, pcauchy, plnorm, pweibull, qcauchy, qlnorm, qweibull
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp), parameter :: pi = acos(-1.0_dp)
real(kind=dp) :: probabilities(3), quantiles(3)

call assert_close(dlnorm(1.0_dp), 1.0_dp / sqrt(2.0_dp * pi), "log-normal density")
call assert_close(dlnorm(1.0_dp, log_=.true.), -0.5_dp * log(2.0_dp * pi), &
   "log-normal log density")
call assert_close(dlnorm(0.0_dp), 0.0_dp, "log-normal density below support")
call assert_close(plnorm(0.0_dp), 0.0_dp, "log-normal cdf below support")
call assert_close(plnorm(exp(1.0_dp), meanlog=1.0_dp, sdlog=2.0_dp), 0.5_dp, &
   "log-normal cdf at median")
call assert_close(qlnorm(0.5_dp, meanlog=1.0_dp, sdlog=2.0_dp), exp(1.0_dp), &
   "log-normal median")

probabilities = [0.1_dp, 0.5_dp, 0.9_dp]
quantiles = qlnorm(probabilities, meanlog=0.3_dp, sdlog=0.8_dp)
call assert_vector_close(plnorm(quantiles, meanlog=0.3_dp, sdlog=0.8_dp), probabilities, &
   "log-normal cdf-quantile round trip")

call assert_close(dweibull(1.0_dp, shape=1.0_dp, scale=2.0_dp), &
   0.5_dp * exp(-0.5_dp), "Weibull density")
call assert_close(dweibull(1.0_dp, shape=1.0_dp, scale=2.0_dp, log_=.true.), &
   log(0.5_dp) - 0.5_dp, "Weibull log density")
call assert_close(dweibull(-1.0_dp, shape=1.0_dp, scale=2.0_dp), 0.0_dp, &
   "Weibull density below support")
call assert_close(pweibull(0.0_dp, shape=1.0_dp, scale=2.0_dp), 0.0_dp, &
   "Weibull cdf at zero")
call assert_close(qweibull(0.5_dp, shape=1.0_dp, scale=2.0_dp), 2.0_dp * log(2.0_dp), &
   "Weibull median")

quantiles = qweibull(probabilities, shape=1.7_dp, scale=2.3_dp)
call assert_vector_close(pweibull(quantiles, shape=1.7_dp, scale=2.3_dp), probabilities, &
   "Weibull cdf-quantile round trip")

call assert_close(dcauchy(3.0_dp, location=3.0_dp, scale=2.0_dp), 1.0_dp / (2.0_dp * pi), &
   "Cauchy density at location")
call assert_close(dcauchy(3.0_dp, location=3.0_dp, scale=2.0_dp, log_=.true.), &
   -log(2.0_dp * pi), "Cauchy log density")
call assert_close(pcauchy(3.0_dp, location=3.0_dp, scale=2.0_dp), 0.5_dp, &
   "Cauchy cdf at location")
call assert_close(qcauchy(0.5_dp, location=3.0_dp, scale=2.0_dp), 3.0_dp, &
   "Cauchy median")
call assert_close(pcauchy(1.0_dp, location=3.0_dp, scale=2.0_dp) + &
   pcauchy(5.0_dp, location=3.0_dp, scale=2.0_dp), 1.0_dp, "Cauchy cdf symmetry")

quantiles = qcauchy(probabilities, location=-1.0_dp, scale=2.5_dp)
call assert_vector_close(pcauchy(quantiles, location=-1.0_dp, scale=2.5_dp), probabilities, &
   "Cauchy cdf-quantile round trip")

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
end program test_transformed_distributions
