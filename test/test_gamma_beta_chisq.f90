program test_gamma_beta_chisq
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: dbeta, dchisq, dgamma, dp, pbeta, pchisq, pgamma, qbeta, qchisq, qgamma
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: probabilities(3), quantiles(3), chisq_probabilities(3)

call assert_close(dgamma(0.75_dp, shape=1.0_dp, rate=2.0_dp), &
   2.0_dp * exp(-1.5_dp), "gamma shape-one density")
call assert_close(dgamma(0.75_dp, shape=1.0_dp, rate=2.0_dp, log_=.true.), &
   log(2.0_dp) - 1.5_dp, "gamma log density")
call assert_close(pgamma(0.75_dp, shape=1.0_dp, rate=2.0_dp), &
   1.0_dp - exp(-1.5_dp), "gamma shape-one cdf")
call assert_close(qgamma(0.5_dp, shape=1.0_dp, rate=2.0_dp), &
   log(2.0_dp) / 2.0_dp, "gamma shape-one median")
if (.not. ieee_is_nan(dgamma(1.0_dp, shape=0.0_dp))) &
   error stop "invalid gamma shape should return NaN"

probabilities = [0.1_dp, 0.5_dp, 0.9_dp]
quantiles = qgamma(probabilities, shape=2.5_dp, rate=1.7_dp)
call assert_vector_close(pgamma(quantiles, shape=2.5_dp, rate=1.7_dp), probabilities, &
   "gamma cdf-quantile round trip")

call assert_close(dbeta(0.25_dp, shape1=1.0_dp, shape2=1.0_dp), 1.0_dp, &
   "uniform beta density")
call assert_close(dbeta(0.25_dp, shape1=1.0_dp, shape2=1.0_dp, log_=.true.), 0.0_dp, &
   "uniform beta log density")
call assert_close(pbeta(0.25_dp, shape1=1.0_dp, shape2=1.0_dp), 0.25_dp, &
   "uniform beta cdf")
call assert_close(qbeta(0.25_dp, shape1=1.0_dp, shape2=1.0_dp), 0.25_dp, &
   "uniform beta quantile")
call assert_close(pbeta(-1.0_dp, shape1=2.0_dp, shape2=3.0_dp), 0.0_dp, &
   "beta cdf below support")
call assert_close(pbeta(2.0_dp, shape1=2.0_dp, shape2=3.0_dp), 1.0_dp, &
   "beta cdf above support")

quantiles = qbeta(probabilities, shape1=2.0_dp, shape2=5.0_dp)
call assert_vector_close(pbeta(quantiles, shape1=2.0_dp, shape2=5.0_dp), probabilities, &
   "beta cdf-quantile round trip")

call assert_close(dchisq(2.0_dp, df=2.0_dp), 0.5_dp * exp(-1.0_dp), &
   "chi-square density")
call assert_close(dchisq(2.0_dp, df=2.0_dp, log_=.true.), log(0.5_dp) - 1.0_dp, &
   "chi-square log density")
call assert_close(qchisq(0.5_dp, df=2.0_dp), 2.0_dp * log(2.0_dp), &
   "chi-square df-two median")

quantiles = qchisq(probabilities, df=2.0_dp)
call assert_vector_close(quantiles, -2.0_dp * log(1.0_dp - probabilities), &
   "chi-square df-two quantiles")
chisq_probabilities = pchisq([0.0_dp, 2.0_dp, 10.0_dp], df=2.0_dp)
if (any(chisq_probabilities < 0.0_dp) .or. any(chisq_probabilities > 1.0_dp)) &
   error stop "chi-square cdf outside probability bounds"
if (any(chisq_probabilities(2:) < chisq_probabilities(:2))) &
   error stop "chi-square cdf should be monotone"
call assert_close(chisq_probabilities(1), 0.0_dp, "chi-square cdf at zero")

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
end program test_gamma_beta_chisq
