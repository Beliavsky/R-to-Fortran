program test_normal_distribution
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: dnorm, dp, normal_cdf, qnorm
implicit none

real(kind=dp), parameter :: tight_tolerance = 1.0e-12_dp
real(kind=dp), parameter :: quantile_tolerance = 1.0e-10_dp
real(kind=dp) :: probabilities(3), quantiles(3)

call assert_close(dnorm(0.0_dp), 1.0_dp / sqrt(2.0_dp * acos(-1.0_dp)), &
   tight_tolerance, "standard normal density")
call assert_close(dnorm(0.0_dp, mean=1.0_dp, sd=2.0_dp), 0.17603266338214976_dp, &
   tight_tolerance, "shifted normal density")
call assert_close(dnorm(0.0_dp, log_=.true.), log(dnorm(0.0_dp)), tight_tolerance, "log density")
if (.not. ieee_is_nan(dnorm(0.0_dp, sd=0.0_dp))) error stop "invalid sd should return NaN"

call assert_close(normal_cdf(0.0_dp), 0.5_dp, tight_tolerance, "normal cdf at zero")
call assert_close(normal_cdf(-1.25_dp), 1.0_dp - normal_cdf(1.25_dp), &
   tight_tolerance, "normal cdf symmetry")

probabilities = [0.025_dp, 0.5_dp, 0.975_dp]
quantiles = qnorm(probabilities)
call assert_vector_close(quantiles, [-1.95996398454005_dp, 0.0_dp, 1.95996398454005_dp], &
   quantile_tolerance, "normal quantiles")
call assert_vector_close(normal_cdf(quantiles), probabilities, quantile_tolerance, "cdf-quantile round trip")
call assert_close(qnorm(0.5_dp, mean=3.0_dp, sd=2.0_dp), 3.0_dp, quantile_tolerance, &
   "location-scale quantile")
call assert_close(qnorm(0.025_dp, lower_tail=.false.), 1.95996398454005_dp, &
   quantile_tolerance, "upper-tail quantile")

contains

subroutine assert_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual, expected, tolerance
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual(:), expected(:), tolerance
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
end program test_normal_distribution
