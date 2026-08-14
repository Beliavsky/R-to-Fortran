program test_continuous_density_cdf_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite, ieee_value, &
   ieee_positive_inf, ieee_quiet_nan
use r_mod, only: dcauchy, dexp, dlnorm, dlogis, dp, dunif, dweibull, pcauchy, &
   pexp, plnorm, plogis, punif, pweibull
implicit none

real(kind=dp), parameter :: tol = 1.0e-12_dp
real(kind=dp) :: inf, nan_value
real(kind=dp), allocatable :: values(:)

inf = ieee_value(0.0_dp, ieee_positive_inf)
nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

values = dunif([nan_value, 0.5_dp], min=1.0_dp, max=0.0_dp)
call assert_all_nan(values, "uniform density invalid interval")
values = punif([nan_value, 0.5_dp], min=1.0_dp, max=0.0_dp)
call assert_all_nan(values, "uniform CDF invalid interval")
call assert_true(ieee_is_nan(dunif(nan_value)), "uniform density propagates NaN")
call assert_true(ieee_is_nan(punif(nan_value)), "uniform CDF propagates NaN")
call assert_negative_infinity(dunif(-1.0_dp, log_=.true.), "uniform log density outside support")

values = dexp([-inf, 0.0_dp, inf, nan_value], rate=2.0_dp)
call assert_close(values(1), 0.0_dp, "exponential density at negative infinity")
call assert_close(values(2), 2.0_dp, "exponential density at zero")
call assert_close(values(3), 0.0_dp, "exponential density at positive infinity")
call assert_true(ieee_is_nan(values(4)), "exponential density propagates NaN")
values = pexp([-inf, 0.0_dp, inf, nan_value], rate=2.0_dp)
call assert_cdf_boundaries(values, "exponential CDF")
call assert_true(ieee_is_nan(dexp(1.0_dp, rate=-1.0_dp)), "exponential density invalid rate")
call assert_true(ieee_is_nan(pexp(1.0_dp, rate=-1.0_dp)), "exponential CDF invalid rate")
call assert_negative_infinity(dexp(-1.0_dp, log_=.true.), "exponential log density outside support")

values = dlogis([-inf, 0.0_dp, inf, nan_value])
call assert_density_boundaries(values, 0.25_dp, "logistic density")
values = plogis([-inf, 0.0_dp, inf, nan_value])
call assert_cdf_boundaries(values, "logistic CDF")
call assert_true(ieee_is_nan(dlogis(0.0_dp, scale=-1.0_dp)), "logistic density invalid scale")
call assert_true(ieee_is_nan(plogis(0.0_dp, scale=-1.0_dp)), "logistic CDF invalid scale")

values = dlnorm([-1.0_dp, 0.0_dp, 1.0_dp, inf, nan_value])
call assert_close(values(1), 0.0_dp, "log-normal density below support")
call assert_close(values(2), 0.0_dp, "log-normal density at zero")
call assert_close(values(3), 1.0_dp / sqrt(2.0_dp * acos(-1.0_dp)), "log-normal density at one")
call assert_close(values(4), 0.0_dp, "log-normal density at infinity")
call assert_true(ieee_is_nan(values(5)), "log-normal density propagates NaN")
values = plnorm([-inf, 0.0_dp, inf, nan_value])
call assert_cdf_boundaries(values, "log-normal CDF")
call assert_true(ieee_is_nan(dlnorm(1.0_dp, sdlog=-1.0_dp)), "log-normal density invalid sdlog")
call assert_true(ieee_is_nan(plnorm(1.0_dp, sdlog=-1.0_dp)), "log-normal CDF invalid sdlog")

call assert_positive_infinity(dweibull(0.0_dp, shape=0.5_dp), "Weibull density singularity")
call assert_close(dweibull(0.0_dp, shape=1.0_dp, scale=2.0_dp), 0.5_dp, "Weibull density at zero")
call assert_close(dweibull(0.0_dp, shape=2.0_dp), 0.0_dp, "Weibull density zero limit")
values = pweibull([-inf, 0.0_dp, inf, nan_value], shape=2.0_dp)
call assert_cdf_boundaries(values, "Weibull CDF")
call assert_true(ieee_is_nan(dweibull(1.0_dp, shape=-1.0_dp)), "Weibull density invalid shape")
call assert_true(ieee_is_nan(pweibull(1.0_dp, shape=1.0_dp, scale=-1.0_dp)), "Weibull CDF invalid scale")

values = dcauchy([-inf, 0.0_dp, inf, nan_value])
call assert_density_boundaries(values, 1.0_dp / acos(-1.0_dp), "Cauchy density")
values = pcauchy([-inf, 0.0_dp, inf, nan_value])
call assert_cdf_boundaries(values, "Cauchy CDF")
call assert_true(ieee_is_nan(dcauchy(0.0_dp, scale=-1.0_dp)), "Cauchy density invalid scale")
call assert_true(ieee_is_nan(pcauchy(0.0_dp, scale=-1.0_dp)), "Cauchy CDF invalid scale")

contains

subroutine assert_density_boundaries(actual, center, label)
real(kind=dp), intent(in) :: actual(:), center
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " at negative infinity")
call assert_close(actual(2), center, trim(label) // " at center")
call assert_close(actual(3), 0.0_dp, trim(label) // " at positive infinity")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " propagates NaN")
end subroutine assert_density_boundaries

subroutine assert_cdf_boundaries(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " at negative infinity")
call assert_close(actual(2), merge(0.5_dp, 0.0_dp, index(label, "logistic") > 0 .or. &
   index(label, "Cauchy") > 0), trim(label) // " at zero")
call assert_close(actual(3), 1.0_dp, trim(label) // " at positive infinity")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " propagates NaN")
end subroutine assert_cdf_boundaries

subroutine assert_all_nan(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(all(ieee_is_nan(actual)), label)
end subroutine assert_all_nan

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
call assert_true(abs(actual - expected) <= tol, label)
end subroutine assert_close

subroutine assert_positive_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label
call assert_true(.not. ieee_is_finite(actual) .and. actual > 0.0_dp, label)
end subroutine assert_positive_infinity

subroutine assert_negative_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label
call assert_true(.not. ieee_is_finite(actual) .and. actual < 0.0_dp, label)
end subroutine assert_negative_infinity

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label
if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_continuous_density_cdf_variants
