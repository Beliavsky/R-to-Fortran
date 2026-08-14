program test_normal_chisq_boundaries
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_positive_inf, &
   ieee_quiet_nan, ieee_value
use r_mod, only: dchisq, dnorm, dp, normal_cdf, pchisq, qchisq, qnorm
implicit none

real(kind=dp), parameter :: tol = 1.0e-12_dp
real(kind=dp) :: inf, nan_value
real(kind=dp), allocatable :: values(:)

inf = ieee_value(0.0_dp, ieee_positive_inf)
nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

values = dnorm([-inf, 0.0_dp, inf, nan_value])
call assert_close(values(1), 0.0_dp, "normal density at negative infinity")
call assert_close(values(2), 1.0_dp / sqrt(2.0_dp * acos(-1.0_dp)), "normal density at zero")
call assert_close(values(3), 0.0_dp, "normal density at positive infinity")
call assert_true(ieee_is_nan(values(4)), "normal density propagates NaN")
call assert_negative_infinity(dnorm(inf, log_=.true.), "normal log density at infinity")
call assert_true(ieee_is_nan(dnorm(0.0_dp, sd=-1.0_dp)), "normal density invalid sd")

values = normal_cdf([-inf, 0.0_dp, inf, nan_value])
call assert_close(values(1), 0.0_dp, "normal CDF at negative infinity")
call assert_close(values(2), 0.5_dp, "normal CDF at zero")
call assert_close(values(3), 1.0_dp, "normal CDF at positive infinity")
call assert_true(ieee_is_nan(values(4)), "normal CDF propagates NaN")

values = qnorm([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value])
call assert_true(ieee_is_nan(values(1)), "normal quantile below probability range")
call assert_negative_infinity(values(2), "normal quantile lower endpoint")
call assert_close(values(3), 0.0_dp, "normal quantile median")
call assert_positive_infinity(values(4), "normal quantile upper endpoint")
call assert_true(ieee_is_nan(values(5)), "normal quantile above probability range")
call assert_true(ieee_is_nan(values(6)), "normal quantile propagates NaN")
call assert_true(ieee_is_nan(qnorm(0.5_dp, sd=-1.0_dp)), "normal quantile invalid sd")
call assert_positive_infinity(qnorm(0.0_dp, lower_tail=.false.), "upper-tail normal lower endpoint")
call assert_negative_infinity(qnorm(1.0_dp, lower_tail=.false.), "upper-tail normal upper endpoint")

values = dchisq([-inf, 0.0_dp, 2.0_dp, inf, nan_value], df=2.0_dp)
call assert_close(values(1), 0.0_dp, "chi-square density below support")
call assert_close(values(2), 0.5_dp, "chi-square density at zero")
call assert_close(values(3), 0.5_dp * exp(-1.0_dp), "chi-square density interior")
call assert_close(values(4), 0.0_dp, "chi-square density at infinity")
call assert_true(ieee_is_nan(values(5)), "chi-square density propagates NaN")
call assert_negative_infinity(dchisq(-1.0_dp, df=2.0_dp, log_=.true.), &
   "chi-square log density below support")

values = pchisq([-inf, 0.0_dp, inf, nan_value], df=2.0_dp)
call assert_close(values(1), 0.0_dp, "chi-square CDF below support")
call assert_close(values(2), 0.0_dp, "chi-square CDF at zero")
call assert_close(values(3), 1.0_dp, "chi-square CDF at infinity")
call assert_true(ieee_is_nan(values(4)), "chi-square CDF propagates NaN")
call assert_true(ieee_is_nan(pchisq(1.0_dp, df=-1.0_dp)), "chi-square CDF invalid df")
call assert_true(ieee_is_nan(pchisq(1.0_dp, df=2.0_dp, ncp=-1.0_dp)), &
   "noncentral chi-square CDF invalid ncp")
call assert_true(ieee_is_nan(pchisq(1.0_dp, df=2.0_dp, ncp=nan_value)), &
   "noncentral chi-square CDF NaN ncp")

values = qchisq([-0.1_dp, 0.0_dp, 1.0_dp, 1.1_dp, nan_value], df=2.0_dp)
call assert_true(ieee_is_nan(values(1)), "chi-square quantile below probability range")
call assert_close(values(2), 0.0_dp, "chi-square quantile lower endpoint")
call assert_positive_infinity(values(3), "chi-square quantile upper endpoint")
call assert_true(ieee_is_nan(values(4)), "chi-square quantile above probability range")
call assert_true(ieee_is_nan(values(5)), "chi-square quantile propagates NaN")
call assert_true(ieee_is_nan(qchisq(0.5_dp, df=-1.0_dp)), "chi-square quantile invalid df")

contains

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

end program test_normal_chisq_boundaries
