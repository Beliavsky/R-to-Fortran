program test_continuous_quantile_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_quiet_nan, ieee_value
use r_mod, only: dp, qcauchy, qexp, qlnorm, qlogis, qunif, qweibull
implicit none

real(kind=dp) :: nan_value
real(kind=dp) :: values(6)
real(kind=dp), parameter :: probabilities(5) = [-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp]

nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

values = qunif([probabilities, nan_value], min=-2.0_dp, max=4.0_dp)
call assert_invalid(values, "uniform quantile")
call assert_close(values(2), -2.0_dp, "uniform lower endpoint")
call assert_close(values(3), 1.0_dp, "uniform median")
call assert_close(values(4), 4.0_dp, "uniform upper endpoint")

values = qexp([probabilities, nan_value], rate=2.0_dp)
call assert_positive_support(values, log(2.0_dp) / 2.0_dp, "exponential quantile")

values = qlnorm([probabilities, nan_value], meanlog=1.0_dp, sdlog=2.0_dp)
call assert_positive_support(values, exp(1.0_dp), "log-normal quantile")

values = qweibull([probabilities, nan_value], shape=1.7_dp, scale=2.3_dp)
call assert_positive_support(values, 2.3_dp * log(2.0_dp)**(1.0_dp / 1.7_dp), &
   "Weibull quantile")

values = qlogis([probabilities, nan_value], location=3.0_dp, scale=2.0_dp)
call assert_real_line_support(values, 3.0_dp, "logistic quantile")

values = qcauchy([probabilities, nan_value], location=3.0_dp, scale=2.0_dp)
call assert_real_line_support(values, 3.0_dp, "Cauchy quantile")

contains

subroutine assert_positive_support(actual, median_value, label)
real(kind=dp), intent(in) :: actual(:), median_value
character(len=*), intent(in) :: label

call assert_invalid(actual, label)
call assert_close(actual(2), 0.0_dp, trim(label) // " lower endpoint")
call assert_close(actual(3), median_value, trim(label) // " median")
call assert_positive_infinity(actual(4), trim(label) // " upper endpoint")
end subroutine assert_positive_support

subroutine assert_real_line_support(actual, median_value, label)
real(kind=dp), intent(in) :: actual(:), median_value
character(len=*), intent(in) :: label

call assert_invalid(actual, label)
call assert_negative_infinity(actual(2), trim(label) // " lower endpoint")
call assert_close(actual(3), median_value, trim(label) // " median")
call assert_positive_infinity(actual(4), trim(label) // " upper endpoint")
end subroutine assert_real_line_support

subroutine assert_invalid(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label

if (size(actual) /= 6) error stop trim(label) // " shape failed"
call assert_true(ieee_is_nan(actual(1)), trim(label) // " below probability range")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " above probability range")
call assert_true(ieee_is_nan(actual(6)), trim(label) // " propagates NaN")
end subroutine assert_invalid

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (.not. ieee_is_finite(actual) .or. abs(actual - expected) > 1.0e-11_dp) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_positive_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (ieee_is_finite(actual) .or. actual <= 0.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_positive_infinity

subroutine assert_negative_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (ieee_is_finite(actual) .or. actual >= 0.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_negative_infinity

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label

if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_continuous_quantile_variants
