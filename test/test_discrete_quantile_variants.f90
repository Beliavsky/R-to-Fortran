program test_discrete_quantile_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_quiet_nan, ieee_value
use r_mod, only: dp, qbinom, qgeom, qhyper, qnbinom, qpois, qsignrank, qwilcox
implicit none

real(kind=dp) :: nan_value
real(kind=dp) :: values(6)

nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

values = qbinom([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 4, 0.5_dp)
call assert_finite_support(values, 2.0_dp, 4.0_dp, "binomial quantile")

values = qhyper([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 5, 7, 4)
call assert_finite_support(values, 2.0_dp, 4.0_dp, "hypergeometric quantile")

values = qwilcox([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 2, 3)
call assert_finite_support(values, 3.0_dp, 6.0_dp, "Wilcoxon quantile")

values = qsignrank([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 3)
call assert_finite_support(values, 3.0_dp, 6.0_dp, "signed-rank quantile")

values = qpois([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 2.0_dp)
call assert_infinite_support(values, 2.0_dp, "Poisson quantile")

values = qgeom([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 0.25_dp)
call assert_infinite_support(values, 2.0_dp, "geometric quantile")

values = qnbinom([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], 3, 0.4_dp)
call assert_infinite_support(values, 4.0_dp, "negative-binomial quantile")

call assert_positive_infinity(qpois(0.0_dp, 2.0_dp, lower_tail=.false.), &
   "Poisson upper-tail zero probability")
call assert_close(qpois(1.0_dp, 2.0_dp, lower_tail=.false.), 0.0_dp, &
   "Poisson upper-tail unit probability")
call assert_true(ieee_is_nan(qpois(nan_value, 2.0_dp, lower_tail=.false.)), &
   "Poisson upper-tail NaN")

contains

subroutine assert_finite_support(actual, median_value, maximum, label)
real(kind=dp), intent(in) :: actual(:), median_value, maximum
character(len=*), intent(in) :: label

call assert_common(actual, median_value, label)
call assert_close(actual(4), maximum, trim(label) // " unit endpoint")
end subroutine assert_finite_support

subroutine assert_infinite_support(actual, median_value, label)
real(kind=dp), intent(in) :: actual(:), median_value
character(len=*), intent(in) :: label

call assert_common(actual, median_value, label)
call assert_positive_infinity(actual(4), trim(label) // " unit endpoint")
end subroutine assert_infinite_support

subroutine assert_common(actual, median_value, label)
real(kind=dp), intent(in) :: actual(:), median_value
character(len=*), intent(in) :: label

if (size(actual) /= 6) error stop trim(label) // " shape failed"
call assert_true(ieee_is_nan(actual(1)), trim(label) // " below probability range")
call assert_close(actual(2), 0.0_dp, trim(label) // " zero endpoint")
call assert_close(actual(3), median_value, trim(label) // " median")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " above probability range")
call assert_true(ieee_is_nan(actual(6)), trim(label) // " propagates NaN")
end subroutine assert_common

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (.not. ieee_is_finite(actual) .or. abs(actual - expected) > 1.0e-12_dp) then
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

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label

if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_discrete_quantile_variants
