program test_special_function_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_value, &
   ieee_positive_inf
use r_mod, only: dp, r_beta, r_choose, r_factorial, r_lbeta, r_lchoose, &
   r_digamma, r_lfactorial, r_psigamma, r_trigamma
implicit none

real(kind=dp), parameter :: tight_tolerance = 2.0e-12_dp
real(kind=dp), parameter :: approximation_tolerance = 2.0e-7_dp
real(kind=dp), parameter :: pi = acos(-1.0_dp)
real(kind=dp) :: values(4)
real(kind=dp) :: inf

inf = ieee_value(0.0_dp, ieee_positive_inf)

! R defines choose(n, k) for negative and noninteger n by the generalized
! binomial coefficient, while k is rounded to the nearest integer.
values = r_choose(-3.0_dp, [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp])
call assert_vector_close(values, [1.0_dp, -3.0_dp, 6.0_dp, -10.0_dp], &
   tight_tolerance, "negative upper choose")
call assert_close(r_choose(2.5_dp, 4.0_dp), -0.0390625_dp, tight_tolerance, &
   "noninteger upper choose")
call assert_close(r_choose(-2.5_dp, 3.0_dp), -6.5625_dp, tight_tolerance, &
   "negative noninteger choose")
call assert_close(r_choose(5.0_dp, 2.5_dp), 10.0_dp, tight_tolerance, &
   "rounded lower choose")
call assert_close(r_choose(5.0_dp, -1.0_dp), 0.0_dp, tight_tolerance, &
   "negative lower choose")
call assert_close(r_lchoose(-3.0_dp, 2.0_dp), log(6.0_dp), tight_tolerance, &
   "negative upper log choose")
call assert_negative_infinity(r_lchoose(5.0_dp, -1.0_dp), &
   "negative lower log choose")

values = r_beta([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], 1.0_dp)
call assert_vector_close(values, [1.0_dp, 0.5_dp, 1.0_dp / 3.0_dp, 0.25_dp], &
   tight_tolerance, "vector beta")
values = r_lbeta(1.0_dp, [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp])
call assert_vector_close(values, -log([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]), &
   tight_tolerance, "vector log beta")

values = r_factorial([0.0_dp, 1.0_dp, 3.0_dp, 5.0_dp])
call assert_vector_close(values, [1.0_dp, 1.0_dp, 6.0_dp, 120.0_dp], &
   tight_tolerance, "vector factorial")
values = r_lfactorial([0.0_dp, 1.0_dp, 3.0_dp, 5.0_dp])
call assert_vector_close(values, log([1.0_dp, 1.0_dp, 6.0_dp, 120.0_dp]), &
   tight_tolerance, "vector log factorial")
call assert_true(ieee_is_nan(r_factorial(-1.0_dp)), "negative factorial is NaN")

call assert_close(r_psigamma(1.0_dp, 2), -2.4041138063191886_dp, &
   approximation_tolerance, "second polygamma at one")
call assert_close(r_psigamma(2.0_dp, 2), -0.4041138063191886_dp, &
   approximation_tolerance, "second polygamma recurrence")
call assert_close(r_psigamma(1.0_dp, 3), pi**4 / 15.0_dp, &
   approximation_tolerance, "third polygamma at one")

call assert_close(r_digamma(0.5_dp), -0.5772156649015329_dp - 2.0_dp * log(2.0_dp), &
   tight_tolerance, "digamma at one half")
call assert_close(r_digamma(-0.5_dp), 2.0_dp - 0.5772156649015329_dp - &
   2.0_dp * log(2.0_dp), tight_tolerance, "digamma reflection")
call assert_close(r_digamma(25.0_dp), 3.198742512851974_dp, tight_tolerance, &
   "digamma large argument")
call assert_close(r_digamma(1.125_dp) - r_digamma(0.125_dp), 8.0_dp, &
   tight_tolerance, "digamma small-argument recurrence")

call assert_close(r_trigamma(0.5_dp), pi**2 / 2.0_dp, tight_tolerance, &
   "trigamma at one half")
call assert_close(r_trigamma(-0.5_dp), pi**2 - (pi**2 / 2.0_dp - 4.0_dp), &
   tight_tolerance, "trigamma reflection")
call assert_close(r_trigamma(1.125_dp) - r_trigamma(0.125_dp), -64.0_dp, &
   tight_tolerance, "trigamma small-argument recurrence")
values = r_digamma([0.5_dp, 1.0_dp, 2.0_dp, 25.0_dp])
call assert_vector_close(values, [-0.5772156649015329_dp - 2.0_dp * log(2.0_dp), &
   -0.5772156649015329_dp, 1.0_dp - 0.5772156649015329_dp, &
   3.198742512851974_dp], tight_tolerance, "elemental digamma")

call assert_true(ieee_is_nan(r_digamma(0.0_dp)), "digamma zero pole")
call assert_true(ieee_is_nan(r_digamma(-2.0_dp)), "digamma negative integer pole")
call assert_true(ieee_is_nan(r_trigamma(0.0_dp)), "trigamma zero pole")
call assert_true(ieee_is_nan(r_psigamma(1.0_dp, -1)), "negative polygamma order")
call assert_positive_infinity(r_digamma(inf), "digamma positive infinity")
call assert_close(r_trigamma(inf), 0.0_dp, tight_tolerance, &
   "trigamma positive infinity")

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

if (size(actual) /= size(expected) .or. any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close

subroutine assert_negative_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (ieee_is_finite(actual) .or. actual >= 0.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_negative_infinity

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

end program test_special_function_variants
