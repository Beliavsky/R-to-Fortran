program test_special_functions
use r_mod, only: dp, r_beta, r_choose, r_digamma, r_factorial, r_gamma, r_lbeta, &
   r_lchoose, r_lfactorial, r_lgamma, r_psigamma, r_trigamma
implicit none

real(kind=dp), parameter :: tight_tolerance = 1.0e-11_dp
real(kind=dp), parameter :: approximation_tolerance = 2.0e-6_dp
real(kind=dp), parameter :: pi = acos(-1.0_dp)
real(kind=dp) :: values(3)

call assert_close(r_gamma(1.0_dp), 1.0_dp, tight_tolerance, "gamma at one")
call assert_close(r_gamma(0.5_dp), sqrt(pi), tight_tolerance, "gamma at one half")
call assert_close(r_gamma(5.0_dp), 24.0_dp, tight_tolerance, "gamma at integer")
call assert_close(r_lgamma(5.0_dp), log(24.0_dp), tight_tolerance, "log gamma")
values = r_gamma([1.0_dp, 2.0_dp, 3.0_dp])
call assert_vector_close(values, [1.0_dp, 1.0_dp, 2.0_dp], tight_tolerance, &
   "elemental gamma")

call assert_close(r_beta(2.0_dp, 3.0_dp), 1.0_dp / 12.0_dp, tight_tolerance, "beta")
call assert_close(r_beta(3.0_dp, 2.0_dp), r_beta(2.0_dp, 3.0_dp), tight_tolerance, &
   "beta symmetry")
call assert_close(r_lbeta(2.0_dp, 3.0_dp), log(1.0_dp / 12.0_dp), tight_tolerance, &
   "log beta")

call assert_close(r_choose(5.0_dp, 2.0_dp), 10.0_dp, tight_tolerance, "choose")
call assert_close(r_choose(5.0_dp, 3.0_dp), r_choose(5.0_dp, 2.0_dp), tight_tolerance, &
   "choose symmetry")
call assert_close(r_choose(5.0_dp, 6.0_dp), 0.0_dp, tight_tolerance, &
   "choose outside support")
call assert_close(r_lchoose(5.0_dp, 2.0_dp), log(10.0_dp), tight_tolerance, "log choose")
values = r_choose([4.0_dp, 5.0_dp, 6.0_dp], 2.0_dp)
call assert_vector_close(values, [6.0_dp, 10.0_dp, 15.0_dp], tight_tolerance, &
   "elemental choose")

call assert_close(r_factorial(0.0_dp), 1.0_dp, tight_tolerance, "zero factorial")
call assert_close(r_factorial(5.0_dp), 120.0_dp, tight_tolerance, "factorial")
call assert_close(r_lfactorial(5.0_dp), log(120.0_dp), tight_tolerance, "log factorial")

call assert_close(r_digamma(1.0_dp), -0.5772156649015329_dp, approximation_tolerance, &
   "digamma at one")
call assert_close(r_digamma(2.0_dp) - r_digamma(1.0_dp), 1.0_dp, approximation_tolerance, &
   "digamma recurrence")
call assert_close(r_trigamma(1.0_dp), pi**2 / 6.0_dp, approximation_tolerance, &
   "trigamma at one")
call assert_close(r_psigamma(2.0_dp, 0), r_digamma(2.0_dp), tight_tolerance, &
   "psigamma derivative zero")
call assert_close(r_psigamma(2.0_dp, 1), r_trigamma(2.0_dp), tight_tolerance, &
   "psigamma derivative one")

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
end program test_special_functions
