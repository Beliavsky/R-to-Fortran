program test_numerical_integration
use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf, ieee_value
use r_mod, only: dp, integrate, integrate_result_t
implicit none

real(kind=dp), parameter :: tight_tolerance = 1.0e-10_dp
real(kind=dp), parameter :: improper_tolerance = 2.0e-6_dp
real(kind=dp) :: infinity, pi
type(integrate_result_t) :: result

pi = acos(-1.0_dp)
infinity = ieee_value(1.0_dp, ieee_positive_inf)

result = integrate(square, 0.0_dp, 1.0_dp, rel_tol=1.0e-12_dp, subdivisions=100)
call assert_close(result%value, 1.0_dp / 3.0_dp, tight_tolerance, "finite polynomial integral")
call assert_success(result, "finite polynomial integral")

result = integrate(square, 1.0_dp, 0.0_dp, rel_tol=1.0e-12_dp, subdivisions=100)
call assert_close(result%value, -1.0_dp / 3.0_dp, tight_tolerance, "reversed finite integral")
call assert_success(result, "reversed finite integral")

result = integrate(exponential_decay, 0.0_dp, infinity, rel_tol=1.0e-10_dp, subdivisions=200)
call assert_close(result%value, 1.0_dp, improper_tolerance, "one-sided improper integral")
call assert_success(result, "one-sided improper integral")

result = integrate(gaussian, -infinity, infinity, rel_tol=1.0e-10_dp, subdivisions=200)
call assert_close(result%value, sqrt(pi), improper_tolerance, "two-sided improper integral")
call assert_success(result, "two-sided improper integral")

contains

function square(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value

value = x * x
end function square

function exponential_decay(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value

value = exp(-x)
end function exponential_decay

function gaussian(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value

value = exp(-x * x)
end function gaussian

subroutine assert_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual, expected, tolerance
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_success(actual, label)
type(integrate_result_t), intent(in) :: actual
character(len=*), intent(in) :: label

if (actual%message /= 0) error stop trim(label) // " status failed"
if (actual%subdivisions < 2 .or. mod(actual%subdivisions, 2) /= 0) then
   error stop trim(label) // " subdivision count failed"
end if
if (actual%abs_error < 0.0_dp) error stop trim(label) // " error estimate failed"
end subroutine assert_success
end program test_numerical_integration
