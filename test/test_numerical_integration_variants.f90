program test_numerical_integration_variants
use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf, ieee_quiet_nan, ieee_value
use r_mod, only: dp, integrate, integrate_result_t
implicit none

real(kind=dp), parameter :: tolerance = 2.0e-6_dp
real(kind=dp) :: infinity, pi
type(integrate_result_t) :: result

infinity = ieee_value(1.0_dp, ieee_positive_inf)
pi = acos(-1.0_dp)

result = integrate(constant, -3.0_dp, 5.0_dp)
call assert_result(result, 16.0_dp, 1.0e-12_dp, "default constant integral")
result = integrate(linear, -2.0_dp, 4.0_dp, subdivisions=5)
call assert_result(result, 6.0_dp, 1.0e-12_dp, "odd-panel linear integral")
if (mod(result%subdivisions, 2) /= 0 .or. result%subdivisions > 6) &
   error stop "odd subdivision normalization failed"
result = integrate(cubic, 0.0_dp, 2.0_dp, rel_tol=0.0_dp, subdivisions=1)
call assert_result(result, 4.0_dp, 1.0e-12_dp, "minimum-panel cubic integral")
if (result%subdivisions /= 2) error stop "minimum subdivision clamp failed"

result = integrate(constant, 2.5_dp, 2.5_dp)
call assert_result(result, 0.0_dp, 1.0e-12_dp, "zero-width integral")
if (result%abs_error /= 0.0_dp) error stop "zero-width error estimate failed"
result = integrate(constant, 5.0_dp, -3.0_dp)
call assert_result(result, -16.0_dp, 1.0e-12_dp, "reversed constant integral")

result = integrate(left_exponential, -infinity, 0.0_dp, rel_tol=1.0e-10_dp, subdivisions=200)
call assert_result(result, 1.0_dp, tolerance, "left improper exponential integral")
result = integrate(cauchy_kernel, -infinity, infinity, rel_tol=1.0e-10_dp, subdivisions=200)
call assert_result(result, pi, tolerance, "two-sided Cauchy integral")

result = integrate(nonfinite_midpoint, 0.0_dp, 1.0_dp, rel_tol=0.0_dp, subdivisions=2)
call assert_result(result, 1.0_dp / 3.0_dp, 1.0e-12_dp, &
   "non-finite sampled value omission")

contains

function constant(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value
value = 2.0_dp + 0.0_dp * x
end function constant

function linear(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value
value = x
end function linear

function cubic(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value
value = x**3
end function cubic

function left_exponential(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value
value = exp(x)
end function left_exponential

function cauchy_kernel(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value
value = 1.0_dp / (1.0_dp + x*x)
end function cauchy_kernel

function nonfinite_midpoint(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value
if (abs(x - 0.5_dp) <= epsilon(1.0_dp)) then
   value = ieee_value(0.0_dp, ieee_quiet_nan)
else
   value = 1.0_dp
end if
end function nonfinite_midpoint

subroutine assert_result(actual, expected, allowed_error, label)
type(integrate_result_t), intent(in) :: actual
real(kind=dp), intent(in) :: expected, allowed_error
character(len=*), intent(in) :: label
if (abs(actual%value - expected) > allowed_error) then
   write(*, '(a, 3(1x, es24.16))') trim(label) // " failed:", actual%value, expected, allowed_error
   error stop trim(label) // " value failed"
end if
if (actual%message /= 0) error stop trim(label) // " status failed"
if (actual%subdivisions < 2 .or. mod(actual%subdivisions, 2) /= 0) &
   error stop trim(label) // " subdivisions failed"
if (actual%abs_error < 0.0_dp) error stop trim(label) // " error estimate failed"
end subroutine assert_result
end program test_numerical_integration_variants
