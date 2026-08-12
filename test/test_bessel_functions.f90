program test_bessel_functions
use r_mod, only: besselI, besselJ, besselK, besselY, dp
implicit none

real(kind=dp), parameter :: tight_tolerance = 1.0e-12_dp
real(kind=dp), parameter :: approximation_tolerance = 2.0e-7_dp
real(kind=dp), parameter :: pi = acos(-1.0_dp)
real(kind=dp) :: values(3)

call assert_close(besselJ(0.0_dp, 0), 1.0_dp, tight_tolerance, "Bessel J0 at zero")
call assert_close(besselJ(0.0_dp, 1), 0.0_dp, tight_tolerance, "Bessel J1 at zero")
call assert_close(besselJ(1.0_dp, 0), 0.7651976865579666_dp, tight_tolerance, &
   "Bessel J0 reference")
call assert_close(besselJ(1.0_dp, 1), 0.4400505857449335_dp, tight_tolerance, &
   "Bessel J1 reference")
call assert_close(besselJ(1.0_dp, 2), 2.0_dp * besselJ(1.0_dp, 1) - besselJ(1.0_dp, 0), &
   tight_tolerance, "Bessel J recurrence")
call assert_close(besselJ(1.0_dp, 0.5_dp), sqrt(2.0_dp / pi) * sin(1.0_dp), &
   tight_tolerance, "fractional Bessel J")

values = besselJ([0.0_dp, 1.0_dp, 2.0_dp], 0)
call assert_vector_close(values, [1.0_dp, 0.7651976865579666_dp, 0.2238907791412357_dp], &
   tight_tolerance, "vector Bessel J")

call assert_close(besselY(1.0_dp, 0), 0.0882569642156770_dp, tight_tolerance, &
   "Bessel Y0 reference")
call assert_close(besselY(1.0_dp, 1), -0.781212821300289_dp, tight_tolerance, &
   "Bessel Y1 reference")
call assert_close(besselY(1.0_dp, 2), 2.0_dp * besselY(1.0_dp, 1) - besselY(1.0_dp, 0), &
   tight_tolerance, "Bessel Y recurrence")

call assert_close(besselI(0.0_dp, 0), 1.0_dp, tight_tolerance, "Bessel I0 at zero")
call assert_close(besselI(0.0_dp, 1), 0.0_dp, tight_tolerance, "Bessel I1 at zero")
call assert_close(besselI(1.0_dp, 0), 1.266065877752008_dp, tight_tolerance, &
   "Bessel I0 reference")
call assert_close(besselI(1.0_dp, 1), 0.565159103992485_dp, tight_tolerance, &
   "Bessel I1 reference")
call assert_close(besselI(1.0_dp, 0, expon_scaled=.true.), besselI(1.0_dp, 0) * exp(-1.0_dp), &
   tight_tolerance, "scaled Bessel I")

call assert_close(besselK(1.0_dp, 0), 0.421024438240708_dp, approximation_tolerance, &
   "Bessel K0 reference")
call assert_close(besselK(1.0_dp, 1), 0.601907230197235_dp, approximation_tolerance, &
   "Bessel K1 reference")
call assert_close(besselK(1.0_dp, 2), besselK(1.0_dp, 0) + 2.0_dp * besselK(1.0_dp, 1), &
   tight_tolerance, "Bessel K recurrence")
call assert_close(besselK(2.0_dp, 0.5_dp), sqrt(pi / 4.0_dp) * exp(-2.0_dp), &
   tight_tolerance, "half-order Bessel K")
call assert_close(besselK(1.0_dp, 0, expon_scaled=.true.), besselK(1.0_dp, 0) * exp(1.0_dp), &
   approximation_tolerance, "scaled Bessel K")

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
end program test_bessel_functions
