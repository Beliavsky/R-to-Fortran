program test_bessel_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
use r_mod, only: besselI, besselJ, besselK, besselY, dp
implicit none

real(kind=dp), parameter :: tight_tolerance = 2.0e-12_dp
real(kind=dp), parameter :: approximation_tolerance = 3.0e-7_dp
real(kind=dp) :: x(3), values(3)
real(kind=dp), allocatable :: empty(:)

! Integer negative orders obey exact symmetry identities.
call assert_close(besselJ(1.0_dp, -1), -besselJ(1.0_dp, 1), tight_tolerance, &
   "negative order Bessel J1")
call assert_close(besselJ(1.0_dp, -2), besselJ(1.0_dp, 2), tight_tolerance, &
   "negative order Bessel J2")
call assert_close(besselY(1.0_dp, -1), -besselY(1.0_dp, 1), tight_tolerance, &
   "negative order Bessel Y1")
call assert_close(besselY(1.0_dp, -2), besselY(1.0_dp, 2), tight_tolerance, &
   "negative order Bessel Y2")
call assert_close(besselI(1.0_dp, -1), besselI(1.0_dp, 1), tight_tolerance, &
   "negative order Bessel I1")
call assert_close(besselI(1.0_dp, -2), besselI(1.0_dp, 2), tight_tolerance, &
   "negative order Bessel I2")
call assert_close(besselK(1.0_dp, -1), besselK(1.0_dp, 1), tight_tolerance, &
   "negative order Bessel K1")

x = [0.5_dp, 1.0_dp, 2.0_dp]
values = besselJ(x, 0.5_dp)
call assert_vector_close(values, [0.540973789934528_dp, 0.671396707141803_dp, &
   0.513016136561828_dp], tight_tolerance, "fractional vector Bessel J")

values = besselI(x, 0, expon_scaled=.true.)
call assert_vector_close(values, [0.645035270449150_dp, 0.465759607593640_dp, &
   0.308508322553671_dp], tight_tolerance, "scaled vector Bessel I")
values = besselK(x, 0, expon_scaled=.true.)
call assert_vector_close(values, [1.52410938577391_dp, 1.14446307980689_dp, &
   0.841568215070772_dp], approximation_tolerance, "scaled vector Bessel K")

allocate(empty(0))
call assert_size_zero(besselJ(empty, 0), "empty Bessel J")
call assert_size_zero(besselY(empty, 0), "empty Bessel Y")
call assert_size_zero(besselI(empty, 0), "empty Bessel I")
call assert_size_zero(besselK(empty, 0), "empty Bessel K")

contains

subroutine assert_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual, expected, tolerance
character(len=*), intent(in) :: label

if (.not. ieee_is_finite(actual) .or. abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual(:), expected(:), tolerance
character(len=*), intent(in) :: label

if (size(actual) /= size(expected) .or. any(.not. ieee_is_finite(actual)) .or. &
   any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close

subroutine assert_size_zero(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label

if (size(actual) /= 0) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_size_zero

end program test_bessel_variants
