program test_numerical_utilities
use r_mod, only: dp, fft, kronecker, nextn, polyroot
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: a(2, 2), b(2, 2), signal(4)
real(kind=dp), allocatable :: product(:,:), roots(:)
complex(kind=dp), allocatable :: spectrum(:)

spectrum = fft([1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp])
call assert_complex_vector_close(spectrum, [cmplx(1.0_dp, 0.0_dp, kind=dp), &
   cmplx(1.0_dp, 0.0_dp, kind=dp), cmplx(1.0_dp, 0.0_dp, kind=dp), &
   cmplx(1.0_dp, 0.0_dp, kind=dp)], "FFT impulse")

spectrum = fft([1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp])
call assert_complex_vector_close(spectrum, [cmplx(4.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, 0.0_dp, kind=dp), cmplx(0.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, 0.0_dp, kind=dp)], "FFT constant signal")

signal = [1.0_dp, -2.0_dp, 0.5_dp, 3.0_dp]
spectrum = fft(signal)
call assert_close(sum(abs(spectrum)**2) / real(size(signal), kind=dp), sum(signal**2), &
   "FFT Parseval identity")

a(1, :) = [1.0_dp, 2.0_dp]
a(2, :) = [3.0_dp, 4.0_dp]
b(1, :) = [0.0_dp, 5.0_dp]
b(2, :) = [6.0_dp, 7.0_dp]
product = kronecker(a, b)
call assert_matrix_close(product, reshape([0.0_dp, 6.0_dp, 0.0_dp, 18.0_dp, &
   5.0_dp, 7.0_dp, 15.0_dp, 21.0_dp, 0.0_dp, 12.0_dp, 0.0_dp, 24.0_dp, &
   10.0_dp, 14.0_dp, 20.0_dp, 28.0_dp], [4, 4]), "Kronecker product")

if (nextn(1) /= 1) error stop "nextn at one failed"
if (nextn(7) /= 8) error stop "nextn power of two failed"
if (nextn(17) /= 18) error stop "nextn mixed factors failed"
if (nextn(25) /= 25) error stop "nextn existing smooth number failed"

roots = polyroot([6.0_dp, -5.0_dp, 1.0_dp])
if (size(roots) /= 2) error stop "quadratic root count failed"
call assert_close(minval(roots), 2.0_dp, "quadratic smaller root modulus")
call assert_close(maxval(roots), 3.0_dp, "quadratic larger root modulus")

roots = polyroot([1.0_dp, 0.0_dp, 1.0_dp])
call assert_real_vector_close(roots, [1.0_dp, 1.0_dp], "complex root moduli")
roots = polyroot([-4.0_dp, 2.0_dp])
call assert_real_vector_close(roots, [2.0_dp], "linear root modulus")
if (size(polyroot([3.0_dp])) /= 0) error stop "constant polynomial roots failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_real_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected) .or. any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_real_vector_close

subroutine assert_complex_vector_close(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected) .or. any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_complex_vector_close

subroutine assert_matrix_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) then
   write(*, '(a)') trim(label) // " shape failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_matrix_close
end program test_numerical_utilities
