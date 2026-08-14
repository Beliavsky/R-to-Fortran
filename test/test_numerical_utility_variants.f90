program test_numerical_utility_variants
use r_mod, only: dp, fft, kronecker, nextn, polyroot
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: odd_signal(5)
real(kind=dp), allocatable :: matrix_result(:,:), roots(:)
complex(kind=dp), allocatable :: spectrum(:)

spectrum = fft([real(kind=dp) ::])
if (size(spectrum) /= 0) error stop "empty FFT failed"
spectrum = fft([2.5_dp])
call assert_complex_vector(spectrum, [cmplx(2.5_dp, 0.0_dp, kind=dp)], &
   "singleton FFT")
spectrum = fft([0.0_dp, 1.0_dp, 0.0_dp, 0.0_dp])
call assert_complex_vector(spectrum, [cmplx(1.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, -1.0_dp, kind=dp), cmplx(-1.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, 1.0_dp, kind=dp)], "shifted impulse FFT")

odd_signal = [1.0_dp, -2.0_dp, 3.0_dp, 0.5_dp, -1.5_dp]
spectrum = fft(odd_signal)
call assert_close(real(spectrum(1), kind=dp), sum(odd_signal), "FFT zero frequency")
call assert_complex_close(spectrum(2), conjg(spectrum(5)), "FFT first conjugate pair")
call assert_complex_close(spectrum(3), conjg(spectrum(4)), "FFT second conjugate pair")
call assert_complex_close(sum(spectrum), &
   cmplx(real(size(odd_signal), kind=dp) * odd_signal(1), 0.0_dp, kind=dp), &
   "FFT coefficient sum")

matrix_result = kronecker(reshape([real(kind=dp) ::], [0, 2]), &
   reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]))
if (any(shape(matrix_result) /= [0, 4])) error stop "zero-row Kronecker shape failed"
matrix_result = kronecker(reshape([1.0_dp, 2.0_dp], [2, 1]), &
   reshape([real(kind=dp) ::], [3, 0]))
if (any(shape(matrix_result) /= [6, 0])) error stop "zero-column Kronecker shape failed"
matrix_result = kronecker(reshape([2.0_dp], [1, 1]), &
   reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [2, 3]))
call assert_real_matrix(matrix_result, &
   reshape([2.0_dp, 4.0_dp, 6.0_dp, 8.0_dp, 10.0_dp, 12.0_dp], [2, 3]), &
   "scalar-matrix Kronecker product")

if (nextn(-100) /= 1) error stop "negative nextn clamp failed"
if (nextn(0) /= 1) error stop "zero nextn clamp failed"
if (nextn(2) /= 2 .or. nextn(3) /= 3 .or. nextn(5) /= 5) &
   error stop "prime-factor nextn values failed"
if (nextn(31) /= 32) error stop "nextn power boundary failed"
if (nextn(33) /= 36) error stop "nextn mixed-factor boundary failed"
if (nextn(100) /= 100) error stop "existing smooth nextn failed"

if (size(polyroot([real(kind=dp) ::])) /= 0) error stop "empty polynomial roots failed"
roots = polyroot([0.0_dp, 1.0_dp])
call assert_real_vector(roots, [0.0_dp], "zero linear root")
roots = polyroot([12.0_dp, -10.0_dp, 2.0_dp])
call assert_real_vector_sorted(roots, [2.0_dp, 3.0_dp], "scaled polynomial roots")
roots = polyroot([1.0_dp, 2.0_dp, 0.0_dp])
if (size(roots) /= 2 .or. any(roots < huge(1.0_dp) / 2.0_dp)) &
   error stop "zero leading coefficient sentinel failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > tolerance) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_complex_close(actual, expected, label)
complex(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > tolerance) error stop trim(label) // " failed"
end subroutine assert_complex_close

subroutine assert_complex_vector(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_complex_vector

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

subroutine assert_real_vector_sorted(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= 2 .or. size(expected) /= 2) error stop trim(label) // " shape failed"
if (abs(minval(actual) - expected(1)) > tolerance .or. &
   abs(maxval(actual) - expected(2)) > tolerance) error stop trim(label) // " values failed"
end subroutine assert_real_vector_sorted

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label
if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix
end program test_numerical_utility_variants
