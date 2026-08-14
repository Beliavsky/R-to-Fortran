program test_linear_algebra_variants
use r_mod, only: backsolve, chol, chol2inv, det_real, diag, dp, forwardsolve, &
   kappa_real, solve_real
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
integer :: integer_coefficient(2, 2), integer_rhs_matrix(2, 2)
real(kind=dp) :: real_coefficient(2, 2), singular(2, 2)
real(kind=dp), allocatable :: real_vector(:), real_matrix(:,:), factor(:,:)
complex(kind=dp), allocatable :: complex_vector(:)

integer_coefficient = reshape([2, 0, 0, 4], [2, 2])
real_vector = solve_real(integer_coefficient, [6, 8])
call assert_real_vector(real_vector, [3.0_dp, 2.0_dp], "integer linear solve")
real_vector = solve_real(integer_coefficient, [6.0_dp, 8.0_dp])
call assert_real_vector(real_vector, [3.0_dp, 2.0_dp], "mixed vector linear solve")

real_coefficient = real(integer_coefficient, kind=dp)
integer_rhs_matrix = reshape([6, 8, 2, 12], [2, 2])
real_matrix = solve_real(real_coefficient, integer_rhs_matrix)
call assert_real_matrix(real_matrix, reshape([3.0_dp, 2.0_dp, 1.0_dp, 3.0_dp], [2, 2]), &
   "mixed matrix linear solve")
complex_vector = solve_real(integer_coefficient, [cmplx(2.0_dp, 4.0_dp, kind=dp), &
   cmplx(8.0_dp, -4.0_dp, kind=dp)])
call assert_complex_vector(complex_vector, [cmplx(1.0_dp, 2.0_dp, kind=dp), &
   cmplx(2.0_dp, -1.0_dp, kind=dp)], "complex right-hand-side solve")

singular = reshape([1.0_dp, 2.0_dp, 2.0_dp, 4.0_dp], [2, 2])
real_vector = solve_real(singular, [3.0_dp, 6.0_dp])
call assert_real_vector(real_vector, [0.0_dp, 0.0_dp], "singular solve fallback")
call assert_close(det_real(singular), 0.0_dp, "singular determinant")
call assert_close(det_real(reshape([real(kind=dp) ::], [0, 0])), 1.0_dp, &
   "empty determinant")
call assert_close(det_real(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], &
   [2, 3])), 0.0_dp, "rectangular determinant fallback")
if (kappa_real(reshape([real(kind=dp) ::], [0, 0])) < huge(1.0_dp) / 2.0_dp) &
   error stop "empty condition estimate failed"

real_vector = solve_real(reshape([real(kind=dp) ::], [0, 0]), [real(kind=dp) ::])
if (size(real_vector) /= 0) error stop "empty vector solve failed"
real_matrix = solve_real(reshape([real(kind=dp) ::], [0, 0]), &
   reshape([real(kind=dp) ::], [0, 3]))
if (any(shape(real_matrix) /= [0, 3])) error stop "empty matrix solve shape failed"

factor = chol(reshape([real(kind=dp) ::], [0, 0]))
if (any(shape(factor) /= [0, 0])) error stop "empty Cholesky shape failed"
real_matrix = chol2inv(factor)
if (any(shape(real_matrix) /= [0, 0])) error stop "empty Cholesky inverse shape failed"
real_matrix = chol2inv(chol(real(integer_coefficient, kind=dp)), size=0)
if (any(shape(real_matrix) /= [0, 0])) error stop "zero-size partial inverse failed"

real_vector = forwardsolve(integer_coefficient, [2, 8])
call assert_real_vector(real_vector, [1.0_dp, 2.0_dp], "integer forward solve")
real_vector = backsolve(integer_coefficient, [2.0_dp, 8.0_dp])
call assert_real_vector(real_vector, [1.0_dp, 2.0_dp], "mixed back solve")
real_matrix = forwardsolve(integer_coefficient, reshape([2.0_dp, 8.0_dp, 4.0_dp, 4.0_dp], [2, 2]))
call assert_real_matrix(real_matrix, reshape([1.0_dp, 2.0_dp, 2.0_dp, 1.0_dp], [2, 2]), &
   "mixed matrix forward solve")
real_matrix = backsolve(real_coefficient, reshape([2, 8, 4, 4], [2, 2]))
call assert_real_matrix(real_matrix, reshape([1.0_dp, 2.0_dp, 2.0_dp, 1.0_dp], [2, 2]), &
   "mixed matrix back solve")

if (any(shape(diag([real(kind=dp) ::])) /= [0, 0])) error stop "empty real diagonal failed"
if (any(shape(diag([integer ::], -2)) /= [0, 0])) error stop "negative resized diagonal failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > tolerance) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

subroutine assert_complex_vector(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_complex_vector

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label
if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix
end program test_linear_algebra_variants
