program test_linear_algebra
use r_mod, only: backsolve, chol, cov2cor, det_real, dp, forwardsolve, isSymmetric, solve_real
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: spd(3, 3), coefficient(2, 2), lower(3, 3), upper(3, 3)
real(kind=dp) :: covariance(2, 2), nonsymmetric(3, 3)
real(kind=dp) :: expected_vector(3), rhs_vector(3)
real(kind=dp) :: expected_solutions(2, 2), rhs_matrix(2, 2)
real(kind=dp), allocatable :: actual_vector(:), actual_matrix(:,:), factor(:,:)

spd(1, :) = [4.0_dp, 2.0_dp, 2.0_dp]
spd(2, :) = [2.0_dp, 5.0_dp, 1.0_dp]
spd(3, :) = [2.0_dp, 1.0_dp, 3.0_dp]

factor = chol(spd)
call assert_matrix_close(matmul(transpose(factor), factor), spd, "Cholesky reconstruction")
call assert_close(det_real(spd), 32.0_dp, "3x3 determinant")
call assert_close(det_real(reshape([1.0_dp, 3.0_dp, 2.0_dp, 4.0_dp], [2, 2])), &
   -2.0_dp, "2x2 determinant")

coefficient(1, :) = [3.0_dp, 1.0_dp]
coefficient(2, :) = [1.0_dp, 2.0_dp]
actual_vector = solve_real(coefficient, [5.0_dp, 0.0_dp])
call assert_vector_close(actual_vector, [2.0_dp, -1.0_dp], "vector linear solve")

expected_solutions(:, 1) = [2.0_dp, -1.0_dp]
expected_solutions(:, 2) = [1.0_dp, 3.0_dp]
rhs_matrix = matmul(coefficient, expected_solutions)
actual_matrix = solve_real(coefficient, rhs_matrix)
call assert_matrix_close(actual_matrix, expected_solutions, "matrix linear solve")

lower(1, :) = [2.0_dp, 0.0_dp, 0.0_dp]
lower(2, :) = [1.0_dp, 3.0_dp, 0.0_dp]
lower(3, :) = [-1.0_dp, 2.0_dp, 4.0_dp]
expected_vector = [1.0_dp, 2.0_dp, -1.0_dp]
rhs_vector = matmul(lower, expected_vector)
actual_vector = forwardsolve(lower, rhs_vector)
call assert_vector_close(actual_vector, expected_vector, "forward solve")
rhs_vector = matmul(transpose(lower), expected_vector)
actual_vector = forwardsolve(lower, rhs_vector, transpose=.true.)
call assert_vector_close(actual_vector, expected_vector, "transposed forward solve")

upper = transpose(lower)
rhs_vector = matmul(upper, expected_vector)
actual_vector = backsolve(upper, rhs_vector)
call assert_vector_close(actual_vector, expected_vector, "back solve")
rhs_vector = matmul(transpose(upper), expected_vector)
actual_vector = backsolve(upper, rhs_vector, transpose=.true.)
call assert_vector_close(actual_vector, expected_vector, "transposed back solve")

covariance(1, :) = [4.0_dp, 2.0_dp]
covariance(2, :) = [2.0_dp, 9.0_dp]
actual_matrix = cov2cor(covariance)
call assert_matrix_close(actual_matrix, reshape([1.0_dp, 1.0_dp / 3.0_dp, &
   1.0_dp / 3.0_dp, 1.0_dp], [2, 2]), "covariance to correlation")

if (.not. isSymmetric(spd)) error stop "symmetric matrix not recognized"
nonsymmetric = spd
nonsymmetric(1, 2) = nonsymmetric(1, 2) + 0.25_dp
if (isSymmetric(nonsymmetric)) error stop "nonsymmetric matrix not recognized"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
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
end program test_linear_algebra
