program test_secondary_linear_algebra
use r_mod, only: chol, chol2inv, dp, eigen_sym_values, kappa_real
implicit none

real(kind=dp) :: a(2,2), expected_inverse(2,2)
real(kind=dp), allocatable :: factor(:,:), inverse(:,:), values(:)

a = reshape([4.0_dp, 2.0_dp, 2.0_dp, 3.0_dp], [2, 2])
expected_inverse = reshape([0.375_dp, -0.25_dp, -0.25_dp, 0.5_dp], [2, 2])
factor = chol(a)
inverse = chol2inv(factor)
call assert_matrix_close(inverse, expected_inverse, "Cholesky inverse")
call assert_matrix_close(matmul(a, inverse), &
   reshape([1.0_dp, 0.0_dp, 0.0_dp, 1.0_dp], [2, 2]), "inverse reconstruction")

inverse = chol2inv(reshape([2, 0, 0, 3], [2, 2]))
call assert_matrix_close(inverse, reshape([0.25_dp, 0.0_dp, 0.0_dp, 1.0_dp / 9.0_dp], [2, 2]), &
   "integer Cholesky inverse")
inverse = chol2inv(factor, size=1)
if (any(shape(inverse) /= [1, 1])) error stop "partial Cholesky inverse shape failed"
call assert_close(inverse(1, 1), 0.25_dp, "partial Cholesky inverse")

values = eigen_sym_values(reshape([2.0_dp, 1.0_dp, 1.0_dp, 2.0_dp], [2, 2]))
if (size(values) /= 2) error stop "symmetric eigenvalue shape failed"
call assert_close(minval(values), 1.0_dp, "minimum symmetric eigenvalue")
call assert_close(maxval(values), 3.0_dp, "maximum symmetric eigenvalue")
if (size(eigen_sym_values(reshape([real(kind=dp) ::], [0, 0]))) /= 0) &
   error stop "empty symmetric eigenvalues failed"

call assert_close(kappa_real(reshape([1.0_dp, 0.0_dp, 0.0_dp, 1.0_dp], [2, 2])), &
   1.0_dp, "identity condition estimate")
call assert_close(kappa_real(reshape([2.0_dp, 0.0_dp, 0.0_dp, 4.0_dp], [2, 2])), &
   2.0_dp, "diagonal condition estimate")
if (kappa_real(reshape([1.0_dp, 2.0_dp, 2.0_dp, 4.0_dp], [2, 2])) < huge(1.0_dp) / 2.0_dp) &
   error stop "singular condition estimate failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > 1.0e-10_dp) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_matrix_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label
if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-10_dp)) error stop trim(label) // " values failed"
end subroutine assert_matrix_close
end program test_secondary_linear_algebra
