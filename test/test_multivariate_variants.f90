program test_multivariate_variants
use r_mod, only: cor, cov, cov2cor, dp, isSymmetric, mahalanobis, scale
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: correlated_covariance(2, 2), observations(3, 2)
real(kind=dp) :: scale_data(2, 2), nearly_symmetric(2, 2)
real(kind=dp), allocatable :: matrix_result(:,:), vector_result(:)
integer :: symmetric_integer(2, 2), nonsquare_integer(2, 3)

call assert_close(cov([1.0_dp, 2.0_dp, 3.0_dp, 99.0_dp], &
   [2.0_dp, 4.0_dp, 6.0_dp]), 2.0_dp, "unequal-length covariance")
call assert_close(cor([1.0_dp, 2.0_dp, 3.0_dp, 99.0_dp], &
   [6.0_dp, 4.0_dp, 2.0_dp]), -1.0_dp, "unequal-length correlation")

matrix_result = cor(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [4, 1]), &
   reshape([2.0_dp, 4.0_dp, 6.0_dp], [3, 1]))
if (any(shape(matrix_result) /= [1, 1])) error stop "cross-correlation shape failed"
call assert_close(matrix_result(1, 1), 1.0_dp, "truncated cross-correlation")

correlated_covariance = reshape([2.0_dp, 1.0_dp, 1.0_dp, 2.0_dp], [2, 2])
observations(1, :) = [0.0_dp, 0.0_dp]
observations(2, :) = [1.0_dp, 1.0_dp]
observations(3, :) = [1.0_dp, -1.0_dp]
vector_result = mahalanobis(observations, [0.0_dp, 0.0_dp], correlated_covariance)
call assert_vector_close(vector_result, [0.0_dp, 2.0_dp / 3.0_dp, 2.0_dp], &
   "correlated Mahalanobis distances")
vector_result = mahalanobis(reshape([real(kind=dp) ::], [0, 2]), &
   [0.0_dp, 0.0_dp], correlated_covariance)
if (size(vector_result) /= 0) error stop "empty Mahalanobis result failed"

scale_data = reshape([3.0_dp, 4.0_dp, 0.0_dp, 0.0_dp], [2, 2])
matrix_result = scale(scale_data, center=.false., scale=.true.)
call assert_matrix_close(matrix_result, reshape([0.6_dp, 0.8_dp, 0.0_dp, 0.0_dp], [2, 2]), &
   "uncentered scaling")
matrix_result = scale(reshape([real(kind=dp) ::], [0, 3]))
if (any(shape(matrix_result) /= [0, 3])) error stop "empty-row scale shape failed"
matrix_result = scale(reshape([real(kind=dp) ::], [3, 0]))
if (any(shape(matrix_result) /= [3, 0])) error stop "empty-column scale shape failed"

matrix_result = cov2cor(correlated_covariance)
call assert_matrix_close(matrix_result, reshape([1.0_dp, 0.5_dp, 0.5_dp, 1.0_dp], [2, 2]), &
   "covariance-to-correlation conversion")

symmetric_integer = reshape([1, 2, 2, 3], [2, 2])
if (.not. isSymmetric(symmetric_integer)) error stop "integer symmetry failed"
nonsquare_integer = 0
if (isSymmetric(nonsquare_integer)) error stop "nonsquare integer symmetry failed"
nearly_symmetric = reshape([1.0_dp, 2.0_dp + 1.0e-8_dp, 2.0_dp, 3.0_dp], [2, 2])
if (isSymmetric(nearly_symmetric)) error stop "default symmetry tolerance failed"
if (.not. isSymmetric(nearly_symmetric, tol=2.0e-8_dp)) &
   error stop "explicit symmetry tolerance failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > tolerance) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_vector_close

subroutine assert_matrix_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label
if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_matrix_close
end program test_multivariate_variants
