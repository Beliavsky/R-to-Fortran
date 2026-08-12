program test_multivariate_statistics
use r_mod, only: cor, dp, mahalanobis, scale
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: x(4, 2), y(4, 2), observations(3, 2), center(2), covariance(2, 2)
real(kind=dp) :: scale_data(3, 2)
real(kind=dp), allocatable :: actual_matrix(:,:), actual_vector(:)

x(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
x(:, 2) = [4.0_dp, 3.0_dp, 2.0_dp, 1.0_dp]
y(:, 1) = 2.0_dp * x(:, 1) + 5.0_dp
y(:, 2) = -3.0_dp * x(:, 1)
actual_matrix = cor(x, y)
call assert_matrix_close(actual_matrix, reshape([1.0_dp, -1.0_dp, -1.0_dp, 1.0_dp], [2, 2]), &
   "cross-matrix correlation")

scale_data(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp]
scale_data(:, 2) = [10.0_dp, 20.0_dp, 30.0_dp]
actual_matrix = scale(scale_data)
call assert_vector_close(sum(actual_matrix, dim=1), [0.0_dp, 0.0_dp], &
   "scaled column means")
call assert_vector_close(sum(actual_matrix**2, dim=1), [2.0_dp, 2.0_dp], &
   "scaled column sums of squares")
call assert_matrix_close(actual_matrix, reshape([-1.0_dp, 0.0_dp, 1.0_dp, &
   -1.0_dp, 0.0_dp, 1.0_dp], [3, 2]), "centered and scaled matrix")

actual_matrix = scale(scale_data, center=.true., scale=.false.)
call assert_matrix_close(actual_matrix, reshape([-1.0_dp, 0.0_dp, 1.0_dp, &
   -10.0_dp, 0.0_dp, 10.0_dp], [3, 2]), "center-only matrix")
actual_matrix = scale(scale_data, center=.false., scale=.false.)
call assert_matrix_close(actual_matrix, scale_data, "disabled matrix scaling")

actual_matrix = scale([1.0_dp, 2.0_dp, 3.0_dp])
if (any(shape(actual_matrix) /= [3, 1])) error stop "scaled vector shape failed"
call assert_vector_close(actual_matrix(:, 1), [-1.0_dp, 0.0_dp, 1.0_dp], &
   "scaled vector values")

center = [1.0_dp, 2.0_dp]
covariance = reshape([4.0_dp, 0.0_dp, 0.0_dp, 9.0_dp], [2, 2])
observations(1, :) = center
observations(2, :) = [3.0_dp, 5.0_dp]
observations(3, :) = [5.0_dp, 2.0_dp]
actual_vector = mahalanobis(observations, center, covariance)
call assert_vector_close(actual_vector, [0.0_dp, 2.0_dp, 4.0_dp], "Mahalanobis distances")

contains

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
end program test_multivariate_statistics
