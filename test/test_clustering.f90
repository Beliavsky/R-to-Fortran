program test_clustering
use r_mod, only: cutree, dist, dp, hclust, hclust_result_t
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: points(3, 2), line_points(4, 1)
real(kind=dp), allocatable :: distances(:,:)
integer, allocatable :: groups(:)
type(hclust_result_t) :: fit

points(1, :) = [0.0_dp, 0.0_dp]
points(2, :) = [3.0_dp, 4.0_dp]
points(3, :) = [3.0_dp, -4.0_dp]

distances = dist(points)
call assert_matrix_close(distances, reshape([0.0_dp, 5.0_dp, 5.0_dp, &
   5.0_dp, 0.0_dp, 8.0_dp, 5.0_dp, 8.0_dp, 0.0_dp], [3, 3]), &
   "Euclidean distances")
distances = dist(points, method="manhattan")
call assert_matrix_close(distances, reshape([0.0_dp, 7.0_dp, 7.0_dp, &
   7.0_dp, 0.0_dp, 8.0_dp, 7.0_dp, 8.0_dp, 0.0_dp], [3, 3]), &
   "Manhattan distances")
distances = dist(points, method="maximum")
call assert_matrix_close(distances, reshape([0.0_dp, 4.0_dp, 4.0_dp, &
   4.0_dp, 0.0_dp, 8.0_dp, 4.0_dp, 8.0_dp, 0.0_dp], [3, 3]), &
   "maximum distances")
distances = dist(points, method="canberra")
call assert_matrix_close(distances, reshape([0.0_dp, 2.0_dp, 2.0_dp, &
   2.0_dp, 0.0_dp, 1.0_dp, 2.0_dp, 1.0_dp, 0.0_dp], [3, 3]), &
   "Canberra distances")

line_points(:, 1) = [0.0_dp, 1.0_dp, 10.0_dp, 11.0_dp]
distances = dist(line_points)
fit = hclust(distances, method="complete", labels=[character(len=1) :: "a", "b", "c", "d"])
if (fit%method /= 1) error stop "complete-linkage method code failed"
call assert_integer_matrix_equal(fit%merge, reshape([-1, -3, 1, -2, -4, 2], [3, 2]), &
   "complete-linkage merges")
call assert_vector_close(fit%height, [1.0_dp, 1.0_dp, 11.0_dp], &
   "complete-linkage heights")
if (any(fit%order /= [1, 2, 3, 4])) error stop "complete-linkage order failed"
if (any(fit%labels /= [character(len=1) :: "a", "b", "c", "d"])) &
   error stop "hierarchical-clustering labels failed"

groups = cutree(fit, k=2)
call assert_two_pair_partition(groups, "two-cluster tree cut")
groups = cutree(fit, h=1.0_dp)
call assert_two_pair_partition(groups, "height-based tree cut")
groups = cutree(fit, k=4)
if (size(groups) /= 4) error stop "four-cluster tree cut size failed"
if (any(groups /= [1, 2, 3, 4])) error stop "four-cluster tree cut failed"
groups = cutree(fit)
if (any(groups /= groups(1))) error stop "default one-cluster tree cut failed"

contains

subroutine assert_two_pair_partition(actual, label)
integer, intent(in) :: actual(:)
character(len=*), intent(in) :: label

if (size(actual) /= 4) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (actual(1) /= actual(2) .or. actual(3) /= actual(4) .or. actual(1) == actual(3)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_two_pair_partition

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

subroutine assert_integer_matrix_equal(actual, expected, label)
integer, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) then
   write(*, '(a)') trim(label) // " shape failed"
   error stop 1
end if
if (any(actual /= expected)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_integer_matrix_equal
end program test_clustering
