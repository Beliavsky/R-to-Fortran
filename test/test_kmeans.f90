program test_kmeans
use r_mod, only: dp, kmeans, kmeans_result_t, set_seed_int
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: points(4, 2)
real(kind=dp) :: sorted_centers(2), matrix_centers(2, 2)
type(kmeans_result_t) :: fit, repeated

fit = kmeans([0.0_dp, 1.0_dp, 10.0_dp, 11.0_dp], centers=2)
call assert_two_pair_partition(fit%cluster, "one-dimensional k-means partition")
if (any(fit%size /= [2, 2])) error stop "one-dimensional k-means sizes failed"
sorted_centers = sort_two(fit%centers(:, 1))
call assert_vector_close(sorted_centers, [0.5_dp, 10.5_dp], &
   "one-dimensional k-means centers")
call assert_vector_close(fit%withinss, [0.5_dp, 0.5_dp], &
   "one-dimensional within-cluster sums")
call assert_close(fit%totss, 101.0_dp, "one-dimensional total sum of squares")
call assert_close(fit%tot_withinss, 1.0_dp, "one-dimensional total within sum")
call assert_close(fit%betweenss, 100.0_dp, "one-dimensional between sum")
if (fit%iter < 1) error stop "one-dimensional k-means iteration count failed"

points(1, :) = [0.0_dp, 0.0_dp]
points(2, :) = [0.0_dp, 2.0_dp]
points(3, :) = [10.0_dp, 10.0_dp]
points(4, :) = [10.0_dp, 12.0_dp]
fit = kmeans(points, centers=2)
call assert_two_pair_partition(fit%cluster, "matrix k-means partition")
if (any(fit%size /= [2, 2])) error stop "matrix k-means sizes failed"
matrix_centers = fit%centers
if (matrix_centers(1, 1) > matrix_centers(2, 1)) &
   matrix_centers = fit%centers([2, 1], :)
call assert_matrix_close(matrix_centers, reshape([0.0_dp, 10.0_dp, 1.0_dp, 11.0_dp], [2, 2]), &
   "matrix k-means centers")
call assert_vector_close(fit%withinss, [2.0_dp, 2.0_dp], "matrix within-cluster sums")
call assert_close(fit%totss, 204.0_dp, "matrix total sum of squares")
call assert_close(fit%tot_withinss, 4.0_dp, "matrix total within sum")
call assert_close(fit%betweenss, 200.0_dp, "matrix between sum")

call set_seed_int(24680)
fit = kmeans(points, centers=2, nstart=4)
call set_seed_int(24680)
repeated = kmeans(points, centers=2, nstart=4)
call assert_matrix_close(fit%centers, repeated%centers, "seeded k-means centers")
if (any(fit%cluster /= repeated%cluster)) error stop "seeded k-means clusters failed"
call assert_vector_close(fit%withinss, repeated%withinss, "seeded k-means diagnostics")

fit = kmeans([1.0_dp, 2.0_dp, 3.0_dp], centers=1)
if (any(fit%cluster /= 1) .or. any(fit%size /= [3])) error stop "single-cluster membership failed"
call assert_close(fit%centers(1, 1), 2.0_dp, "single-cluster center")
call assert_close(fit%withinss(1), 2.0_dp, "single-cluster within sum")
call assert_close(fit%betweenss, 0.0_dp, "single-cluster between sum")

contains

pure function sort_two(x) result(out)
real(kind=dp), intent(in) :: x(2)
real(kind=dp) :: out(2)

out = [minval(x), maxval(x)]
end function sort_two

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
end program test_kmeans
