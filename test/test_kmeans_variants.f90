program test_kmeans_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
use r_mod, only: dp, kmeans, kmeans_result_t, set_seed_int
implicit none

character(len=14), parameter :: algorithms(4) = [character(len=14) :: &
   "Lloyd", "MacQueen", "Hartigan-Wong", "unknown"]
real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: points(6, 2), identical(4, 2), empty_rows(0, 2), empty_cols(3, 0)
type(kmeans_result_t) :: fit, repeated
integer :: algorithm_index

points(1, :) = [0.0_dp, 0.0_dp]
points(2, :) = [0.0_dp, 1.0_dp]
points(3, :) = [10.0_dp, 10.0_dp]
points(4, :) = [10.0_dp, 11.0_dp]
points(5, :) = [20.0_dp, 0.0_dp]
points(6, :) = [20.0_dp, 1.0_dp]

do algorithm_index = 1, size(algorithms)
   call set_seed_int(701 + algorithm_index)
   fit = kmeans(points, centers=3, nstart=3, iter_max=100, &
      algorithm=trim(algorithms(algorithm_index)))
   call assert_valid_fit(fit, 6, 3, 2, trim(algorithms(algorithm_index)))
   if (any(fit%size /= [2, 2, 2])) error stop "balanced k-means sizes failed"
   if (fit%tot_withinss > 1.5_dp + tolerance) &
      error stop "k-means separated-cluster objective failed"
end do

call set_seed_int(777)
fit = kmeans(points, centers=3, nstart=5, algorithm="MacQueen")
call set_seed_int(777)
repeated = kmeans(points, centers=3, nstart=5, algorithm="MacQueen")
if (any(fit%cluster /= repeated%cluster) .or. &
   any(abs(fit%centers - repeated%centers) > tolerance)) &
   error stop "MacQueen reproducibility failed"

fit = kmeans(points(1:2, :), centers=4, iter_max=0)
call assert_valid_fit(fit, 2, 4, 2, "excess centers")
if (count(fit%size == 0) < 2) error stop "excess-center empty clusters failed"

identical = 3.0_dp
fit = kmeans(identical, centers=3)
call assert_valid_fit(fit, 4, 3, 2, "identical points")
if (abs(fit%totss) > tolerance .or. abs(fit%tot_withinss) > tolerance .or. &
   abs(fit%betweenss) > tolerance) error stop "identical-point sums of squares failed"

fit = kmeans(points(:, 1), centers=0)
call assert_valid_fit(fit, 6, 1, 1, "clamped vector center count")
if (any(fit%cluster /= 1) .or. fit%size(1) /= 6) &
   error stop "clamped vector center membership failed"

fit = kmeans(empty_rows, centers=2)
call assert_valid_fit(fit, 0, 2, 2, "empty rows")
if (fit%iter /= 0 .or. any(fit%size /= 0)) error stop "empty-row diagnostics failed"

fit = kmeans(empty_cols, centers=2)
call assert_valid_fit(fit, 3, 2, 0, "empty columns")
if (fit%iter /= 0 .or. any(fit%size /= 0)) error stop "empty-column diagnostics failed"

contains

subroutine assert_valid_fit(actual, observations, centers, variables, label)
type(kmeans_result_t), intent(in) :: actual
integer, intent(in) :: observations, centers, variables
character(len=*), intent(in) :: label
if (any(shape(actual%centers) /= [centers, variables])) &
   error stop trim(label) // " center shape failed"
if (size(actual%cluster) /= observations .or. size(actual%size) /= centers .or. &
   size(actual%withinss) /= centers) error stop trim(label) // " result shape failed"
if (observations > 0 .and. variables > 0) then
   if (sum(actual%size) /= observations) error stop trim(label) // " size total failed"
   if (any(actual%cluster < 1) .or. any(actual%cluster > centers)) &
      error stop trim(label) // " cluster bounds failed"
else
   if (sum(actual%size) /= 0 .or. any(actual%cluster /= 0)) &
      error stop trim(label) // " empty membership failed"
end if
if (any(.not. ieee_is_finite(actual%centers)) .or. &
   any(.not. ieee_is_finite(actual%withinss))) &
   error stop trim(label) // " finiteness failed"
if (abs(actual%tot_withinss - sum(actual%withinss)) > tolerance) &
   error stop trim(label) // " within sum failed"
if (abs(actual%betweenss - (actual%totss - actual%tot_withinss)) > tolerance) &
   error stop trim(label) // " between sum failed"
end subroutine assert_valid_fit
end program test_kmeans_variants
