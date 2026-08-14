program test_clustering_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
use r_mod, only: cutree, dist, dp, hclust, hclust_result_t
implicit none

character(len=8), parameter :: methods(8) = [character(len=8) :: &
   "complete", "single", "average", "mcquitty", "centroid", "median", &
   "ward.D", "ward.D2"]
real(kind=dp), parameter :: expected_last(6) = &
   [11.0_dp, 9.0_dp, 10.0_dp, 10.0_dp, 9.5_dp, 9.5_dp]
real(kind=dp) :: points(4, 1), singleton(1, 2), invalid(2, 3)
real(kind=dp), allocatable :: distances(:,:)
integer, allocatable :: groups(:)
type(hclust_result_t) :: fit
integer :: method_index

points(:, 1) = [0.0_dp, 1.0_dp, 10.0_dp, 11.0_dp]
distances = dist(points)
do method_index = 1, size(methods)
   fit = hclust(distances, method=trim(methods(method_index)))
   if (fit%method /= method_index) error stop "linkage method code failed"
   if (any(shape(fit%merge) /= [3, 2])) error stop "linkage merge shape failed"
   if (size(fit%height) /= 3 .or. size(fit%order) /= 4) &
      error stop "linkage result shape failed"
   if (any(.not. ieee_is_finite(fit%height))) error stop "linkage height finiteness failed"
   if (abs(fit%height(1) - 1.0_dp) > 1.0e-12_dp .or. &
      abs(fit%height(2) - 1.0_dp) > 1.0e-12_dp) &
      error stop "initial linkage heights failed"
   if (method_index <= size(expected_last)) then
      if (abs(fit%height(3) - expected_last(method_index)) > 1.0e-12_dp) &
         error stop "final linkage height failed"
   end if
   groups = cutree(fit, k=2)
   call assert_pair_partition(groups)
end do

fit = hclust(distances, method="unknown")
if (fit%method /= 1) error stop "unknown linkage fallback failed"
groups = cutree(fit, k=0)
if (any(groups /= groups(1))) error stop "lower cluster-count clamp failed"
groups = cutree(fit, k=99)
if (any(groups /= [1, 2, 3, 4])) error stop "upper cluster-count clamp failed"
groups = cutree(fit, h=-1.0_dp)
if (any(groups /= [1, 2, 3, 4])) error stop "low height cut failed"
groups = cutree(fit, h=huge(1.0_dp))
if (any(groups /= groups(1))) error stop "high height cut failed"

singleton = 0.0_dp
fit = hclust(dist(singleton), labels=[character(len=4) :: "only"])
if (any(shape(fit%merge) /= [0, 2]) .or. size(fit%height) /= 0) &
   error stop "singleton clustering shape failed"
if (size(fit%order) /= 1 .or. fit%order(1) /= 1) &
   error stop "singleton clustering order failed"
if (trim(fit%labels(1)) /= "only") error stop "singleton clustering label failed"
groups = cutree(fit, k=1)
if (size(groups) /= 1 .or. groups(1) /= 1) error stop "singleton tree cut failed"

invalid = 0.0_dp
fit = hclust(invalid)
if (size(fit%merge, 1) /= 0 .or. size(fit%order) /= 0) &
   error stop "nonsquare distance rejection failed"

distances = dist(reshape([real(kind=dp) ::], [0, 2]))
if (any(shape(distances) /= [0, 0])) error stop "empty distance matrix failed"

contains

subroutine assert_pair_partition(actual)
integer, intent(in) :: actual(:)
if (size(actual) /= 4) error stop "two-cluster partition shape failed"
if (actual(1) /= actual(2) .or. actual(3) /= actual(4) .or. &
   actual(1) == actual(3)) error stop "two-cluster partition failed"
end subroutine assert_pair_partition
end program test_clustering_variants
