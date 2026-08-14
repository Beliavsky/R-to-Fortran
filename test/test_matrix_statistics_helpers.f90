program test_matrix_statistics_helpers
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: apply_col_cumsum, apply_col_sd, apply_row_sd, colMeans, dp
implicit none

real(kind=dp) :: x(3, 2)
real(kind=dp), allocatable :: empty_rows(:,:), empty_cols(:,:)
real(kind=dp), allocatable :: values(:)

x = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [3, 2])
call assert_real_vector(colMeans(x), [2.0_dp, 5.0_dp], "column means")
call assert_real_matrix(apply_col_cumsum(x), &
   reshape([1.0_dp, 3.0_dp, 6.0_dp, 4.0_dp, 9.0_dp, 15.0_dp], [3, 2]), &
   "column cumulative sums")
call assert_real_vector(apply_col_sd(x), [1.0_dp, 1.0_dp], "column standard deviations")
call assert_real_vector(apply_row_sd(x), &
   [3.0_dp / sqrt(2.0_dp), 3.0_dp / sqrt(2.0_dp), 3.0_dp / sqrt(2.0_dp)], &
   "row standard deviations")

allocate(empty_rows(0, 2), empty_cols(3, 0))
values = colMeans(empty_rows)
if (size(values) /= 2 .or. .not. all(ieee_is_nan(values))) &
   error stop "empty-row column means failed"
if (any(shape(apply_col_cumsum(empty_rows)) /= [0, 2])) &
   error stop "empty-row cumulative sums failed"
values = apply_col_sd(empty_rows)
if (size(values) /= 2 .or. .not. all(ieee_is_nan(values))) &
   error stop "empty-row column deviations failed"
if (size(colMeans(empty_cols)) /= 0) error stop "empty-column means failed"
if (size(apply_col_sd(empty_cols)) /= 0) error stop "empty-column deviations failed"

contains

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix
end program test_matrix_statistics_helpers
