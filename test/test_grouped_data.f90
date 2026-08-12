program test_grouped_data
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: aggregate, aggregate_result_t, by_matrix_result_t, dp, r_by
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: matrix_values(5, 2)
real(kind=dp), allocatable :: grouped_values(:)
type(aggregate_result_t) :: summary
type(by_matrix_result_t) :: matrix_summary

summary = aggregate([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [character(len=1) :: "b", "a", "b", "c", "a"], "group", "score", "mean")
call assert_string_equal(summary%group_name, "group", "aggregate group name")
call assert_string_equal(summary%value_name, "score", "aggregate value name")
call assert_character_vector_equal(summary%labels, [character(len=1) :: "b", "a", "c"], &
   "aggregate character labels")
call assert_real_vector_close(summary%values, [7.0_dp, 25.0_dp, 8.0_dp], &
   "aggregate character means")

summary = aggregate([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [2, 1, 2, 3, 1], "id", "score", "sum")
call assert_character_vector_equal(summary%labels, [character(len=1) :: "2", "1", "3"], &
   "aggregate integer labels")
call assert_real_vector_close(summary%values, [14.0_dp, 50.0_dp, 8.0_dp], &
   "aggregate integer sums")

summary = aggregate([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [2, 1, 2, 3, 1], "id", "score", "min")
call assert_real_vector_close(summary%values, [4.0_dp, 20.0_dp, 8.0_dp], "aggregate minima")
summary = aggregate([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [2, 1, 2, 3, 1], "id", "score", "max")
call assert_real_vector_close(summary%values, [10.0_dp, 30.0_dp, 8.0_dp], "aggregate maxima")
summary = aggregate([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [2, 1, 2, 3, 1], "id", "score", "length")
call assert_real_vector_close(summary%values, [2.0_dp, 2.0_dp, 1.0_dp], "aggregate lengths")

summary = aggregate([1.0_dp, 2.0_dp, 99.0_dp], [1, 2], "id", "value", "mean")
call assert_real_vector_close(summary%values, [1.0_dp, 2.0_dp], "aggregate shortest-input length")
summary = aggregate([1.0_dp], [1], "id", "value", "unsupported")
if (.not. ieee_is_nan(summary%values(1))) error stop "unsupported aggregate function failed"

grouped_values = r_by([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [character(len=1) :: "b", "a", "b", "c", "a"], "mean")
call assert_real_vector_close(grouped_values, [7.0_dp, 25.0_dp, 8.0_dp], "vector by means")
grouped_values = r_by([10.0_dp, 20.0_dp, 4.0_dp, 8.0_dp, 30.0_dp], &
   [2, 1, 2, 3, 1], "sum")
call assert_real_vector_close(grouped_values, [14.0_dp, 50.0_dp, 8.0_dp], "vector by sums")

matrix_values(:, 1) = [1.0_dp, 3.0_dp, 10.0_dp, 14.0_dp, 5.0_dp]
matrix_values(:, 2) = [2.0_dp, 4.0_dp, 20.0_dp, 24.0_dp, 6.0_dp]
matrix_summary = r_by(matrix_values, [character(len=1) :: "a", "a", "b", "b", "a"], &
   "colmeans")
call assert_character_vector_equal(matrix_summary%labels, [character(len=1) :: "a", "b"], &
   "matrix by character labels")
call assert_real_matrix_close(matrix_summary%values, reshape([3.0_dp, 12.0_dp, 4.0_dp, 22.0_dp], [2, 2]), &
   "matrix by column means")

matrix_summary = r_by(matrix_values, [2, 2, 1, 1, 2], "colsums")
call assert_character_vector_equal(matrix_summary%labels, [character(len=1) :: "2", "1"], &
   "matrix by integer labels")
call assert_real_matrix_close(matrix_summary%values, reshape([9.0_dp, 24.0_dp, 12.0_dp, 44.0_dp], [2, 2]), &
   "matrix by column sums")

contains

subroutine assert_string_equal(actual, expected, label)
character(len=*), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (trim(actual) /= trim(expected)) error stop trim(label) // " failed"
end subroutine assert_string_equal

subroutine assert_character_vector_equal(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector_equal

subroutine assert_real_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_vector_close

subroutine assert_real_matrix_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix_close
end program test_grouped_data
