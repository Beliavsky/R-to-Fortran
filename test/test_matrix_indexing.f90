program test_matrix_indexing
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: dp, matrix_elem, r_matrix_col, r_matrix_col_filter, r_matrix_index, &
   r_matrix_row, r_matrix_row_filter, r_matrix_rows
implicit none

integer :: integer_matrix(3, 3)
real(kind=dp) :: real_matrix(3, 3)
logical :: matrix_mask(3, 3)
integer, allocatable :: integer_values(:)
real(kind=dp), allocatable :: real_values(:)

integer_matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
real_matrix = real(integer_matrix, kind=dp)

if (abs(matrix_elem(real_matrix, 2, 3) - 8.0_dp) > 1.0e-12_dp) &
   error stop "matrix element failed"
call assert_integer_vector(r_matrix_col(integer_matrix, 2), [4, 5, 6], "integer column")
call assert_real_vector(r_matrix_col(real_matrix, 3), [7.0_dp, 8.0_dp, 9.0_dp], &
   "real column")
call assert_integer_vector(r_matrix_row(integer_matrix, 2), [2, 5, 8], "integer row")
call assert_real_vector(r_matrix_row(real_matrix, 3), [3.0_dp, 6.0_dp, 9.0_dp], "real row")

call assert_integer_matrix(r_matrix_rows(integer_matrix, [3, 1]), &
   reshape([3, 1, 6, 4, 9, 7], [2, 3]), "selected integer rows")
call assert_real_matrix(r_matrix_rows(real_matrix, [2, 4]), &
   reshape([2.0_dp, 0.0_dp, 5.0_dp, 0.0_dp, 8.0_dp, 0.0_dp], [2, 3]), &
   "selected real rows with invalid index")
call assert_integer_matrix(r_matrix_row_filter(integer_matrix, [.true., .false.]), &
   reshape([1, 3, 4, 6, 7, 9], [2, 3]), "recycled row filter")
call assert_real_matrix(r_matrix_col_filter(real_matrix, [.false., .true.]), &
   reshape([4.0_dp, 5.0_dp, 6.0_dp], [3, 1]), "recycled column filter")
if (size(r_matrix_row_filter(integer_matrix, [logical ::]), 1) /= 0) &
   error stop "empty row filter failed"
if (size(r_matrix_col_filter(real_matrix, [logical ::]), 2) /= 0) &
   error stop "empty column filter failed"

call assert_integer_vector(r_matrix_index(integer_matrix, [1, 6, 9, 0]), [1, 6, 9], &
   "positive linear matrix indexing")
call assert_real_vector(r_matrix_index(real_matrix, [-1, -4]), &
   [2.0_dp, 3.0_dp, 5.0_dp, 6.0_dp, 7.0_dp, 8.0_dp, 9.0_dp], &
   "negative linear matrix indexing")
integer_values = r_matrix_index(integer_matrix, [1, 10])
if (size(integer_values) /= 2) error stop "integer out-of-range matrix index shape failed"
if (integer_values(1) /= 1 .or. integer_values(2) /= -huge(0)) &
   error stop "integer out-of-range matrix index values failed"
real_values = r_matrix_index(real_matrix, [1, 10])
if (size(real_values) /= 2) error stop "real out-of-range matrix index shape failed"
if (abs(real_values(1) - 1.0_dp) > 1.0e-12_dp .or. .not. ieee_is_nan(real_values(2))) &
   error stop "real out-of-range matrix index values failed"

call assert_integer_vector(r_matrix_index(integer_matrix, [.true., .false.]), [1, 3, 5, 7, 9], &
   "recycled logical matrix indexing")
real_values = r_matrix_index(real_matrix, &
   [.false., .false., .false., .false., .false., .false., .false., .false., .false., .true.])
if (size(real_values) /= 1 .or. .not. ieee_is_nan(real_values(1))) &
   error stop "extended logical matrix indexing failed"
matrix_mask = reshape([.true., .false., .false., .false., .true., .false., &
   .false., .false., .true.], [3, 3])
call assert_real_vector(r_matrix_index(real_matrix, matrix_mask), [1.0_dp, 5.0_dp, 9.0_dp], &
   "logical matrix-mask indexing")

call assert_integer_vector(r_matrix_index([10, 20, 30], [3, 0, 1]), [30, 10], &
   "integer vector indexing")
integer_values = r_matrix_index([10, 20], [1, 3])
if (size(integer_values) /= 2 .or. integer_values(1) /= 10 .or. &
   integer_values(2) /= -huge(0)) error stop "integer vector NA indexing failed"
call assert_integer_vector(r_matrix_index([10, 20, 30, 40], [-2, -4]), [10, 30], &
   "negative integer vector indexing")
call assert_real_vector(r_matrix_index([1.0_dp, 2.0_dp, 3.0_dp], [.true., .false.]), &
   [1.0_dp, 3.0_dp], "logical real-vector indexing")

contains

subroutine assert_integer_vector(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

subroutine assert_integer_matrix(actual, expected, label)
integer, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_matrix

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix
end program test_matrix_indexing
