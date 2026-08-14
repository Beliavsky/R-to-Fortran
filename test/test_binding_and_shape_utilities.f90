program test_binding_and_shape_utilities
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: cbind, cbind2, col_index_mat, dp, lower_tri, matrix_set_grow_real, &
   max_col, rbind, row_index_mat, upper_tri
implicit none

real(kind=dp) :: real_matrix(2, 2), shape_matrix(3, 2)
integer :: integer_matrix(2, 2)
real(kind=dp), allocatable :: grown(:,:)

call assert_real_matrix(cbind2([1.0_dp, 2.0_dp, 3.0_dp], [10.0_dp, 20.0_dp]), &
   reshape([1.0_dp, 2.0_dp, 3.0_dp, 10.0_dp, 20.0_dp, 10.0_dp], [3, 2]), &
   "two-column binding with recycling")
call assert_real_matrix(cbind([1.0_dp, 2.0_dp], [10.0_dp], [100.0_dp, 200.0_dp, 300.0_dp]), &
   reshape([1.0_dp, 2.0_dp, 1.0_dp, 10.0_dp, 10.0_dp, 10.0_dp, &
   100.0_dp, 200.0_dp, 300.0_dp], [3, 3]), "three-column binding with recycling")
call assert_real_matrix(cbind2([real(kind=dp) ::], [1.0_dp, 2.0_dp, 3.0_dp]), &
   reshape([1.0_dp, 2.0_dp, 3.0_dp], [3, 1]), "empty column omission")

call assert_real_matrix(rbind([1.0_dp, 2.0_dp, 3.0_dp], [10.0_dp, 11.0_dp]), &
   reshape([1.0_dp, 10.0_dp, 2.0_dp, 11.0_dp, 3.0_dp, 10.0_dp], [2, 3]), &
   "real row binding with recycling")
call assert_integer_matrix(rbind([1, 2, 3], [10, 11]), &
   reshape([1, 10, 2, 11, 3, 10], [2, 3]), "integer row binding with recycling")
call assert_integer_matrix(rbind([integer ::], [1, 2, 3]), reshape([1, 2, 3], [1, 3]), &
   "empty row omission")

real_matrix = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2])
integer_matrix = reshape([1, 2, 3, 4], [2, 2])
call assert_real_matrix(rbind([10.0_dp], real_matrix), &
   reshape([10.0_dp, 1.0_dp, 2.0_dp, 10.0_dp, 3.0_dp, 4.0_dp], [3, 2]), &
   "scalar row above matrix")
call assert_real_matrix(rbind(real_matrix, [10.0_dp, 11.0_dp, 12.0_dp]), &
   reshape([1.0_dp, 2.0_dp, 10.0_dp, 3.0_dp, 4.0_dp, 11.0_dp], [3, 2]), &
   "long row below matrix")
call assert_integer_matrix(rbind([10], integer_matrix), &
   reshape([10, 1, 2, 10, 3, 4], [3, 2]), "integer scalar row above matrix")
call assert_real_matrix(rbind([real(kind=dp) ::], real_matrix), real_matrix, &
   "empty vector above matrix")
call assert_integer_matrix(rbind(integer_matrix, [integer ::]), integer_matrix, &
   "empty vector below matrix")

shape_matrix = 0.0_dp
call assert_logical_matrix(lower_tri(shape_matrix), reshape([.false., .true., .true., &
   .false., .false., .true.], [3, 2]), "lower triangular mask")
call assert_logical_matrix(upper_tri(shape_matrix, diag=.true.), reshape([.true., .false., .false., &
   .true., .true., .false.], [3, 2]), "upper triangular mask with diagonal")
call assert_integer_matrix(row_index_mat(shape_matrix), reshape([1, 2, 3, 1, 2, 3], [3, 2]), &
   "row index matrix")
call assert_integer_matrix(col_index_mat(shape_matrix), reshape([1, 1, 1, 2, 2, 2], [3, 2]), &
   "column index matrix")

call assert_integer_vector(max_col(reshape([1.0_dp, 5.0_dp, 3.0_dp, 5.0_dp, &
   2.0_dp, 4.0_dp], [2, 3]), ties_method="first"), [2, 1], "row maxima columns")

allocate(grown(1, 1))
grown(1, 1) = 5.0_dp
call matrix_set_grow_real(grown, 2, 3, 9.0_dp)
if (any(shape(grown) /= [2, 3])) error stop "matrix growth shape failed"
if (abs(grown(1, 1) - 5.0_dp) > 1.0e-12_dp .or. abs(grown(2, 3) - 9.0_dp) > 1.0e-12_dp) &
   error stop "matrix growth retained values failed"
if (.not. ieee_is_nan(grown(2, 1)) .or. .not. ieee_is_nan(grown(1, 2))) &
   error stop "matrix growth NA fill failed"
call matrix_set_grow_real(grown, 1, 2, 7.0_dp)
if (any(shape(grown) /= [2, 3]) .or. abs(grown(1, 2) - 7.0_dp) > 1.0e-12_dp) &
   error stop "in-place matrix assignment failed"

contains

subroutine assert_integer_vector(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector

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

subroutine assert_logical_matrix(actual, expected, label)
logical, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(actual .neqv. expected)) error stop trim(label) // " values failed"
end subroutine assert_logical_matrix
end program test_binding_and_shape_utilities
