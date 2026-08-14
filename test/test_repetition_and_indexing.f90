program test_repetition_and_indexing
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_value, ieee_quiet_nan
use r_mod, only: dp, r_drop_index, r_drop_indices, r_head, r_index_real, &
   r_index_scalar_real, r_rep_char, r_rep_int, r_rep_real
implicit none

integer :: integer_matrix(3, 2)
real(kind=dp) :: real_matrix(3, 2)
real(kind=dp) :: indexed(4)

call assert_integer_vector(r_rep_int([1, 2, 3], times=2), [1, 2, 3, 1, 2, 3], &
   "integer block repetition")
call assert_integer_vector(r_rep_int([1, 2, 3], each=2), [1, 1, 2, 2, 3, 3], &
   "integer element repetition")
call assert_integer_vector(r_rep_int([1, 2, 3], times_vec=[1, 0, 2]), [1, 3, 3], &
   "integer repetition counts")
call assert_integer_vector(r_rep_int([1, 2, 3], times=0, len_out=5), [1, 2, 3, 1, 2], &
   "length-out precedence")
call assert_integer_vector(r_rep_int([1, 2, 3], each=2, times_vec=[6], len_out=7), &
   [1, 1, 2, 2, 3, 3, 1], "length-out after each")
if (size(r_rep_int([1, 2], len_out=0)) /= 0) error stop "zero repetition length failed"
if (size(r_rep_int([integer ::], times=3)) /= 0) error stop "empty integer repetition failed"

call assert_real_vector(r_rep_real([1.5_dp, 2.5_dp], times=2), &
   [1.5_dp, 2.5_dp, 1.5_dp, 2.5_dp], "real repetition")
call assert_character_vector(r_rep_char([character(len=2) :: "a", "bc"], each=2), &
   [character(len=2) :: "a", "a", "bc", "bc"], "character repetition")
call assert_character_vector(r_rep_char([character(len=1) :: "a", "b"], times=0, len_out=3), &
   [character(len=1) :: "a", "b", "a"], "character length-out precedence")

call assert_integer_vector(r_drop_index([10, 20, 30, 40], 2), [10, 30, 40], &
   "drop integer index")
call assert_real_vector(r_drop_index([1.0_dp, 2.0_dp, 3.0_dp], 8), &
   [1.0_dp, 2.0_dp, 3.0_dp], "ignore out-of-range drop")
call assert_integer_vector(r_drop_indices([10, 20, 30, 40, 50], [2, 2, 4, 9]), &
   [10, 30, 50], "drop integer indices")
call assert_real_vector(r_drop_indices([1.0_dp, 2.0_dp, 3.0_dp], [1, 3]), [2.0_dp], &
   "drop real indices")
if (size(r_drop_index([integer ::], 1)) /= 0) error stop "empty drop failed"
call assert_integer_vector(r_drop_index([10, 20, 30], 0), [10, 20, 30], &
   "zero scalar drop ignored")
call assert_real_vector(r_drop_index([1.0_dp, 2.0_dp, 3.0_dp], 3), [1.0_dp, 2.0_dp], &
   "drop final real index")
call assert_integer_vector(r_drop_indices([10, 20, 30], [integer ::]), [10, 20, 30], &
   "empty drop-index vector")
if (size(r_drop_indices([1.0_dp, 2.0_dp], [1, 2])) /= 0) &
   error stop "drop all real indices failed"

call assert_integer_vector(r_head([1, 2, 3, 4], 2), [1, 2], "integer head")
call assert_real_vector(r_head([1.0_dp, 2.0_dp], 8), [1.0_dp, 2.0_dp], "oversized head")
call assert_integer_vector(r_head([1, 2, 3], -1), [1, 2], "negative head")

integer_matrix = reshape([1, 2, 3, 4, 5, 6], [3, 2])
real_matrix = real(integer_matrix, kind=dp)
call assert_integer_matrix(r_head(integer_matrix, 2), reshape([1, 2, 4, 5], [2, 2]), &
   "integer matrix head")
call assert_real_matrix(r_head(real_matrix, 1), reshape([1.0_dp, 4.0_dp], [1, 2]), &
   "real matrix head")

if (abs(r_index_scalar_real([1.0_dp, 2.0_dp], 2) - 2.0_dp) > 1.0e-12_dp) &
   error stop "scalar real indexing failed"
if (.not. ieee_is_nan(r_index_scalar_real([1.0_dp, 2.0_dp], 0))) &
   error stop "out-of-range scalar indexing failed"
indexed = r_index_real([10, 20, 30], [1.0_dp, 2.9_dp, 4.0_dp, &
   ieee_value(0.0_dp, ieee_quiet_nan)])
if (abs(indexed(1) - 10.0_dp) > 1.0e-12_dp .or. abs(indexed(2) - 20.0_dp) > 1.0e-12_dp) &
   error stop "real-vector indexing values failed"
if (.not. ieee_is_nan(indexed(3)) .or. .not. ieee_is_nan(indexed(4))) &
   error stop "real-vector indexing NA failed"
call assert_real_vector(r_index_real([10, 20, 30, 40], [-2.9_dp, 0.0_dp, -4.0_dp]), &
   [10.0_dp, 30.0_dp], "negative real-index exclusion")
call assert_real_vector(r_index_real([10, 20, 30], [0.0_dp, 2.9_dp]), [20.0_dp], &
   "zero real index omission")
indexed(1:3) = r_index_real([10, 20, 30], [2.9_dp, &
   ieee_value(0.0_dp, ieee_quiet_nan), 5.0_dp])
if (abs(indexed(1) - 20.0_dp) > 1.0e-12_dp .or. .not. ieee_is_nan(indexed(2)) .or. &
   .not. ieee_is_nan(indexed(3))) error stop "real-index NA and bounds failed"

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

subroutine assert_character_vector(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector
end program test_repetition_and_indexing
