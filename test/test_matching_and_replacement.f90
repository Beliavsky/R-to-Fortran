program test_matching_and_replacement
use r_mod, only: dp, match, r_in, replace, which, which_arr_ind, which_first, which_last
implicit none

logical :: matrix_mask(2, 3)

call assert_integer_vector(match([3, 1, 4], [1, 3, 3]), [2, 1, -huge(0)], &
   "integer matching")
call assert_integer_vector(match([2.5_dp, -1.0_dp], [-1.0_dp, 2.5_dp, 2.5_dp]), [2, 1], &
   "real matching")
call assert_integer_vector(match([character(len=5) :: "beta", "none"], &
   [character(len=5) :: "alpha", "beta", "beta"]), [2, -huge(0)], "character matching")
if (size(match([integer ::], [1, 2])) /= 0) error stop "empty match query failed"
call assert_integer_vector(match([1, 2], [integer ::]), [-huge(0), -huge(0)], &
   "empty match table")

call assert_logical_vector(r_in([1, 2, 4], [2, 4]), [.false., .true., .true.], &
   "integer membership")
call assert_logical_vector(r_in([1.0_dp, 2.5_dp, 4.0_dp], [1, 4]), &
   [.true., .false., .true.], "real-in-integer membership")
call assert_logical_vector(r_in([1, 3], [1.0_dp, 2.0_dp]), [.true., .false.], &
   "integer-in-real membership")
call assert_logical_vector(r_in([character(len=4) :: "red", "blue"], &
   [character(len=5) :: "green", "red"]), [.true., .false.], "character membership")
call assert_logical_vector(r_in([.true., .false.], [.false.]), [.false., .true.], &
   "logical membership")
if (.not. r_in(2, [1, 2, 3])) error stop "integer scalar membership failed"
if (r_in(2.5_dp, [2, 3])) error stop "mixed scalar membership failed"

call assert_integer_vector(which([.false., .true., .false., .true.]), [2, 4], &
   "vector which")
if (which_first([.false., .true., .true.]) /= 2) error stop "which_first failed"
if (which_last([.true., .false., .true.]) /= 3) error stop "which_last failed"
if (which_first([.false., .false.]) /= 0) error stop "empty which_first failed"
if (which_last([.false., .false.]) /= 0) error stop "empty which_last failed"

matrix_mask = reshape([.true., .false., .false., .true., .true., .false.], [2, 3])
call assert_integer_vector(which(matrix_mask), [1, 4, 5], "matrix which")
call assert_integer_matrix(which_arr_ind(matrix_mask), reshape([1, 2, 1, 1, 2, 3], [3, 2]), &
   "array-index which")

call assert_real_vector(replace([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 4], 9.0_dp), &
   [1.0_dp, 9.0_dp, 3.0_dp, 9.0_dp], "real indexed scalar replacement")
call assert_real_vector(replace([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [1, 2, 3], &
   [8.0_dp, 7.0_dp]), [8.0_dp, 7.0_dp, 8.0_dp, 4.0_dp], "real indexed recycling")
call assert_real_vector(replace([1.0_dp, 2.0_dp, 3.0_dp], [0, 4], 9.0_dp), &
   [1.0_dp, 2.0_dp, 3.0_dp], "out-of-range replacement")
call assert_real_vector(replace([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   [.true., .false., .true., .true.], [9.0_dp, 8.0_dp]), &
   [9.0_dp, 2.0_dp, 8.0_dp, 9.0_dp], "real mask recycling")

call assert_integer_vector(replace([1, 2, 3, 4], [2, 4], 0), [1, 0, 3, 0], &
   "integer indexed replacement")
call assert_integer_vector(replace([1, 2, 3], [2, 2], [8, 9]), [1, 9, 3], &
   "duplicate indexed replacement")
call assert_integer_vector(replace([1, 2, 3], [integer ::], [8]), [1, 2, 3], &
   "empty indexed replacement")
call assert_integer_vector(replace([1, 2, 3, 4], [.false., .true., .true., .false.], [8, 9]), &
   [1, 8, 9, 4], "integer mask replacement")
call assert_real_vector(replace([1, 2, 3], [2], 2.5_dp), [1.0_dp, 2.5_dp, 3.0_dp], &
   "integer-to-real indexed replacement")
call assert_real_vector(replace([1, 2, 3], [.true., .false., .true.], [0.5_dp, 1.5_dp]), &
   [0.5_dp, 2.0_dp, 1.5_dp], "integer-to-real mask replacement")

call assert_character_vector(replace([character(len=3) :: "one", "two", "six"], &
   [.false., .true., .true.], "changed"), &
   [character(len=7) :: "one", "changed", "changed"], "character scalar replacement")
call assert_character_vector(replace([character(len=3) :: "one", "two", "six"], &
   [.true., .false., .true.], [character(len=5) :: "alpha", "beta"]), &
   [character(len=5) :: "alpha", "two", "beta"], "character vector replacement")
call assert_logical_vector(replace([.true., .false., .false.], [.true., .false., .true.], .true.), &
   [.true., .false., .true.], "logical replacement")

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

subroutine assert_logical_vector(actual, expected, label)
logical, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual .neqv. expected)) error stop trim(label) // " values failed"
end subroutine assert_logical_vector

subroutine assert_character_vector(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector
end program test_matching_and_replacement
