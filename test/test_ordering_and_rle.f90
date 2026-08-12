program test_ordering_and_rle
use r_mod, only: dp, inverse_rle, order_real, rank_average, rank_first, rle, &
   rle_char_t, rle_int_t, rle_logical_t, rle_real_t, sort_list
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
type(rle_real_t) :: real_runs
type(rle_int_t) :: int_runs
type(rle_char_t) :: char_runs
type(rle_logical_t) :: logical_runs

call assert_integer_vector_equal(sort_list([3, 1, 2, 1]), [2, 4, 3, 1], &
   "stable integer sort indices")
call assert_integer_vector_equal(sort_list([3.0_dp, 1.0_dp, 2.0_dp, 1.0_dp], decreasing=.true.), &
   [1, 3, 2, 4], "stable decreasing real sort indices")
call assert_integer_vector_equal(sort_list([character(len=1) :: "b", "a", "c", "a"]), &
   [2, 4, 1, 3], "stable character sort indices")
call assert_integer_vector_equal(order_real([30.0_dp, 10.0_dp, 20.0_dp, 10.0_dp]), &
   [2, 4, 3, 1], "real ordering")

call assert_real_vector_close(rank_first([30.0_dp, 10.0_dp, 20.0_dp, 10.0_dp]), &
   [4.0_dp, 1.0_dp, 3.0_dp, 2.0_dp], "first-tie ranks")
call assert_real_vector_close(rank_average([30.0_dp, 10.0_dp, 20.0_dp, 10.0_dp]), &
   [4.0_dp, 1.5_dp, 3.0_dp, 1.5_dp], "average-tie ranks")
if (size(rank_average([real(kind=dp) ::])) /= 0) error stop "empty ranking failed"

int_runs = rle([1, 1, 2, 2, 2, 1])
call assert_integer_vector_equal(int_runs%lengths, [2, 3, 1], "integer run lengths")
call assert_integer_vector_equal(int_runs%values, [1, 2, 1], "integer run values")
call assert_integer_vector_equal(inverse_rle(int_runs), [1, 1, 2, 2, 2, 1], "integer RLE round trip")

real_runs = rle([1.5_dp, 1.5_dp, -2.0_dp, 3.0_dp, 3.0_dp])
call assert_integer_vector_equal(real_runs%lengths, [2, 1, 2], "real run lengths")
call assert_real_vector_close(real_runs%values, [1.5_dp, -2.0_dp, 3.0_dp], "real run values")
call assert_real_vector_close(inverse_rle(real_runs), &
   [1.5_dp, 1.5_dp, -2.0_dp, 3.0_dp, 3.0_dp], "real RLE round trip")

char_runs = rle([character(len=4) :: "red", "red", "blue", "blue", "red"])
call assert_integer_vector_equal(char_runs%lengths, [2, 2, 1], "character run lengths")
call assert_character_vector_equal(char_runs%values, &
   [character(len=4) :: "red", "blue", "red"], "character run values")
call assert_character_vector_equal(inverse_rle(char_runs), &
   [character(len=4) :: "red", "red", "blue", "blue", "red"], "character RLE round trip")

logical_runs = rle([.true., .true., .false., .true.])
call assert_integer_vector_equal(logical_runs%lengths, [2, 1, 1], "logical run lengths")
call assert_logical_vector_equal(logical_runs%values, [.true., .false., .true.], "logical run values")
call assert_logical_vector_equal(inverse_rle(logical_runs), [.true., .true., .false., .true.], &
   "logical RLE round trip")

int_runs = rle([integer ::])
if (size(int_runs%lengths) /= 0 .or. size(int_runs%values) /= 0) error stop "empty RLE metadata failed"
if (size(inverse_rle(int_runs)) /= 0) error stop "empty RLE round trip failed"

contains

subroutine assert_integer_vector_equal(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector_equal

subroutine assert_real_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_vector_close

subroutine assert_logical_vector_equal(actual, expected, label)
logical, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual .neqv. expected)) error stop trim(label) // " values failed"
end subroutine assert_logical_vector_equal

subroutine assert_character_vector_equal(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector_equal
end program test_ordering_and_rle
