program test_ordering_and_rle
use, intrinsic :: ieee_arithmetic, only: ieee_is_negative, ieee_negative_inf, ieee_positive_inf, &
   ieee_quiet_nan, ieee_value
use r_mod, only: dp, inverse_rle, order_real, rank_average, rank_first, r_is_nan, r_na_real, rle, &
   rle_char_t, rle_int_t, rle_logical_t, rle_real_t, sort, sort_list
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
type(rle_real_t) :: real_runs
type(rle_int_t) :: int_runs
type(rle_char_t) :: char_runs
type(rle_logical_t) :: logical_runs
real(kind=dp) :: nan_value, na_value, negative_inf, positive_inf
real(kind=dp), allocatable :: ordered(:), ranks(:)
integer :: integer_na

nan_value = ieee_value(0.0_dp, ieee_quiet_nan)
na_value = r_na_real()
negative_inf = ieee_value(0.0_dp, ieee_negative_inf)
positive_inf = ieee_value(0.0_dp, ieee_positive_inf)
integer_na = -huge(0)

call assert_integer_vector_equal(sort_list([3, 1, 2, 1]), [2, 4, 3, 1], &
   "stable integer sort indices")
call assert_integer_vector_equal(sort([3, integer_na, 1, integer_na, 2, 1]), [1, 1, 2, 3], &
   "integer sort omits NA")
call assert_integer_vector_equal(sort([3, integer_na, 1, integer_na, 2, 1], decreasing=.true.), &
   [3, 2, 1, 1], "decreasing integer sort omits NA")
call assert_integer_vector_equal(sort_list([3, integer_na, 1, integer_na, 2, 1]), &
   [3, 6, 5, 1, 2, 4], "integer sort indices place NA last")
call assert_integer_vector_equal(sort_list([3, integer_na, 1, integer_na, 2, 1], decreasing=.true.), &
   [1, 5, 3, 6, 2, 4], "decreasing integer sort indices place NA last")
call assert_integer_vector_equal(sort_list([3.0_dp, 1.0_dp, 2.0_dp, 1.0_dp], decreasing=.true.), &
   [1, 3, 2, 4], "stable decreasing real sort indices")
call assert_integer_vector_equal(sort_list([character(len=1) :: "b", "a", "c", "a"]), &
   [2, 4, 1, 3], "stable character sort indices")
call assert_integer_vector_equal(order_real([30.0_dp, 10.0_dp, 20.0_dp, 10.0_dp]), &
   [2, 4, 3, 1], "real ordering")
call assert_integer_vector_equal(order_real([positive_inf, nan_value, na_value, negative_inf, &
   -0.0_dp, 0.0_dp, 2.0_dp, -1.0_dp]), [4, 8, 5, 6, 7, 1, 2, 3], &
   "real ordering with missing and extreme values")
call assert_integer_vector_equal(order_real([2.0_dp, na_value, nan_value, na_value, nan_value, 1.0_dp]), &
   [6, 1, 2, 3, 4, 5], "stable ordering of mixed missing values")
call assert_integer_vector_equal(sort_list([positive_inf, nan_value, na_value, negative_inf, &
   -0.0_dp, 0.0_dp, 2.0_dp, -1.0_dp], decreasing=.true.), [1, 7, 5, 6, 8, 4, 2, 3], &
   "decreasing real ordering with missing values")
call assert_integer_vector_equal(sort_list([na_value, nan_value]), [1, 2], &
   "all-missing real sort indices")
if (size(sort([na_value, nan_value])) /= 0) error stop "all-missing real sort failed"
if (size(sort([integer_na, integer_na])) /= 0) error stop "all-missing integer sort failed"
if (size(sort([real(kind=dp) ::])) /= 0) error stop "empty real sort failed"
if (size(sort([integer ::])) /= 0) error stop "empty integer sort failed"
if (size(sort([character(len=1) ::])) /= 0) error stop "empty character sort failed"
if (size(sort_list([real(kind=dp) ::])) /= 0) error stop "empty real sort indices failed"
if (size(sort_list([integer ::])) /= 0) error stop "empty integer sort indices failed"
if (size(sort_list([character(len=1) ::])) /= 0) error stop "empty character sort indices failed"
if (size(order_real([real(kind=dp) ::])) /= 0) error stop "empty real ordering failed"

ordered = sort([positive_inf, nan_value, na_value, negative_inf, -0.0_dp, 0.0_dp, 2.0_dp, -1.0_dp])
call assert_real_vector_close(ordered, [negative_inf, -1.0_dp, -0.0_dp, 0.0_dp, 2.0_dp, positive_inf], &
   "sort omits missing values")
if (.not. ieee_is_negative(ordered(3)) .or. ieee_is_negative(ordered(4))) &
   error stop "ascending sort signed-zero stability failed"
ordered = sort([-0.0_dp, 0.0_dp, 2.0_dp, -1.0_dp], decreasing=.true.)
call assert_real_vector_close(ordered, [2.0_dp, -0.0_dp, 0.0_dp, -1.0_dp], "decreasing sort")
if (.not. ieee_is_negative(ordered(2)) .or. ieee_is_negative(ordered(3))) &
   error stop "decreasing sort signed-zero stability failed"

call assert_real_vector_close(rank_first([30.0_dp, 10.0_dp, 20.0_dp, 10.0_dp]), &
   [4.0_dp, 1.0_dp, 3.0_dp, 2.0_dp], "first-tie ranks")
call assert_real_vector_close(rank_average([30.0_dp, 10.0_dp, 20.0_dp, 10.0_dp]), &
   [4.0_dp, 1.5_dp, 3.0_dp, 1.5_dp], "average-tie ranks")
ranks = rank_first([positive_inf, nan_value, na_value, negative_inf, -0.0_dp, 0.0_dp, 2.0_dp, -1.0_dp])
call assert_real_vector_close(ranks, [6.0_dp, 7.0_dp, 8.0_dp, 1.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 2.0_dp], &
   "first ranks with missing values")
ranks = rank_average([positive_inf, nan_value, na_value, negative_inf, -0.0_dp, 0.0_dp, 2.0_dp, -1.0_dp])
call assert_real_vector_close(ranks, [6.0_dp, 7.0_dp, 8.0_dp, 1.0_dp, 3.5_dp, 3.5_dp, 5.0_dp, 2.0_dp], &
   "average ranks with missing values")
if (size(rank_average([real(kind=dp) ::])) /= 0) error stop "empty ranking failed"
if (size(rank_first([real(kind=dp) ::])) /= 0) error stop "empty first-tie ranking failed"

int_runs = rle([1, 1, 2, 2, 2, 1])
call assert_integer_vector_equal(int_runs%lengths, [2, 3, 1], "integer run lengths")
call assert_integer_vector_equal(int_runs%values, [1, 2, 1], "integer run values")
call assert_integer_vector_equal(inverse_rle(int_runs), [1, 1, 2, 2, 2, 1], "integer RLE round trip")

int_runs = rle([1, integer_na, integer_na, 2, 2, integer_na])
call assert_integer_vector_equal(int_runs%lengths, [1, 1, 1, 2, 1], "integer RLE NA run lengths")
call assert_integer_vector_equal(int_runs%values, [1, integer_na, integer_na, 2, integer_na], &
   "integer RLE NA values")
call assert_integer_vector_equal(inverse_rle(int_runs), [1, integer_na, integer_na, 2, 2, integer_na], &
   "integer RLE NA round trip")

real_runs = rle([1.5_dp, 1.5_dp, -2.0_dp, 3.0_dp, 3.0_dp])
call assert_integer_vector_equal(real_runs%lengths, [2, 1, 2], "real run lengths")
call assert_real_vector_close(real_runs%values, [1.5_dp, -2.0_dp, 3.0_dp], "real run values")
call assert_real_vector_close(inverse_rle(real_runs), &
   [1.5_dp, 1.5_dp, -2.0_dp, 3.0_dp, 3.0_dp], "real RLE round trip")

real_runs = rle([1.0_dp, na_value, na_value, nan_value, nan_value, na_value, &
   positive_inf, positive_inf, negative_inf, negative_inf, -0.0_dp, 0.0_dp])
call assert_integer_vector_equal(real_runs%lengths, [1, 1, 1, 1, 1, 1, 2, 2, 2], &
   "real RLE missing and extreme run lengths")
if (.not. all(real_runs%values([2, 3, 4, 5, 6]) /= real_runs%values([2, 3, 4, 5, 6]))) &
   error stop "real RLE missing values failed"
if (any(r_is_nan(real_runs%values([2, 3, 6])))) error stop "real RLE NA payload failed"
if (.not. all(r_is_nan(real_runs%values([4, 5])))) error stop "real RLE NaN payload failed"
call assert_real_vector_missing_equal(inverse_rle(real_runs), &
   [1.0_dp, na_value, na_value, nan_value, nan_value, na_value, positive_inf, positive_inf, &
   negative_inf, negative_inf, -0.0_dp, 0.0_dp], "real RLE missing round trip")

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

real_runs = rle([-0.0_dp, 0.0_dp, -0.0_dp])
call assert_integer_vector_equal(real_runs%lengths, [3], "signed-zero RLE run length")
if (size(real_runs%values) /= 1) error stop "signed-zero RLE values failed"

int_runs = rle([integer ::])
if (size(int_runs%lengths) /= 0 .or. size(int_runs%values) /= 0) error stop "empty RLE metadata failed"
if (size(inverse_rle(int_runs)) /= 0) error stop "empty RLE round trip failed"
char_runs = rle([character(len=1) ::])
if (size(char_runs%lengths) /= 0 .or. size(char_runs%values) /= 0) &
   error stop "empty character RLE failed"
logical_runs = rle([logical ::])
if (size(logical_runs%lengths) /= 0 .or. size(logical_runs%values) /= 0) &
   error stop "empty logical RLE failed"

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

subroutine assert_real_vector_missing_equal(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (actual(i) /= actual(i) .or. expected(i) /= expected(i)) then
      if ((actual(i) == actual(i)) .or. (expected(i) == expected(i))) error stop trim(label) // " missing failed"
      if (r_is_nan(actual(i)) .neqv. r_is_nan(expected(i))) error stop trim(label) // " payload failed"
   else if (actual(i) /= expected(i)) then
      error stop trim(label) // " values failed"
   end if
end do
end subroutine assert_real_vector_missing_equal

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
