program test_categorical_data
use r_mod, only: anyDuplicated, ave, dp, duplicated, intersect, prop_table, setdiff, &
   setequal, table2, table_char, table_char_t, tabulate, union, unique
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
integer, allocatable :: counts(:), contingency(:,:)
type(table_char_t) :: character_counts

counts = tabulate([1, 2, 2, 4, 0, -1, 5], 4)
call assert_integer_vector_equal(counts, [1, 2, 0, 1], "integer tabulation")
counts = tabulate([1.0_dp, 1.9_dp, 2.1_dp, 3.0_dp], 3)
call assert_integer_vector_equal(counts, [1, 2, 1], "real-label tabulation")

contingency = table2([1, 1, 2, 2, 2, 3], [1, 2, 1, 2, 2, 1], 2, 2)
call assert_integer_matrix_equal(contingency, reshape([1, 1, 1, 2], [2, 2]), "two-way table")
call assert_real_matrix_close(prop_table(contingency), &
   reshape([0.2_dp, 0.2_dp, 0.2_dp, 0.4_dp], [2, 2]), "overall proportions")
call assert_real_matrix_close(prop_table(contingency, margin=1), &
   reshape([0.5_dp, 1.0_dp / 3.0_dp, 0.5_dp, 2.0_dp / 3.0_dp], [2, 2]), "row proportions")
call assert_real_matrix_close(prop_table(contingency, margin=2), &
   reshape([0.5_dp, 0.5_dp, 1.0_dp / 3.0_dp, 2.0_dp / 3.0_dp], [2, 2]), "column proportions")

character_counts = table_char([character(len=6) :: "pear", "apple", "pear", "banana", "apple"])
call assert_character_vector_equal(character_counts%gene, &
   [character(len=6) :: "apple", "banana", "pear"], "character table labels")
call assert_integer_vector_equal(character_counts%Freq, [2, 1, 2], "character table counts")

call assert_real_vector_close(ave([1.0_dp, 3.0_dp, 10.0_dp, 14.0_dp], [1, 1, 2, 2], "mean"), &
   [2.0_dp, 2.0_dp, 12.0_dp, 12.0_dp], "integer-group averages")
call assert_real_vector_close(ave([1.0_dp, 3.0_dp, 10.0_dp, 14.0_dp], &
   [character(len=1) :: "a", "a", "b", "b"], "sum"), &
   [4.0_dp, 4.0_dp, 24.0_dp, 24.0_dp], "character-group sums")

call assert_integer_vector_equal(unique([3, 1, 3, 2, 1]), [3, 1, 2], "stable unique values")
call assert_logical_vector_equal(duplicated([3, 1, 3, 2, 1]), &
   [.false., .false., .true., .false., .true.], "forward duplicated flags")
call assert_logical_vector_equal(duplicated([3, 1, 3, 2, 1], fromLast=.true.), &
   [.true., .true., .false., .false., .false.], "reverse duplicated flags")
if (anyDuplicated([3, 1, 3, 2, 1]) /= 3) error stop "first duplicate index failed"

call assert_integer_vector_equal(union([3, 1, 3], [2, 1, 4]), [3, 1, 2, 4], "set union")
call assert_integer_vector_equal(intersect([3, 1, 3, 2], [2, 3]), [3, 2], "set intersection")
call assert_integer_vector_equal(setdiff([3, 1, 3, 2], [2, 3]), [1], "set difference")
if (.not. setequal([1, 2, 2, 3], [3, 1, 2])) error stop "set equality failed"
if (setequal([1, 2], [1, 3])) error stop "set inequality failed"

contains

subroutine assert_integer_vector_equal(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector_equal

subroutine assert_integer_matrix_equal(actual, expected, label)
integer, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_matrix_equal

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
end program test_categorical_data
