program test_categorical_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_negative, ieee_quiet_nan, ieee_value
use r_mod, only: anyDuplicated, dp, duplicated, intersect, match, prop_table, &
   is_na, r_in, r_inf, r_is_nan, r_na_real, setdiff, setequal, table2, &
   table_char, table_char_t, tabulate, union, unique
implicit none

integer, allocatable :: integer_values(:), integer_table(:,:)
real(kind=dp), allocatable :: real_values(:), proportions(:,:)
character(len=:), allocatable :: character_values(:)
logical, allocatable :: logical_values(:)
type(table_char_t) :: character_table

integer_values = tabulate([integer ::], 3)
call assert_integer_vector(integer_values, [0, 0, 0], "empty tabulation")
if (size(tabulate([1, 2, 3], -2)) /= 0) error stop "negative bin count failed"
integer_values = tabulate([0.999_dp, 1.0_dp, 1.999_dp, 2.0_dp, 3.999_dp, 4.0_dp, &
   r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), r_inf(), -r_inf(), 2147483648.0_dp], 3)
call assert_integer_vector(integer_values, [2, 1, 1], "real tabulation boundaries")
integer_table = table2([1, 2, 2, 9], [1, 2, 1], 2, 2)
call assert_integer_matrix(integer_table, reshape([1, 1, 0, 1], [2, 2]), &
   "truncated contingency table")
integer_table = table2([integer ::], [integer ::], 0, 3)
if (any(shape(integer_table) /= [0, 3])) error stop "empty contingency shape failed"

proportions = prop_table(reshape([0, 0, 0, 0], [2, 2]))
if (.not. all(ieee_is_nan(proportions))) error stop "zero overall proportions failed"
proportions = prop_table(reshape([0, 1, 0, 1], [2, 2]), margin=1)
if (.not. all(ieee_is_nan(proportions(1, :)))) error stop "zero row proportions failed"
call assert_real_vector(proportions(2, :), [0.5_dp, 0.5_dp], "nonzero row proportions")
proportions = prop_table(reshape([0, 0, 1, 1], [2, 2]), margin=2)
if (.not. all(ieee_is_nan(proportions(:, 1)))) error stop "zero column proportions failed"
call assert_real_vector(proportions(:, 2), [0.5_dp, 0.5_dp], "nonzero column proportions")

character_table = table_char([character(len=1) ::])
if (size(character_table%gene) /= 0 .or. size(character_table%Freq) /= 0) &
   error stop "empty character table failed"

real_values = unique([2.0_dp, 1.0_dp, 2.0_dp, 1.0_dp])
call assert_real_vector(real_values, [2.0_dp, 1.0_dp], "stable real unique")
real_values = unique([r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), r_na_real(), &
   -r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), r_na_real(), r_inf(), &
   -0.0_dp, 0.0_dp, -r_inf()])
if (size(real_values) /= 5) error stop "nonfinite real unique shape failed"
if (real_values(1) /= r_inf() .or. .not. r_is_nan(real_values(2)) .or. &
   .not. is_na(real_values(3)) .or. r_is_nan(real_values(3)) .or. &
   real_values(4) /= -r_inf() .or. .not. ieee_is_negative(real_values(5))) &
   error stop "nonfinite real unique values failed"
character_values = unique([character(len=5) :: "beta", "alpha", "beta"])
call assert_character_vector(character_values, [character(len=5) :: "beta", "alpha"], &
   "stable character unique")
logical_values = unique([.true., .false., .true., .false.])
call assert_logical_vector(logical_values, [.true., .false.], "stable logical unique")
call assert_logical_vector(duplicated([character(len=1) :: "a", "b", "a"]), &
   [.false., .false., .true.], "character duplicated")
call assert_logical_vector(duplicated([.true., .false., .true.], fromLast=.true.), &
   [.true., .false., .false.], "reverse logical duplicated")
call assert_logical_vector(duplicated([r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), &
   r_na_real(), -r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), r_na_real(), &
   r_inf(), -0.0_dp, 0.0_dp, -r_inf()]), &
   [.false., .false., .false., .false., .true., .true., .true., .false., .true., .true.], &
   "nonfinite real duplicated")
call assert_logical_vector(duplicated([r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), &
   r_na_real(), -r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), r_na_real(), &
   r_inf(), -0.0_dp, 0.0_dp, -r_inf()], fromLast=.true.), &
   [.true., .true., .true., .true., .false., .false., .false., .true., .false., .false.], &
   "reverse nonfinite real duplicated")
if (anyDuplicated([r_inf(), ieee_value(0.0_dp, ieee_quiet_nan), r_na_real(), &
   -r_inf(), ieee_value(0.0_dp, ieee_quiet_nan)]) /= 5) &
   error stop "nonfinite anyDuplicated failed"
if (anyDuplicated([3, 1, 3, 2, 1], fromLast=.true.) /= 1) &
   error stop "reverse anyDuplicated position failed"
if (anyDuplicated([character(len=1) :: "a", "b", "c"]) /= 0) &
   error stop "distinct anyDuplicated failed"

real_values = union([2.0_dp, 1.0_dp], [1.0_dp, 3.0_dp])
call assert_real_vector(real_values, [2.0_dp, 1.0_dp, 3.0_dp], "real union")
real_values = union([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), r_inf(), -0.0_dp], &
   [ieee_value(0.0_dp, ieee_quiet_nan), -r_inf(), r_na_real(), 0.0_dp])
call assert_nonfinite_set(real_values, 5, "nonfinite real union")
real_values = intersect([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), r_inf(), -0.0_dp], &
   [ieee_value(0.0_dp, ieee_quiet_nan), -r_inf(), r_na_real(), 0.0_dp])
if (size(real_values) /= 3 .or. .not. is_na(real_values(1)) .or. r_is_nan(real_values(1)) .or. &
   .not. r_is_nan(real_values(2)) .or. .not. ieee_is_negative(real_values(3))) &
   error stop "nonfinite real intersection failed"
real_values = setdiff([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), r_inf(), -0.0_dp], &
   [ieee_value(0.0_dp, ieee_quiet_nan), -r_inf(), r_na_real(), 0.0_dp])
call assert_real_vector(real_values, [r_inf()], "nonfinite real set difference")
if (.not. setequal([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), -0.0_dp], &
   [0.0_dp, ieee_value(0.0_dp, ieee_quiet_nan), r_na_real()])) &
   error stop "nonfinite real set equality failed"
character_values = intersect([character(len=5) :: "red", "blue", "red"], &
   [character(len=5) :: "green", "blue"])
call assert_character_vector(character_values, [character(len=4) :: "blue"], &
   "character intersection")
character_values = setdiff([character(len=3) :: "red", "tan", "red"], &
   [character(len=3) :: "tan"])
call assert_character_vector(character_values, [character(len=3) :: "red"], &
   "character set difference")
if (.not. setequal([real(kind=dp) ::], [real(kind=dp) ::])) &
   error stop "empty real set equality failed"
if (.not. setequal([character(len=1) :: "a", "a"], [character(len=1) :: "a"])) &
   error stop "character set equality failed"

integer_values = match([1, 2], [integer ::])
call assert_integer_vector(integer_values, [-huge(0), -huge(0)], "empty match table")
integer_values = match([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), &
   r_inf(), -r_inf(), 0.0_dp], [ieee_value(0.0_dp, ieee_quiet_nan), &
   -r_inf(), r_na_real(), r_inf(), -0.0_dp])
call assert_integer_vector(integer_values, [3, 1, 4, 2, 5], "nonfinite real match")
if (size(match([integer ::], [1, 2])) /= 0) error stop "empty match query failed"
call assert_logical_vector(r_in([1, 2], [integer ::]), [.false., .false.], &
   "empty integer membership table")
call assert_logical_vector(r_in([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), &
   r_inf(), -r_inf(), 0.0_dp], [r_na_real(), r_inf()]), &
   [.true., .false., .true., .false., .false.], "nonfinite real membership")
call assert_logical_vector(r_in([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), &
   r_inf(), -r_inf(), 1.0_dp, 1.5_dp, -2147483648.0_dp], [-huge(0), 1, 2]), &
   [.true., .false., .false., .false., .true., .false., .false.], &
   "real queries in integer table")
call assert_logical_vector(r_in([-huge(0), 1, 2], &
   [r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan), 1.0_dp]), &
   [.true., .true., .false.], "integer queries in real table")
if (.not. r_in(r_na_real(), [-huge(0), 1])) error stop "scalar real NA membership failed"
if (r_in(ieee_value(0.0_dp, ieee_quiet_nan), [-huge(0), 1])) &
   error stop "scalar real NaN membership failed"
if (.not. r_in(-huge(0), [r_na_real(), 1.0_dp])) &
   error stop "scalar integer NA membership failed"
if (size(r_in([real(kind=dp) ::], [1.0_dp])) /= 0) &
   error stop "empty membership query failed"

contains

subroutine assert_nonfinite_set(actual, expected_size, label)
real(kind=dp), intent(in) :: actual(:)
integer, intent(in) :: expected_size
character(len=*), intent(in) :: label
if (size(actual) /= expected_size) error stop trim(label) // " shape failed"
if (.not. is_na(actual(1)) .or. r_is_nan(actual(1)) .or. .not. r_is_nan(actual(2)) .or. &
   actual(3) /= r_inf() .or. .not. ieee_is_negative(actual(4)) .or. &
   actual(5) /= -r_inf()) error stop trim(label) // " values failed"
end subroutine assert_nonfinite_set

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
character(len=*), intent(in) :: actual(:), expected(:), label
integer :: i
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector
end program test_categorical_variants
