program test_conversion_utilities
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: as_hexmode, as_octmode, as_roman, dp, fivenum, inttobits, &
   str_to_int, str_to_real
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
integer :: bits(32)
integer :: integer_values(4)
real(kind=dp) :: real_values(4)

if (str_to_int("42") /= 42) error stop "positive integer parsing failed"
if (str_to_int("  -17 ") /= -17) error stop "signed integer parsing failed"
if (str_to_int("+8") /= 8) error stop "explicit-positive integer parsing failed"
if (str_to_int("") /= -huge(0)) error stop "empty integer parsing failed"
if (str_to_int("12x") /= -huge(0)) error stop "invalid integer parsing failed"
integer_values = str_to_int([character(len=4) :: "1", "-2", "+30", "bad"])
call assert_integer_vector_equal(integer_values, [1, -2, 30, -huge(0)], "elemental integer parsing")

call assert_close(str_to_real("3.25"), 3.25_dp, "decimal real parsing")
call assert_close(str_to_real("-1.5e2"), -150.0_dp, "exponent real parsing")
real_values = str_to_real([character(len=6) :: "1.5", "-2", "3e1", "bad"])
call assert_real_vector_close(real_values(1:3), [1.5_dp, -2.0_dp, 30.0_dp], "elemental real parsing")
if (.not. ieee_is_nan(real_values(4))) error stop "invalid real parsing failed"

bits = inttobits(5)
call assert_integer_vector_equal(bits(1:5), [1, 0, 1, 0, 0], "positive integer bits")
if (any(bits(4:32) /= 0)) error stop "positive high bits failed"
bits = inttobits(0)
if (any(bits /= 0)) error stop "zero integer bits failed"
bits = inttobits(-1)
if (any(bits /= 1)) error stop "negative integer bits failed"

call assert_string_equal(as_octmode(64), "100", "octal formatting")
call assert_string_equal(as_hexmode(255), "ff", "hexadecimal formatting")
call assert_string_equal(as_roman(1), "I", "minimum Roman numeral")
call assert_string_equal(as_roman(4), "IV", "subtractive Roman numeral")
call assert_string_equal(as_roman(944), "CMXLIV", "compound Roman numeral")
call assert_string_equal(as_roman(3899), "MMMDCCCXCIX", "maximum Roman numeral")
call assert_string_equal(as_roman(0), "NA", "low invalid Roman numeral")
call assert_string_equal(as_roman(3900), "NA", "high invalid Roman numeral")

call assert_real_vector_close(fivenum([5.0_dp, 1.0_dp, 4.0_dp, 2.0_dp, 3.0_dp]), &
   [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], "odd five-number summary")
call assert_real_vector_close(fivenum([6.0_dp, 1.0_dp, 5.0_dp, 2.0_dp, 4.0_dp, 3.0_dp]), &
   [1.0_dp, 2.0_dp, 3.5_dp, 5.0_dp, 6.0_dp], "even five-number summary")
call assert_real_vector_close(fivenum([2.0_dp, 2.0_dp, 2.0_dp]), &
   [2.0_dp, 2.0_dp, 2.0_dp, 2.0_dp, 2.0_dp], "constant five-number summary")

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) error stop trim(label) // " failed"
end subroutine assert_close

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

subroutine assert_string_equal(actual, expected, label)
character(len=*), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (actual /= expected) error stop trim(label) // " failed"
end subroutine assert_string_equal
end program test_conversion_utilities
