program test_conversion_utilities
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_negative, ieee_negative_inf, &
   ieee_positive_inf, ieee_quiet_nan, ieee_value
use r_mod, only: as_hexmode, as_octmode, as_roman, dp, fivenum, inttobits, &
   str_to_int, str_to_real
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
integer :: bits(32)
integer :: integer_values(4)
real(kind=dp) :: real_values(4)
real(kind=dp) :: five_values(5), negative_inf, positive_inf

if (str_to_int("42") /= 42) error stop "positive integer parsing failed"
if (str_to_int("  -17 ") /= -17) error stop "signed integer parsing failed"
if (str_to_int("+8") /= 8) error stop "explicit-positive integer parsing failed"
if (str_to_int("0012") /= 12) error stop "leading-zero integer parsing failed"
if (str_to_int("1.9") /= 1) error stop "positive fractional integer parsing failed"
if (str_to_int("-1.9") /= -1) error stop "negative fractional integer parsing failed"
if (str_to_int("1e2") /= 100) error stop "exponent integer parsing failed"
if (str_to_int("1e-1") /= 0) error stop "fractional exponent integer parsing failed"
if (str_to_int("0x10") /= 16) error stop "hexadecimal integer parsing failed"
if (str_to_int("0x1.8p1") /= 3) error stop "hexadecimal exponent integer parsing failed"
if (str_to_int("2147483647") /= huge(0)) error stop "maximum integer parsing failed"
if (str_to_int("2147483647.9") /= huge(0)) error stop "fractional maximum integer parsing failed"
if (str_to_int("2147483648") /= -huge(0)) error stop "positive integer overflow failed"
if (str_to_int("-2147483648") /= -huge(0)) error stop "negative integer overflow failed"
if (str_to_int("Inf") /= -huge(0)) error stop "infinite integer rejection failed"
if (str_to_int("NaN") /= -huge(0)) error stop "NaN integer rejection failed"
if (str_to_int("999999999999999999999999") /= -huge(0)) error stop "long integer overflow failed"
if (str_to_int("") /= -huge(0)) error stop "empty integer parsing failed"
if (str_to_int("12x") /= -huge(0)) error stop "invalid integer parsing failed"
integer_values = str_to_int([character(len=4) :: "1", "-2", "+30", "bad"])
call assert_integer_vector_equal(integer_values, [1, -2, 30, -huge(0)], "elemental integer parsing")

call assert_close(str_to_real("3.25"), 3.25_dp, "decimal real parsing")
call assert_close(str_to_real("-1.5e2"), -150.0_dp, "exponent real parsing")
call assert_close(str_to_real("+.5"), 0.5_dp, "leading-decimal real parsing")
call assert_close(str_to_real("0x10"), 16.0_dp, "hexadecimal integer real parsing")
call assert_close(str_to_real("-0Xff"), -255.0_dp, "signed hexadecimal real parsing")
call assert_close(str_to_real("0x1.8"), 1.5_dp, "hexadecimal fraction parsing")
call assert_close(str_to_real("0x1.8p1"), 3.0_dp, "hexadecimal exponent parsing")
call assert_close(str_to_real("0x1d2"), 466.0_dp, "hexadecimal d digit parsing")
if (str_to_real("1e309") /= ieee_value(0.0_dp, ieee_positive_inf)) &
   error stop "decimal overflow parsing failed"
if (str_to_real("-1e309") /= ieee_value(0.0_dp, ieee_negative_inf)) &
   error stop "negative decimal overflow parsing failed"
if (str_to_real("0x1p1024") /= ieee_value(0.0_dp, ieee_positive_inf)) &
   error stop "hexadecimal overflow parsing failed"
if (str_to_real("0x1p-1074") <= 0.0_dp) error stop "hexadecimal subnormal parsing failed"
if (str_to_real("0x1p-1075") /= 0.0_dp) error stop "hexadecimal underflow parsing failed"
if (.not. ieee_is_negative(str_to_real("-0x1p-1075"))) &
   error stop "negative hexadecimal underflow sign failed"
if (.not. ieee_is_negative(str_to_real("-1e-400"))) &
   error stop "negative decimal underflow sign failed"
if (str_to_real("inf") /= ieee_value(0.0_dp, ieee_positive_inf)) error stop "infinity parsing failed"
if (str_to_real("-Infinity") /= ieee_value(0.0_dp, ieee_negative_inf)) &
   error stop "negative infinity parsing failed"
if (.not. ieee_is_nan(str_to_real("NaN"))) error stop "NaN parsing failed"
if (.not. ieee_is_nan(str_to_real("1D2"))) error stop "Fortran exponent rejection failed"
if (.not. ieee_is_nan(str_to_real("1,2"))) error stop "comma real rejection failed"
if (.not. ieee_is_nan(str_to_real("1 2"))) error stop "partial real rejection failed"
if (.not. ieee_is_nan(str_to_real("1e+"))) error stop "incomplete exponent rejection failed"
if (.not. ieee_is_nan(str_to_real("0x"))) error stop "empty hexadecimal rejection failed"
if (.not. ieee_is_nan(str_to_real("0x1p"))) error stop "incomplete hexadecimal exponent rejection failed"
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
bits = inttobits(-huge(0) - 1)
if (any(bits(1:31) /= 0) .or. bits(32) /= 1) error stop "minimum integer bits failed"

call assert_string_equal(as_octmode(64), "100", "octal formatting")
call assert_string_equal(as_hexmode(255), "ff", "hexadecimal formatting")
call assert_string_equal(as_octmode(-1), "37777777777", "negative octal formatting")
call assert_string_equal(as_octmode(-8), "37777777770", "negative octal place formatting")
call assert_string_equal(as_hexmode(-1), "ffffffff", "negative hexadecimal formatting")
call assert_string_equal(as_hexmode(-8), "fffffff8", "negative hexadecimal place formatting")
call assert_string_equal(as_hexmode(huge(0)), "7fffffff", "maximum hexadecimal formatting")
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
call assert_real_vector_close(fivenum([7.0_dp]), &
   [7.0_dp, 7.0_dp, 7.0_dp, 7.0_dp, 7.0_dp], "singleton five-number summary")
call assert_real_vector_close(fivenum([1.0_dp, 2.0_dp]), &
   [1.0_dp, 1.0_dp, 1.5_dp, 2.0_dp, 2.0_dp], "two-value five-number summary")
call assert_real_vector_close(fivenum([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]), &
   [1.0_dp, 1.5_dp, 2.5_dp, 3.5_dp, 4.0_dp], "four-value five-number summary")
if (.not. all(ieee_is_nan(fivenum([real(kind=dp) ::])))) &
   error stop "empty five-number summary failed"
call assert_real_vector_close(fivenum([1.0_dp, ieee_value(0.0_dp, ieee_quiet_nan), 3.0_dp]), &
   [1.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 3.0_dp], "missing-value five-number summary")
if (.not. all(ieee_is_nan(fivenum([ieee_value(0.0_dp, ieee_quiet_nan), &
   ieee_value(1.0_dp, ieee_quiet_nan)])))) error stop "all-missing five-number summary failed"
negative_inf = ieee_value(0.0_dp, ieee_negative_inf)
positive_inf = ieee_value(0.0_dp, ieee_positive_inf)
five_values = fivenum([negative_inf, 1.0_dp, positive_inf])
if (five_values(1) /= negative_inf .or. five_values(2) /= negative_inf .or. &
   five_values(3) /= 1.0_dp .or. five_values(4) /= positive_inf .or. &
   five_values(5) /= positive_inf) error stop "infinite five-number summary failed"

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
