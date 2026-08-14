program test_string_variants
use r_mod, only: casefold, char_ends_with, char_join, chartr, nchar, &
   replace_all_fixed, replace_first_fixed, r_substr, r_substr_replace, &
   strsplit_fixed, tolower, toupper, trimws, urldecode
implicit none

character(len=:), allocatable :: pieces(:)

if (nchar("") /= 0) error stop "empty character length failed"
if (.not. char_ends_with("", "")) error stop "empty suffix on empty string failed"
if (char_ends_with("", "x")) error stop "nonempty suffix on empty string failed"
call assert_string(char_join([character(len=3) :: "a", "", "b"], "--"), &
   "a----b", "join with empty element")
call assert_string(char_join([1], ","), "1", "singleton integer join")
call assert_string(char_join([integer ::], ","), "", "empty integer join")

call assert_string(r_substr("abcdef", 4, 3), "", "reversed substring range")
call assert_string(r_substr("abcdef", 1, 99), "abcdef", "oversized substring end")
call assert_string(r_substr_replace("abcdef", 9, 12, "XYZ"), "abcdef", &
   "out-of-range substring replacement")
call assert_string(r_substr_replace("abcdef", 4, 2, "XYZ"), "abcdef", &
   "reversed substring replacement")
call assert_string(r_substr_replace("abcdef", 1, 3, "LONG"), "LONdef", &
   "long substring replacement truncation")
call assert_string(r_substr_replace("abcdef", 1, 3, ""), "abcdef", &
   "empty substring replacement")

pieces = strsplit_fixed("", ",")
call assert_string_vector(pieces, [character(len=1) :: ""], "empty fixed split")
pieces = strsplit_fixed(",a,,b,", ",")
call assert_string_vector(pieces, [character(len=1) :: "", "a", "", "b", ""], &
   "leading adjacent trailing split")
pieces = strsplit_fixed("abc", "--")
call assert_string_vector(pieces, [character(len=3) :: "abc"], "missing delimiter split")
pieces = strsplit_fixed("", "")
if (size(pieces) /= 0) error stop "empty character split failed"

call assert_string(toupper(""), "", "empty uppercase conversion")
call assert_string(tolower("123-!?"), "123-!?", "nonletter lowercase conversion")
call assert_string(casefold("MiXeD", upper=.false.), "mixed", "explicit lower casefold")
call assert_string(trimws("     "), "", "all-blank trimming")
call assert_string(trimws("  x  ", which="unknown"), "x", "unknown trim mode fallback")

call assert_string(replace_first_fixed("abc", "missing", "x"), "abc", &
   "missing first replacement")
call assert_string(replace_first_fixed("abc", "", "X"), "Xabc", &
   "empty first replacement")
call assert_string(replace_all_fixed("aaaa", "aa", "b"), "bb", &
   "nonoverlapping fixed replacement")
call assert_string(replace_all_fixed("abc", "b", "LONG"), "aLONGc", &
   "growing fixed replacement")
call assert_string(replace_all_fixed("abcabc", "b", ""), "acac", &
   "deleting fixed replacement")
call assert_string(replace_all_fixed("abc", "", "X"), "abc", &
   "empty all-replacement fallback")

call assert_string(chartr("abc", "12", "abc cab"), "12c c12", &
   "short translation alphabet")
call assert_string(chartr("", "", "unchanged"), "unchanged", &
   "empty character translation")
call assert_string(urldecode("%41%42%43"), "ABC", "uppercase URL escapes")
call assert_string(urldecode("%7e%2f"), "~/", "lowercase URL escapes")
call assert_string(urldecode("a+b"), "a+b", "URL plus preservation")
call assert_string(urldecode("tail%"), "tail%", "incomplete URL escape")

contains

subroutine assert_string(actual, expected, label)
character(len=*), intent(in) :: actual, expected, label
if (actual /= expected) error stop trim(label) // " failed"
end subroutine assert_string

subroutine assert_string_vector(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:), label
integer :: i
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_string_vector
end program test_string_variants
