program test_string_utilities
use r_mod, only: casefold, char_ends_with, char_join, chartr, nchar, &
   replace_all_fixed, replace_first_fixed, r_substr, r_substr_replace, &
   strsplit_fixed, tolower, toupper, trimws, urldecode
implicit none

character(len=:), allocatable :: pieces(:)

if (nchar("alpha   ") /= 5) error stop "trimmed character length failed"
if (any(nchar([character(len=5) :: "a", "abc", ""]) /= [1, 3, 0])) &
   error stop "elemental character length failed"
if (.not. char_ends_with("report.csv", ".csv")) error stop "suffix match failed"
if (char_ends_with("csv", "report.csv")) error stop "long suffix rejection failed"
if (.not. char_ends_with("value", "")) error stop "empty suffix failed"

call assert_string_equal(char_join([character(len=5) :: "red", "green", "blue"], ","), &
   "red,green,blue", "character join")
call assert_string_equal(char_join([1, -2, 30], " | "), "1 | -2 | 30", "integer join")
call assert_string_equal(char_join([character(len=1) ::], ","), "", "empty join")

call assert_string_equal(r_substr("abcdef", 2, 4), "bcd", "substring")
call assert_string_equal(r_substr("abcdef", -2, 2), "ab", "clamped substring")
call assert_string_equal(r_substr("abcdef", 7, 9), "", "empty substring")
call assert_string_equal(r_substr_replace("abcdef", 2, 4, "XYZ"), "aXYZef", "substring replacement")
call assert_string_equal(r_substr_replace("abcdef", 2, 5, "Q"), "aQcdef", "short substring replacement")

pieces = strsplit_fixed("a--b--c", "--")
call assert_string_vector_equal(pieces, [character(len=1) :: "a", "b", "c"], "fixed split")
pieces = strsplit_fixed("a,b,", ",")
call assert_string_vector_equal(pieces, [character(len=1) :: "a", "b", ""], "trailing empty split")
pieces = strsplit_fixed("abc", "")
call assert_string_vector_equal(pieces, [character(len=1) :: "a", "b", "c"], "character split")

call assert_string_equal(toupper("Abc-xyZ"), "ABC-XYZ", "upper-case conversion")
call assert_string_equal(tolower("AbC-XYz"), "abc-xyz", "lower-case conversion")
call assert_string_equal(casefold("MiXeD"), "mixed", "default case folding")
call assert_string_equal(casefold("MiXeD", upper=.true.), "MIXED", "upper case folding")
call assert_string_equal(trimws("  text  "), "text", "two-sided whitespace trimming")
call assert_string_equal(trimws("  text  ", which="left"), "text  ", "left whitespace trimming")
call assert_string_equal(trimws("  text  ", which="right"), "  text", "right whitespace trimming")

call assert_string_equal(replace_first_fixed("one two two", "two", "2"), "one 2 two", &
   "first fixed replacement")
call assert_string_equal(replace_all_fixed("one two two", "two", "2"), "one 2 2", &
   "all fixed replacement")
call assert_string_equal(replace_all_fixed("unchanged", "missing", "x"), "unchanged", &
   "missing fixed replacement")
call assert_string_equal(chartr("abc", "123", "cab cab"), "312 312", "character translation")

call assert_string_equal(urldecode("A%20value%2Fpath"), "A value/path", "URL decoding")
call assert_string_equal(urldecode("keep%ZZtext"), "keep%ZZtext", "malformed URL escape")

contains

subroutine assert_string_equal(actual, expected, label)
character(len=*), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (actual /= expected) then
   write(*, '(a, 2(1x, a))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_string_equal

subroutine assert_string_vector_equal(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_string_vector_equal
end program test_string_utilities
