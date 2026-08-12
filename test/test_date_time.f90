program test_date_time
use r_mod, only: date_format, date_format_vec, date_from_iso, date_from_iso_vec, &
   date_from_yyyymmdd_vec, date_range, date_seq_day, date_seq_length, &
   date_to_char, date_to_char_vec, dp, sys_time_format
implicit none

integer :: leap_day

if (date_from_iso("1970-01-01") /= 0) error stop "Unix epoch parsing failed"
if (date_from_iso("1969-12-31") /= -1) error stop "pre-epoch parsing failed"
if (date_to_char(0) /= "1970-01-01") error stop "Unix epoch formatting failed"
if (date_to_char(-1) /= "1969-12-31") error stop "pre-epoch formatting failed"

leap_day = date_from_iso("2000-02-29")
if (date_to_char(leap_day) /= "2000-02-29") error stop "leap-day round trip failed"
if (leap_day - date_from_iso("2000-02-28") /= 1) error stop "leap-day predecessor failed"
if (date_from_iso("2000-03-01") - leap_day /= 1) error stop "leap-day successor failed"

call assert_integer_vector_equal(date_from_iso_vec([character(len=10) :: &
   "1969-12-31", "1970-01-01", "1970-01-02"]), [-1, 0, 1], "ISO vector parsing")
call assert_integer_vector_equal(date_from_yyyymmdd_vec([19691231.0_dp, 19700101.0_dp, 19700102.0_dp]), &
   [-1, 0, 1], "numeric date parsing")
call assert_character_vector_equal(date_to_char_vec([-1, 0, 1]), &
   [character(len=10) :: "1969-12-31", "1970-01-01", "1970-01-02"], "date vector formatting")

if (trim(date_format(leap_day, "%Y")) /= "2000") error stop "year formatting failed"
if (trim(date_format(leap_day, "%Y-%m")) /= "2000-02") error stop "year-month formatting failed"
if (trim(date_format(leap_day, "%m")) /= "02") error stop "month formatting failed"
if (trim(date_format(leap_day, "%d")) /= "29") error stop "day formatting failed"
if (trim(date_format(leap_day, "%d/%m/%Y")) /= "29/02/2000") error stop "calendar formatting failed"
call assert_character_vector_equal(date_format_vec([leap_day, leap_day + 1], "%Y-%m"), &
   [character(len=10) :: "2000-02", "2000-03"], "vector date formatting")

call assert_integer_vector_equal(date_seq_day(leap_day - 1, leap_day + 1), &
   [leap_day - 1, leap_day, leap_day + 1], "forward date sequence")
call assert_integer_vector_equal(date_seq_day(leap_day + 1, leap_day - 1, by=-1), &
   [leap_day + 1, leap_day, leap_day - 1], "reverse date sequence")
if (size(date_seq_day(leap_day, leap_day + 2, by=-1)) /= 0) error stop "invalid direction sequence failed"
if (size(date_seq_day(leap_day, leap_day + 2, by=0)) /= 0) error stop "zero-step sequence failed"
call assert_integer_vector_equal(date_seq_length(leap_day, 7, 3), &
   [leap_day, leap_day + 7, leap_day + 14], "fixed-length date sequence")
if (size(date_seq_length(leap_day, 1, -2)) /= 0) error stop "negative sequence length failed"
call assert_integer_vector_equal(date_range([leap_day + 5, leap_day - 2, leap_day]), &
   [leap_day - 2, leap_day + 5], "date range")

if (sys_time_format(0.0_dp, "%Y-%m-%d %H:%M:%S") /= "1970-01-01 00:00:00") &
   error stop "epoch timestamp formatting failed"
if (sys_time_format(-1.0_dp, "%Y-%m-%d %H:%M:%S") /= "1969-12-31 23:59:59") &
   error stop "negative timestamp formatting failed"
if (sys_time_format(3661.0_dp, "%H:%M:%S") /= "01:01:01") error stop "time formatting failed"

contains

subroutine assert_integer_vector_equal(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector_equal

subroutine assert_character_vector_equal(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector_equal
end program test_date_time
