program test_date_time_variants
use r_mod, only: date_format, date_format_vec, date_from_iso, date_from_iso_vec, &
   date_from_yyyymmdd_vec, date_seq_day, date_seq_length, date_to_char, &
   date_to_char_vec, dp, sys_time_format
implicit none

integer :: dates(7), i

if (date_from_iso("1900-03-01") - date_from_iso("1900-02-28") /= 1) &
   error stop "1900 non-leap-century rule failed"
if (date_from_iso("2000-03-01") - date_from_iso("2000-02-28") /= 2) &
   error stop "2000 leap-century rule failed"
if (date_from_iso("2100-03-01") - date_from_iso("2100-02-28") /= 1) &
   error stop "2100 non-leap-century rule failed"

dates = date_from_iso_vec([character(len=10) :: "1800-01-01", "1899-12-31", &
   "1900-03-01", "1970-01-01", "2000-02-29", "2100-03-01", "2400-02-29"])
do i = 1, size(dates)
   if (date_from_iso(date_to_char(dates(i))) /= dates(i)) &
      error stop "wide-range date round trip failed"
end do
call assert_character_vector(date_to_char_vec(dates([1, 4, 7])), &
   [character(len=10) :: "1800-01-01", "1970-01-01", "2400-02-29"], &
   "wide-range date formatting")

call assert_integer_vector(date_from_yyyymmdd_vec([19700101.4_dp, 19700101.49_dp, &
   19700101.0_dp]), [0, 0, 0], "rounded numeric dates")
if (size(date_from_iso_vec([character(len=10) ::])) /= 0) &
   error stop "empty ISO parsing failed"
if (size(date_from_yyyymmdd_vec([real(kind=dp) ::])) /= 0) &
   error stop "empty numeric date parsing failed"
if (size(date_to_char_vec([integer ::])) /= 0) error stop "empty date formatting failed"
if (size(date_format_vec([integer ::], "%Y")) /= 0) &
   error stop "empty formatted dates failed"

call assert_integer_vector(date_seq_day(0, 5, by=2), [0, 2, 4], &
   "nondivisible forward date sequence")
call assert_integer_vector(date_seq_day(5, 0, by=-2), [5, 3, 1], &
   "nondivisible reverse date sequence")
call assert_integer_vector(date_seq_day(7, 7, by=3), [7], &
   "equal-endpoint positive sequence")
call assert_integer_vector(date_seq_day(7, 7, by=-3), [7], &
   "equal-endpoint negative sequence")
if (size(date_seq_day(0, 5, by=-1)) /= 0) error stop "invalid forward date direction failed"
if (size(date_seq_day(5, 0, by=1)) /= 0) error stop "invalid reverse date direction failed"
call assert_integer_vector(date_seq_length(10, 0, 4), [10, 10, 10, 10], &
   "repeated fixed-length date sequence")
call assert_integer_vector(date_seq_length(10, 3, 1), [10], &
   "single fixed-length date sequence")
if (size(date_seq_length(10, 3, 0)) /= 0) error stop "zero-length date sequence failed"

if (trim(date_format(0, "%F")) /= "1970-01-01") &
   error stop "fallback date format failed"
if (trim(date_format(0, "unsupported")) /= "1970-01-01") &
   error stop "unsupported date format fallback failed"
if (sys_time_format(0.0_dp, "%Y-%m-%d") /= "1970-01-01") &
   error stop "timestamp date-only format failed"
if (sys_time_format(0.0_dp, "%Y-%m-%d %H:%M:%S %Z") /= &
   "1970-01-01 00:00:00 UTC") error stop "timestamp UTC format failed"
if (sys_time_format(86400.0_dp, "unsupported") /= "1970-01-02 00:00:00") &
   error stop "timestamp fallback format failed"
if (sys_time_format(-86401.0_dp, "%Y-%m-%d %H:%M:%S") /= &
   "1969-12-30 23:59:59") error stop "multi-day negative timestamp failed"
if (sys_time_format(-1.0_dp, "%Y-%m-%d %H:%M:%S") /= &
   "1969-12-31 23:59:59") error stop "pre-epoch timestamp failed"

contains

subroutine assert_integer_vector(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector

subroutine assert_character_vector(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:), label
integer :: j
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do j = 1, size(actual)
   if (trim(actual(j)) /= trim(expected(j))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector
end program test_date_time_variants
