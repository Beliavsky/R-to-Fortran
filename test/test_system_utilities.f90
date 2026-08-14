program test_system_utilities
use r_mod, only: date_to_char, dp, proc_time_vec, r_elapsed, sys_date, &
   sys_date_string, sys_getenv, sys_time, sys_timezone
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
implicit none

character(len=:), allocatable :: stamp, path_value
real(kind=dp) :: elapsed_before, elapsed_after, clock_value
real(kind=dp) :: timing(5)
integer :: date_before, date_after

if (sys_timezone() /= "UTC") error stop "timezone helper failed"
path_value = sys_getenv("PATH")
if (len_trim(path_value) == 0) error stop "environment lookup failed"
if (sys_getenv("XR2F_FPM_TEST_VARIABLE_THAT_SHOULD_NOT_EXIST") /= "") &
   error stop "missing environment lookup failed"

elapsed_before = r_elapsed()
timing = proc_time_vec()
elapsed_after = r_elapsed()
if (any(timing([1, 2, 4, 5]) /= 0.0_dp)) error stop "process-time zero slots failed"
if (timing(3) < elapsed_before .or. timing(3) > elapsed_after) error stop "process-time elapsed slot failed"

clock_value = sys_time()
if (.not. ieee_is_finite(clock_value) .or. clock_value <= 0.0_dp) error stop "system time failed"
date_before = sys_date()
stamp = sys_date_string()
date_after = sys_date()
if (len(stamp) /= 19) error stop "date string length failed"
if (stamp(5:5) /= "-" .or. stamp(8:8) /= "-" .or. stamp(11:11) /= " ") &
   error stop "date string separators failed"
if (stamp(14:14) /= ":" .or. stamp(17:17) /= ":") error stop "time string separators failed"
if (stamp(1:10) /= date_to_char(date_before) .and. stamp(1:10) /= date_to_char(date_after)) &
   error stop "date string calendar value failed"
end program test_system_utilities
