program test_file_utilities
use r_mod, only: dp, file_create, file_exists, file_extension, file_info, file_info_t, &
   file_path, file_path_value, file_remove, file_rename, file_size, tempfile
implicit none

character(len=:), allocatable :: original_path, renamed_path, joined
type(file_info_t) :: info
integer :: unit, ios

original_path = tempfile("xr2f_fpm_file_")
renamed_path = original_path // ".renamed"
if (file_exists(original_path)) then
   if (.not. file_remove(original_path)) error stop "stale original cleanup failed"
end if
if (file_exists(renamed_path)) then
   if (.not. file_remove(renamed_path)) error stop "stale renamed cleanup failed"
end if

if (.not. file_create(original_path)) error stop "file creation failed"
if (.not. file_exists(original_path)) error stop "created-file existence failed"
call assert_close(file_size(original_path), 0.0_dp, "empty file size")

open(newunit=unit, file=original_path, access="stream", form="unformatted", &
   status="replace", action="write", iostat=ios)
if (ios /= 0) error stop "test file open failed"
write(unit, iostat=ios) "abc"
close(unit)
if (ios /= 0) error stop "test file write failed"

info = file_info(original_path)
call assert_string_equal(info%path, original_path, "file-info path")
call assert_close(info%size, 3.0_dp, "file-info size")
if (info%isdir) error stop "file-info directory flag failed"
call assert_string_equal(trim(info%mode), "file", "file-info mode")
call assert_close(file_size(original_path), 3.0_dp, "file-size helper")

if (.not. file_rename(original_path, renamed_path)) error stop "file rename failed"
if (file_exists(original_path)) error stop "renamed source still exists"
if (.not. file_exists(renamed_path)) error stop "renamed destination missing"
call assert_close(file_size(renamed_path), 3.0_dp, "renamed file size")

call assert_string_equal(file_extension("archive.tar.gz"), ".gz", "compound filename extension")
call assert_string_equal(file_extension("folder/name"), "", "missing filename extension")
call assert_string_equal(file_extension("folder.with.dot/name.txt"), ".txt", "directory-dot extension")
call assert_string_equal(file_extension("name.bad-ext"), "", "invalid filename extension")
call assert_string_equal(file_extension("name."), "", "empty filename extension")

joined = file_path("parent", "child.txt")
if (len_trim(joined) < len("parent/child.txt")) error stop "joined path length failed"
if (joined(1:6) /= "parent") error stop "joined path prefix failed"
if (joined(len_trim(joined)-8:len_trim(joined)) /= "child.txt") error stop "joined path suffix failed"
call assert_string_equal(file_path("", "child.txt"), "child.txt", "empty parent path")
call assert_string_equal(file_path_value(joined), joined, "path value")

if (.not. file_remove(renamed_path)) error stop "file removal failed"
if (file_exists(renamed_path)) error stop "removed file still exists"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > 1.0e-12_dp) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_string_equal(actual, expected, label)
character(len=*), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (trim(actual) /= trim(expected)) error stop trim(label) // " failed"
end subroutine assert_string_equal
end program test_file_utilities
