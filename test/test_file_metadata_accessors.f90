program test_file_metadata_accessors
use r_mod, only: file_atime, file_create, file_ctime, file_exe, file_exists, &
   file_info, file_info_t, file_isdir, file_mode, file_mtime, file_remove, &
   print_file_info, tempfile
implicit none

character(len=:), allocatable :: path, missing
character(len=:), allocatable :: paths(:)
type(file_info_t), allocatable :: info(:)

path = tempfile("r_mod_metadata_")
missing = path // ".missing"
if (file_exists(path)) then
   if (.not. file_remove(path)) error stop "stale metadata file cleanup failed"
end if
if (.not. file_create(path)) error stop "metadata file creation failed"

if (trim(file_mode(path)) /= "file") error stop "file mode accessor failed"
if (file_isdir(path)) error stop "file directory accessor failed"
if (len(file_mtime(path)) /= 0) error stop "file modification-time accessor failed"
if (len(file_ctime(path)) /= 0) error stop "file creation-time accessor failed"
if (len(file_atime(path)) /= 0) error stop "file access-time accessor failed"
if (trim(file_exe(path)) /= "no") error stop "file executable accessor failed"

allocate(character(len=len(missing)) :: paths(2))
paths(1) = path
paths(2) = missing
info = file_info(paths)
if (size(info) /= 2) error stop "vector file-info shape failed"
if (trim(info(1)%path) /= trim(path) .or. trim(info(1)%mode) /= "file") &
   error stop "existing vector file-info failed"
if (trim(info(2)%path) /= trim(missing)) error stop "missing vector file-info path failed"
call print_file_info(info(1))
call print_file_info(info)

if (.not. file_remove(path)) error stop "metadata file removal failed"
end program test_file_metadata_accessors
