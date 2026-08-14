program test_directory_utilities
use r_mod, only: dir_create, dir_exists, file_create, file_exists, file_path, &
   getwd, list_files, tempfile, unlink_recursive
implicit none

character(len=:), allocatable :: root, nested, alpha_path, beta_path, gamma_path
character(len=:), allocatable :: names(:), text_names(:), recursive_names(:), full_names(:)
character(len=:), allocatable :: paths(:)
logical, allocatable :: exists_values(:)
integer :: i

root = tempfile("r_mod_directory_")
nested = file_path(root, "nested")
alpha_path = file_path(root, "alpha.txt")
beta_path = file_path(root, "beta.dat")
gamma_path = file_path(nested, "gamma.txt")
if (file_exists(root)) then
   if (unlink_recursive(root) /= 0) error stop "stale directory cleanup failed"
end if

if (.not. dir_create(nested, recursive=.true.)) error stop "recursive directory creation failed"
if (.not. dir_exists(root) .or. .not. dir_exists(nested)) error stop "directory existence failed"
allocate(character(len=len(root) + 16) :: paths(3))
paths(1) = root
paths(2) = nested
paths(3) = file_path(root, "missing")
exists_values = dir_exists(paths)
if (any(exists_values .neqv. [.true., .true., .false.])) error stop "vector directory existence failed"
if (.not. file_create(alpha_path)) error stop "first directory file creation failed"
if (.not. file_create(beta_path)) error stop "second directory file creation failed"
if (.not. file_create(gamma_path)) error stop "nested directory file creation failed"

names = list_files(root)
call assert_contains(names, "alpha.txt", "nonrecursive listing")
call assert_contains(names, "beta.dat", "nonrecursive listing")
if (size(names) /= 2) error stop "nonrecursive listing count failed"

text_names = list_files(root, pattern=".*txt$")
if (size(text_names) /= 1) error stop "pattern listing count failed"
call assert_contains(text_names, "alpha.txt", "pattern listing")

recursive_names = list_files(root, pattern=".*txt$", recursive=.true.)
if (size(recursive_names) /= 2) error stop "recursive listing count failed"
call assert_basename_present(recursive_names, "alpha.txt", "recursive listing")
call assert_basename_present(recursive_names, "gamma.txt", "recursive listing")

full_names = list_files(root, full_names=.true.)
if (size(full_names) /= 2) error stop "full-name listing count failed"
if (any([(index(full_names(i), trim(root)) == 0, i=1,size(full_names))])) &
   error stop "full-name listing path failed"

if (len_trim(getwd()) == 0) error stop "working-directory lookup failed"
if (unlink_recursive(root) /= 0) error stop "recursive directory removal failed"
if (file_exists(root) .or. dir_exists(root)) error stop "removed directory still exists"

contains

subroutine assert_contains(values, expected, label)
character(len=*), intent(in) :: values(:), expected, label
integer :: i
do i = 1, size(values)
   if (trim(values(i)) == expected) return
end do
error stop trim(label) // " failed"
end subroutine assert_contains

subroutine assert_basename_present(values, expected, label)
character(len=*), intent(in) :: values(:), expected, label
integer :: i, n
do i = 1, size(values)
   n = len_trim(values(i))
   if (n >= len(expected)) then
      if (values(i)(n - len(expected) + 1:n) == expected) return
   end if
end do
error stop trim(label) // " failed"
end subroutine assert_basename_present
end program test_directory_utilities
