program test_group_key_helpers
use r_mod, only: ave_group_key
implicit none

character(len=:), allocatable :: keys(:)
character(len=1), parameter :: separator = achar(31)

keys = ave_group_key([character(len=1) :: "a", "b"], [character(len=1) :: "x", "y"])
call assert_keys(keys, [character(len=3) :: "a" // separator // "x", "b" // separator // "y"], &
   "character-character keys")

keys = ave_group_key([character(len=1) :: "a", "b"], [1, -2])
call assert_keys(keys, [character(len=4) :: "a" // separator // "1", "b" // separator // "-2"], &
   "character-integer keys")

keys = ave_group_key([1, -2], [character(len=1) :: "a", "b"])
call assert_keys(keys, [character(len=4) :: "1" // separator // "a", "-2" // separator // "b"], &
   "integer-character keys")

keys = ave_group_key([1, -2], [3, 4])
call assert_keys(keys, [character(len=4) :: "1" // separator // "3", "-2" // separator // "4"], &
   "integer-integer keys")

keys = ave_group_key([character(len=1) :: "a", "b", "c"], [1, 2])
if (size(keys) /= 2) error stop "shortest grouping length failed"
keys = ave_group_key([integer ::], [integer ::])
if (size(keys) /= 0) error stop "empty grouping keys failed"

contains

subroutine assert_keys(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:), label
integer :: i
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_keys
end program test_group_key_helpers
