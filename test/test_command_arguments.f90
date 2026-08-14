program test_command_arguments
use r_mod, only: r_command_args
implicit none

character(len=:), allocatable :: args(:), prefixed(:)
integer :: n

n = command_argument_count()
args = r_command_args(.true.)
if (size(args) /= n) error stop "trailing command argument count failed"

prefixed = r_command_args(.false., "--file=test_command_arguments.f90")
if (size(prefixed) /= n + 1) error stop "prefixed command argument count failed"
if (trim(prefixed(1)) /= "--file=test_command_arguments.f90") &
   error stop "command argument prefix failed"
if (n > 0) then
   if (any(prefixed(2:) /= args)) error stop "prefixed trailing arguments failed"
end if
end program test_command_arguments
