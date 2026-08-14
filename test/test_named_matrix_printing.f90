program test_named_matrix_printing
use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan, ieee_value
use r_mod, only: dp, print_matrix_rstyle_named
implicit none

real(kind=dp) :: real_values(2, 3), nan_value
integer :: integer_values(2, 3)

real_values = reshape([1.0_dp, 2.0_dp, 10.25_dp, 20.5_dp, 100.0_dp, 200.0_dp], [2, 3])
integer_values = reshape([1, 2, 10, 20, 100, 200], [2, 3])

call print_matrix_rstyle_named(real_values, &
   [character(len=7) :: "count", "measure", "total"], &
   int_cols=[.true., .false., .true.], &
   row_names=[character(len=6) :: "first", "second"], digits=2)
call print_matrix_rstyle_named(real_values, [character(len=5) :: "only"])
call print_matrix_rstyle_named(integer_values, &
   [character(len=7) :: "count", "measure", "total"])
call print_matrix_rstyle_named(integer_values, [character(len=5) :: "only"])

nan_value = ieee_value(0.0_dp, ieee_quiet_nan)
real_values(1, 2) = nan_value
call print_matrix_rstyle_named(real_values, &
   [character(len=2) :: "a", "b", "c"], digits=0)

call print_matrix_rstyle_named(reshape([real(kind=dp) ::], [0, 3]), &
   [character(len=2) :: "a", "b", "c"])
call print_matrix_rstyle_named(reshape([real(kind=dp) ::], [2, 0]), &
   [character(len=1) ::], row_names=[character(len=2) :: "r1", "r2"])
call print_matrix_rstyle_named(reshape([integer ::], [0, 0]), [character(len=1) ::])
end program test_named_matrix_printing
