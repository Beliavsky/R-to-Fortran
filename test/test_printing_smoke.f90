program test_printing_smoke
use r_mod, only: dp, display, print_char_vector, print_complex_vector, &
   print_factor, print_integer_vector, print_matrix, print_matrix_rstyle, &
   print_named_real_row, print_named_real_vector, print_real_scalar, &
   print_real_vector, print_summary, print_table1, print_table2
implicit none

call print_real_scalar(1.25_dp, int_like=.false.)
call print_real_vector([1.0_dp, 2.5_dp], int_like=.false., digits=2)
call print_real_vector([real(kind=dp) ::])
call print_integer_vector([1, 2, 3])
call print_char_vector([character(len=3) :: "one", "two"])
call print_complex_vector([cmplx(1.0_dp, -2.0_dp, kind=dp)])
call print_matrix(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]))
call print_matrix(reshape([1, 2, 3, 4], [2, 2]))
call print_matrix(reshape([.true., .false., .false., .true.], [2, 2]))
call print_matrix_rstyle(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]), digits=1)
call print_named_real_vector([1.0_dp, 2.0_dp], [character(len=1) :: "a", "b"], digits=1)
call print_named_real_row([1.0_dp, 2.0_dp], [character(len=1) :: "a", "b"], digits=1, row_name="r")
call print_factor([1, 2, 1], [character(len=3) :: "low", "high"])
call print_table1([2, 3], [character(len=1) :: "a", "b"])
call print_table2(reshape([1, 2, 3, 4], [2, 2]), &
   [character(len=2) :: "r1", "r2"], [character(len=2) :: "c1", "c2"])
call print_table2(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]), &
   [character(len=2) :: "r1", "r2"], [character(len=2) :: "c1", "c2"], digits=2)
call print_summary([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp])
call print_summary(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]))

call display(1.0_dp)
call display(1)
call display(.true.)
call display("text")
call display([1.0_dp, 2.0_dp])
call display([1, 2])
call display([.true., .false.])
call display([character(len=1) :: "a", "b"])
call display(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]))
call display(reshape([1, 2, 3, 4], [2, 2]))
call display(reshape([.true., .false., .false., .true.], [2, 2]))
call display(reshape([character(len=1) :: "a", "b", "c", "d"], [2, 2]))
end program test_printing_smoke
