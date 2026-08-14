program test_runtime_controls
use r_mod, only: dp, print_matrix, print_real_scalar, r_elapsed, &
   set_print_int_like, set_print_int_like_tol, set_recycle_stop, &
   set_recycle_warn, sys_sleep
implicit none

real(kind=dp) :: started, finished

call set_print_int_like(.false.)
call set_print_int_like_tol(1.0e-8_dp)
call print_real_scalar(2.0_dp)
call print_matrix(reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [2, 2]))
call set_print_int_like(.true.)
call set_print_int_like_tol(1000.0_dp * epsilon(1.0_dp))

call set_recycle_warn(.false.)
call set_recycle_stop(.false.)

started = r_elapsed()
call sys_sleep(0.01_dp)
finished = r_elapsed()
if (finished - started < 0.005_dp) error stop "positive sleep duration failed"

started = r_elapsed()
call sys_sleep(-1.0_dp)
finished = r_elapsed()
if (finished - started > 0.5_dp) error stop "negative sleep duration failed"
end program test_runtime_controls
