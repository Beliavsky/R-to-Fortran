program test_r_mod
use r_mod, only: dp, dnorm, r_seq_int, sd
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: observations(4)

observations = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]

call assert_close(sd(observations), sqrt(5.0_dp / 3.0_dp), "sd")
call assert_close(dnorm(0.0_dp), 0.3989422804014327_dp, "dnorm")
if (any(r_seq_int(2, 4) /= [2, 3, 4])) error stop "r_seq_int failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close
end program test_r_mod
