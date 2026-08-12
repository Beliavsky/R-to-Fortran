program test_t_and_f_distributions
use r_mod, only: df, dp, dt, pf, pt, qf, qt
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp), parameter :: pi = acos(-1.0_dp)
real(kind=dp) :: probabilities(3), quantiles(3)

call assert_close(dt(0.0_dp, df=1.0_dp), 1.0_dp / pi, "Student t density at zero")
call assert_close(dt(0.0_dp, df=1.0_dp, log_=.true.), -log(pi), &
   "Student t log density at zero")
call assert_close(dt(1.0_dp, df=1.0_dp), 1.0_dp / (2.0_dp * pi), &
   "Student t Cauchy density identity")
call assert_close(pt(0.0_dp, df=7.0_dp), 0.5_dp, "Student t cdf at zero")
call assert_close(pt(-1.0_dp, df=1.0_dp), 0.25_dp, "Student t Cauchy cdf identity")
call assert_close(qt(0.25_dp, df=1.0_dp), -1.0_dp, "Student t Cauchy quantile identity")
call assert_close(pt(-2.0_dp, df=5.0_dp) + pt(2.0_dp, df=5.0_dp), 1.0_dp, &
   "Student t cdf symmetry")

probabilities = [0.1_dp, 0.5_dp, 0.9_dp]
quantiles = qt(probabilities, df=6.0_dp)
call assert_vector_close(pt(quantiles, df=6.0_dp), probabilities, &
   "Student t cdf-quantile round trip")

call assert_close(df(1.0_dp, df1=2.0_dp, df2=2.0_dp), 0.25_dp, &
   "F density identity")
call assert_close(df(1.0_dp, df1=2.0_dp, df2=2.0_dp, log_=.true.), log(0.25_dp), &
   "F log density identity")
call assert_close(df(-1.0_dp, df1=2.0_dp, df2=2.0_dp), 0.0_dp, &
   "F density below support")
call assert_close(pf(0.0_dp, df1=2.0_dp, df2=2.0_dp), 0.0_dp, "F cdf at zero")
call assert_close(pf(3.0_dp, df1=2.0_dp, df2=2.0_dp), 0.75_dp, "F cdf identity")
call assert_close(qf(0.25_dp, df1=2.0_dp, df2=2.0_dp), 1.0_dp / 3.0_dp, &
   "F quantile identity")

quantiles = qf(probabilities, df1=4.0_dp, df2=9.0_dp)
call assert_vector_close(pf(quantiles, df1=4.0_dp, df2=9.0_dp), probabilities, &
   "F cdf-quantile round trip")

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close
end program test_t_and_f_distributions
