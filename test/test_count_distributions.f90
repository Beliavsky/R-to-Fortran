program test_count_distributions
use r_mod, only: dhyper, dnbinom, dp, phyper, pnbinom, qhyper, qnbinom
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: values(3)

call assert_close(dnbinom(0.0_dp, nsize=2, prob=0.5_dp), 0.25_dp, &
   "negative-binomial zero mass")
call assert_close(dnbinom(1.0_dp, nsize=2, prob=0.5_dp), 0.25_dp, &
   "negative-binomial mass")
call assert_close(dnbinom(1.0_dp, nsize=2, prob=0.5_dp, log_=.true.), log(0.25_dp), &
   "negative-binomial log mass")
call assert_close(dnbinom(-1.0_dp, nsize=2, prob=0.5_dp), 0.0_dp, &
   "negative-binomial mass below support")
call assert_close(pnbinom(1.0_dp, nsize=2, prob=0.5_dp), 0.5_dp, &
   "negative-binomial cdf")

values = dnbinom([0.0_dp, 1.0_dp, 2.0_dp], nsize=1, prob=0.25_dp)
call assert_vector_close(values, 0.25_dp * [1.0_dp, 0.75_dp, 0.75_dp**2], &
   "negative-binomial geometric identity")
values = qnbinom([0.2_dp, 0.5_dp, 0.8_dp], nsize=2, prob=0.5_dp)
call assert_vector_close(values, [0.0_dp, 1.0_dp, 3.0_dp], "negative-binomial quantiles")

call assert_close(dhyper(0.0_dp, m=5, n=5, k=2), 2.0_dp / 9.0_dp, &
   "hypergeometric lower mass")
call assert_close(dhyper(1.0_dp, m=5, n=5, k=2), 5.0_dp / 9.0_dp, &
   "hypergeometric middle mass")
call assert_close(dhyper(2.0_dp, m=5, n=5, k=2, log_=.true.), log(2.0_dp / 9.0_dp), &
   "hypergeometric log mass")
call assert_close(phyper(0.0_dp, m=5, n=5, k=2), 2.0_dp / 9.0_dp, &
   "hypergeometric lower cdf")
call assert_close(phyper(1.0_dp, m=5, n=5, k=2), 7.0_dp / 9.0_dp, &
   "hypergeometric middle cdf")

values = dhyper([0.0_dp, 1.0_dp, 2.0_dp], m=5, n=5, k=2)
call assert_vector_close(values, [2.0_dp, 5.0_dp, 2.0_dp] / 9.0_dp, &
   "hypergeometric probability mass sum")
call assert_close(sum(values), 1.0_dp, "hypergeometric normalized mass")
values = qhyper([0.1_dp, 0.5_dp, 0.9_dp], m=5, n=5, k=2)
call assert_vector_close(values, [0.0_dp, 1.0_dp, 2.0_dp], "hypergeometric quantiles")

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
end program test_count_distributions
