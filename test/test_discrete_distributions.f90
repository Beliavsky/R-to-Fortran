program test_discrete_distributions
use r_mod, only: dbinom, dgeom, dpois, dp, pbinom, pgeom, ppois, qbinom, qgeom, qpois
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: probabilities(3)

call assert_close(dbinom(2.0_dp, nsize=4, prob=0.5_dp), 6.0_dp / 16.0_dp, &
   "binomial mass")
call assert_close(dbinom(2.0_dp, nsize=4, prob=0.5_dp, log_=.true.), &
   log(6.0_dp / 16.0_dp), "binomial log mass")
call assert_close(dbinom(-1.0_dp, nsize=4, prob=0.5_dp), 0.0_dp, &
   "binomial mass below support")
call assert_close(dbinom(5.0_dp, nsize=4, prob=0.5_dp), 0.0_dp, &
   "binomial mass above support")
call assert_close(pbinom(1.0_dp, nsize=4, prob=0.5_dp), 5.0_dp / 16.0_dp, &
   "binomial cdf")
probabilities = qbinom([0.1_dp, 0.5_dp, 0.9_dp], nsize=4, prob=0.5_dp)
call assert_vector_close(probabilities, [1.0_dp, 2.0_dp, 3.0_dp], "binomial quantiles")

call assert_close(dpois(0.0_dp, lambda=2.0_dp), exp(-2.0_dp), "Poisson zero mass")
call assert_close(dpois(3.0_dp, lambda=2.0_dp), exp(-2.0_dp) * 8.0_dp / 6.0_dp, &
   "Poisson mass")
call assert_close(dpois(3.0_dp, lambda=2.0_dp, log_=.true.), &
   -2.0_dp + 3.0_dp * log(2.0_dp) - log(6.0_dp), "Poisson log mass")
call assert_close(dpois(-1.0_dp, lambda=2.0_dp), 0.0_dp, "Poisson mass below support")
call assert_close(ppois(2.0_dp, lambda=2.0_dp), 5.0_dp * exp(-2.0_dp), "Poisson cdf")
call assert_close(ppois(2.0_dp, lambda=2.0_dp, lower_tail=.false.), &
   1.0_dp - 5.0_dp * exp(-2.0_dp), "Poisson upper tail")
probabilities = qpois([0.1_dp, 0.5_dp, 0.9_dp], lambda=2.0_dp)
call assert_vector_close(probabilities, [0.0_dp, 2.0_dp, 4.0_dp], "Poisson quantiles")

call assert_close(dgeom(0.0_dp, prob=0.25_dp), 0.25_dp, "geometric zero mass")
call assert_close(dgeom(2.0_dp, prob=0.25_dp), 0.25_dp * 0.75_dp**2, &
   "geometric mass")
call assert_close(dgeom(2.0_dp, prob=0.25_dp, log_=.true.), &
   log(0.25_dp) + 2.0_dp * log(0.75_dp), "geometric log mass")
call assert_close(dgeom(-1.0_dp, prob=0.25_dp), 0.0_dp, "geometric mass below support")
call assert_close(pgeom(2.0_dp, prob=0.25_dp), 1.0_dp - 0.75_dp**3, "geometric cdf")
probabilities = qgeom([0.1_dp, 0.5_dp, 0.9_dp], prob=0.25_dp)
call assert_vector_close(probabilities, [0.0_dp, 2.0_dp, 8.0_dp], "geometric quantiles")

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
end program test_discrete_distributions
