program test_random_generators
use r_mod, only: dp, rbeta, rbinom, rgeom, rgamma, rhyper, rnorm_vec, rpois, runif_vec, set_seed_int
implicit none

real(kind=dp), allocatable :: first_real(:), second_real(:)
integer, allocatable :: first_int(:), second_int(:)

call set_seed_int(12345)
first_real = runif_vec(12)
call set_seed_int(12345)
second_real = runif_vec(12)
call assert_real_reproducible(first_real, second_real, "uniform RNG")
if (any(first_real < 0.0_dp .or. first_real >= 1.0_dp)) error stop "uniform RNG support failed"

call set_seed_int(23456)
first_real = rnorm_vec(11)
call set_seed_int(23456)
second_real = rnorm_vec(11)
call assert_real_reproducible(first_real, second_real, "normal RNG")

call set_seed_int(34567)
first_real = rgamma(10, shape=2.5_dp, rate=1.7_dp)
call set_seed_int(34567)
second_real = rgamma(10, shape=2.5_dp, rate=1.7_dp)
call assert_real_reproducible(first_real, second_real, "gamma RNG")
if (any(first_real <= 0.0_dp)) error stop "gamma RNG support failed"

call set_seed_int(45678)
first_real = rbeta(10, shape1=2.0_dp, shape2=5.0_dp)
call set_seed_int(45678)
second_real = rbeta(10, shape1=2.0_dp, shape2=5.0_dp)
call assert_real_reproducible(first_real, second_real, "beta RNG")
if (any(first_real < 0.0_dp .or. first_real > 1.0_dp)) error stop "beta RNG support failed"

call set_seed_int(56789)
first_int = rbinom(12, size_=7, prob=0.35_dp)
call set_seed_int(56789)
second_int = rbinom(12, size_=7, prob=0.35_dp)
call assert_integer_reproducible(first_int, second_int, "binomial RNG")
if (any(first_int < 0 .or. first_int > 7)) error stop "binomial RNG support failed"

call set_seed_int(67890)
first_int = rpois(12, lambda=3.5_dp)
call set_seed_int(67890)
second_int = rpois(12, lambda=3.5_dp)
call assert_integer_reproducible(first_int, second_int, "Poisson RNG")
if (any(first_int < 0)) error stop "Poisson RNG support failed"

call set_seed_int(78901)
first_int = rgeom(12, prob=0.3_dp)
call set_seed_int(78901)
second_int = rgeom(12, prob=0.3_dp)
call assert_integer_reproducible(first_int, second_int, "geometric RNG")
if (any(first_int < 0)) error stop "geometric RNG support failed"

call set_seed_int(89012)
first_int = rhyper(12, m=5, nwhite=7, k=4)
call set_seed_int(89012)
second_int = rhyper(12, m=5, nwhite=7, k=4)
call assert_integer_reproducible(first_int, second_int, "hypergeometric RNG")
if (any(first_int < 0 .or. first_int > 4)) error stop "hypergeometric RNG support failed"

if (size(runif_vec(0)) /= 0) error stop "zero-length uniform RNG failed"
if (size(rnorm_vec(0)) /= 0) error stop "zero-length normal RNG failed"
if (size(rbinom(0, size_=3, prob=0.5_dp)) /= 0) error stop "zero-length binomial RNG failed"

contains

subroutine assert_real_reproducible(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected) .or. any(actual /= expected)) then
   write(*, '(a)') trim(label) // " reproducibility failed"
   error stop 1
end if
end subroutine assert_real_reproducible

subroutine assert_integer_reproducible(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected) .or. any(actual /= expected)) then
   write(*, '(a)') trim(label) // " reproducibility failed"
   error stop 1
end if
end subroutine assert_integer_reproducible
end program test_random_generators
