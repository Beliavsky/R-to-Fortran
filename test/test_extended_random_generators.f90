program test_extended_random_generators
use r_mod, only: dp, rcauchy, rchisq, rexp, rf_rng, rlnorm, rlogis, rmultinom, &
   rnbinom, rsignrank, rt_vec, rweibull, rwilcox, set_seed_int
implicit none

real(kind=dp), allocatable :: first_real(:), second_real(:)
integer, allocatable :: first_int(:), second_int(:)
integer, allocatable :: first_matrix(:,:), second_matrix(:,:)

call set_seed_int(10101)
first_real = rexp(10, rate=2.0_dp)
call set_seed_int(10101)
second_real = rexp(10, rate=2.0_dp)
call assert_real_reproducible(first_real, second_real, "exponential RNG")
if (any(first_real < 0.0_dp)) error stop "exponential RNG support failed"

call set_seed_int(20202)
first_real = rchisq(10, df=4.0_dp)
call set_seed_int(20202)
second_real = rchisq(10, df=4.0_dp)
call assert_real_reproducible(first_real, second_real, "chi-square RNG")
if (any(first_real < 0.0_dp)) error stop "chi-square RNG support failed"

call set_seed_int(30303)
first_real = rt_vec(10, df=6.0_dp)
call set_seed_int(30303)
second_real = rt_vec(10, df=6.0_dp)
call assert_real_reproducible(first_real, second_real, "Student t RNG")

call set_seed_int(40404)
first_real = rf_rng(10, df1=4.0_dp, df2=8.0_dp)
call set_seed_int(40404)
second_real = rf_rng(10, df1=4.0_dp, df2=8.0_dp)
call assert_real_reproducible(first_real, second_real, "F RNG")
if (any(first_real < 0.0_dp)) error stop "F RNG support failed"

call set_seed_int(50505)
first_real = rlogis(10, location=2.0_dp, scale=3.0_dp)
call set_seed_int(50505)
second_real = rlogis(10, location=2.0_dp, scale=3.0_dp)
call assert_real_reproducible(first_real, second_real, "logistic RNG")

call set_seed_int(60606)
first_real = rlnorm(10, meanlog=0.5_dp, sdlog=0.8_dp)
call set_seed_int(60606)
second_real = rlnorm(10, meanlog=0.5_dp, sdlog=0.8_dp)
call assert_real_reproducible(first_real, second_real, "log-normal RNG")
if (any(first_real <= 0.0_dp)) error stop "log-normal RNG support failed"

call set_seed_int(70707)
first_real = rweibull(10, shape=1.5_dp, scale=2.0_dp)
call set_seed_int(70707)
second_real = rweibull(10, shape=1.5_dp, scale=2.0_dp)
call assert_real_reproducible(first_real, second_real, "Weibull RNG")
if (any(first_real < 0.0_dp)) error stop "Weibull RNG support failed"

call set_seed_int(80808)
first_real = rcauchy(10, location=-1.0_dp, scale=2.0_dp)
call set_seed_int(80808)
second_real = rcauchy(10, location=-1.0_dp, scale=2.0_dp)
call assert_real_reproducible(first_real, second_real, "Cauchy RNG")

call set_seed_int(90909)
first_int = rnbinom(12, size_=3.0_dp, prob=0.4_dp)
call set_seed_int(90909)
second_int = rnbinom(12, size_=3.0_dp, prob=0.4_dp)
call assert_integer_reproducible(first_int, second_int, "negative-binomial RNG")
if (any(first_int < 0)) error stop "negative-binomial RNG support failed"

call set_seed_int(11111)
first_int = rwilcox(12, m=2, n2=3)
call set_seed_int(11111)
second_int = rwilcox(12, m=2, n2=3)
call assert_integer_reproducible(first_int, second_int, "Wilcoxon RNG")
if (any(first_int < 0 .or. first_int > 6)) error stop "Wilcoxon RNG support failed"

call set_seed_int(12121)
first_int = rsignrank(12, n_obs=4)
call set_seed_int(12121)
second_int = rsignrank(12, n_obs=4)
call assert_integer_reproducible(first_int, second_int, "signed-rank RNG")
if (any(first_int < 0 .or. first_int > 10)) error stop "signed-rank RNG support failed"

call set_seed_int(13131)
first_matrix = rmultinom(8, size_=9, prob=[0.2_dp, 0.3_dp, 0.5_dp])
call set_seed_int(13131)
second_matrix = rmultinom(8, size_=9, prob=[0.2_dp, 0.3_dp, 0.5_dp])
call assert_matrix_reproducible(first_matrix, second_matrix, "multinomial RNG")
if (any(first_matrix < 0)) error stop "multinomial RNG support failed"
if (any(sum(first_matrix, dim=1) /= 9)) error stop "multinomial RNG count total failed"

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

subroutine assert_matrix_reproducible(actual, expected, label)
integer, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected)) .or. any(actual /= expected)) then
   write(*, '(a)') trim(label) // " reproducibility failed"
   error stop 1
end if
end subroutine assert_matrix_reproducible
end program test_extended_random_generators
