program test_random_generator_boundaries
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_positive_inf, &
   ieee_quiet_nan, ieee_value
use r_mod, only: dp, rbeta, rcauchy, rchisq, rexp, rf_rng, rgamma, rlnorm, &
   rlogis, rnorm_vec, rt_vec, runif_vec, rweibull, set_seed_int
implicit none

real(kind=dp) :: inf
real(kind=dp), allocatable :: values(:), after(:), expected(:)

inf = ieee_value(0.0_dp, ieee_positive_inf)

values = rexp(4, rate=inf)
call assert_all_equal(values, 0.0_dp, "exponential RNG infinite rate")
call assert_all_equal(rexp(4, rate=-inf), 0.0_dp, "exponential RNG negative infinite rate")
call assert_all_nan(rexp(4, rate=0.0_dp), "exponential RNG zero rate")
call assert_all_nan(rexp(4, rate=-1.0_dp), "exponential RNG invalid rate")

values = rgamma(4, shape=2.0_dp, rate=inf)
call assert_all_equal(values, 0.0_dp, "gamma RNG infinite rate")
values = rgamma(4, shape=2.0_dp, rate=0.0_dp)
call assert_true(all(.not. ieee_is_finite(values) .and. values > 0.0_dp), &
   "gamma RNG zero rate")
values = rgamma(4, shape=2.0_dp, scale=0.0_dp)
call assert_all_equal(values, 0.0_dp, "gamma RNG zero scale")
call assert_all_equal(rgamma(4, shape=0.0_dp), 0.0_dp, "gamma RNG zero shape")
values = rgamma(4, shape=inf)
call assert_true(all(.not. ieee_is_finite(values) .and. values > 0.0_dp), &
   "gamma RNG infinite shape")
call assert_all_nan(rgamma(4, shape=-1.0_dp), "gamma RNG invalid shape")
call assert_all_nan(rgamma(4, shape=2.0_dp, scale=-1.0_dp), "gamma RNG invalid scale")
call assert_all_equal(rbeta(4, shape1=0.0_dp, shape2=2.0_dp), 0.0_dp, &
   "beta RNG zero first shape")
call assert_all_equal(rbeta(4, shape1=2.0_dp, shape2=0.0_dp), 1.0_dp, &
   "beta RNG zero second shape")
call assert_all_equal(rbeta(4, shape1=inf, shape2=2.0_dp), 1.0_dp, &
   "beta RNG infinite first shape")
call assert_all_equal(rbeta(4, shape1=2.0_dp, shape2=inf), 0.0_dp, &
   "beta RNG infinite second shape")
call assert_all_equal(rbeta(4, shape1=inf, shape2=inf), 0.5_dp, &
   "beta RNG both infinite shapes")
call assert_all_nan(rbeta(4, shape1=-1.0_dp, shape2=2.0_dp), "beta RNG invalid shape")

call set_seed_int(24680)
values = rbeta(20, shape1=0.0_dp, shape2=0.0_dp)
after = runif_vec(4)
call assert_true(all(values == 0.0_dp .or. values == 1.0_dp), "beta RNG zero shapes support")
call assert_true(any(values == 0.0_dp) .and. any(values == 1.0_dp), &
   "beta RNG zero shapes variation")
call set_seed_int(24680)
expected = runif_vec(20)
expected = runif_vec(4)
call assert_true(all(after == expected), "beta RNG zero shapes consumes one uniform per draw")

values = rchisq(4, df=0.0_dp)
call assert_all_equal(values, 0.0_dp, "chi-square RNG zero df")
call assert_all_nan(rchisq(4, df=-1.0_dp), "chi-square RNG invalid df")
call assert_all_nan(rchisq(4, df=inf), "chi-square RNG infinite df")
call assert_all_nan(rt_vec(4, df=-1.0_dp), "Student t RNG invalid df")
call assert_all_nan(rf_rng(4, df1=-1.0_dp, df2=2.0_dp), "F RNG invalid df")
call assert_all_equal(rf_rng(4, df1=inf, df2=inf), 1.0_dp, &
   "F RNG both infinite df")

call set_seed_int(24680)
values = rt_vec(4, df=inf)
call set_seed_int(24680)
expected = rnorm_vec(4)
call assert_true(all(values == expected), "Student t infinite df reduces to normal")
call set_seed_int(24680)
values = rf_rng(4, df1=inf, df2=3.0_dp)
call set_seed_int(24680)
expected = 1.0_dp / (rchisq(4, df=3.0_dp) / 3.0_dp)
call assert_true(all(values == expected), "F infinite numerator df reduction")
call set_seed_int(24680)
values = rf_rng(4, df1=3.0_dp, df2=inf)
call set_seed_int(24680)
expected = rchisq(4, df=3.0_dp) / 3.0_dp
call assert_true(all(values == expected), "F infinite denominator df reduction")

values = rlogis(4, location=2.5_dp, scale=0.0_dp)
call assert_all_equal(values, 2.5_dp, "logistic RNG zero scale")
values = rlogis(4, location=inf, scale=1.0_dp)
call assert_true(all(.not. ieee_is_finite(values) .and. values > 0.0_dp), &
   "logistic RNG infinite location")
call assert_all_nan(rlogis(4, location=inf, scale=inf), &
   "logistic RNG infinite location and scale")
call assert_all_nan(rlogis(4, location=nan_value(), scale=1.0_dp), &
   "logistic RNG NaN location")
values = rlnorm(4, meanlog=2.0_dp, sdlog=0.0_dp)
call assert_all_equal(values, exp(2.0_dp), "log-normal RNG zero sdlog")
values = rlnorm(4, meanlog=inf, sdlog=1.0_dp)
call assert_true(all(.not. ieee_is_finite(values) .and. values > 0.0_dp), &
   "log-normal RNG infinite mean")
call assert_all_equal(rlnorm(4, meanlog=-inf, sdlog=1.0_dp), 0.0_dp, &
   "log-normal RNG negative infinite mean")
call assert_all_nan(rlnorm(4, sdlog=-1.0_dp), "log-normal RNG invalid sdlog")
values = rweibull(4, shape=2.0_dp, scale=0.0_dp)
call assert_all_equal(values, 0.0_dp, "Weibull RNG zero scale")
call assert_all_equal(rweibull(4, shape=nan_value(), scale=0.0_dp), 0.0_dp, &
   "Weibull RNG zero scale dominates invalid shape")
call assert_all_nan(rweibull(4, shape=-1.0_dp), "Weibull RNG invalid shape")
call assert_all_nan(rweibull(4, shape=inf, scale=2.0_dp), &
   "Weibull RNG infinite shape")
call assert_all_nan(rweibull(4, shape=2.0_dp, scale=inf), &
   "Weibull RNG infinite scale")
values = rcauchy(4, location=-3.0_dp, scale=0.0_dp)
call assert_all_equal(values, -3.0_dp, "Cauchy RNG zero scale")
values = rcauchy(4, location=inf, scale=1.0_dp)
call assert_true(all(.not. ieee_is_finite(values) .and. values > 0.0_dp), &
   "Cauchy RNG infinite location")
call assert_all_nan(rcauchy(4, location=inf, scale=inf), &
   "Cauchy RNG infinite location and scale")
call assert_all_nan(rcauchy(4, scale=-1.0_dp), "Cauchy RNG invalid scale")

call set_seed_int(24680)
call assert_preserves_rng(rexp(4, rate=inf), "exponential infinite rate")
call assert_preserves_rng(rexp(4, rate=-inf), "exponential negative infinite rate")
call assert_preserves_rng(rexp(4, rate=0.0_dp), "exponential zero rate")
call assert_preserves_rng(rgamma(4, shape=2.0_dp, rate=inf), "gamma infinite rate")
call assert_preserves_rng(rgamma(4, shape=2.0_dp, rate=0.0_dp), "gamma zero rate")
call assert_preserves_rng(rgamma(4, shape=2.0_dp, scale=0.0_dp), "gamma zero scale")
call assert_preserves_rng(rchisq(4, df=0.0_dp), "chi-square zero df")
call assert_preserves_rng(rlogis(4, location=2.5_dp, scale=0.0_dp), "logistic zero scale")
call assert_preserves_rng(rlogis(4, location=inf, scale=1.0_dp), &
   "logistic infinite location")
call assert_preserves_rng(rlogis(4, location=inf, scale=inf), &
   "logistic infinite location and scale")
call assert_preserves_rng(rlnorm(4, meanlog=2.0_dp, sdlog=0.0_dp), "log-normal zero sdlog")
call assert_preserves_rng(rlnorm(4, meanlog=inf, sdlog=1.0_dp), &
   "log-normal infinite mean")
call assert_preserves_rng(rlnorm(4, meanlog=-inf, sdlog=1.0_dp), &
   "log-normal negative infinite mean")
call assert_preserves_rng(rweibull(4, shape=2.0_dp, scale=0.0_dp), "Weibull zero scale")
call assert_preserves_rng(rweibull(4, shape=nan_value(), scale=0.0_dp), &
   "Weibull zero scale with invalid shape")
call assert_preserves_rng(rweibull(4, shape=inf, scale=2.0_dp), &
   "Weibull infinite shape")
call assert_preserves_rng(rweibull(4, shape=2.0_dp, scale=inf), &
   "Weibull infinite scale")
call assert_preserves_rng(rcauchy(4, location=-3.0_dp, scale=0.0_dp), "Cauchy zero scale")
call assert_preserves_rng(rcauchy(4, location=inf, scale=1.0_dp), &
   "Cauchy infinite location")
call assert_preserves_rng(rcauchy(4, location=inf, scale=inf), &
   "Cauchy infinite location and scale")
call assert_preserves_rng(rgamma(4, shape=0.0_dp), "gamma zero shape")
call assert_preserves_rng(rgamma(4, shape=inf), "gamma infinite shape")
call assert_preserves_rng(rbeta(4, shape1=0.0_dp, shape2=2.0_dp), &
   "beta zero first shape")
call assert_preserves_rng(rbeta(4, shape1=2.0_dp, shape2=0.0_dp), &
   "beta zero second shape")
call assert_preserves_rng(rbeta(4, shape1=inf, shape2=2.0_dp), &
   "beta infinite first shape")
call assert_preserves_rng(rbeta(4, shape1=2.0_dp, shape2=inf), &
   "beta infinite second shape")
call assert_preserves_rng(rbeta(4, shape1=inf, shape2=inf), &
   "beta both infinite shapes")
call assert_preserves_rng(rexp(4, rate=-1.0_dp), "exponential invalid rate")
call assert_preserves_rng(rgamma(4, shape=-1.0_dp), "gamma invalid shape")
call assert_preserves_rng(rgamma(4, shape=2.0_dp, scale=-1.0_dp), "gamma invalid scale")
call assert_preserves_rng(rbeta(4, shape1=-1.0_dp, shape2=2.0_dp), "beta invalid shape")
call assert_preserves_rng(rchisq(4, df=-1.0_dp), "chi-square invalid df")
call assert_preserves_rng(rchisq(4, df=inf), "chi-square infinite df")
call assert_preserves_rng(rt_vec(4, df=-1.0_dp), "Student t invalid df")
call assert_preserves_rng(rf_rng(4, df1=-1.0_dp, df2=2.0_dp), "F invalid df")
call assert_preserves_rng(rf_rng(4, df1=inf, df2=inf), "F both infinite df")
call assert_preserves_rng(rlnorm(4, sdlog=-1.0_dp), "log-normal invalid sdlog")
call assert_preserves_rng(rweibull(4, shape=-1.0_dp), "Weibull invalid shape")
call assert_preserves_rng(rcauchy(4, scale=-1.0_dp), "Cauchy invalid scale")

call assert_empty_preserves_rng(rexp(0, rate=2.0_dp), "zero-length exponential")
call assert_empty_preserves_rng(rgamma(0, shape=2.0_dp), "zero-length gamma")
call assert_empty_preserves_rng(rbeta(0, shape1=2.0_dp, shape2=3.0_dp), &
   "zero-length beta")
call assert_empty_preserves_rng(rchisq(0, df=3.0_dp), "zero-length chi-square")
call assert_empty_preserves_rng(rt_vec(0, df=3.0_dp), "zero-length Student t")
call assert_empty_preserves_rng(rf_rng(0, df1=3.0_dp, df2=4.0_dp), "zero-length F")
call assert_empty_preserves_rng(rlogis(0), "zero-length logistic")
call assert_empty_preserves_rng(rlnorm(0), "zero-length log-normal")
call assert_empty_preserves_rng(rweibull(0, shape=2.0_dp), "zero-length Weibull")
call assert_empty_preserves_rng(rcauchy(0), "zero-length Cauchy")

contains

subroutine assert_all_equal(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected
character(len=*), intent(in) :: label
call assert_true(all(actual == expected), label)
end subroutine assert_all_equal

function nan_value() result(value)
real(kind=dp) :: value
value = ieee_value(0.0_dp, ieee_quiet_nan)
end function nan_value

subroutine assert_all_nan(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(all(ieee_is_nan(actual)), label)
end subroutine assert_all_nan

subroutine assert_preserves_rng(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
real(kind=dp), allocatable :: actual_after(:), expected_after(:)
call assert_true(size(actual) == 4, trim(label) // " result size")
actual_after = runif_vec(4)
call set_seed_int(24680)
expected_after = runif_vec(4)
call assert_true(all(actual_after == expected_after), trim(label) // " preserves RNG state")
call set_seed_int(24680)
end subroutine assert_preserves_rng

subroutine assert_empty_preserves_rng(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
real(kind=dp), allocatable :: actual_after(:), expected_after(:)
call assert_true(size(actual) == 0, trim(label) // " result size")
actual_after = runif_vec(4)
call set_seed_int(24680)
expected_after = runif_vec(4)
call assert_true(all(actual_after == expected_after), trim(label) // " preserves RNG state")
call set_seed_int(24680)
end subroutine assert_empty_preserves_rng

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label
if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_random_generator_boundaries
