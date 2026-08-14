program test_integer_rng_degenerate_cases
use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf, ieee_quiet_nan, ieee_value
use r_mod, only: dp, is_na, rbinom, rgeom, rhyper, rmultinom, rnbinom, rpois, &
   rsignrank, rwilcox, runif_vec, set_seed_int
implicit none

integer, allocatable :: values(:), counts(:,:)
real(kind=dp) :: inf, nan
real(kind=dp), allocatable :: after(:), expected(:)

nan = ieee_value(0.0_dp, ieee_quiet_nan)
inf = ieee_value(0.0_dp, ieee_positive_inf)

call assert_all_equal(rbinom(5, size_=0, prob=0.5_dp), 0, "binomial zero size")
call assert_all_equal(rbinom(5, size_=7, prob=0.0_dp), 0, "binomial zero probability")
call assert_all_equal(rbinom(5, size_=7, prob=1.0_dp), 7, "binomial unit probability")
call assert_all_na(rbinom(5, size_=-1, prob=0.5_dp), "binomial invalid size")
call assert_all_na(rbinom(5, size_=7, prob=-0.1_dp), "binomial negative probability")
call assert_all_na(rbinom(5, size_=7, prob=1.1_dp), "binomial probability above one")
call assert_all_na(rbinom(5, size_=7, prob=nan), "binomial NaN probability")
values = rbinom(4, size_=3, prob=[-0.1_dp, 0.5_dp, 1.1_dp, nan])
call assert_true(is_na(values(1)) .and. .not. is_na(values(2)) .and. &
   is_na(values(3)) .and. is_na(values(4)), "binomial vector probability validation")
call assert_all_equal(rpois(5, lambda=0.0_dp), 0, "Poisson zero mean")
call assert_all_na(rpois(5, lambda=-1.0_dp), "Poisson negative mean")
call assert_all_na(rpois(5, lambda=nan), "Poisson NaN mean")
call assert_all_na(rpois(5, lambda=inf), "Poisson infinite mean")
values = rpois(4, lambda=[-1.0_dp, 2.0_dp, nan, inf])
call assert_true(is_na(values(1)) .and. .not. is_na(values(2)) .and. &
   is_na(values(3)) .and. is_na(values(4)), "Poisson vector mean validation")
call assert_all_equal(rgeom(5, prob=1.0_dp), 0, "geometric unit probability")
call assert_all_na(rgeom(5, prob=0.0_dp), "geometric zero probability")
call assert_all_na(rgeom(5, prob=-0.1_dp), "geometric negative probability")
call assert_all_na(rgeom(5, prob=1.1_dp), "geometric probability above one")
call assert_all_na(rgeom(5, prob=nan), "geometric NaN probability")
call assert_all_equal(rnbinom(5, size_=3.0_dp, prob=1.0_dp), 0, &
   "negative-binomial unit probability")
call assert_all_equal(rnbinom(5, size_=3.0_dp, mu=0.0_dp), 0, &
   "negative-binomial zero mean")
call assert_all_na(rnbinom(5, size_=-1.0_dp, prob=0.5_dp), &
   "negative-binomial invalid size")
call assert_all_na(rnbinom(5, size_=0.0_dp, prob=0.5_dp), &
   "negative-binomial zero size")
call assert_all_na(rnbinom(5, size_=3.0_dp, prob=0.0_dp), &
   "negative-binomial zero probability")
call assert_all_na(rnbinom(5, size_=3.0_dp, prob=-0.1_dp), &
   "negative-binomial negative probability")
call assert_all_na(rnbinom(5, size_=3.0_dp, prob=1.1_dp), &
   "negative-binomial probability above one")
call assert_all_na(rnbinom(5, size_=3.0_dp, prob=nan), &
   "negative-binomial NaN probability")
call assert_all_na(rnbinom(5, size_=3.0_dp, mu=-1.0_dp), &
   "negative-binomial negative mean")
call assert_all_na(rnbinom(5, size_=3.0_dp, mu=nan), &
   "negative-binomial NaN mean")
call assert_all_equal(rhyper(5, m=0, nwhite=7, k=0), 0, "hypergeometric zero draw")
call assert_all_equal(rhyper(5, m=5, nwhite=7, k=0), 0, &
   "hypergeometric zero draw with nonempty population")
call assert_all_equal(rhyper(5, m=5, nwhite=7, k=12), 5, &
   "hypergeometric full-population draw")
call assert_all_na(rhyper(5, m=-1, nwhite=7, k=3), &
   "hypergeometric negative first population")
call assert_all_na(rhyper(5, m=5, nwhite=-1, k=3), &
   "hypergeometric negative second population")
call assert_all_na(rhyper(5, m=5, nwhite=7, k=-1), &
   "hypergeometric negative draw")
call assert_all_na(rhyper(5, m=5, nwhite=7, k=13), &
   "hypergeometric excessive draw")
call assert_all_equal(rwilcox(5, m=0, n2=3), 0, "Wilcoxon zero first sample")
call assert_all_equal(rwilcox(5, m=3, n2=0), 0, "Wilcoxon zero second sample")
call assert_all_equal(rsignrank(5, n_obs=0), 0, "signed-rank zero sample")
call assert_all_na(rwilcox(5, m=-1, n2=3), "Wilcoxon negative first sample")
call assert_all_na(rwilcox(5, m=3, n2=-1), "Wilcoxon negative second sample")
call assert_all_na(rsignrank(5, n_obs=-1), "signed-rank negative sample")

counts = rmultinom(4, size_=0, prob=[0.2_dp, 0.3_dp, 0.5_dp])
call assert_true(all(shape(counts) == [3, 4]), "multinomial zero-size shape")
call assert_true(all(counts == 0), "multinomial zero-size counts")

counts = rmultinom(4, size_=5, prob=[1.0_dp])
call assert_true(all(shape(counts) == [1, 4]), "single-category multinomial shape")
call assert_true(all(counts == 5), "single-category multinomial counts")
counts = rmultinom(4, size_=5, prob=[0.0_dp, 2.0_dp, 0.0_dp])
call assert_true(all(shape(counts) == [3, 4]), "collapsed multinomial shape")
call assert_true(all(counts(1, :) == 0) .and. all(counts(2, :) == 5) .and. &
   all(counts(3, :) == 0), "collapsed multinomial counts")

call set_seed_int(24680)
values = rbinom(4, size_=0, prob=0.5_dp)
after = runif_vec(4)
call set_seed_int(24680)
expected = runif_vec(4)
call assert_true(all(after == expected), "degenerate binomial preserves RNG state")

call set_seed_int(24680)
call assert_preserves_rng(rpois(4, lambda=0.0_dp), "degenerate Poisson")
call assert_preserves_rng(rpois(4, lambda=-1.0_dp), "Poisson negative mean")
call assert_preserves_rng(rpois(4, lambda=nan), "Poisson NaN mean")
call assert_preserves_rng(rpois(4, lambda=inf), "Poisson infinite mean")
call assert_preserves_rng(rgeom(4, prob=0.0_dp), "geometric zero probability")
call assert_preserves_rng(rgeom(4, prob=-0.1_dp), "geometric negative probability")
call assert_preserves_rng(rgeom(4, prob=1.1_dp), "geometric probability above one")
call assert_preserves_rng(rgeom(4, prob=nan), "geometric NaN probability")
call assert_preserves_rng(rbinom(4, size_=7, prob=0.0_dp), &
   "degenerate binomial zero probability")
call assert_preserves_rng(rbinom(4, size_=7, prob=1.0_dp), &
   "degenerate binomial unit probability")
call assert_preserves_rng(rbinom(4, size_=-1, prob=0.5_dp), "binomial invalid size")
call assert_preserves_rng(rbinom(4, size_=7, prob=-0.1_dp), &
   "binomial negative probability")
call assert_preserves_rng(rbinom(4, size_=7, prob=1.1_dp), &
   "binomial probability above one")
call assert_preserves_rng(rbinom(4, size_=7, prob=nan), "binomial NaN probability")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, prob=1.0_dp), &
   "degenerate negative-binomial probability")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, mu=0.0_dp), &
   "degenerate negative-binomial mean")
call assert_preserves_rng(rnbinom(4, size_=-1.0_dp, prob=0.5_dp), &
   "negative-binomial invalid size")
call assert_preserves_rng(rnbinom(4, size_=0.0_dp, prob=0.5_dp), &
   "negative-binomial zero size")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, prob=0.0_dp), &
   "negative-binomial zero probability")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, prob=-0.1_dp), &
   "negative-binomial negative probability")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, prob=1.1_dp), &
   "negative-binomial probability above one")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, prob=nan), &
   "negative-binomial NaN probability")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, mu=-1.0_dp), &
   "negative-binomial negative mean")
call assert_preserves_rng(rnbinom(4, size_=3.0_dp, mu=nan), &
   "negative-binomial NaN mean")
call assert_preserves_rng(rhyper(4, m=0, nwhite=7, k=0), "degenerate hypergeometric")
call assert_preserves_rng(rhyper(4, m=7, nwhite=0, k=3), &
   "degenerate hypergeometric upper support")
call assert_preserves_rng(rhyper(4, m=5, nwhite=7, k=0), &
   "hypergeometric zero draw")
call assert_preserves_rng(rhyper(4, m=5, nwhite=7, k=12), &
   "hypergeometric full-population draw")
call assert_preserves_rng(rhyper(4, m=-1, nwhite=7, k=3), &
   "hypergeometric negative first population")
call assert_preserves_rng(rhyper(4, m=5, nwhite=-1, k=3), &
   "hypergeometric negative second population")
call assert_preserves_rng(rhyper(4, m=5, nwhite=7, k=-1), &
   "hypergeometric negative draw")
call assert_preserves_rng(rhyper(4, m=5, nwhite=7, k=13), &
   "hypergeometric excessive draw")
call assert_preserves_rng(rwilcox(4, m=0, n2=3), "degenerate Wilcoxon")
call assert_preserves_rng(rsignrank(4, n_obs=0), "degenerate signed-rank")
call assert_preserves_rng(rwilcox(4, m=-1, n2=3), "Wilcoxon negative first sample")
call assert_preserves_rng(rwilcox(4, m=3, n2=-1), "Wilcoxon negative second sample")
call assert_preserves_rng(rsignrank(4, n_obs=-1), "signed-rank negative sample")

call set_seed_int(24680)
counts = rmultinom(4, size_=0, prob=[0.2_dp, 0.3_dp, 0.5_dp])
after = runif_vec(4)
call set_seed_int(24680)
expected = runif_vec(4)
call assert_true(all(after == expected), "degenerate multinomial preserves RNG state")

call set_seed_int(24680)
counts = rmultinom(4, size_=5, prob=[1.0_dp])
after = runif_vec(4)
call set_seed_int(24680)
expected = runif_vec(4)
call assert_true(all(after == expected), "single-category multinomial preserves RNG state")

call set_seed_int(24680)
counts = rmultinom(4, size_=5, prob=[0.0_dp, 2.0_dp, 0.0_dp])
after = runif_vec(4)
call set_seed_int(24680)
expected = runif_vec(4)
call assert_true(all(after == expected), "collapsed multinomial preserves RNG state")

call set_seed_int(24680)
call assert_empty_preserves_rng(rbinom(0, size_=3, prob=0.5_dp), "zero-length binomial")
call assert_empty_preserves_rng(rpois(0, lambda=2.0_dp), "zero-length Poisson")
call assert_empty_preserves_rng(rgeom(0, prob=0.5_dp), "zero-length geometric")
call assert_empty_preserves_rng(rnbinom(0, size_=3.0_dp, prob=0.5_dp), &
   "zero-length negative-binomial")
call assert_empty_preserves_rng(rhyper(0, m=5, nwhite=7, k=3), &
   "zero-length hypergeometric")
call assert_empty_preserves_rng(rwilcox(0, m=3, n2=4), "zero-length Wilcoxon")
call assert_empty_preserves_rng(rsignrank(0, n_obs=4), "zero-length signed-rank")
call assert_empty_multinomial_preserves_rng(rmultinom(0, size_=3, prob=[0.5_dp, 0.5_dp]), &
   "zero-length multinomial")

contains

subroutine assert_all_equal(actual, expected_value, label)
integer, intent(in) :: actual(:), expected_value
character(len=*), intent(in) :: label
call assert_true(all(actual == expected_value), label)
end subroutine assert_all_equal

subroutine assert_all_na(actual, label)
integer, intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(all(is_na(actual)), label)
end subroutine assert_all_na

subroutine assert_preserves_rng(actual, label)
integer, intent(in) :: actual(:)
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
integer, intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(size(actual) == 0, trim(label) // " result size")
call assert_rng_unchanged(label)
end subroutine assert_empty_preserves_rng

subroutine assert_empty_multinomial_preserves_rng(actual, label)
integer, intent(in) :: actual(:,:)
character(len=*), intent(in) :: label
call assert_true(all(shape(actual) == [2, 0]), trim(label) // " result shape")
call assert_rng_unchanged(label)
end subroutine assert_empty_multinomial_preserves_rng

subroutine assert_rng_unchanged(label)
character(len=*), intent(in) :: label
real(kind=dp), allocatable :: actual_after(:), expected_after(:)
actual_after = runif_vec(4)
call set_seed_int(24680)
expected_after = runif_vec(4)
call assert_true(all(actual_after == expected_after), trim(label) // " preserves RNG state")
call set_seed_int(24680)
end subroutine assert_rng_unchanged

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label
if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_integer_rng_degenerate_cases
