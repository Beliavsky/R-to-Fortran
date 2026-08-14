program test_discrete_cdf_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_positive_inf, ieee_quiet_nan, ieee_value
use r_mod, only: dp, pbinom, pgeom, phyper, pnbinom, ppois, psignrank, pwilcox
implicit none

real(kind=dp) :: inf, nan_value

inf = ieee_value(0.0_dp, ieee_positive_inf)
nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

call assert_cdf_contract(pbinom([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 4, 0.5_dp), &
   "binomial CDF")
call assert_cdf_contract(ppois([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 2.0_dp), &
   "Poisson CDF")
call assert_cdf_contract(pgeom([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 0.25_dp), &
   "geometric CDF")
call assert_cdf_contract(pnbinom([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 3, 0.4_dp), &
   "negative-binomial CDF")
call assert_cdf_contract(phyper([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 5, 7, 4), &
   "hypergeometric CDF")
call assert_cdf_contract(pwilcox([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 2, 3), &
   "Wilcoxon CDF")
call assert_cdf_contract(psignrank([-inf, 1.0_dp, 1.5_dp, inf, nan_value], 3), &
   "signed-rank CDF")

call assert_close(ppois(-inf, 2.0_dp, lower_tail=.false.), 1.0_dp, &
   "Poisson upper tail at negative infinity")
call assert_close(ppois(inf, 2.0_dp, lower_tail=.false.), 0.0_dp, &
   "Poisson upper tail at positive infinity")
call assert_true(ieee_is_nan(ppois(nan_value, 2.0_dp, lower_tail=.false.)), &
   "Poisson upper tail NaN")

contains

subroutine assert_cdf_contract(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label

if (size(actual) /= 5) error stop trim(label) // " shape failed"
call assert_close(actual(1), 0.0_dp, trim(label) // " at negative infinity")
call assert_close(actual(3), actual(2), trim(label) // " floors noninteger quantile")
call assert_close(actual(4), 1.0_dp, trim(label) // " at positive infinity")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " propagates NaN")
end subroutine assert_cdf_contract

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > 1.0e-12_dp) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label

if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_discrete_cdf_variants
