program test_rank_distribution_boundaries
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_positive_inf, &
   ieee_quiet_nan, ieee_value
use r_mod, only: dp, dsignrank, dwilcox, psignrank, pwilcox, qsignrank, qwilcox
implicit none

real(kind=dp) :: inf, nan_value
real(kind=dp), allocatable :: values(:)

inf = ieee_value(0.0_dp, ieee_positive_inf)
nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

values = dwilcox([-inf, -1.0_dp, 0.0_dp, 1.5_dp, 6.0_dp, inf, nan_value], m=2, n=3)
call assert_equal(values(1), 0.0_dp, "Wilcoxon mass at negative infinity")
call assert_equal(values(2), 0.0_dp, "Wilcoxon mass below support")
call assert_equal(values(3), 0.1_dp, "Wilcoxon mass at zero")
call assert_equal(values(4), 0.0_dp, "Wilcoxon noninteger mass")
call assert_equal(values(5), 0.1_dp, "Wilcoxon mass at upper support")
call assert_equal(values(6), 0.0_dp, "Wilcoxon mass at positive infinity")
call assert_true(ieee_is_nan(values(7)), "Wilcoxon mass propagates NaN")

values = pwilcox([-inf, -1.0_dp, 0.0_dp, 6.0_dp, inf, nan_value], m=2, n=3)
call assert_cdf_boundaries(values, "Wilcoxon CDF")
values = qwilcox([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], m=2, n=3)
call assert_quantile_boundaries(values, 3.0_dp, 6.0_dp, "Wilcoxon quantile")

values = dsignrank([-inf, -1.0_dp, 0.0_dp, 1.5_dp, 6.0_dp, inf, nan_value], n=3)
call assert_equal(values(1), 0.0_dp, "signed-rank mass at negative infinity")
call assert_equal(values(2), 0.0_dp, "signed-rank mass below support")
call assert_equal(values(3), 0.125_dp, "signed-rank mass at zero")
call assert_equal(values(4), 0.0_dp, "signed-rank noninteger mass")
call assert_equal(values(5), 0.125_dp, "signed-rank mass at upper support")
call assert_equal(values(6), 0.0_dp, "signed-rank mass at positive infinity")
call assert_true(ieee_is_nan(values(7)), "signed-rank mass propagates NaN")

values = psignrank([-inf, -1.0_dp, 0.0_dp, 6.0_dp, inf, nan_value], n=3)
call assert_cdf_boundaries(values, "signed-rank CDF")
values = qsignrank([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], n=3)
call assert_quantile_boundaries(values, 3.0_dp, 6.0_dp, "signed-rank quantile")

call assert_true(ieee_is_nan(dwilcox(1.0_dp, m=0, n=3)), "Wilcoxon mass invalid m")
call assert_true(ieee_is_nan(pwilcox(1.0_dp, m=2, n=0)), "Wilcoxon CDF invalid n")
call assert_true(ieee_is_nan(qwilcox(0.5_dp, m=-1, n=3)), "Wilcoxon quantile invalid m")
call assert_true(ieee_is_nan(dsignrank(1.0_dp, n=0)), "signed-rank mass invalid n")
call assert_true(ieee_is_nan(psignrank(1.0_dp, n=-1)), "signed-rank CDF invalid n")
call assert_true(ieee_is_nan(qsignrank(0.5_dp, n=0)), "signed-rank quantile invalid n")

contains

subroutine assert_cdf_boundaries(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_equal(actual(1), 0.0_dp, trim(label) // " at negative infinity")
call assert_equal(actual(2), 0.0_dp, trim(label) // " below support")
call assert_true(actual(3) > 0.0_dp, trim(label) // " at zero")
call assert_equal(actual(4), 1.0_dp, trim(label) // " at upper support")
call assert_equal(actual(5), 1.0_dp, trim(label) // " at positive infinity")
call assert_true(ieee_is_nan(actual(6)), trim(label) // " propagates NaN")
end subroutine assert_cdf_boundaries

subroutine assert_quantile_boundaries(actual, median_value, maximum, label)
real(kind=dp), intent(in) :: actual(:), median_value, maximum
character(len=*), intent(in) :: label
call assert_true(ieee_is_nan(actual(1)), trim(label) // " below probability range")
call assert_equal(actual(2), 0.0_dp, trim(label) // " lower endpoint")
call assert_equal(actual(3), median_value, trim(label) // " median")
call assert_equal(actual(4), maximum, trim(label) // " upper endpoint")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " above probability range")
call assert_true(ieee_is_nan(actual(6)), trim(label) // " propagates NaN")
end subroutine assert_quantile_boundaries

subroutine assert_equal(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
call assert_true(abs(actual - expected) <= 1.0e-12_dp, label)
end subroutine assert_equal

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label
if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_rank_distribution_boundaries
