program test_classical_distribution_boundaries
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_positive_inf, &
   ieee_quiet_nan, ieee_value
use r_mod, only: dbeta, dgamma, df, dp, dt, pbeta, pgamma, pf, pt, qbeta, &
   qf, qgamma, qt
implicit none

real(kind=dp), parameter :: tol = 1.0e-12_dp
real(kind=dp) :: inf, nan_value
real(kind=dp), allocatable :: values(:)

inf = ieee_value(0.0_dp, ieee_positive_inf)
nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

values = dgamma([-inf, 0.0_dp, 1.0_dp, inf, nan_value], shape=2.0_dp)
call assert_density_support(values, exp(-1.0_dp), "gamma density")
values = pgamma([-inf, 0.0_dp, inf, nan_value], shape=2.0_dp)
call assert_positive_cdf(values, "gamma CDF")
values = qgamma([-0.1_dp, 0.0_dp, 1.0_dp, 1.1_dp, nan_value], shape=2.0_dp)
call assert_positive_quantile_boundaries(values, "gamma quantile")
call assert_true(ieee_is_nan(dgamma(1.0_dp, shape=-1.0_dp)), "gamma density invalid shape")
call assert_true(ieee_is_nan(pgamma(1.0_dp, shape=2.0_dp, rate=-1.0_dp)), "gamma CDF invalid rate")
call assert_true(ieee_is_nan(qgamma(0.5_dp, shape=-1.0_dp)), "gamma quantile invalid shape")

values = dbeta([-inf, 0.0_dp, 0.5_dp, 1.0_dp, inf, nan_value], shape1=2.0_dp, shape2=3.0_dp)
call assert_close(values(1), 0.0_dp, "beta density below support")
call assert_close(values(2), 0.0_dp, "beta density at zero")
call assert_close(values(3), 1.5_dp, "beta density interior")
call assert_close(values(4), 0.0_dp, "beta density at one")
call assert_close(values(5), 0.0_dp, "beta density above support")
call assert_true(ieee_is_nan(values(6)), "beta density propagates NaN")
values = pbeta([-inf, 0.0_dp, 1.0_dp, inf, nan_value], shape1=2.0_dp, shape2=3.0_dp)
call assert_unit_cdf(values, "beta CDF")
values = qbeta([-0.1_dp, 0.0_dp, 1.0_dp, 1.1_dp, nan_value], shape1=2.0_dp, shape2=3.0_dp)
call assert_unit_quantile_boundaries(values, "beta quantile")
call assert_true(ieee_is_nan(dbeta(0.5_dp, shape1=-1.0_dp, shape2=2.0_dp)), "beta density invalid shape")
call assert_true(ieee_is_nan(pbeta(0.5_dp, shape1=1.0_dp, shape2=-1.0_dp)), "beta CDF invalid shape")
call assert_true(ieee_is_nan(qbeta(0.5_dp, shape1=0.0_dp, shape2=1.0_dp)), "beta quantile invalid shape")

values = dt([-inf, 0.0_dp, inf, nan_value], df=3.0_dp)
call assert_real_density(values, 2.0_dp / (sqrt(3.0_dp) * acos(-1.0_dp)), "Student t density")
values = pt([-inf, 0.0_dp, inf, nan_value], df=3.0_dp)
call assert_real_cdf(values, "Student t CDF")
values = qt([-0.1_dp, 0.0_dp, 0.5_dp, 1.0_dp, 1.1_dp, nan_value], df=3.0_dp)
call assert_real_quantile_boundaries(values, "Student t quantile")
call assert_true(ieee_is_nan(dt(0.0_dp, df=-1.0_dp)), "Student t density invalid df")
call assert_true(ieee_is_nan(pt(0.0_dp, df=-1.0_dp)), "Student t CDF invalid df")
call assert_true(ieee_is_nan(qt(0.5_dp, df=-1.0_dp)), "Student t quantile invalid df")

values = df([-inf, 0.0_dp, 1.0_dp, inf, nan_value], df1=3.0_dp, df2=4.0_dp)
call assert_density_support(values, 0.343550031275323_dp, "F density")
values = pf([-inf, 0.0_dp, inf, nan_value], df1=3.0_dp, df2=4.0_dp)
call assert_positive_cdf(values, "F CDF")
values = qf([-0.1_dp, 0.0_dp, 1.0_dp, 1.1_dp, nan_value], df1=3.0_dp, df2=4.0_dp)
call assert_positive_quantile_boundaries(values, "F quantile")
call assert_true(ieee_is_nan(df(1.0_dp, df1=-1.0_dp, df2=2.0_dp)), "F density invalid df")
call assert_true(ieee_is_nan(pf(1.0_dp, df1=1.0_dp, df2=-2.0_dp)), "F CDF invalid df")
call assert_true(ieee_is_nan(qf(0.5_dp, df1=0.0_dp, df2=2.0_dp)), "F quantile invalid df")

contains

subroutine assert_density_support(actual, interior, label)
real(kind=dp), intent(in) :: actual(:), interior
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " below support")
call assert_close(actual(2), 0.0_dp, trim(label) // " at zero")
call assert_close(actual(3), interior, trim(label) // " interior")
call assert_close(actual(4), 0.0_dp, trim(label) // " at infinity")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " propagates NaN")
end subroutine assert_density_support

subroutine assert_real_density(actual, center, label)
real(kind=dp), intent(in) :: actual(:), center
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " at negative infinity")
call assert_close(actual(2), center, trim(label) // " at zero")
call assert_close(actual(3), 0.0_dp, trim(label) // " at positive infinity")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " propagates NaN")
end subroutine assert_real_density

subroutine assert_positive_cdf(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " below support")
call assert_close(actual(2), 0.0_dp, trim(label) // " at zero")
call assert_close(actual(3), 1.0_dp, trim(label) // " at infinity")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " propagates NaN")
end subroutine assert_positive_cdf

subroutine assert_unit_cdf(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " below support")
call assert_close(actual(2), 0.0_dp, trim(label) // " at zero")
call assert_close(actual(3), 1.0_dp, trim(label) // " at one")
call assert_close(actual(4), 1.0_dp, trim(label) // " above support")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " propagates NaN")
end subroutine assert_unit_cdf

subroutine assert_real_cdf(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_close(actual(1), 0.0_dp, trim(label) // " at negative infinity")
call assert_close(actual(2), 0.5_dp, trim(label) // " at zero")
call assert_close(actual(3), 1.0_dp, trim(label) // " at positive infinity")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " propagates NaN")
end subroutine assert_real_cdf

subroutine assert_positive_quantile_boundaries(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(ieee_is_nan(actual(1)), trim(label) // " below probability range")
call assert_close(actual(2), 0.0_dp, trim(label) // " lower endpoint")
call assert_positive_infinity(actual(3), trim(label) // " upper endpoint")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " above probability range")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " propagates NaN")
end subroutine assert_positive_quantile_boundaries

subroutine assert_unit_quantile_boundaries(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(ieee_is_nan(actual(1)), trim(label) // " below probability range")
call assert_close(actual(2), 0.0_dp, trim(label) // " lower endpoint")
call assert_close(actual(3), 1.0_dp, trim(label) // " upper endpoint")
call assert_true(ieee_is_nan(actual(4)), trim(label) // " above probability range")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " propagates NaN")
end subroutine assert_unit_quantile_boundaries

subroutine assert_real_quantile_boundaries(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label
call assert_true(ieee_is_nan(actual(1)), trim(label) // " below probability range")
call assert_negative_infinity(actual(2), trim(label) // " lower endpoint")
call assert_close(actual(3), 0.0_dp, trim(label) // " median")
call assert_positive_infinity(actual(4), trim(label) // " upper endpoint")
call assert_true(ieee_is_nan(actual(5)), trim(label) // " above probability range")
call assert_true(ieee_is_nan(actual(6)), trim(label) // " propagates NaN")
end subroutine assert_real_quantile_boundaries

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
call assert_true(abs(actual - expected) <= tol, label)
end subroutine assert_close

subroutine assert_positive_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label
call assert_true(.not. ieee_is_finite(actual) .and. actual > 0.0_dp, label)
end subroutine assert_positive_infinity

subroutine assert_negative_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label
call assert_true(.not. ieee_is_finite(actual) .and. actual < 0.0_dp, label)
end subroutine assert_negative_infinity

subroutine assert_true(condition, label)
logical, intent(in) :: condition
character(len=*), intent(in) :: label
if (.not. condition) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_true

end program test_classical_distribution_boundaries
