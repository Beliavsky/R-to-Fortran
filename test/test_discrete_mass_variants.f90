program test_discrete_mass_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
use r_mod, only: dbinom, dgeom, dhyper, dnbinom, dpois, dp, dsignrank, dwilcox
implicit none

real(kind=dp) :: values(3)

call assert_zero(dbinom(1.5_dp, nsize=4, prob=0.5_dp), "noninteger binomial mass")
call assert_zero(dpois(2.5_dp, lambda=2.0_dp), "noninteger Poisson mass")
call assert_zero(dgeom(2.5_dp, prob=0.25_dp), "noninteger geometric mass")
call assert_zero(dnbinom(2.5_dp, nsize=3, prob=0.4_dp), &
   "noninteger negative-binomial mass")
call assert_zero(dhyper(1.5_dp, m=5, n=7, k=4), "noninteger hypergeometric mass")
call assert_zero(dwilcox(1.5_dp, m=2, n=3), "noninteger Wilcoxon mass")
call assert_zero(dsignrank(1.5_dp, n=3), "noninteger signed-rank mass")

call assert_negative_infinity(dbinom(-1.0_dp, nsize=4, prob=0.5_dp, log_=.true.), &
   "binomial log mass outside support")
call assert_negative_infinity(dpois(-1.0_dp, lambda=2.0_dp, log_=.true.), &
   "Poisson log mass outside support")
call assert_negative_infinity(dgeom(-1.0_dp, prob=0.25_dp, log_=.true.), &
   "geometric log mass outside support")
call assert_negative_infinity(dnbinom(-1.0_dp, nsize=3, prob=0.4_dp, log_=.true.), &
   "negative-binomial log mass outside support")
call assert_negative_infinity(dhyper(-1.0_dp, m=5, n=7, k=4, log_=.true.), &
   "hypergeometric log mass outside support")
call assert_negative_infinity(dwilcox(-1.0_dp, m=2, n=3, log_=.true.), &
   "Wilcoxon log mass outside support")
call assert_negative_infinity(dsignrank(-1.0_dp, n=3, log_=.true.), &
   "signed-rank log mass outside support")

values = dpois([0.0_dp, 1.5_dp, 2.0_dp], lambda=2.0_dp)
call assert_zero(values(2), "vector noninteger Poisson mass")
call assert_positive(values(1), "vector Poisson zero mass")
call assert_positive(values(3), "vector Poisson integer mass")

contains

subroutine assert_zero(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (.not. ieee_is_finite(actual) .or. actual /= 0.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_zero

subroutine assert_positive(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (.not. ieee_is_finite(actual) .or. actual <= 0.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_positive

subroutine assert_negative_infinity(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (ieee_is_finite(actual) .or. actual >= 0.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_negative_infinity

end program test_discrete_mass_variants
