program test_rank_distributions
use r_mod, only: dp, dsignrank, dwilcox, psignrank, pwilcox, qsignrank, qwilcox
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: values(7)

values(1:5) = dwilcox([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], m=2, n=2)
call assert_vector_close(values(1:5), [1.0_dp, 1.0_dp, 2.0_dp, 1.0_dp, 1.0_dp] / 6.0_dp, &
   "Wilcoxon probability table")
call assert_close(sum(values(1:5)), 1.0_dp, "Wilcoxon normalized mass")
call assert_close(dwilcox(2.0_dp, m=2, n=2), 1.0_dp / 3.0_dp, "Wilcoxon center mass")
call assert_close(dwilcox(0.0_dp, m=2, n=2, log_=.true.), -log(6.0_dp), &
   "Wilcoxon log mass")
call assert_close(dwilcox(-1.0_dp, m=2, n=2), 0.0_dp, "Wilcoxon mass below support")
call assert_close(pwilcox(-1.0_dp, m=2, n=2), 0.0_dp, "Wilcoxon cdf below support")
call assert_close(pwilcox(1.0_dp, m=2, n=2), 1.0_dp / 3.0_dp, "Wilcoxon cdf")
call assert_vector_close(qwilcox([0.1_dp, 0.5_dp, 0.9_dp], m=2, n=2), &
   [0.0_dp, 2.0_dp, 4.0_dp], "Wilcoxon quantiles")

values = dsignrank([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], n=3)
call assert_vector_close(values, [1.0_dp, 1.0_dp, 1.0_dp, 2.0_dp, 1.0_dp, 1.0_dp, 1.0_dp] / 8.0_dp, &
   "signed-rank probability table")
call assert_close(sum(values), 1.0_dp, "signed-rank normalized mass")
call assert_close(dsignrank(3.0_dp, n=3), 0.25_dp, "signed-rank center mass")
call assert_close(dsignrank(0.0_dp, n=3, log_=.true.), -log(8.0_dp), &
   "signed-rank log mass")
call assert_close(dsignrank(-1.0_dp, n=3), 0.0_dp, "signed-rank mass below support")
call assert_close(psignrank(-1.0_dp, n=3), 0.0_dp, "signed-rank cdf below support")
call assert_close(psignrank(2.0_dp, n=3), 3.0_dp / 8.0_dp, "signed-rank cdf")
call assert_vector_close(qsignrank([0.1_dp, 0.5_dp, 0.9_dp], n=3), &
   [0.0_dp, 3.0_dp, 6.0_dp], "signed-rank quantiles")

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
end program test_rank_distributions
