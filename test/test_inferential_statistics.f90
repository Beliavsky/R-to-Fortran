program test_inferential_statistics
use r_mod, only: chisq_test, chisq_test_result_t, dp, prop_test, prop_test_result_t, &
   t_test, t_test_p_value, t_test_result_t
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
type(t_test_result_t) :: tfit
type(chisq_test_result_t) :: chifit
type(prop_test_result_t) :: propfit
integer :: table(2, 2)

tfit = t_test([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], mu=2.0_dp)
call assert_close(tfit%estimate, 3.0_dp, "one-sample t estimate")
call assert_close(tfit%stderr, sqrt(0.5_dp), "one-sample t standard error")
call assert_close(tfit%statistic, sqrt(2.0_dp), "one-sample t statistic")
call assert_close(tfit%parameter, 4.0_dp, "one-sample t degrees of freedom")
call assert_close(tfit%null_value, 2.0_dp, "one-sample t null value")
if (tfit%method /= 1) error stop "one-sample t method failed"
call assert_close(t_test_p_value([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], mu=2.0_dp), &
   tfit%p_value, "one-sample t p-value helper")

tfit = t_test([1.0_dp, 2.0_dp, 3.0_dp], [2.0_dp, 3.0_dp, 4.0_dp], var_equal=.true.)
call assert_close(tfit%estimate, 2.0_dp, "two-sample first estimate")
call assert_close(tfit%estimate2, 3.0_dp, "two-sample second estimate")
call assert_close(tfit%stderr, sqrt(2.0_dp / 3.0_dp), "pooled t standard error")
call assert_close(tfit%statistic, -sqrt(1.5_dp), "pooled t statistic")
call assert_close(tfit%parameter, 4.0_dp, "pooled t degrees of freedom")
if (tfit%method /= 3) error stop "pooled t method failed"

tfit = t_test([1.0_dp, 2.0_dp, 3.0_dp], [2.0_dp, 4.0_dp, 6.0_dp], paired=.true.)
call assert_close(tfit%estimate, -2.0_dp, "paired t difference estimate")
call assert_close(tfit%stderr, 1.0_dp / sqrt(3.0_dp), "paired t standard error")
call assert_close(tfit%statistic, -2.0_dp * sqrt(3.0_dp), "paired t statistic")
call assert_close(tfit%parameter, 2.0_dp, "paired t degrees of freedom")
if (tfit%method /= 4) error stop "paired t method failed"

chifit = chisq_test([10, 20, 30], p=[0.2_dp, 0.3_dp, 0.5_dp])
call assert_close(chifit%statistic, 5.0_dp / 9.0_dp, "chi-square goodness-of-fit statistic")
if (chifit%parameter /= 2) error stop "chi-square goodness-of-fit degrees of freedom failed"
if (chifit%method /= 1) error stop "chi-square goodness-of-fit method failed"
call assert_probability(chifit%p_value, "chi-square goodness-of-fit p value")

table(1, :) = [10, 20]
table(2, :) = [20, 10]
chifit = chisq_test(table)
call assert_close(chifit%statistic, 20.0_dp / 3.0_dp, "chi-square independence statistic")
if (chifit%parameter /= 1) error stop "chi-square independence degrees of freedom failed"
if (chifit%method /= 2) error stop "chi-square independence method failed"

propfit = prop_test(60, 100, p=0.5_dp, correct=.false.)
call assert_close(propfit%estimate, 0.6_dp, "one-proportion estimate")
call assert_close(propfit%null_value, 0.5_dp, "one-proportion null value")
call assert_close(propfit%statistic, 4.0_dp, "one-proportion statistic")
if (propfit%parameter /= 1 .or. propfit%method /= 1) error stop "one-proportion metadata failed"

propfit = prop_test(60, 100, p=0.5_dp, correct=.true.)
call assert_close(propfit%statistic, 3.61_dp, "corrected one-proportion statistic")

propfit = prop_test([30, 60], [50, 100], correct=.false.)
call assert_close(propfit%estimate, 0.6_dp, "two-proportion first estimate")
call assert_close(propfit%estimate2, 0.6_dp, "two-proportion second estimate")
call assert_close(propfit%statistic, 0.0_dp, "equal two-proportion statistic")
if (propfit%parameter /= 1 .or. propfit%method /= 2) error stop "two-proportion metadata failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_probability(actual, label)
real(kind=dp), intent(in) :: actual
character(len=*), intent(in) :: label

if (actual < 0.0_dp .or. actual > 1.0_dp) then
   write(*, '(a, 1x, es24.16)') trim(label) // " failed:", actual
   error stop 1
end if
end subroutine assert_probability
end program test_inferential_statistics
