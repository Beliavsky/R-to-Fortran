program test_nonparametric_statistics
use r_mod, only: cor_test, cor_test_result_t, dp, fisher_test, fisher_test_result_t, &
   kruskal_test, kruskal_test_result_t, ks_test, ks_test_result_t, wilcox_test, wilcox_test_result_t
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
type(cor_test_result_t) :: corfit
type(fisher_test_result_t) :: fisherfit
type(wilcox_test_result_t) :: wilcoxfit
type(kruskal_test_result_t) :: kruskalfit
type(ks_test_result_t) :: ksfit
integer :: table(2, 2)

corfit = cor_test([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   [1.0_dp, 2.0_dp, 4.0_dp, 3.0_dp])
call assert_close(corfit%estimate, 0.8_dp, "Pearson correlation estimate")
call assert_close(corfit%statistic, 4.0_dp * sqrt(2.0_dp) / 3.0_dp, &
   "Pearson correlation statistic")
if (corfit%parameter /= 2 .or. corfit%method /= 1) error stop "Pearson correlation metadata failed"
call assert_probability(corfit%p_value, "Pearson correlation p value")

corfit = cor_test([1, 2, 3, 4], [10, 30, 20, 40], method="spearman")
call assert_close(corfit%estimate, 0.8_dp, "Spearman correlation estimate")
call assert_close(corfit%statistic, 4.0_dp * sqrt(2.0_dp) / 3.0_dp, &
   "Spearman correlation statistic")
if (corfit%parameter /= 2 .or. corfit%method /= 2) error stop "Spearman correlation metadata failed"

table(1, :) = [1, 1]
table(2, :) = [1, 1]
fisherfit = fisher_test(table)
call assert_close(fisherfit%p_value, 1.0_dp, "Fisher exact p value")
call assert_close(fisherfit%estimate, 1.0_dp, "Fisher odds-ratio estimate")
if (fisherfit%method /= 1) error stop "Fisher method failed"

wilcoxfit = wilcox_test([1.0_dp, 2.0_dp], [3.0_dp, 4.0_dp])
call assert_close(wilcoxfit%statistic, 0.0_dp, "Wilcoxon rank-sum statistic")
if (wilcoxfit%method /= 1) error stop "Wilcoxon rank-sum method failed"
call assert_probability(wilcoxfit%p_value, "Wilcoxon rank-sum p value")

wilcoxfit = wilcox_test([3.0_dp, 5.0_dp, 1.0_dp], [1.0_dp, 2.0_dp, 2.0_dp], paired=.true.)
call assert_close(wilcoxfit%statistic, 5.0_dp, "Wilcoxon signed-rank statistic")
if (wilcoxfit%method /= 2) error stop "Wilcoxon signed-rank method failed"
call assert_probability(wilcoxfit%p_value, "Wilcoxon signed-rank p value")

kruskalfit = kruskal_test([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [1, 1, 2, 2])
call assert_close(kruskalfit%statistic, 2.4_dp, "Kruskal-Wallis statistic")
if (kruskalfit%parameter /= 1) error stop "Kruskal-Wallis degrees of freedom failed"
call assert_probability(kruskalfit%p_value, "Kruskal-Wallis p value")

ksfit = ks_test([0.0_dp])
call assert_close(ksfit%statistic, 0.5_dp, "Kolmogorov-Smirnov statistic")
if (ksfit%n /= 1) error stop "Kolmogorov-Smirnov sample size failed"
call assert_probability(ksfit%p_value, "Kolmogorov-Smirnov p value")

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
end program test_nonparametric_statistics
