program test_statistical_result_printing
use r_mod, only: chisq_test, chisq_test_result_t, cor_test, &
   cor_test_result_t, dp, fisher_test, fisher_test_result_t, kruskal_test, &
   kruskal_test_result_t, ks_test, ks_test_result_t, print_chisq_test, &
   print_cor_test, print_factanal, print_fisher_test, print_kruskal_test, &
   print_ks_test, print_prop_test, print_t_test, print_wilcox_test, &
   prop_test, prop_test_result_t, t_test, t_test_result_t, wilcox_test, &
   wilcox_test_result_t
implicit none

type(chisq_test_result_t) :: chi
type(cor_test_result_t) :: correlation
type(fisher_test_result_t) :: fisher
type(kruskal_test_result_t) :: kruskal
type(ks_test_result_t) :: kolmogorov
type(prop_test_result_t) :: proportion
type(t_test_result_t) :: student
type(wilcox_test_result_t) :: wilcoxon
integer :: counts(2, 2)
real(kind=dp) :: observations(5, 2)

student = t_test([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], mu=2.0_dp)
call print_t_test(student)
student = t_test([1.0_dp, 2.0_dp, 3.0_dp], [2.0_dp, 3.0_dp, 5.0_dp])
call print_t_test(student)
student = t_test([1.0_dp, 2.0_dp, 3.0_dp], [2.0_dp, 3.0_dp, 5.0_dp], &
   var_equal=.true.)
call print_t_test(student)
student = t_test([1.0_dp, 2.0_dp, 3.0_dp], [2.0_dp, 3.0_dp, 5.0_dp], &
   paired=.true.)
call print_t_test(student)

chi = chisq_test([10, 20, 30])
call print_chisq_test(chi)
counts(1, :) = [10, 20]
counts(2, :) = [20, 10]
chi = chisq_test(counts)
call print_chisq_test(chi)

proportion = prop_test(60, 100, p=0.5_dp)
call print_prop_test(proportion)
proportion = prop_test([30, 50], [50, 100])
call print_prop_test(proportion)

correlation = cor_test([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   [1.0_dp, 2.0_dp, 4.0_dp, 3.0_dp])
call print_cor_test(correlation)
correlation = cor_test([1, 2, 3, 4], [10, 30, 20, 40], method="spearman")
call print_cor_test(correlation)

counts = 1
fisher = fisher_test(counts)
call print_fisher_test(fisher)

wilcoxon = wilcox_test([1.0_dp, 2.0_dp], [3.0_dp, 4.0_dp])
call print_wilcox_test(wilcoxon)
wilcoxon = wilcox_test([3.0_dp, 5.0_dp, 1.0_dp], &
   [1.0_dp, 2.0_dp, 2.0_dp], paired=.true.)
call print_wilcox_test(wilcoxon)

kruskal = kruskal_test([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [1, 1, 2, 2])
call print_kruskal_test(kruskal)
kolmogorov = ks_test([0.0_dp, 0.5_dp, 1.0_dp])
call print_ks_test(kolmogorov)

observations(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
observations(:, 2) = [1.2_dp, 1.8_dp, 3.1_dp, 3.9_dp, 5.2_dp]
call print_factanal(observations, 1)
end program test_statistical_result_printing
