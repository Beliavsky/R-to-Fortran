program hypothesis_tests
use r_mod, only: chisq_test, chisq_test_result_t, cor_test, &
   cor_test_result_t, dp, print_chisq_test, print_cor_test, &
   print_t_test, t_test, t_test_result_t
implicit none

integer :: counts(2, 2)
type(t_test_result_t) :: mean_test
type(chisq_test_result_t) :: independence_test
type(cor_test_result_t) :: correlation_test

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: Hypothesis tests"
write(*, '(a, /)') repeat("=", 72)

mean_test = t_test([12.1_dp, 11.8_dp, 12.4_dp, 12.0_dp, 12.3_dp], mu=12.0_dp)
write(*, '(a)') "One-sample t test:"
call print_t_test(mean_test)
write(*, '(a, f8.5)') "p-value from result field: ", mean_test%p_value

counts(1, :) = [18, 12]
counts(2, :) = [9, 21]
independence_test = chisq_test(counts)
write(*, '(/, a)') "Chi-square test of independence:"
call print_chisq_test(independence_test)

correlation_test = cor_test( &
   [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], &
   [1.2_dp, 1.9_dp, 3.4_dp, 3.8_dp, 5.1_dp])
write(*, '(/, a)') "Pearson correlation test:"
call print_cor_test(correlation_test)
write(*, '(a, f8.5)') "Estimated correlation: ", correlation_test%estimate
end program hypothesis_tests
