program distributions
use r_mod, only: dnorm, dp, normal_cdf, print_real_vector, qnorm, &
   rnorm_vec, sd, set_seed_int
implicit none

real(kind=dp) :: probabilities(3), quantiles(3), sample(8)

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: Probability distributions and random numbers"
write(*, '(a, /)') repeat("=", 72)

probabilities = [0.025_dp, 0.5_dp, 0.975_dp]
quantiles = qnorm(probabilities)

write(*, '(a)') "Standard normal quantiles:"
call print_real_vector(quantiles)
write(*, '(a)') "Densities at those quantiles:"
call print_real_vector(dnorm(quantiles))
write(*, '(a)') "CDF values recovered from the quantiles:"
call print_real_vector(normal_cdf(quantiles))

call set_seed_int(2026)
sample = 10.0_dp + 2.0_dp * rnorm_vec(size(sample))
write(*, '(a)') "Seeded normal sample:"
call print_real_vector(sample)
write(*, '(a, f8.4)') "Sample mean: ", sum(sample) / real(size(sample), kind=dp)
write(*, '(a, f8.4)') "Sample standard deviation: ", sd(sample)
end program distributions
