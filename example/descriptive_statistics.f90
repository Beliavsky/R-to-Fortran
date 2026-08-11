program descriptive_statistics
use r_mod, only: dp, dnorm, print_real_vector, sd
implicit none

real(kind=dp) :: observations(5)
real(kind=dp) :: density(5)

observations = [1.2_dp, 2.4_dp, 2.8_dp, 3.1_dp, 4.5_dp]
density = dnorm(observations)

write(*, '(a, f8.4)') "Sample standard deviation: ", sd(observations)
write(*, '(a)') "Standard normal densities:"
call print_real_vector(density)
end program descriptive_statistics
