# Generate Monte Carlo simulation draws for two-sample t-test power and
# write to a shared text file for deterministic Fortran replication.

set.seed(123)

nsim <- 5000
n1 <- 30
n2 <- 30
mu1 <- 0
mu2 <- 0.5
sigma <- 1

x <- matrix(rnorm(nsim * n1, mean = mu1, sd = sigma), nrow = nsim, ncol = n1)
y <- matrix(rnorm(nsim * n2, mean = mu2, sd = sigma), nrow = nsim, ncol = n2)
dat <- matrix(NA_real_, nrow = nsim, ncol = n1 + n2)
dat[, 1:n1] = x
dat[, (n1 + 1):(n1 + n2)] = y

colnames(dat) <- c(sprintf("x%d", 1:n1), sprintf("y%d", 1:n2))

write.table(
  dat,
  file = "monte_carlo_power_t_test_data.txt",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)

cat("wrote monte_carlo_power_t_test_data.txt\n")
