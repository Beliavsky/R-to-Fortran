# Monte Carlo power calculation for a two-sample t test, using a
# pre-generated simulation matrix so R and Fortran use the same data.

dat <- read.table(
  "monte_carlo_power_t_test_data.txt",
  header = TRUE
)

nsim <- nrow(dat)
n1 <- 30
n2 <- 30
alpha <- 0.05
idx_x <- 1:n1
idx_y <- (n1 + 1):(n1 + n2)

reject <- logical(nsim)

for (i in 1:nsim) {
  x <- dat[i, idx_x]
  y <- dat[i, idx_y]
  reject[i] <- t.test(x, y, var.equal = TRUE)$p.value < alpha
}

cat("Estimated power =", mean(reject), "\n")
cat("Monte Carlo standard error =", sqrt(mean(reject) * (1 - mean(reject)) / nsim), "\n")
