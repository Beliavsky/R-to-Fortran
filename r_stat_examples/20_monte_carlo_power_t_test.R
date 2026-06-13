# Monte Carlo power calculation for a two-sample t test in base R

set.seed(123)

nsim <- 5000
n1 <- 30
n2 <- 30
mu1 <- 0
mu2 <- 0.5
sigma <- 1
alpha <- 0.05

reject <- logical(nsim)

for (i in 1:nsim) {
  x <- rnorm(n1, mu1, sigma)
  y <- rnorm(n2, mu2, sigma)
  reject[i] <- t.test(x, y, var.equal = TRUE)$p.value < alpha
}

cat("Estimated power =", mean(reject), "\n")
cat("Monte Carlo standard error =", sqrt(mean(reject) * (1 - mean(reject)) / nsim), "\n")
