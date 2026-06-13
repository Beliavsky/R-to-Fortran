# Multivariate normal simulation in base R
# Uses only base R by forming a Cholesky factor.

set.seed(123)

rmvnorm_chol <- function(n, mu, Sigma) {
  p <- length(mu)
  z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  L <- chol(Sigma)
  sweep(z %*% L, 2, mu, "+")
}

mu <- c(1, 2, 3)
Sigma <- matrix(c(1.0, 0.5, 0.2,
                  0.5, 2.0, 0.4,
                  0.2, 0.4, 1.5),
                nrow = 3, byrow = TRUE)

x <- rmvnorm_chol(1000, mu, Sigma)

cat("Sample means:\n")
print(colMeans(x))

cat("\nSample covariance matrix:\n")
print(cov(x))
