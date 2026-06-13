# Ordinary least squares by matrix formulas in base R

set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 1 + 2 * x1 - 3 * x2 + rnorm(n)

X <- cbind(1, x1, x2)
colnames(X) <- c("(Intercept)", "x1", "x2")

beta_hat <- solve(t(X) %*% X, t(X) %*% y)
resid <- as.vector(y - X %*% beta_hat)
sigma2_hat <- sum(resid^2) / (n - ncol(X))
vcov_beta <- sigma2_hat * solve(t(X) %*% X)
se_beta <- sqrt(diag(vcov_beta))
t_stat <- beta_hat / se_beta

cat("Manual OLS estimates:\n")
print(beta_hat)

cat("\nStandard errors:\n")
print(se_beta)

cat("\nt statistics:\n")
print(t_stat)

cat("\nCompare with lm():\n")
print(coef(summary(lm(y ~ x1 + x2))))
