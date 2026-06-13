# Covariance matrix and Mahalanobis distance in base R

set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- 0.6 * x1 + rnorm(n, sd = 0.8)
x3 <- rnorm(n)

x <- cbind(x1, x2, x3)

cat("Covariance matrix:\n")
print(cov(x))

cat("\nCorrelation matrix:\n")
print(cor(x))

center <- colMeans(x)
S <- cov(x)
d2 <- mahalanobis(x, center = center, cov = S)

cat("\nLargest squared Mahalanobis distances:\n")
print(head(sort(d2, decreasing = TRUE), 10))

cutoff <- qchisq(0.975, df = ncol(x))
cat("\nNumber exceeding chi-square 97.5% cutoff =", sum(d2 > cutoff), "\n")
