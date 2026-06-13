# Time series decomposition in base R

set.seed(123)
n <- 120
trend <- seq(10, 20, length.out = n)
season <- rep(c(-2, -1, 1, 2), length.out = n)
noise <- rnorm(n, sd = 0.8)

x <- ts(trend + season + noise, frequency = 4, start = c(2000, 1))

fit <- decompose(x, type = "additive")

cat("First few observed values:\n")
print(head(x))

cat("\nFirst few trend estimates:\n")
print(head(fit$trend, 12))

cat("\nSeasonal component:\n")
print(fit$figure)
