# Empirical CDF and Kolmogorov-Smirnov test in base R

set.seed(123)
x <- rnorm(100)

ec <- ecdf(x)

cat("Empirical CDF at -1, 0, 1:\n")
print(ec(c(-1, 0, 1)))

cat("\nKS test against standard normal:\n")
print(ks.test(x, "pnorm", mean = 0, sd = 1))

cat("\nKS test against fitted normal, shown as an example only.\n")
cat("The usual KS p-value is not exact after estimating parameters.\n")
print(ks.test(x, "pnorm", mean = mean(x), sd = sd(x)))
