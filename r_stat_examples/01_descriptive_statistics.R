# Descriptive statistics in base R

set.seed(123)
x <- rnorm(100, mean = 10, sd = 2)

cat("n =", length(x), "\n")
cat("mean =", mean(x), "\n")
cat("median =", median(x), "\n")
cat("sd =", sd(x), "\n")
cat("variance =", var(x), "\n")
cat("min =", min(x), "\n")
cat("max =", max(x), "\n")
cat("quantiles:\n")
print(quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1)))

# A simple custom skewness and excess kurtosis
skewness <- mean((x - mean(x))^3) / sd(x)^3
excess_kurtosis <- mean((x - mean(x))^4) / sd(x)^4 - 3

cat("skewness =", skewness, "\n")
cat("excess kurtosis =", excess_kurtosis, "\n")
