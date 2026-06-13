# One-sample t test in base R

set.seed(123)
x <- rnorm(40, mean = 5.4, sd = 1.2)

# Test H0: mean = 5
fit <- t.test(x, mu = 5)

print(fit)

cat("\nSample mean =", mean(x), "\n")
cat("Sample sd =", sd(x), "\n")
