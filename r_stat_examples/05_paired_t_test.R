# Paired t test in base R

set.seed(123)
before <- rnorm(30, mean = 100, sd = 12)
after <- before + rnorm(30, mean = -4, sd = 6)

fit <- t.test(before, after, paired = TRUE)

print(fit)

cat("\nMean before =", mean(before), "\n")
cat("Mean after  =", mean(after), "\n")
cat("Mean change =", mean(after - before), "\n")
