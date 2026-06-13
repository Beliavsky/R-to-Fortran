set.seed(123)
n = 10^7
x = rnorm(n)
cat("\n", n, mean(x), sd(x), min(x), max(x), x[1], x[n])
