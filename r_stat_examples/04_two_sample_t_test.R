# Two-sample t tests in base R

set.seed(123)
x <- rnorm(35, mean = 10.0, sd = 2.0)
y <- rnorm(40, mean = 11.0, sd = 2.5)

cat("Welch two-sample t test:\n")
print(t.test(x, y))

cat("\nEqual variance two-sample t test:\n")
print(t.test(x, y, var.equal = TRUE))
