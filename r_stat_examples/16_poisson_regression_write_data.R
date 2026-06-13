# Generate Poisson regression data and write it as a headed text file.

set.seed(123)
n <- 300
x <- rnorm(n)
exposure <- runif(n, 0.5, 2.0)
lambda <- exposure * exp(0.3 + 0.7 * x)
y <- rpois(n, lambda = lambda)

dat <- matrix(c(y, x, exposure), nrow = n, ncol = 3)
colnames(dat) <- c("y", "x", "exposure")

write.table(
  dat,
  file = "poisson_regression_data.txt",
  row.names = FALSE,
  quote = FALSE
)

cat("wrote poisson_regression_data.txt\n")
