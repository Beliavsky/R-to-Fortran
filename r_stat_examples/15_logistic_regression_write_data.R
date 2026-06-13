# Generate logistic regression data and write it as a headed text file.

set.seed(123)
n <- 300
x1 <- rnorm(n)
x2 <- rnorm(n)
eta <- -0.5 + 1.0 * x1 - 0.8 * x2
p <- 1 / (1 + exp(-eta))
y <- rbinom(n, size = 1, prob = p)

dat <- matrix(c(y, x1, x2), nrow = n, ncol = 3)
colnames(dat) <- c("y", "x1", "x2")

write.table(
  dat,
  file = "logistic_regression_data.txt",
  row.names = FALSE,
  quote = FALSE
)

cat("wrote logistic_regression_data.txt\n")
