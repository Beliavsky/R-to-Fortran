# Generate multiple linear regression data and write it as a headed text file.

set.seed(123)
n <- 150
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- sample(c("A", "B"), n, replace = TRUE)
x3B <- ifelse(x3 == "B", 1.0, 0.0)
y <- 1 + 2 * x1 - 1.5 * x2 + 3 * x3B + rnorm(n)

dat <- matrix(c(y, x1, x2, x3B), nrow = n, ncol = 4)
colnames(dat) <- c("y", "x1", "x2", "x3B")

write.table(
  dat,
  file = "multiple_regression_data.txt",
  row.names = FALSE,
  quote = FALSE
)

cat("wrote multiple_regression_data.txt\n")
