# Generate QR decomposition input data and write it as a headed text file.

set.seed(123)

n <- 8

x1 <- rnorm(n)
x2 <- rnorm(n)

X <- cbind(
  intercept = 1,
  x1 = x1,
  x2 = x2
)

beta_true <- c(2, -1, 0.5)

y <- as.vector(X %*% beta_true + rnorm(n, sd = 0.2))

dat <- matrix(c(x1, x2, y), nrow = n, ncol = 3)
colnames(dat) <- c("x1", "x2", "y")

write.table(
  dat,
  file = "xqrx_input.txt",
  row.names = FALSE,
  quote = FALSE
)

cat("wrote xqrx_input.txt\n")
