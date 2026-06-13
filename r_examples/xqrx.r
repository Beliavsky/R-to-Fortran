# Demonstrate QR decomposition functions in base R.
# No plotting is used.

cat("QR decomposition functions in base R\n\n")

set.seed(123)

# Create an overdetermined linear regression problem:
# y = X beta + error

n <- 8
p <- 3

x1 <- rnorm(n)
x2 <- rnorm(n)

X <- cbind(
  intercept = 1,
  x1 = x1,
  x2 = x2
)

beta_true <- c(2, -1, 0.5)

y <- as.vector(X %*% beta_true + rnorm(n, sd = 0.2))

cat("Design matrix X:\n")
print(X)

cat("\nResponse y:\n")
print(y)

cat("\n")


cat("1. qr(x): QR decomposition\n\n")

qrx <- qr(X)

cat("qrx:\n")
print(qrx)

cat("\nqrx$rank:\n")
print(qrx$rank)

cat("\nqrx$pivot:\n")
print(qrx$pivot)
