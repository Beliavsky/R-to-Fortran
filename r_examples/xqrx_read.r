# Demonstrate QR decomposition functions in base R from a headed text file.
# Run xqrx_write.r first to create xqrx_input.txt.

cat("QR decomposition functions in base R\n\n")

dat <- read.table(
  "xqrx_input.txt",
  header = TRUE
)

x1 <- dat[, 1]
x2 <- dat[, 2]
y <- dat[, 3]

X <- cbind(
  intercept = 1,
  x1 = x1,
  x2 = x2
)

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
