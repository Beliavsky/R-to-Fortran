# QR decomposition example using data read from a file.
# The input file is plain whitespace-delimited text so the translated
# Fortran program can read the same data.

cat("QR decomposition from read.table() data\n\n")

path <- "xqr_read_data.txt"
dat <- read.table(path, header = TRUE)

x1 <- dat[, 1]
x2 <- dat[, 2]
y <- dat[, 3]

X <- cbind(
  intercept = 1,
  x1 = x1,
  x2 = x2
)

cat("Input design matrix X:\n")
print(X)

cat("\nInput response y:\n")
print(y)

qrx <- qr(X)

cat("\nqr(X)$rank:\n")
print(qrx$rank)

cat("\nqr(X)$pivot:\n")
print(qrx$pivot)

coef_qr <- qr.coef(qrx, y)
fitted_qr <- qr.fitted(qrx, y)
resid_qr <- qr.resid(qrx, y)

cat("\nqr.coef(qrx, y):\n")
print(coef_qr)

cat("\nqr.fitted(qrx, y):\n")
print(fitted_qr)

cat("\nqr.resid(qrx, y):\n")
print(resid_qr)

cat("\nSum of squared residuals:\n")
print(sum(resid_qr^2))

cat("\nCompare with lm.fit(X, y)$coefficients:\n")
print(lm.fit(X, y)$coefficients)
