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

cat("\nThe qr object stores the QR decomposition in compact form.\n\n")


cat("Extract Q and R using qr.Q() and qr.R()\n\n")

Q <- qr.Q(qrx)
R <- qr.R(qrx)

cat("Q:\n")
print(Q)

cat("\nR:\n")
print(R)

cat("\nCheck X approximately equals Q %*% R:\n")
print(Q %*% R)

cat("\nMaximum absolute reconstruction error:\n")
print(max(abs(X - Q %*% R)))

cat("\n")


cat("2. qr.coef(qr, y): least-squares coefficients\n\n")

coef_qr <- qr.coef(qrx, y)

cat("qr.coef(qrx, y):\n")
print(coef_qr)

cat("\nCompare with lm.fit(X, y)$coefficients:\n")
print(lm.fit(X, y)$coefficients)

cat("\nCompare with lm(y ~ x1 + x2):\n")
print(coef(lm(y ~ x1 + x2)))

cat("\n")


cat("3. qr.fitted(qr, y): fitted values\n\n")

fitted_qr <- qr.fitted(qrx, y)

cat("qr.fitted(qrx, y):\n")
print(fitted_qr)

cat("\nCompare with X %*% qr.coef(qrx, y):\n")
print(as.vector(X %*% coef_qr))

cat("\n")


cat("4. qr.resid(qr, y): residuals\n\n")

resid_qr <- qr.resid(qrx, y)

cat("qr.resid(qrx, y):\n")
print(resid_qr)

cat("\nCompare with y - qr.fitted(qrx, y):\n")
print(y - fitted_qr)

cat("\nSum of squared residuals:\n")
print(sum(resid_qr^2))

cat("\n")


cat("5. qr.qty(qr, y): compute t(Q) %*% y\n\n")

qty <- qr.qty(qrx, y)

cat("qr.qty(qrx, y):\n")
print(qty)

cat("\nCompare with t(Q) %*% y:\n")
print(as.vector(t(Q) %*% y))

cat("\n")

cat("The first qr$rank elements are related to the fitted part.\n")
cat("The remaining elements are related to the residual part.\n\n")


cat("6. qr.qy(qr, y): compute Q %*% y\n\n")

# qr.qy() expects a vector whose length is compatible with Q.
# Since Q is n by n in the full implicit decomposition, use a length-n vector.

v <- seq_len(n)

cat("v:\n")
print(v)

cat("\nqr.qy(qrx, v):\n")
print(qr.qy(qrx, v))

cat("\nCompare with qr.Q(qrx, complete = TRUE) %*% v:\n")

Q_full <- qr.Q(qrx, complete = TRUE)
print(as.vector(Q_full %*% v))

cat("\n")


cat("7. qr.solve(a, b): solve linear systems or least-squares problems\n\n")

cat("Overdetermined least-squares problem: solve X beta approximately equals y\n")

coef_solve <- qr.solve(X, y)

cat("qr.solve(X, y):\n")
print(coef_solve)

cat("\nCompare with qr.coef(qrx, y):\n")
print(coef_qr)

cat("\n")


cat("Square linear system example\n\n")

A <- matrix(c(
  2, 1, 1,
  1, 3, 2,
  1, 0, 0
), nrow = 3, byrow = TRUE)

b <- c(4, 5, 6)

cat("A:\n")
print(A)

cat("\nb:\n")
print(b)

x_sol <- qr.solve(A, b)

cat("\nqr.solve(A, b):\n")
print(x_sol)

cat("\nCheck A %*% x_sol:\n")
print(as.vector(A %*% x_sol))

cat("\nCompare with solve(A, b):\n")
print(solve(A, b))

cat("\n")


cat("8. Demonstrate tol in qr() and qr.solve()\n\n")

cat("Create a nearly rank-deficient matrix.\n")

eps <- 1e-8

B <- cbind(
  a = c(1, 2, 3, 4),
  b = c(2, 4, 6, 8) + eps,
  c = c(1, 0, 1, 0)
)

cat("\nB:\n")
print(B)

cat("\nqr(B, tol = 1e-7)$rank:\n")
print(qr(B, tol = 1e-7)$rank)

cat("\nqr(B, tol = 1e-12)$rank:\n")
print(qr(B, tol = 1e-12)$rank)

cat("\nThe tolerance affects whether nearly dependent columns are treated as dependent.\n\n")


cat("9. Demonstrate LAPACK = TRUE versus default\n\n")

qrx_default <- qr(X, LAPACK = FALSE)
qrx_lapack <- qr(X, LAPACK = TRUE)

cat("qr(X, LAPACK = FALSE)$rank:\n")
print(qrx_default$rank)

cat("\nqr(X, LAPACK = TRUE)$rank:\n")
print(qrx_lapack$rank)

cat("\nCoefficients using default QR:\n")
print(qr.coef(qrx_default, y))

cat("\nCoefficients using LAPACK QR:\n")
print(qr.coef(qrx_lapack, y))

cat("\n")


cat("10. Manual least-squares calculation using QR pieces\n\n")

cat("For full-rank X, least-squares solves R beta = t(Q) y.\n")

Q_thin <- qr.Q(qrx)
R_thin <- qr.R(qrx)

beta_manual <- backsolve(R_thin, as.vector(t(Q_thin) %*% y))

cat("\nbeta_manual:\n")
print(beta_manual)

cat("\nqr.coef(qrx, y):\n")
print(coef_qr)

cat("\n")


cat("Summary\n")
cat("qr(x)       computes a QR decomposition.\n")
cat("qr.coef    computes least-squares coefficients.\n")
cat("qr.fitted  computes fitted values.\n")
cat("qr.resid   computes residuals.\n")
cat("qr.qty     computes t(Q) times a vector or matrix.\n")
cat("qr.qy      computes Q times a vector or matrix.\n")
cat("qr.solve   solves linear systems or least-squares problems.\n")
