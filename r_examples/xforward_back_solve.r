# Demonstrate forwardsolve() and backsolve() in base R.
# No plotting is used.

cat("forwardsolve() and backsolve() in base R\n\n")

cat("forwardsolve() solves L x = b when L is lower triangular.\n")
cat("backsolve() solves U x = b when U is upper triangular.\n\n")


cat("1. Solving a lower triangular system with forwardsolve()\n\n")

L <- matrix(c(
  2,  0,  0,
  3,  1,  0,
 -1,  4,  5
), nrow = 3, byrow = TRUE)

b <- c(4, 8, 13)

cat("L:\n")
print(L)

cat("\nb:\n")
print(b)

x <- forwardsolve(L, b)

cat("\nSolution x <- forwardsolve(L, b):\n")
print(x)

cat("\nCheck L %*% x:\n")
print(as.vector(L %*% x))

cat("\n")


cat("2. Solving an upper triangular system with backsolve()\n\n")

U <- matrix(c(
  2,  3, -1,
  0,  1,  4,
  0,  0,  5
), nrow = 3, byrow = TRUE)

b <- c(7, 9, 10)

cat("U:\n")
print(U)

cat("\nb:\n")
print(b)

x <- backsolve(U, b)

cat("\nSolution x <- backsolve(U, b):\n")
print(x)

cat("\nCheck U %*% x:\n")
print(as.vector(U %*% x))

cat("\n")


cat("3. Solving with multiple right-hand sides\n\n")

L <- matrix(c(
  1, 0, 0,
  2, 3, 0,
  4, 5, 6
), nrow = 3, byrow = TRUE)

B <- matrix(c(
  1,  2,
  8,  7,
  32, 38
), nrow = 3, byrow = TRUE)

cat("L:\n")
print(L)

cat("\nB:\n")
print(B)

X <- forwardsolve(L, B)

cat("\nX <- forwardsolve(L, B):\n")
print(X)

cat("\nCheck L %*% X:\n")
print(L %*% X)

cat("\n")


cat("4. Using forwardsolve() and backsolve() with Cholesky factorization\n\n")

A <- matrix(c(
  4,  2,  0,
  2,  5,  1,
  0,  1,  3
), nrow = 3, byrow = TRUE)

b <- c(2, 7, 4)

cat("A:\n")
print(A)

cat("\nb:\n")
print(b)

cat("\nchol(A) returns an upper triangular matrix R such that t(R) %*% R = A.\n")

R <- chol(A)

cat("\nR <- chol(A):\n")
print(R)

cat("\nCheck t(R) %*% R:\n")
print(t(R) %*% R)

cat("\nSolve A x = b using two triangular solves.\n")
cat("First solve t(R) y = b with forwardsolve().\n")
cat("Then solve R x = y with backsolve().\n")

y <- forwardsolve(t(R), b)
x <- backsolve(R, y)

cat("\ny:\n")
print(y)

cat("\nx:\n")
print(x)

cat("\nCheck A %*% x:\n")
print(as.vector(A %*% x))

cat("\nCompare with solve(A, b):\n")
print(solve(A, b))

cat("\n")


cat("5. Using backsolve() with QR decomposition\n\n")

set.seed(123)

n <- 6
p <- 3

X <- cbind(
  intercept = 1,
  x1 = rnorm(n),
  x2 = rnorm(n)
)

beta_true <- c(1, 2, -1)
y <- as.vector(X %*% beta_true + rnorm(n, sd = 0.1))

cat("X:\n")
print(X)

cat("\ny:\n")
print(y)

qrx <- qr(X)

Q <- qr.Q(qrx)
R <- qr.R(qrx)

cat("\nR from QR decomposition:\n")
print(R)

cat("\nFor full-rank X, least squares solves R beta = t(Q) y.\n")

beta_hat <- backsolve(R, as.vector(t(Q) %*% y))

cat("\nbeta_hat using backsolve():\n")
print(beta_hat)

cat("\nCompare with qr.coef(qrx, y):\n")
print(qr.coef(qrx, y))

cat("\n")


cat("6. Unit diagonal example\n\n")

cat("Base R forwardsolve() and backsolve() do not have a unitri argument.\n")
cat("If the intended diagonal is all ones, put ones on the diagonal explicitly.\n\n")

L_unit <- matrix(c(
  1, 0, 0,
  2, 1, 0,
  3, 4, 1
), nrow = 3, byrow = TRUE)

b <- c(5, 16, 58)

cat("L_unit:\n")
print(L_unit)

cat("\nb:\n")
print(b)

x <- forwardsolve(L_unit, b)

cat("\nforwardsolve(L_unit, b):\n")
print(x)

cat("\nCheck L_unit %*% x:\n")
print(as.vector(L_unit %*% x))

cat("\n")


cat("7. Incorrect diagonal example\n\n")

cat("If the diagonal entries are not really part of the triangular system,\n")
cat("do not leave them in the matrix. forwardsolve() will use them.\n\n")

L_bad <- matrix(c(
  99, 0, 0,
   2, 99, 0,
   3,  4, 99
), nrow = 3, byrow = TRUE)

b <- c(5, 16, 58)

cat("L_bad:\n")
print(L_bad)

cat("\nb:\n")
print(b)

cat("\nforwardsolve(L_bad, b) uses the actual diagonal values, including 99:\n")
print(forwardsolve(L_bad, b))

cat("\nIf the intended diagonal is 1, set it explicitly:\n")

diag(L_bad) <- 1

cat("\nCorrected L_bad:\n")
print(L_bad)

cat("\nforwardsolve(L_bad, b):\n")
print(forwardsolve(L_bad, b))

cat("\nCheck corrected L_bad %*% x:\n")
x <- forwardsolve(L_bad, b)
print(as.vector(L_bad %*% x))

cat("\n")


cat("8. Transposed triangular systems\n\n")

cat("The transpose argument can solve t(L) x = b or t(U) x = b.\n\n")

L <- matrix(c(
  2, 0, 0,
  3, 1, 0,
  4, 5, 6
), nrow = 3, byrow = TRUE)

b <- c(10, 11, 12)

cat("L:\n")
print(L)

cat("\nb:\n")
print(b)

cat("\nSolve t(L) x = b using forwardsolve(L, b, transpose = TRUE):\n")
x <- forwardsolve(L, b, transpose = TRUE)
print(x)

cat("\nCheck t(L) %*% x:\n")
print(as.vector(t(L) %*% x))

cat("\n")

U <- matrix(c(
  2, 3, 4,
  0, 1, 5,
  0, 0, 6
), nrow = 3, byrow = TRUE)

b <- c(10, 11, 12)

cat("U:\n")
print(U)

cat("\nb:\n")
print(b)

cat("\nSolve t(U) x = b using backsolve(U, b, transpose = TRUE):\n")
x <- backsolve(U, b, transpose = TRUE)
print(x)

cat("\nCheck t(U) %*% x:\n")
print(as.vector(t(U) %*% x))

cat("\n")


cat("9. Using the k argument\n\n")

cat("The k argument lets you use only the leading k by k triangular part.\n\n")

U <- matrix(c(
  2, 3, 4,
  0, 5, 6,
  0, 0, 7
), nrow = 3, byrow = TRUE)

b <- c(8, 10, 12)

cat("U:\n")
print(U)

cat("\nb:\n")
print(b)

cat("\nbacksolve(U, b):\n")
print(backsolve(U, b))

cat("\nbacksolve(U, b, k = 2) solves only the leading 2 by 2 system:\n")
print(backsolve(U, b, k = 2))

cat("\nCheck leading 2 by 2 system:\n")
x2 <- backsolve(U, b, k = 2)
print(as.vector(U[1:2, 1:2] %*% x2))
print(b[1:2])

cat("\n")


cat("10. Important warning: the matrix must be triangular\n\n")

A <- matrix(c(
  2, 1,
  3, 4
), nrow = 2, byrow = TRUE)

b <- c(5, 11)

cat("A:\n")
print(A)

cat("\nb:\n")
print(b)

cat("\nA is not triangular. For a general system, use solve(A, b):\n")
print(solve(A, b))
