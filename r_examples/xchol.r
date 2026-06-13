# Demonstrate chol() and chol2inv() on non-random data.
# No plotting is used.

cat("Cholesky factorization with chol() and inverse from chol2inv()\n\n")

cat("chol(A) computes an upper triangular matrix R such that:\n")
cat("t(R) %*% R = A\n\n")

cat("chol2inv(R) computes the inverse of A from the Cholesky factor R.\n\n")


cat("1. A simple symmetric positive definite matrix\n\n")

A <- matrix(c(
  4,  2,  0,
  2,  5,  1,
  0,  1,  3
), nrow = 3, byrow = TRUE)

cat("A:\n")
print(A)

cat("\nCheck that A is symmetric:\n")
print(isSymmetric(A))

R <- chol(A)

cat("\nR <- chol(A):\n")
print(R)

cat("\nCheck t(R) %*% R:\n")
print(t(R) %*% R)

cat("\nMaximum absolute reconstruction error:\n")
print(max(abs(A - t(R) %*% R)))

cat("\n")


cat("2. Inverse using chol2inv()\n\n")

A_inv_chol <- chol2inv(R)

cat("A_inv_chol <- chol2inv(R):\n")
print(A_inv_chol)

cat("\nCompare with solve(A):\n")
print(solve(A))

cat("\nCheck A %*% A_inv_chol:\n")
print(A %*% A_inv_chol)

cat("\nCheck A_inv_chol %*% A:\n")
print(A_inv_chol %*% A)

cat("\n")


cat("3. Solving A x = b using Cholesky factors\n\n")

b <- c(2, 7, 4)

cat("b:\n")
print(b)

cat("\nSince A = t(R) %*% R, solve in two triangular steps:\n")
cat("First solve t(R) y = b using forwardsolve().\n")
cat("Then solve R x = y using backsolve().\n\n")

y <- forwardsolve(t(R), b)
x <- backsolve(R, y)

cat("y:\n")
print(y)

cat("\nx:\n")
print(x)

cat("\nCheck A %*% x:\n")
print(as.vector(A %*% x))

cat("\nCompare with solve(A, b):\n")
print(solve(A, b))

cat("\nCompare with chol2inv(R) %*% b:\n")
print(as.vector(chol2inv(R) %*% b))

cat("\n")


cat("4. Multiple right-hand sides\n\n")

B <- matrix(c(
  2,  1,
  7,  0,
  4, -1
), nrow = 3, byrow = TRUE)

cat("B:\n")
print(B)

Y <- forwardsolve(t(R), B)
X <- backsolve(R, Y)

cat("\nSolution X to A %*% X = B:\n")
print(X)

cat("\nCheck A %*% X:\n")
print(A %*% X)

cat("\nCompare with solve(A, B):\n")
print(solve(A, B))

cat("\n")


cat("5. Using chol2inv() with a leading submatrix\n\n")

cat("chol2inv(R, size = k) uses the leading k by k part of R.\n\n")

A2 <- matrix(c(
  4, 2,
  2, 5
), nrow = 2, byrow = TRUE)

R2 <- R[1:2, 1:2]

cat("Leading 2 by 2 submatrix A2:\n")
print(A2)

cat("\nLeading 2 by 2 block of R:\n")
print(R2)

cat("\nCheck t(R2) %*% R2:\n")
print(t(R2) %*% R2)

cat("\nchol2inv(R, size = 2):\n")
print(chol2inv(R, size = 2))

cat("\nCompare with solve(A2):\n")
print(solve(A2))

cat("\n")


cat("6. A positive definite matrix from exact integer data\n\n")

cat("Create C, then form A = t(C) %*% C.\n")
cat("This guarantees A is symmetric positive definite if C has full column rank.\n\n")

C <- matrix(c(
  1,  2,  0,
  0,  1,  1,
  2,  0,  1,
  1, -1,  2
), nrow = 4, byrow = TRUE)

A <- t(C) %*% C

cat("C:\n")
print(C)

cat("\nA <- t(C) %*% C:\n")
print(A)

cat("\nEigenvalues of A:\n")
print(eigen(A, symmetric = TRUE, only.values = TRUE)$values)

R <- chol(A)

cat("\nR <- chol(A):\n")
print(R)

cat("\nCheck t(R) %*% R:\n")
print(t(R) %*% R)

cat("\nInverse from chol2inv(R):\n")
print(chol2inv(R))

cat("\nCheck A %*% chol2inv(R):\n")
print(A %*% chol2inv(R))

cat("\n")
