# Demonstrate features of solve() in base R.
# No plotting is used.

cat("Demonstrating solve() in base R\n\n")

cat("solve(A, b) solves A %*% x = b.\n")
cat("solve(A) computes the inverse of A.\n\n")


cat("1. Solve a square linear system A x = b\n\n")

A <- matrix(c(
  2, 1,
  5, 7
), nrow = 2, byrow = TRUE)

b <- c(11, 13)

cat("A:\n")
print(A)

cat("\nb:\n")
print(b)

x <- solve(A, b)

cat("\nx <- solve(A, b):\n")
print(x)

cat("\nCheck A %*% x:\n")
print(as.vector(A %*% x))

cat("\n")


cat("2. Compute the inverse of a matrix with solve(A)\n\n")

A_inv <- solve(A)

cat("A_inv <- solve(A):\n")
print(A_inv)

cat("\nCheck A %*% A_inv:\n")
print(A %*% A_inv)

cat("\nCheck A_inv %*% A:\n")
print(A_inv %*% A)

cat("\n")


cat("3. Multiple right-hand sides\n\n")

A <- matrix(c(
  3, 1, 0,
  1, 4, 2,
  0, 2, 5
), nrow = 3, byrow = TRUE)

B <- matrix(c(
  4,  1,
  9,  2,
  8, -1
), nrow = 3, byrow = TRUE)

cat("A:\n")
print(A)

cat("\nB:\n")
print(B)

X <- solve(A, B)

cat("\nX <- solve(A, B):\n")
print(X)

cat("\nCheck A %*% X:\n")
print(A %*% X)

cat("\n")


cat("4. Compare solve(A, b) with solve(A) %*% b\n\n")

b <- c(4, 9, 8)

cat("b:\n")
print(b)

x1 <- solve(A, b)
x2 <- solve(A) %*% b

cat("\nsolve(A, b):\n")
print(x1)

cat("\nsolve(A) %*% b:\n")
print(as.vector(x2))

cat("\nMaximum absolute difference:\n")
print(max(abs(x1 - as.vector(x2))))

cat("\nIn practice, solve(A, b) is usually preferred to explicitly forming solve(A).\n\n")


cat("5. Solving for an inverse using an identity matrix\n\n")

I <- diag(nrow(A))

cat("I <- diag(nrow(A)):\n")
print(I)

cat("\nsolve(A, I):\n")
print(solve(A, I))

cat("\nCompare with solve(A):\n")
print(solve(A))

cat("\n")


cat("6. Named rows and columns\n\n")

A <- matrix(c(
  4, 1, 2,
  1, 3, 0,
  2, 0, 5
), nrow = 3, byrow = TRUE)

rownames(A) <- c("eq1", "eq2", "eq3")
colnames(A) <- c("x", "y", "z")

b <- c(eq1 = 7, eq2 = 5, eq3 = 11)

cat("A with row and column names:\n")
print(A)

cat("\nb:\n")
print(b)

x <- solve(A, b)

cat("\nsolve(A, b):\n")
print(x)

cat("\nThe result is named using the column names of A.\n")

cat("\nCheck A %*% x:\n")
print(A %*% x)

cat("\n")


cat("7. Using solve() with complex matrices\n\n")

A <- matrix(c(
  1 + 1i, 2,
  3,      4 - 1i
), nrow = 2, byrow = TRUE)

b <- c(1 + 0i, 2 + 1i)

cat("Complex A:\n")
print(A)

cat("\nComplex b:\n")
print(b)

x <- solve(A, b)

cat("\nsolve(A, b):\n")
print(x)

cat("\nCheck A %*% x:\n")
print(as.vector(A %*% x))

cat("\n")


cat("8. The tol argument\n\n")

cat("The tol argument controls the tolerance for detecting singularity.\n\n")

A <- matrix(c(
  1, 1,
  1, 1 + 1e-12
), nrow = 2, byrow = TRUE)

b <- c(2, 2 + 1e-12)

cat("Nearly singular A:\n")
print(A)

cat("\nb:\n")
print(b)

cat("\nCondition number estimate from kappa(A):\n")
print(kappa(A))

cat("\nsolve(A, b, tol = 1e-7):\n")
print(try(solve(A, b, tol = 1e-7), silent = TRUE))

cat("\nsolve(A, b, tol = 1e-15):\n")
print(try(solve(A, b, tol = 1e-15), silent = TRUE))

cat("\n")


cat("9. Singular matrix example\n\n")

A_sing <- matrix(c(
  1, 2,
  2, 4
), nrow = 2, byrow = TRUE)

b <- c(3, 6)

cat("A_sing:\n")
print(A_sing)

cat("\nb:\n")
print(b)

cat("\nThe second row is 2 times the first row, so A_sing is singular.\n")

cat("\nAttempt solve(A_sing, b):\n")
print(try(solve(A_sing, b), silent = TRUE))

cat("\n")


cat("10. Least squares is not what solve() does\n\n")

cat("solve() requires a square, nonsingular coefficient matrix.\n")
cat("For an overdetermined least-squares problem, use lm.fit(), qr.coef(), or qr.solve().\n\n")

X <- matrix(c(
  1, 0,
  1, 1,
  1, 2,
  1, 3
), nrow = 4, byrow = TRUE)

y <- c(1, 2, 2, 4)

cat("X, a 4 by 2 design matrix:\n")
print(X)

cat("\ny:\n")
print(y)

cat("\nAttempt solve(X, y):\n")
print(try(solve(X, y), silent = TRUE))

cat("\nUse qr.solve(X, y) for least squares:\n")
print(qr.solve(X, y))

cat("\nCompare with lm.fit(X, y)$coefficients:\n")
print(lm.fit(X, y)$coefficients)

cat("\n")


cat("11. Solving normal equations manually\n\n")

cat("This is shown for demonstration. Numerically, qr.solve(X, y) is usually better.\n\n")

XtX <- t(X) %*% X
Xty <- t(X) %*% y

cat("t(X) %*% X:\n")
print(XtX)

cat("\nt(X) %*% y:\n")
print(Xty)

beta_normal <- solve(XtX, Xty)

cat("\nbeta_normal <- solve(t(X) %*% X, t(X) %*% y):\n")
print(as.vector(beta_normal))

cat("\nCompare with qr.solve(X, y):\n")
print(qr.solve(X, y))

cat("\n")


cat("12. Solving a matrix equation A X = B\n\n")

A <- matrix(c(
  2, 0, 1,
  1, 3, 0,
  0, 1, 4
), nrow = 3, byrow = TRUE)

B <- matrix(c(
  1,  2,  3,
  4,  5,  6,
  7,  8, 10
), nrow = 3, byrow = TRUE)

cat("A:\n")
print(A)

cat("\nB:\n")
print(B)

X <- solve(A, B)

cat("\nX <- solve(A, B):\n")
print(X)

cat("\nCheck A %*% X:\n")
print(A %*% X)
