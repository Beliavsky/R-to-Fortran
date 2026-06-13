# Demonstrate features of eigen() in R

cat("1) Symmetric real matrix\n")
A <- matrix(c(4, 1,
              1, 3), nrow = 2, byrow = TRUE)

print(A)

ea <- eigen(A)

cat("\nEigenvalues:\n")
print(ea$values)

cat("\nEigenvectors, stored as columns:\n")
print(ea$vectors)

cat("\nCheck A %*% v = lambda * v for first eigenpair:\n")
v1 <- ea$vectors[, 1]
lambda1 <- ea$values[1]
print(A %*% v1)
print(lambda1 * v1)

cat("\nReconstruct A from eigen decomposition:\n")
A_recon <- ea$vectors %*% diag(ea$values) %*% t(ea$vectors)
print(A_recon)

cat("\nMaximum absolute reconstruction error:\n")
print(max(abs(A - A_recon)))


cat("\n\n2) Ask only for eigenvalues\n")
evals_only <- eigen(A, only.values = TRUE)

cat("\nReturned object:\n")
print(evals_only)

cat("\nEigenvalues only:\n")
print(evals_only$values)

cat("\nVectors are NULL:\n")
print(evals_only$vectors)


cat("\n\n3) Non-symmetric real matrix\n")
B <- matrix(c(2, 1,
              4, 2), nrow = 2, byrow = TRUE)

print(B)

eb <- eigen(B)

cat("\nEigenvalues:\n")
print(eb$values)

cat("\nEigenvectors:\n")
print(eb$vectors)

cat("\nCheck B %*% v = lambda * v for each eigenpair:\n")
for (j in seq_along(eb$values)) {
  v <- eb$vectors[, j]
  lambda <- eb$values[j]
  cat("\nEigenpair", j, "\n")
  print(B %*% v)
  print(lambda * v)
}


cat("\n\n4) Matrix with complex eigenvalues\n")
C <- matrix(c(0, -1,
              1,  0), nrow = 2, byrow = TRUE)

print(C)

ec <- eigen(C)

cat("\nEigenvalues:\n")
print(ec$values)

cat("\nEigenvectors:\n")
print(ec$vectors)

cat("\nCheck C %*% v = lambda * v for each complex eigenpair:\n")
for (j in seq_along(ec$values)) {
  v <- ec$vectors[, j]
  lambda <- ec$values[j]
  cat("\nEigenpair", j, "\n")
  print(C %*% v)
  print(lambda * v)
}


cat("\n\n5) Diagonal matrix\n")
D <- diag(c(10, 5, 1))

print(D)

ed <- eigen(D)

cat("\nEigenvalues:\n")
print(ed$values)

cat("\nEigenvectors:\n")
print(ed$vectors)

cat("\nNote: eigen() returns eigenvalues sorted in decreasing order.\n")


cat("\n\n6) Positive definite matrix and matrix square root\n")
M <- matrix(c(5, 2,
              2, 2), nrow = 2, byrow = TRUE)

print(M)

em <- eigen(M, symmetric = TRUE)

cat("\nEigenvalues:\n")
print(em$values)

cat("\nAll eigenvalues positive?\n")
print(all(em$values > 0))

M_sqrt <- em$vectors %*% diag(sqrt(em$values)) %*% t(em$vectors)

cat("\nMatrix square root:\n")
print(M_sqrt)

cat("\nCheck M_sqrt %*% M_sqrt equals M:\n")
print(M_sqrt %*% M_sqrt)

cat("\nMaximum absolute error:\n")
print(max(abs(M - M_sqrt %*% M_sqrt)))


cat("\n\n7) Matrix inverse using eigen decomposition\n")
M_inv <- em$vectors %*% diag(1 / em$values) %*% t(em$vectors)

cat("\nInverse from eigen decomposition:\n")
print(M_inv)

cat("\nCompare with solve(M):\n")
print(solve(M))

cat("\nMaximum absolute difference:\n")
print(max(abs(M_inv - solve(M))))


cat("\n\n8) Spectral radius\n")
E <- matrix(c(0.5, 0.2,
              0.1, 0.7), nrow = 2, byrow = TRUE)

print(E)

ee <- eigen(E, only.values = TRUE)

spectral_radius <- max(Mod(ee$values))

cat("\nEigenvalues:\n")
print(ee$values)

cat("\nSpectral radius = max(Mod(eigenvalues)):\n")
print(spectral_radius)


cat("\n\n9) Principal components from covariance matrix\n")
X <- matrix(c(2.0, 1.0,
              2.5, 1.8,
              3.0, 2.2,
              4.0, 3.0,
              5.0, 3.8), ncol = 2, byrow = TRUE)

cat("\nData matrix X:\n")
print(X)

S <- cov(X)

cat("\nCovariance matrix:\n")
print(S)

es <- eigen(S, symmetric = TRUE)

cat("\nEigenvalues of covariance matrix:\n")
print(es$values)

cat("\nPrincipal component directions:\n")
print(es$vectors)

cat("\nProportion of variance explained:\n")
print(es$values / sum(es$values))

scores <- scale(X, center = TRUE, scale = FALSE) %*% es$vectors

cat("\nPrincipal component scores:\n")
print(scores)


cat("\n\n10) Compare eigenvalues with determinant and trace\n")
F <- matrix(c(6, 2,
              2, 3), nrow = 2, byrow = TRUE)

print(F)

ef <- eigen(F, symmetric = TRUE)

cat("\nEigenvalues:\n")
print(ef$values)

cat("\nSum of eigenvalues:\n")
print(sum(ef$values))

cat("\nTrace of matrix:\n")
print(sum(diag(F)))

cat("\nProduct of eigenvalues:\n")
print(prod(ef$values))

cat("\nDeterminant of matrix:\n")
print(det(F))
