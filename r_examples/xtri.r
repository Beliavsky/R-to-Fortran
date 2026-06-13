# lower_upper_tri_demo.R
# Demonstrate lower.tri() and upper.tri() in R.
#
# lower.tri(x) returns a logical matrix indicating entries below the diagonal.
# upper.tri(x) returns a logical matrix indicating entries above the diagonal.
#
# By default, diag = FALSE, so the diagonal is not included.

cat("\n1. Create a square matrix\n")

x <- matrix(1:25, nrow = 5, ncol = 5, byrow = TRUE)

cat("x:\n")
print(x)


cat("\n2. lower.tri(x)\n")

lt <- lower.tri(x)

cat("Logical matrix from lower.tri(x):\n")
print(lt)

cat("Elements below the diagonal:\n")
print(x[lt])


cat("\n3. upper.tri(x)\n")

ut <- upper.tri(x)

cat("Logical matrix from upper.tri(x):\n")
print(ut)

cat("Elements above the diagonal:\n")
print(x[ut])


cat("\n4. Include the diagonal with diag = TRUE\n")

lt_diag <- lower.tri(x, diag = TRUE)
ut_diag <- upper.tri(x, diag = TRUE)

cat("lower.tri(x, diag = TRUE):\n")
print(lt_diag)

cat("x[lower.tri(x, diag = TRUE)]:\n")
print(x[lt_diag])

cat("upper.tri(x, diag = TRUE):\n")
print(ut_diag)

cat("x[upper.tri(x, diag = TRUE)]:\n")
print(x[ut_diag])


cat("\n5. Extract strict lower and upper triangular parts\n")

lower_part <- x
lower_part[!lower.tri(lower_part)] <- 0

upper_part <- x
upper_part[!upper.tri(upper_part)] <- 0

cat("Strict lower triangular part:\n")
print(lower_part)

cat("Strict upper triangular part:\n")
print(upper_part)


cat("\n6. Extract lower and upper triangular parts including diagonal\n")

lower_part_diag <- x
lower_part_diag[!lower.tri(lower_part_diag, diag = TRUE)] <- 0

upper_part_diag <- x
upper_part_diag[!upper.tri(upper_part_diag, diag = TRUE)] <- 0

cat("Lower triangular part including diagonal:\n")
print(lower_part_diag)

cat("Upper triangular part including diagonal:\n")
print(upper_part_diag)


cat("\n7. Set lower or upper triangular elements to NA\n")

x_lower_na <- x
x_lower_na[lower.tri(x_lower_na)] <- NA

x_upper_na <- x
x_upper_na[upper.tri(x_upper_na)] <- NA

cat("Lower triangle set to NA:\n")
print(x_lower_na)

cat("Upper triangle set to NA:\n")
print(x_upper_na)


cat("\n8. Fill lower triangle with a value\n")

y <- matrix(0, nrow = 5, ncol = 5)

y[lower.tri(y)] <- 1

cat("Matrix with strict lower triangle filled with 1:\n")
print(y)

y[upper.tri(y)] <- 2

cat("Then strict upper triangle filled with 2:\n")
print(y)

diag(y) <- 9

cat("Then diagonal filled with 9:\n")
print(y)


cat("\n9. Copy upper triangle to lower triangle to make a symmetric matrix\n")

a <- matrix(
  c(1,  2,  3,  4,
    0,  5,  6,  7,
    0,  0,  8,  9,
    0,  0,  0, 10),
  nrow = 4,
  byrow = TRUE
)

cat("Upper triangular matrix a:\n")
print(a)

# Copy upper triangle into lower triangle.
a[lower.tri(a)] <- t(a)[lower.tri(a)]

cat("Symmetric matrix:\n")
print(a)


cat("\n10. Copy lower triangle to upper triangle to make a symmetric matrix\n")

b <- matrix(
  c(1,  0,  0,  0,
    2,  5,  0,  0,
    3,  6,  8,  0,
    4,  7,  9, 10),
  nrow = 4,
  byrow = TRUE
)

cat("Lower triangular matrix b:\n")
print(b)

# Copy lower triangle into upper triangle.
b[upper.tri(b)] <- t(b)[upper.tri(b)]

cat("Symmetric matrix:\n")
print(b)


cat("\n11. Work with a correlation matrix\n")

set.seed(123)

z <- matrix(rnorm(50), nrow = 10, ncol = 5)
cor_mat <- cor(z)

cat("Correlation matrix:\n")
print(round(cor_mat, 4))

cat("Unique off-diagonal correlations from upper triangle:\n")
offdiag_cor <- cor_mat[upper.tri(cor_mat)]
print(round(offdiag_cor, 4))

cat("Mean off-diagonal correlation:\n")
print(mean(offdiag_cor))


cat("\n12. Find largest off-diagonal correlation\n")

abs_cor <- abs(cor_mat)

# Ignore diagonal by setting it to NA.
diag(abs_cor) <- NA

max_abs_cor <- max(abs_cor, na.rm = TRUE)

cat("Largest absolute off-diagonal correlation:\n")
print(max_abs_cor)

where <- which(abs_cor == max_abs_cor, arr.ind = TRUE)

cat("Locations of largest absolute off-diagonal correlation:\n")
print(where)


cat("\n13. Use upper.tri() to avoid duplicate pairs\n")

p <- ncol(cor_mat)

pairs <- which(upper.tri(cor_mat), arr.ind = TRUE)

cat("Unique variable pairs from upper triangle:\n")
print(pairs)

cat("Pair correlations:\n")
for (i in seq_len(nrow(pairs))) {
  r <- pairs[i, 1]
  c <- pairs[i, 2]

  cat(
    "pair", i,
    ": column", r, "and column", c,
    "cor =", round(cor_mat[r, c], 4), "\n"
  )
}


cat("\n14. lower.tri() and upper.tri() on a rectangular matrix\n")

rect <- matrix(1:15, nrow = 3, ncol = 5, byrow = TRUE)

cat("rect:\n")
print(rect)

cat("lower.tri(rect):\n")
print(lower.tri(rect))

cat("upper.tri(rect):\n")
print(upper.tri(rect))

cat("rect[lower.tri(rect)]:\n")
print(rect[lower.tri(rect)])

cat("rect[upper.tri(rect)]:\n")
print(rect[upper.tri(rect)])


cat("\n15. Create row and column index matrices manually\n")

nr <- 5
nc <- 5

row_index <- row(matrix(0, nrow = nr, ncol = nc))
col_index <- col(matrix(0, nrow = nr, ncol = nc))

cat("row indices:\n")
print(row_index)

cat("column indices:\n")
print(col_index)

cat("row_index > col_index gives lower.tri equivalent:\n")
print(row_index > col_index)

cat("row_index < col_index gives upper.tri equivalent:\n")
print(row_index < col_index)

cat("Same as lower.tri?\n")
print(identical(row_index > col_index, lower.tri(matrix(0, nr, nc))))

cat("Same as upper.tri?\n")
print(identical(row_index < col_index, upper.tri(matrix(0, nr, nc))))


cat("\nDone.\n")
