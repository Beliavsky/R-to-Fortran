# sweep_demo.R
# Demonstrate the R sweep() function.
#
# Basic form:
#   sweep(x, MARGIN, STATS, FUN = "-")
#
# sweep() applies an operation to rows or columns of an array or matrix.
#
# For a matrix:
#   MARGIN = 1 means operate by row
#   MARGIN = 2 means operate by column

cat("\n1. Create a matrix\n")

x <- matrix(
  c(10, 20, 30,
    40, 50, 60,
    70, 80, 90),
  nrow = 3,
  ncol = 3,
  byrow = TRUE
)

print(x)


cat("\n2. Subtract column means\n")

col_means <- colMeans(x)

cat("Column means:\n")
print(col_means)

z <- sweep(x, 2, col_means, "-")

cat("x with column means subtracted:\n")
print(z)

cat("Column means after centering:\n")
print(colMeans(z))


cat("\n3. Subtract row means\n")

row_means <- rowMeans(x)

cat("Row means:\n")
print(row_means)

z <- sweep(x, 1, row_means, "-")

cat("x with row means subtracted:\n")
print(z)

cat("Row means after centering:\n")
print(rowMeans(z))


cat("\n4. Divide columns by column standard deviations\n")

col_sds <- apply(x, 2, sd)

cat("Column standard deviations:\n")
print(col_sds)

z <- sweep(x, 2, col_sds, "/")

cat("x divided by column standard deviations:\n")
print(z)


cat("\n5. Standardize columns manually\n")

col_means <- colMeans(x)
col_sds <- apply(x, 2, sd)

z <- sweep(x, 2, col_means, "-")
z <- sweep(z, 2, col_sds, "/")

cat("Column-standardized x:\n")
print(z)

cat("Column means:\n")
print(colMeans(z))

cat("Column standard deviations:\n")
print(apply(z, 2, sd))


cat("\n6. Compare manual standardization with scale()\n")

z_scale <- scale(x)

cat("scale(x):\n")
print(z_scale)

cat("Same numeric values?\n")
print(all.equal(as.matrix(z_scale), z, check.attributes = FALSE))


cat("\n7. Add a different value to each column\n")

add_col <- c(100, 200, 300)

cat("Values to add to columns:\n")
print(add_col)

z <- sweep(x, 2, add_col, "+")

print(z)


cat("\n8. Add a different value to each row\n")

add_row <- c(1000, 2000, 3000)

cat("Values to add to rows:\n")
print(add_row)

z <- sweep(x, 1, add_row, "+")

print(z)


cat("\n9. Multiply each column by a different value\n")

mult_col <- c(1, 10, 100)

cat("Column multipliers:\n")
print(mult_col)

z <- sweep(x, 2, mult_col, "*")

print(z)


cat("\n10. Multiply each row by a different value\n")

mult_row <- c(1, 10, 100)

cat("Row multipliers:\n")
print(mult_row)

z <- sweep(x, 1, mult_row, "*")

print(z)


cat("\n11. Use sweep() with a custom function\n")

my_fun <- function(a, b) {
  a - 2 * b
}

stats <- c(1, 2, 3)

cat("Stats for columns:\n")
print(stats)

z <- sweep(x, 2, stats, my_fun)

cat("x[, j] - 2 * stats[j]:\n")
print(z)

cat("\n13. sweep() on a 3-dimensional array\n")

a <- array(1:24, dim = c(2, 3, 4))

cat("Array dimensions:\n")
print(dim(a))

cat("Original array:\n")
print(a)

# Subtract a different value from each first-dimension slice.
stats1 <- c(100, 200)

z <- sweep(a, 1, stats1, "-")

cat("Subtracting by MARGIN = 1:\n")
print(z)

# Subtract a different value from each second-dimension slice.
stats2 <- c(10, 20, 30)

z <- sweep(a, 2, stats2, "-")

cat("Subtracting by MARGIN = 2:\n")
print(z)

# Subtract a different value from each third-dimension slice.
stats3 <- c(1, 2, 3, 4)

z <- sweep(a, 3, stats3, "-")

cat("Subtracting by MARGIN = 3:\n")
print(z)


cat("\n14. Use sweep() with multiple margins\n")

a <- array(1:24, dim = c(2, 3, 4))

# STATS must have dimensions matching the selected margins.
stats_12 <- matrix(
  c(100, 200,
    300, 400,
    500, 600),
  nrow = 2,
  ncol = 3
)

cat("stats_12:\n")
print(stats_12)

z <- sweep(a, c(1, 2), stats_12, "-")

cat("Subtract stats_12 from each [row, column] position across the third dimension:\n")
print(z)


cat("\n15. Center rows using sweep()\n")

x <- matrix(
  c(1, 2, 3, 4,
    10, 20, 30, 40,
    100, 200, 300, 400),
  nrow = 3,
  byrow = TRUE
)

cat("x:\n")
print(x)

row_means <- rowMeans(x)

z <- sweep(x, 1, row_means, "-")

cat("Row-centered x:\n")
print(z)

cat("Row means after centering:\n")
print(rowMeans(z))


cat("\n16. Convert counts to row proportions\n")

counts <- matrix(
  c(10, 20, 30,
    5,  5, 10,
    8, 12, 20),
  nrow = 3,
  byrow = TRUE
)

cat("counts:\n")
print(counts)

row_totals <- rowSums(counts)

cat("row totals:\n")
print(row_totals)

props <- sweep(counts, 1, row_totals, "/")

cat("row proportions:\n")
print(props)

cat("row sums of proportions:\n")
print(rowSums(props))


cat("\n17. Convert counts to column proportions\n")

col_totals <- colSums(counts)

cat("column totals:\n")
print(col_totals)

props <- sweep(counts, 2, col_totals, "/")

cat("column proportions:\n")
print(props)

cat("column sums of proportions:\n")
print(colSums(props))


cat("\n18. Avoiding recycling mistakes\n")

x <- matrix(1:12, nrow = 3, ncol = 4)

cat("x:\n")
print(x)

cat("Correct: subtract one value per column\n")
stats <- c(10, 20, 30, 40)
print(sweep(x, 2, stats, "-"))

cat("Correct: subtract one value per row\n")
stats <- c(100, 200, 300)
print(sweep(x, 1, stats, "-"))
