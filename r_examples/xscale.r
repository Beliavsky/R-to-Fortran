# scale_demo.R
# Demonstrate the R scale() function.
#
# Basic form:
#   scale(x, center = TRUE, scale = TRUE)
#
# scale() is usually used to center and/or standardize numeric vectors,
# matrices, or numeric columns of data frames.

cat("\n1. Scale a numeric vector\n")

x <- c(10, 20, 30, 40, 50)

z <- scale(x)

print(z)

cat("mean(z) =", mean(z), "\n")
cat("sd(z)   =", sd(z), "\n")

cat("typeof(z)  =", typeof(z), "\n")
cat("is.matrix(z) =", is.matrix(z), "\n")


cat("\n2. Center but do not divide by standard deviation\n")

x <- c(10, 20, 30, 40, 50)

z <- scale(x, center = TRUE, scale = FALSE)

print(z)


cat("\n3. Scale but do not center\n")

x <- c(10, 20, 30, 40, 50)

z <- scale(x, center = FALSE, scale = TRUE)

print(z)

cat("\nWhen center = FALSE, scale() divides by the root mean square\n")
cat("computed as sqrt(sum(x^2) / (length(x) - 1)).\n")

rms <- sqrt(sum(x^2) / (length(x) - 1))

cat("rms =", rms, "\n")
cat("x / rms:\n")
print(x / rms)


cat("\n4. Scale columns of a matrix\n")

x <- matrix(
  c(10, 20, 30,
    100, 200, 300),
  nrow = 3,
  ncol = 2
)

cat("x:\n")
print(x)

z <- scale(x)

cat("scale(x):\n")
print(z)

cat("Column means of scaled matrix:\n")
print(colMeans(z))

cat("Column standard deviations of scaled matrix:\n")
print(apply(z, 2, sd))


cat("\n5. Centering and scaling attributes\n")

center <- attr(z, "scaled:center")
sc <- attr(z, "scaled:scale")

cat("scaled:center:\n")
print(center)

cat("scaled:scale:\n")
print(sc)


cat("\n6. Manually reproduce scale() with sweep()\n")

z_manual <- sweep(sweep(x, 2, center, "-"), 2, sc, "/")

cat("Manual result:\n")
print(z_manual)

cat("Same as scale(x)?\n")
print(all.equal(as.matrix(z), z_manual, check.attributes = FALSE))


cat("\n7. User-specified center and scale\n")

x <- matrix(
  c(10, 20, 30,
    100, 200, 300),
  nrow = 3,
  ncol = 2
)

z <- scale(x, center = c(10, 100), scale = c(10, 100))
print(z)

