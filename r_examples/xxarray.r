# Demonstrate the array() function in base R.
# No plotting is used.

cat("Create a one-dimensional array\n")

a1 <- array(1:5, dim = 5)

cat("a1:\n")
print(a1)

cat("dim(a1):\n")
print(dim(a1))

cat("\n")


cat("Create a two-dimensional array, similar to a matrix\n")

a2 <- array(1:12, dim = c(3, 4))

cat("a2:\n")
print(a2)

cat("dim(a2):\n")
print(dim(a2))

cat("a2[2, 3]:\n")
print(a2[2, 3])

cat("\n")


cat("Create a three-dimensional array\n")

a3 <- array(1:24, dim = c(2, 3, 4))

cat("a3:\n")
print(a3)

cat("dim(a3):\n")
print(dim(a3))

cat("Element a3[1, 2, 3]:\n")
print(a3[1, 2, 3])

cat("\n")


cat("Array filling order\n")
cat("R fills arrays column-major, with the first subscript varying fastest.\n")

a <- array(1:8, dim = c(2, 2, 2))

cat("a <- array(1:8, dim = c(2, 2, 2)):\n")
print(a)

cat("Selected elements:\n")
cat("a[1, 1, 1] =", a[1, 1, 1], "\n")
cat("a[2, 1, 1] =", a[2, 1, 1], "\n")
cat("a[1, 2, 1] =", a[1, 2, 1], "\n")
cat("a[2, 2, 1] =", a[2, 2, 1], "\n")
cat("a[1, 1, 2] =", a[1, 1, 2], "\n")

cat("\n")


cat("Recycling values\n")

b <- array(c(10, 20), dim = c(3, 4))

cat("b <- array(c(10, 20), dim = c(3, 4)):\n")
print(b)

cat("\n")


cat("Using dimnames\n")

x <- array(
  1:24,
  dim = c(2, 3, 4),
  dimnames = list(
    row = c("r1", "r2"),
    col = c("c1", "c2", "c3"),
    layer = c("L1", "L2", "L3", "L4")
  )
)

cat("x:\n")
print(x)

cat("dimnames(x):\n")
print(dimnames(x))

cat("Access by names: x['r2', 'c3', 'L4']:\n")
print(x["r2", "c3", "L4"])

cat("\n")


cat("Extracting slices\n")

cat("First layer, x[, , 'L1']:\n")
print(x[, , "L1"])

cat("Second row across columns and layers, x['r2', , ]:\n")
print(x["r2", , ])

cat("Column c2 across rows and layers, x[, 'c2', ]:\n")
print(x[, "c2", ])

cat("\n")


cat("Dropping versus preserving dimensions\n")

cat("x[1, , 'L1'] drops dimensions by default:\n")
print(x[1, , "L1"])
cat("dim(x[1, , 'L1']):\n")
print(dim(x[1, , "L1"]))

cat("x[1, , 'L1', drop = FALSE] preserves dimensions:\n")
print(x[1, , "L1", drop = FALSE])
cat("dim(x[1, , 'L1', drop = FALSE]):\n")
print(dim(x[1, , "L1", drop = FALSE]))

cat("\n")


cat("Array arithmetic\n")

u <- array(1:8, dim = c(2, 2, 2))
v <- array(101:108, dim = c(2, 2, 2))

cat("u:\n")
print(u)

cat("v:\n")
print(v)

cat("u + v:\n")
print(u + v)

cat("2 * u:\n")
print(2 * u)

cat("\n")


cat("Apply functions over array margins\n")

x <- array(1:24, dim = c(2, 3, 4))

cat("x:\n")
print(x)

cat("Sum over first dimension, apply(x, 1, sum):\n")
print(apply(x, 1, sum))

cat("Sum over second dimension, apply(x, 2, sum):\n")
print(apply(x, 2, sum))

cat("Sum over third dimension, apply(x, 3, sum):\n")
print(apply(x, 3, sum))

cat("Sum over first and second dimensions, apply(x, c(1, 2), sum):\n")
print(apply(x, c(1, 2), sum))

cat("\n")


cat("Convert a vector to an array by assigning dim\n")

y <- 1:12

cat("y before assigning dim:\n")
print(y)

dim(y) <- c(3, 4)

cat("y after dim(y) <- c(3, 4):\n")
print(y)

cat("dim(y):\n")
print(dim(y))

cat("\n")


cat("Check whether an object is an array\n")

cat("is.array(a3):\n")
print(is.array(a3))

m <- matrix(1:6, nrow = 2)

cat("m:\n")
print(m)

cat("is.array(m):\n")
print(is.array(m))

cat("A matrix is a two-dimensional array in R.\n")
