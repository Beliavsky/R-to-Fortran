# Demonstrate aperm() on arrays in base R.
# aperm() permutes the dimensions of an array.

cat("Create a 3-dimensional array\n")

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

cat("dim(x):\n")
print(dim(x))

cat("dimnames(x):\n")
print(dimnames(x))

cat("\n")


cat("Permute dimensions with aperm(x, c(2, 1, 3))\n")
cat("This swaps the first and second dimensions.\n")

y <- aperm(x, c(2, 1, 3))

cat("y:\n")
print(y)

cat("dim(y):\n")
print(dim(y))

cat("dimnames(y):\n")
print(dimnames(y))

cat("\n")


cat("Compare individual elements\n")
cat("y[col, row, layer] should equal x[row, col, layer].\n")

cat("x['r2', 'c3', 'L4']:\n")
print(x["r2", "c3", "L4"])

cat("y['c3', 'r2', 'L4']:\n")
print(y["c3", "r2", "L4"])

cat("\n")


cat("Reverse dimension order with aperm(x, c(3, 2, 1))\n")

z <- aperm(x, c(3, 2, 1))

cat("z:\n")
print(z)

cat("dim(z):\n")
print(dim(z))

cat("dimnames(z):\n")
print(dimnames(z))

cat("\n")


cat("Compare individual elements after reversing dimensions\n")
cat("z[layer, col, row] should equal x[row, col, layer].\n")

cat("x['r1', 'c2', 'L3']:\n")
print(x["r1", "c2", "L3"])

cat("z['L3', 'c2', 'r1']:\n")
print(z["L3", "c2", "r1"])

cat("\n")


cat("Using aperm() on a matrix\n")

m <- matrix(1:12, nrow = 3, ncol = 4)

cat("m:\n")
print(m)

cat("aperm(m, c(2, 1)):\n")
print(aperm(m, c(2, 1)))

cat("t(m), for comparison:\n")
print(t(m))

cat("\n")


cat("Using aperm() without explicitly giving perm\n")
cat("For a matrix, aperm(m) reverses the dimensions, like a transpose.\n")

cat("aperm(m):\n")
print(aperm(m))

cat("\n")


cat("Effect of resize = FALSE\n")
cat("By default, resize = TRUE, so the dimensions are rearranged.\n")
cat("With resize = FALSE, the data are permuted but the original dimensions are kept.\n")

u <- aperm(x, c(2, 1, 3), resize = TRUE)
v <- aperm(x, c(2, 1, 3), resize = FALSE)

cat("dim(aperm(x, c(2, 1, 3), resize = TRUE)):\n")
print(dim(u))

cat("dim(aperm(x, c(2, 1, 3), resize = FALSE)):\n")
print(dim(v))

cat("aperm(x, c(2, 1, 3), resize = FALSE):\n")
print(v)

cat("\n")


cat("A compact numeric example\n")

a <- array(1:8, dim = c(2, 2, 2))

cat("a:\n")
print(a)

cat("aperm(a, c(2, 1, 3)), swapping rows and columns inside each slice:\n")
print(aperm(a, c(2, 1, 3)))

cat("aperm(a, c(3, 2, 1)), moving slices to the first dimension:\n")
print(aperm(a, c(3, 2, 1)))
