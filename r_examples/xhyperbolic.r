# Demonstrate hyperbolic functions in base R.
# No plotting is used.

cat("Hyperbolic functions\n")

x <- c(-3, -2, -1, 0, 1, 2, 3)

cat("x:\n")
print(x)

cat("cosh(x):\n")
print(cosh(x))

cat("sinh(x):\n")
print(sinh(x))

cat("tanh(x):\n")
print(tanh(x))

cat("\n")


cat("Inverse hyperbolic functions\n")

# acosh(x) is defined for x >= 1.
u <- c(1, 1.5, 2, 5, 10)

cat("u, for acosh(u), with u >= 1:\n")
print(u)

cat("acosh(u):\n")
print(acosh(u))

cat("\n")

# asinh(x) is defined for all real x.
v <- c(-10, -3, -1, 0, 1, 3, 10)

cat("v, for asinh(v):\n")
print(v)

cat("asinh(v):\n")
print(asinh(v))

cat("\n")

# atanh(x) is defined for -1 < x < 1.
w <- c(-0.99, -0.75, -0.5, 0, 0.5, 0.75, 0.99)

cat("w, for atanh(w), with -1 < w < 1:\n")
print(w)

cat("atanh(w):\n")
print(atanh(w))

cat("\n")


cat("Checking inverse relationships\n")

x <- c(-3, -1, 0, 1, 3)

cat("asinh(sinh(x)):\n")
print(asinh(sinh(x)))

cat("atanh(tanh(x)):\n")
print(atanh(tanh(x)))

# acosh(cosh(x)) does not return x for negative x,
# because cosh(x) = cosh(-x). It returns abs(x).
cat("acosh(cosh(x)):\n")
print(acosh(cosh(x)))

cat("abs(x):\n")
print(abs(x))

cat("\n")


cat("Checking identities\n")

x <- c(-2, -1, 0, 1, 2)

cat("cosh(x)^2 - sinh(x)^2:\n")
print(cosh(x)^2 - sinh(x)^2)

cat("tanh(x) compared with sinh(x) / cosh(x):\n")
print(cbind(
  tanh_x = tanh(x),
  sinh_over_cosh = sinh(x) / cosh(x)
))

cat("\n")


cat("Boundary behavior for atanh(x)\n")

w <- c(-1, -0.999, 0, 0.999, 1)

cat("w:\n")
print(w)

cat("atanh(w):\n")
print(atanh(w))

