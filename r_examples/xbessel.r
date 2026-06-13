# Demonstrate Bessel functions in base R.
# No plotting is used.

cat("Bessel functions in base R\n\n")

cat("Functions demonstrated:\n")
cat("besselJ(x, nu)  Bessel function of the first kind\n")
cat("besselY(x, nu)  Bessel function of the second kind\n")
cat("besselI(x, nu)  Modified Bessel function of the first kind\n")
cat("besselK(x, nu)  Modified Bessel function of the second kind\n\n")


cat("1. Bessel J: besselJ(x, nu)\n\n")

x <- c(0, 0.5, 1, 2, 5, 10)
nu <- 0

cat("x:\n")
print(x)

cat("\nnu:\n")
print(nu)

cat("\nbesselJ(x, nu):\n")
print(besselJ(x, nu))

cat("\nCommon orders nu = 0, 1, 2\n")

J <- cbind(
  J0 = besselJ(x, 0),
  J1 = besselJ(x, 1),
  J2 = besselJ(x, 2)
)

print(J)

cat("\n")


cat("2. Bessel Y: besselY(x, nu)\n\n")

cat("besselY(x, nu) is undefined at x = 0, so use positive x values.\n")

x_pos <- c(0.1, 0.5, 1, 2, 5, 10)

cat("\nx_pos:\n")
print(x_pos)

cat("\nbesselY(x_pos, 0):\n")
print(besselY(x_pos, 0))

cat("\nCommon orders nu = 0, 1, 2\n")

Y <- cbind(
  Y0 = besselY(x_pos, 0),
  Y1 = besselY(x_pos, 1),
  Y2 = besselY(x_pos, 2)
)

print(Y)

cat("\n")


cat("3. Modified Bessel I: besselI(x, nu)\n\n")

x <- c(0, 0.5, 1, 2, 5, 10)

cat("x:\n")
print(x)

cat("\nbesselI(x, 0):\n")
print(besselI(x, 0))

cat("\nCommon orders nu = 0, 1, 2\n")

I <- cbind(
  I0 = besselI(x, 0),
  I1 = besselI(x, 1),
  I2 = besselI(x, 2)
)

print(I)

cat("\n")


cat("4. Modified Bessel K: besselK(x, nu)\n\n")

cat("besselK(x, nu) is undefined or infinite at x = 0, so use positive x values.\n")

x_pos <- c(0.1, 0.5, 1, 2, 5, 10)

cat("\nx_pos:\n")
print(x_pos)

cat("\nbesselK(x_pos, 0):\n")
print(besselK(x_pos, 0))

cat("\nCommon orders nu = 0, 1, 2\n")

K <- cbind(
  K0 = besselK(x_pos, 0),
  K1 = besselK(x_pos, 1),
  K2 = besselK(x_pos, 2)
)

print(K)

cat("\n")


cat("5. Non-integer orders\n\n")

x_pos <- c(0.1, 0.5, 1, 2, 5)

orders <- c(0, 0.5, 1.5, 2.5)

cat("x_pos:\n")
print(x_pos)

cat("\norders:\n")
print(orders)

cat("\nValues of besselJ(x, nu) for several orders:\n")

J_table <- outer(
  x_pos,
  orders,
  Vectorize(function(x, nu) besselJ(x, nu))
)

rownames(J_table) <- paste0("x=", x_pos)
colnames(J_table) <- paste0("nu=", orders)

print(J_table)

cat("\nValues of besselK(x, nu) for several orders:\n")

K_table <- outer(
  x_pos,
  orders,
  Vectorize(function(x, nu) besselK(x, nu))
)

rownames(K_table) <- paste0("x=", x_pos)
colnames(K_table) <- paste0("nu=", orders)

print(K_table)

cat("\n")


cat("6. Exponentially scaled modified Bessel functions\n\n")

cat("besselI(x, nu, expon.scaled = TRUE) returns exp(-abs(x)) * I_nu(x).\n")
cat("besselK(x, nu, expon.scaled = TRUE) returns exp(x) * K_nu(x).\n")
cat("These are useful for large x, where I grows and K decays rapidly.\n\n")

x_large <- c(10, 20, 50, 100)

cat("x_large:\n")
print(x_large)

cat("\nbesselI(x_large, 0):\n")
print(besselI(x_large, 0))

cat("\nbesselI(x_large, 0, expon.scaled = TRUE):\n")
print(besselI(x_large, 0, expon.scaled = TRUE))

cat("\nbesselK(x_large, 0):\n")
print(besselK(x_large, 0))

cat("\nbesselK(x_large, 0, expon.scaled = TRUE):\n")
print(besselK(x_large, 0, expon.scaled = TRUE))

cat("\n")


cat("7. Checking simple known values\n\n")

cat("At x = 0, J_0(0) = 1:\n")
print(besselJ(0, 0))

cat("\nAt x = 0, J_nu(0) = 0 for positive integer nu:\n")
print(c(
  J1_0 = besselJ(0, 1),
  J2_0 = besselJ(0, 2),
  J3_0 = besselJ(0, 3)
))

cat("\nAt x = 0, I_0(0) = 1:\n")
print(besselI(0, 0))

cat("\nAt x = 0, I_nu(0) = 0 for positive integer nu:\n")
print(c(
  I1_0 = besselI(0, 1),
  I2_0 = besselI(0, 2),
  I3_0 = besselI(0, 3)
))

cat("\n")


cat("8. Recurrence check for Bessel J\n\n")

cat("For Bessel J, approximately: J_{nu-1}(x) + J_{nu+1}(x) = 2 * nu / x * J_nu(x).\n")

x <- c(1, 2, 5, 10)
nu <- 2

left <- besselJ(x, nu - 1) + besselJ(x, nu + 1)
right <- 2 * nu / x * besselJ(x, nu)

check <- cbind(
  x = x,
  left = left,
  right = right,
  difference = left - right
)

print(check)

cat("\n")


cat("9. Recurrence check for modified Bessel I\n\n")

cat("For modified Bessel I, approximately: I_{nu-1}(x) - I_{nu+1}(x) = 2 * nu / x * I_nu(x).\n")

x <- c(1, 2, 5, 10)
nu <- 2

left <- besselI(x, nu - 1) - besselI(x, nu + 1)
right <- 2 * nu / x * besselI(x, nu)

check <- cbind(
  x = x,
  left = left,
  right = right,
  difference = left - right
)

print(check)
