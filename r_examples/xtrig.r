# Demonstrate trigonometric functions in base R.
# No plotting is used.

cat("Basic trigonometric functions\n")

x <- c(0, pi / 6, pi / 4, pi / 3, pi / 2, pi)

cat("x in radians:\n")
print(x)

cat("x in degrees:\n")
print(x * 180 / pi)

cat("cos(x):\n")
print(cos(x))

cat("sin(x):\n")
print(sin(x))

cat("tan(x):\n")
print(tan(x))

cat("\n")


cat("Inverse trigonometric functions\n")

u <- c(-1, -sqrt(3) / 2, -sqrt(2) / 2, -0.5, 0, 0.5,
       sqrt(2) / 2, sqrt(3) / 2, 1)

cat("u:\n")
print(u)

cat("acos(u), in radians:\n")
print(acos(u))

cat("acos(u), in degrees:\n")
print(acos(u) * 180 / pi)

cat("asin(u), in radians:\n")
print(asin(u))

cat("asin(u), in degrees:\n")
print(asin(u) * 180 / pi)

v <- c(-1, -sqrt(3), -1, 0, 1, sqrt(3))

cat("v:\n")
print(v)

cat("atan(v), in radians:\n")
print(atan(v))

cat("atan(v), in degrees:\n")
print(atan(v) * 180 / pi)

cat("\n")


cat("atan2(y, x)\n")
cat("atan2 gives the angle of the point (x, y), using the signs of both arguments.\n")

px <- c( 1,  0, -1,  0,  1, -1, -1,  1)
py <- c( 0,  1,  0, -1,  1,  1, -1, -1)

angles <- atan2(py, px)

points <- data.frame(
  x = px,
  y = py,
  angle_rad = angles,
  angle_deg = angles * 180 / pi
)

print(points)

cat("\n")


cat("pi-scaled trigonometric functions\n")
cat("cospi(x), sinpi(x), and tanpi(x) compute cos(pi*x), sin(pi*x), and tan(pi*x).\n")
cat("They are often more accurate for multiples or fractions of pi.\n")

z <- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1)

cat("z:\n")
print(z)

cat("cospi(z):\n")
print(cospi(z))

cat("sinpi(z):\n")
print(sinpi(z))

cat("tanpi(z):\n")
print(tanpi(z))

cat("\n")


cat("Comparison with ordinary trig functions\n")

cat("cospi(z) compared with cos(pi * z):\n")
print(cbind(cospi_z = cospi(z), cos_pi_z = cos(pi * z)))

cat("sinpi(z) compared with sin(pi * z):\n")
print(cbind(sinpi_z = sinpi(z), sin_pi_z = sin(pi * z)))

cat("tanpi(z) compared with tan(pi * z):\n")
print(cbind(tanpi_z = tanpi(z), tan_pi_z = tan(pi * z)))

cat("\n")


cat("Checking identities\n")

x <- c(0, pi / 6, pi / 4, pi / 3, pi / 2)

cat("cos(x)^2 + sin(x)^2:\n")
print(cos(x)^2 + sin(x)^2)

u <- c(-1, -0.5, 0, 0.5, 1)

cat("sin(asin(u)):\n")
print(sin(asin(u)))

cat("cos(acos(u)):\n")
print(cos(acos(u)))

v <- c(-10, -1, 0, 1, 10)

cat("tan(atan(v)):\n")
print(tan(atan(v)))
