# Demonstrate complex numbers in base R.
# No plotting is used.

cat("Creating complex vectors with complex()\n\n")

cat("Default complex(), length.out = 0:\n")
z0 <- complex()
print(z0)
cat("length(z0):\n")
print(length(z0))
cat("\n")


cat("Create complex numbers from real and imaginary parts\n")

z1 <- complex(real = c(1, 2, 3),
              imaginary = c(10, 20, 30))

cat("z1:\n")
print(z1)

cat("Re(z1):\n")
print(Re(z1))

cat("Im(z1):\n")
print(Im(z1))

cat("\n")


cat("Create complex numbers with length.out\n")

z2 <- complex(length.out = 5,
              real = 1,
              imaginary = c(0, 1))

cat("z2:\n")
print(z2)

cat("real and imaginary arguments are recycled if needed.\n\n")


cat("Create complex numbers from modulus and argument\n")
cat("modulus is the distance from the origin.\n")
cat("argument is the angle in radians.\n")

theta <- c(0, pi / 6, pi / 4, pi / 2, pi)
r <- 2

z3 <- complex(modulus = r, argument = theta)

cat("theta:\n")
print(theta)

cat("z3 <- complex(modulus = 2, argument = theta):\n")
print(z3)

cat("Re(z3):\n")
print(Re(z3))

cat("Im(z3):\n")
print(Im(z3))

cat("Mod(z3):\n")
print(Mod(z3))

cat("Arg(z3):\n")
print(Arg(z3))

cat("\n")


cat("as.complex()\n")

x_num <- c(-2, -1, 0, 1, 2)

cat("numeric vector x_num:\n")
print(x_num)

cat("as.complex(x_num):\n")
print(as.complex(x_num))

x_char <- c("1+2i", "3-4i", "5", "-2i")

cat("character vector x_char:\n")
print(x_char)

cat("as.complex(x_char):\n")
print(as.complex(x_char))

cat("\n")


cat("Basic complex components\n")

z <- c(1 + 2i, 3 - 4i, -2 + 0i, 0 - 3i)

cat("z:\n")
print(z)

cat("Real part, Re(z):\n")
print(Re(z))

cat("Imaginary part, Im(z):\n")
print(Im(z))

cat("Modulus, Mod(z):\n")
print(Mod(z))

cat("Argument, Arg(z), in radians:\n")
print(Arg(z))

cat("Argument, Arg(z), in degrees:\n")
print(Arg(z) * 180 / pi)

cat("Complex conjugate, Conj(z):\n")
print(Conj(z))

cat("\n")


cat("Checking identities\n")

cat("z * Conj(z) equals Mod(z)^2:\n")
print(z * Conj(z))
print(Mod(z)^2)

cat("\n")

cat("Re(z) + 1i * Im(z) reconstructs z:\n")
print(Re(z) + 1i * Im(z))

cat("\n")

cat("Mod(z) * exp(1i * Arg(z)) reconstructs z, up to rounding error:\n")
print(Mod(z) * exp(1i * Arg(z)))

cat("\n")


cat("Complex arithmetic\n")

a <- 1 + 2i
b <- 3 - 4i

cat("a:\n")
print(a)

cat("b:\n")
print(b)

cat("a + b:\n")
print(a + b)

cat("a - b:\n")
print(a - b)

cat("a * b:\n")
print(a * b)

cat("a / b:\n")
print(a / b)

cat("a^2:\n")
print(a^2)

cat("\n")


cat("Square roots and logarithms of complex numbers\n")

z <- c(-1, -4, 1 + 1i, -1 + 1i)

cat("z:\n")
print(z)

cat("sqrt(z):\n")
print(sqrt(as.complex(z)))

cat("log(z):\n")
print(log(as.complex(z)))
