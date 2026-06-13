# Demonstrate as.complex() in base R.
# No plotting is used.

cat("Convert numeric values to complex\n")

x <- c(-2, -1, 0, 1, 2)

cat("x:\n")
print(x)

z <- as.complex(x)

cat("as.complex(x):\n")
print(z)

cat("Re(z):\n")
print(Re(z))

cat("Im(z):\n")
print(Im(z))

cat("\n")


cat("Convert integer values to complex\n")

i <- 1:5

cat("i:\n")
print(i)

cat("as.complex(i):\n")
print(as.complex(i))

cat("\n")


cat("Convert logical values to complex\n")

b <- c(FALSE, TRUE, FALSE, TRUE)

cat("b:\n")
print(b)

cat("as.complex(b):\n")
print(as.complex(b))

cat("\n")


cat("Convert character strings to complex\n")

s <- c("1+2i", "3-4i", "5", "-2i", "0+0i")

cat("s:\n")
print(s)

cat("as.complex(s):\n")
print(as.complex(s))

cat("\n")


cat("Invalid character strings become NA with a warning\n")

bad <- c("1+2i", "hello", "3-4i", "not_a_number")

cat("bad:\n")
print(bad)

cat("as.complex(bad):\n")
print(as.complex(bad))

cat("\n")


cat("Convert real numbers before complex arithmetic\n")

x <- c(-1, -4, 0, 1, 4)

cat("x:\n")
print(x)

cat("sqrt(x), without conversion:\n")
print(sqrt(x))

cat("sqrt(as.complex(x)), with conversion:\n")
print(sqrt(as.complex(x)))

cat("\n")


cat("Complex logarithms\n")

x <- c(-1, -2, 1, 2)

cat("x:\n")
print(x)

cat("log(x), without conversion:\n")
print(log(x))

cat("log(as.complex(x)), with conversion:\n")
print(log(as.complex(x)))

cat("\n")


cat("as.complex() on an existing complex vector\n")

z <- c(1 + 2i, 3 - 4i, -5 + 0i)

cat("z:\n")
print(z)

cat("as.complex(z):\n")
print(as.complex(z))

cat("\n")


cat("Using as.complex() with matrices\n")

m <- matrix(1:6, nrow = 2, ncol = 3)

cat("m:\n")
print(m)

cm <- as.complex(m)

cat("as.complex(m):\n")
print(cm)

cat("dim(as.complex(m)):\n")
print(dim(cm))

cat("\n")


cat("Constructing complex values from real and imaginary parts\n")

real_part <- c(1, 2, 3)
imag_part <- c(10, 20, 30)

z <- as.complex(real_part) + 1i * imag_part

cat("real_part:\n")
print(real_part)

cat("imag_part:\n")
print(imag_part)

cat("z <- as.complex(real_part) + 1i * imag_part:\n")
print(z)

cat("Re(z):\n")
print(Re(z))

cat("Im(z):\n")
print(Im(z))
