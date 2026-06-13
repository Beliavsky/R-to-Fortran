# Demonstrate R logarithm and exponential functions

cat("1) Basic input vector\n")
x <- c(0.001, 0.01, 0.1, 1, 2, 10, 100)

cat("\nx:\n")
print(x)


cat("\n\n2) Natural logarithms: log(x) and logb(x)\n")

cat("\nlog(x), default base exp(1):\n")
print(log(x))

cat("\nlog(x, base = exp(1)):\n")
print(log(x, base = exp(1)))

cat("\nlogb(x, base = exp(1)):\n")
print(logb(x, base = exp(1)))

cat("\nCheck that these are the same:\n")
print(all.equal(log(x), log(x, base = exp(1))))
print(all.equal(log(x), logb(x, base = exp(1))))


cat("\n\n3) Base-10 logarithm\n")

cat("\nlog10(x):\n")
print(log10(x))

cat("\nSame as log(x, base = 10):\n")
print(log(x, base = 10))

cat("\nCheck equality:\n")
print(all.equal(log10(x), log(x, base = 10)))


cat("\n\n4) Base-2 logarithm\n")

cat("\nlog2(x):\n")
print(log2(x))

cat("\nSame as log(x, base = 2):\n")
print(log(x, base = 2))

cat("\nCheck equality:\n")
print(all.equal(log2(x), log(x, base = 2)))


cat("\n\n5) log1p(x) computes log(1 + x)\n")

small_x <- c(1e-16, 1e-12, 1e-8, 1e-4, 0.01, 0.1)

cat("\nsmall_x:\n")
print(small_x)

cat("\nlog1p(small_x):\n")
print(log1p(small_x))

cat("\nlog(1 + small_x):\n")
print(log(1 + small_x))

cat("\nDifference log1p(x) - log(1 + x):\n")
print(log1p(small_x) - log(1 + small_x))

cat("\nFor very small x, log1p(x) is more accurate than log(1 + x).\n")


cat("\n\n6) exp(x) computes e^x\n")

z <- c(-5, -2, -1, 0, 1, 2, 5)

cat("\nz:\n")
print(z)

cat("\nexp(z):\n")
print(exp(z))

cat("\nCheck that log(exp(z)) returns z, up to floating point error:\n")
print(log(exp(z)))
print(all.equal(log(exp(z)), z))


cat("\n\n7) expm1(x) computes exp(x) - 1\n")

small_z <- c(-1e-8, -1e-12, 0, 1e-16, 1e-12, 1e-8, 1e-4)

cat("\nsmall_z:\n")
print(small_z)

cat("\nexpm1(small_z):\n")
print(expm1(small_z))

cat("\nexp(small_z) - 1:\n")
print(exp(small_z) - 1)

cat("\nDifference expm1(z) - (exp(z) - 1):\n")
print(expm1(small_z) - (exp(small_z) - 1))

cat("\nFor very small z, expm1(z) is more accurate than exp(z) - 1.\n")


cat("\n\n8) log1p() and expm1() are inverse functions near zero\n")

u <- c(-0.5, -0.1, -1e-8, 0, 1e-8, 0.1, 0.5)

cat("\nu:\n")
print(u)

cat("\nexpm1(log1p(u)):\n")
print(expm1(log1p(u)))

cat("\nlog1p(expm1(u)):\n")
print(log1p(expm1(u)))

cat("\nCheck expm1(log1p(u)) equals u:\n")
print(all.equal(expm1(log1p(u)), u))

cat("\nCheck log1p(expm1(u)) equals u:\n")
print(all.equal(log1p(expm1(u)), u))


cat("\n\n9) Special values and domain behavior\n")

special_x <- c(-1, 0, 1, Inf, NA, NaN)

cat("\nspecial_x:\n")
print(special_x)

cat("\nlog(special_x):\n")
print(log(special_x))

cat("\nlog10(special_x):\n")
print(log10(special_x))

cat("\nexp(special_x):\n")
print(exp(special_x))

cat("\nlog1p(special_x):\n")
print(log1p(special_x))

cat("\nexpm1(special_x):\n")
print(expm1(special_x))


cat("\n\n10) Complex logarithms for negative real numbers\n")

negative_x <- c(-4, -1, 1, 4)

cat("\nnegative_x:\n")
print(negative_x)

cat("\nlog(negative_x) gives NaN for negative real inputs:\n")
print(log(negative_x))

cat("\nlog(as.complex(negative_x)) gives complex values:\n")
print(log(as.complex(negative_x)))
