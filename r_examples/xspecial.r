# Demonstrate special mathematical functions in base R.
# No plotting is used.

cat("Beta function\n")
a <- 2.5
b <- 4.0

cat("a =", a, " b =", b, "\n")
cat("beta(a, b)  =", beta(a, b), "\n")
cat("lbeta(a, b) =", lbeta(a, b), "\n")
cat("exp(lbeta(a, b)) =", exp(lbeta(a, b)), "\n\n")


cat("Gamma function\n")
x <- c(0.5, 1, 2, 3.5, 5)

cat("x:\n")
print(x)

cat("gamma(x):\n")
print(gamma(x))

cat("lgamma(x):\n")
print(lgamma(x))

cat("exp(lgamma(x)):\n")
print(exp(lgamma(x)))

cat("\n")


cat("Digamma and polygamma functions\n")
x <- c(0.5, 1, 2, 5, 10)

cat("x:\n")
print(x)

cat("psigamma(x, deriv = 0), same as digamma(x):\n")
print(psigamma(x, deriv = 0))

cat("digamma(x):\n")
print(digamma(x))

cat("psigamma(x, deriv = 1), same as trigamma(x):\n")
print(psigamma(x, deriv = 1))

cat("trigamma(x):\n")
print(trigamma(x))

cat("psigamma(x, deriv = 2):\n")
print(psigamma(x, deriv = 2))

cat("\n")


cat("Combinations\n")
n <- 10
k <- 0:n

cat("n =", n, "\n")
cat("k:\n")
print(k)

cat("choose(n, k):\n")
print(choose(n, k))

cat("lchoose(n, k):\n")
print(lchoose(n, k))

cat("exp(lchoose(n, k)):\n")
print(exp(lchoose(n, k)))

cat("\n")


cat("Factorials\n")
x <- 0:10

cat("x:\n")
print(x)

cat("factorial(x):\n")
print(factorial(x))

cat("lfactorial(x):\n")
print(lfactorial(x))

cat("exp(lfactorial(x)):\n")
print(exp(lfactorial(x)))

cat("\n")


cat("Checking identities\n")

a <- 3.2
b <- 5.7

cat("beta(a, b) = gamma(a) * gamma(b) / gamma(a + b)\n")
cat("left side:  ", beta(a, b), "\n")
cat("right side: ", gamma(a) * gamma(b) / gamma(a + b), "\n\n")

n <- 12
k <- 5

cat("choose(n, k) = factorial(n) / (factorial(k) * factorial(n - k))\n")
cat("left side:  ", choose(n, k), "\n")
cat("right side: ", factorial(n) / (factorial(k) * factorial(n - k)), "\n\n")

x <- 6

cat("factorial(x) = gamma(x + 1)\n")
cat("left side:  ", factorial(x), "\n")
cat("right side: ", gamma(x + 1), "\n")
