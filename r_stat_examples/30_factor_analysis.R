# Factor analysis in base R

set.seed(123)
n <- 300
f1 <- rnorm(n)
f2 <- rnorm(n)

x1 <- 0.9 * f1 + rnorm(n, sd = 0.3)
x2 <- 0.8 * f1 + rnorm(n, sd = 0.3)
x3 <- 0.7 * f1 + rnorm(n, sd = 0.3)
x4 <- 0.9 * f2 + rnorm(n, sd = 0.3)
x5 <- 0.8 * f2 + rnorm(n, sd = 0.3)
x6 <- 0.7 * f2 + rnorm(n, sd = 0.3)

dat <- data.frame(x1, x2, x3, x4, x5, x6)

fit <- factanal(dat, factors = 2, rotation = "varimax")

print(fit)
