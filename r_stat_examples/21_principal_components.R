# Principal components analysis in base R

set.seed(123)
n <- 200
z1 <- rnorm(n)
z2 <- rnorm(n)
x1 <- z1 + rnorm(n, sd = 0.2)
x2 <- z1 + rnorm(n, sd = 0.2)
x3 <- z2 + rnorm(n, sd = 0.2)
x4 <- z2 + rnorm(n, sd = 0.2)

dat <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

fit <- prcomp(dat, scale. = TRUE)

print(summary(fit))
cat("\nLoadings:\n")
print(fit$rotation)
cat("\nFirst six PC scores:\n")
print(head(fit$x))
