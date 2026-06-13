# Multiple linear regression in base R

set.seed(123)
n <- 150
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- sample(c("A", "B"), n, replace = TRUE)
y <- 1 + 2 * x1 - 1.5 * x2 + ifelse(x3 == "B", 3, 0) + rnorm(n)

dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = factor(x3))

fit <- lm(y ~ x1 + x2 + x3, data = dat)

print(summary(fit))
cat("\nANOVA table:\n")
print(anova(fit))

cat("\nModel matrix first six rows:\n")
print(head(model.matrix(fit)))
