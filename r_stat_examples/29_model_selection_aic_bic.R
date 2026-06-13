# Model selection using AIC and BIC in base R

set.seed(123)
n <- 200
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)
y <- 1 + 2 * x1 - 1.5 * x3 + rnorm(n)

dat <- data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4)

full <- lm(y ~ x1 + x2 + x3 + x4, data = dat)
null <- lm(y ~ 1, data = dat)

cat("Stepwise selection by AIC:\n")
fit_aic <- step(null,
                scope = list(lower = null, upper = full),
                direction = "both",
                trace = FALSE)
print(summary(fit_aic))

cat("\nStepwise selection by BIC:\n")
fit_bic <- step(null,
                scope = list(lower = null, upper = full),
                direction = "both",
                k = log(n),
                trace = FALSE)
print(summary(fit_bic))
