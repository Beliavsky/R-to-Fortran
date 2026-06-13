# Regression diagnostics in base R

set.seed(123)
n <- 100
x <- runif(n, 0, 10)
y <- 1 + 2 * x + rnorm(n, sd = 1 + 0.2 * x)

fit <- lm(y ~ x)

cat("Regression summary:\n")
print(summary(fit))

cat("\nInfluential observations by Cook's distance:\n")
cook <- cooks.distance(fit)
print(head(sort(cook, decreasing = TRUE), 10))

cat("\nBreusch-Pagan-style auxiliary regression for heteroskedasticity:\n")
e2 <- resid(fit)^2
aux <- lm(e2 ~ x)
bp_stat <- length(x) * summary(aux)$r.squared
bp_p <- 1 - pchisq(bp_stat, df = 1)
cat("BP statistic =", bp_stat, "\n")
cat("Approx p-value =", bp_p, "\n")
