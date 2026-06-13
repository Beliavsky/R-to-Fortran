# Simple linear regression in base R

set.seed(123)
x <- runif(100, 0, 10)
y <- 2 + 3 * x + rnorm(100, sd = 4)

fit <- lm(y ~ x)

print(summary(fit))

cat("\nConfidence intervals:\n")
print(confint(fit))

cat("\nPredictions at x = 2, 5, 8:\n")
newdat <- data.frame(x = c(2, 5, 8))
print(predict(fit, newdata = newdat, interval = "prediction"))
