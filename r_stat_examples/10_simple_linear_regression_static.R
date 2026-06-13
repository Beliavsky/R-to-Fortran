# Simple linear regression in base R with static data

x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
e <- c(-1.0, 0.5, 1.0, -0.5, 0.0, 1.5, -1.5, 0.75, -0.75, 0.25)
y <- 2 + 3 * x + e

fit <- lm(y ~ x)

print(summary(fit))

cat("\nConfidence intervals:\n")
print(confint(fit))

cat("\nPredictions at x = 2, 5, 8:\n")
newdat <- data.frame(x = c(2, 5, 8))
print(predict(fit, newdata = newdat, interval = "prediction"))
