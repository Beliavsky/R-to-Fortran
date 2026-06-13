# Confidence and prediction intervals in base R

set.seed(123)
n <- 80
x <- runif(n, 0, 10)
y <- 1 + 2 * x + rnorm(n, sd = 3)

fit <- lm(y ~ x)

newdat <- data.frame(x = seq(0, 10, by = 2.5))

cat("Confidence intervals for the mean response:\n")
print(predict(fit, newdata = newdat, interval = "confidence"))

cat("\nPrediction intervals for a new observation:\n")
print(predict(fit, newdata = newdat, interval = "prediction"))
