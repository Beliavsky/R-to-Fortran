# K-fold cross-validation for linear regression in base R

set.seed(123)
n <- 150
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 1 + 2 * x1 - 1 * x2 + rnorm(n, sd = 1.5)

dat <- data.frame(y = y, x1 = x1, x2 = x2)

k <- 5
fold <- sample(rep(1:k, length.out = n))
mse <- numeric(k)

for (j in 1:k) {
  train <- dat[fold != j, ]
  test <- dat[fold == j, ]
  fit <- lm(y ~ x1 + x2, data = train)
  pred <- predict(fit, newdata = test)
  mse[j] <- mean((test$y - pred)^2)
}

cat("Fold MSE values:\n")
print(mse)

cat("\nMean CV MSE =", mean(mse), "\n")
