# Time series ARIMA modeling in base R

set.seed(123)
data_file = "arma_data.txt"
x <- scan(data_file, numeric(), quiet = TRUE)
fit <- arima(x, order = c(1, 0, 1), include.mean = TRUE)

print(fit)

cat("\nAIC =", AIC(fit), "\n")

cat("\nForecasts using predict():\n")
pred <- predict(fit, n.ahead = 5)
print(pred)
