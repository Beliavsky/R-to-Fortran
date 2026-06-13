# Time series ARIMA modeling in base R

set.seed(123)
x <- arima.sim(model = list(ar = 0.6, ma = -0.4), n = 300)
write(x, file = "arma_data.txt", ncolumns = 1)

fit <- arima(x, order = c(1, 0, 1), include.mean = TRUE)

print(fit)

cat("\nAIC =", AIC(fit), "\n")

cat("\nForecasts using predict():\n")
pred <- predict(fit, n.ahead = 5)
print(pred)
