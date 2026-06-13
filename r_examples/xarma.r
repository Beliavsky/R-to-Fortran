# time_series_no_plot_demo.R
# Demonstrate selected built-in R time-series functions without plotting.
#
# Uses only vectors and matrices, not data frames.
#
# Functions demonstrated:
#   acf()
#   pacf()
#   ccf()
#   ar()
#   ar.yw()
#   ar.burg()
#   ar.ols()
#   ar.mle()
#   arima()
#   arima.sim()
#   ARMAacf()

set.seed(123)

cat("\n1. Simulate an ARMA series with arima.sim()\n")

n <- 200

# ARMA(2, 1):
#   x[t] = 0.7 x[t-1] - 0.2 x[t-2] + e[t] + 0.5 e[t-1]
x <- as.vector(arima.sim(
  model = list(ar = c(0.7, -0.2), ma = 0.5),
  n = n
))

cat("First 10 observations of x:\n")
print(round(x[1:10], 4))

cat("Length of x:\n")
print(length(x))


cat("\n2. acf() without plotting\n")

a <- acf(x, lag.max = 10, plot = FALSE)

cat("Class of acf result:\n")
print(class(a))

cat("Names in acf result:\n")
print(names(a))

cat("Lag and ACF values:\n")
print(cbind(
  lag = as.vector(a$lag),
  acf = round(as.vector(a$acf), 4)
))


cat("\n3. pacf() without plotting\n")

p <- pacf(x, lag.max = 10, plot = FALSE)

cat("Class of pacf result:\n")
print(class(p))

cat("Lag and PACF values:\n")
print(cbind(
  lag = as.vector(p$lag),
  pacf = round(as.vector(p$acf), 4)
))


cat("\n4. ccf() without plotting\n")

# Create a second vector related to x.
# y is a noisy lagged version of x.
y <- numeric(n)
y[1] <- rnorm(1)

for (i in 2:n) {
  y[i] <- 0.6 * x[i - 1] + rnorm(1, sd = 0.8)
}

cxy <- ccf(x, y, lag.max = 10, plot = FALSE)

cat("Lag and CCF values:\n")
print(cbind(
  lag = as.vector(cxy$lag),
  ccf = round(as.vector(cxy$acf), 4)
))


cat("\n5. Multivariate acf() on a matrix\n")

z <- cbind(x, y)

az <- acf(z, lag.max = 5, plot = FALSE)

cat("Dimensions of multivariate acf array:\n")
print(dim(az$acf))

cat("The array is indexed as [lag_index, series_i, series_j].\n")
cat("Here, series 1 is x and series 2 is y.\n")

cat("ACF of x with itself:\n")
print(cbind(
  lag = as.vector(az$lag),
  acf_xx = round(az$acf[, 1, 1], 4)
))

cat("ACF of y with itself:\n")
print(cbind(
  lag = as.vector(az$lag),
  acf_yy = round(az$acf[, 2, 2], 4)
))

cat("Cross-correlation x,y from multivariate acf:\n")
print(cbind(
  lag = as.vector(az$lag),
  acf_xy = round(az$acf[, 1, 2], 4),
  acf_yx = round(az$acf[, 2, 1], 4)
))


cat("\n6. Fit autoregressive models with ar()\n")

fit_ar <- ar(x, order.max = 10, aic = TRUE, method = "yule-walker")

cat("ar() selected order:\n")
print(fit_ar$order)

cat("ar() estimated AR coefficients:\n")
print(round(fit_ar$ar, 4))

cat("ar() estimated innovation variance:\n")
print(fit_ar$var.pred)


cat("\n7. Fit AR model with ar.yw()\n")

fit_yw <- ar.yw(x, order.max = 10, aic = TRUE)

cat("ar.yw() selected order:\n")
print(fit_yw$order)

cat("ar.yw() estimated AR coefficients:\n")
print(round(fit_yw$ar, 4))

cat("ar.yw() estimated innovation variance:\n")
print(fit_yw$var.pred)


cat("\n8. Fit AR model with ar.burg()\n")

fit_burg <- ar.burg(x, order.max = 10, aic = TRUE)

cat("ar.burg() selected order:\n")
print(fit_burg$order)

cat("ar.burg() estimated AR coefficients:\n")
print(round(fit_burg$ar, 4))

cat("ar.burg() estimated innovation variance:\n")
print(fit_burg$var.pred)


cat("\n9. Fit AR model with ar.ols()\n")

fit_ols <- ar.ols(x, order.max = 10, aic = TRUE)

cat("ar.ols() selected order:\n")
print(fit_ols$order)

cat("ar.ols() estimated AR coefficients:\n")
print(round(fit_ols$ar, 4))

cat("ar.ols() estimated innovation variance:\n")
print(fit_ols$var.pred)


cat("\n10. Fit AR model with ar.mle()\n")

fit_mle <- ar.mle(x, order.max = 10, aic = TRUE)

cat("ar.mle() selected order:\n")
print(fit_mle$order)

cat("ar.mle() estimated AR coefficients:\n")
print(round(fit_mle$ar, 4))

cat("ar.mle() estimated innovation variance:\n")
print(fit_mle$var.pred)


cat("\n11. Compare AR fitting methods in a matrix\n")

orders <- c(
  ar = fit_ar$order,
  yw = fit_yw$order,
  burg = fit_burg$order,
  ols = fit_ols$order,
  mle = fit_mle$order
)

vars <- c(
  ar = fit_ar$var.pred,
  yw = fit_yw$var.pred,
  burg = fit_burg$var.pred,
  ols = fit_ols$var.pred,
  mle = fit_mle$var.pred
)

summary_matrix <- cbind(
  selected_order = orders,
  innovation_variance = vars
)

print(round(summary_matrix, 4))


cat("\n12. Fit a fixed-order AR(2) model with each AR method\n")

fit_yw2 <- ar.yw(x, order.max = 2, aic = FALSE)
fit_burg2 <- ar.burg(x, order.max = 2, aic = FALSE)
fit_ols2 <- ar.ols(x, order.max = 2, aic = FALSE)
fit_mle2 <- ar.mle(x, order.max = 2, aic = FALSE)

coef_matrix <- rbind(
  true_ar = c(0.7, -0.2),
  yw = fit_yw2$ar,
  burg = fit_burg2$ar,
  ols = fit_ols2$ar,
  mle = fit_mle2$ar
)

colnames(coef_matrix) <- c("ar1", "ar2")

cat("True AR coefficients and estimated AR(2) coefficients:\n")
print(round(coef_matrix, 4))


cat("\n13. Fit ARMA model with arima()\n")

# Fit the correct ARMA(2, 1) structure used above.
fit_arima <- arima(x, order = c(2, 0, 1), include.mean = TRUE)

cat("arima() fit:\n")
print(fit_arima)

cat("Estimated coefficients:\n")
print(round(fit_arima$coef, 4))

cat("Estimated sigma^2:\n")
print(fit_arima$sigma2)

cat("Log-likelihood:\n")
print(as.numeric(logLik(fit_arima)))

cat("AIC:\n")
print(AIC(fit_arima))


cat("\n14. Fit a few competing ARIMA models and compare AIC\n")

fit_100 <- arima(x, order = c(1, 0, 0), include.mean = TRUE)
fit_200 <- arima(x, order = c(2, 0, 0), include.mean = TRUE)
fit_201 <- arima(x, order = c(2, 0, 1), include.mean = TRUE)
fit_301 <- arima(x, order = c(3, 0, 1), include.mean = TRUE)

aic_vec <- c(
  AR1 = AIC(fit_100),
  AR2 = AIC(fit_200),
  ARMA21 = AIC(fit_201),
  ARMA31 = AIC(fit_301)
)

cat("AIC values:\n")
print(round(aic_vec, 4))

cat("Best model by AIC:\n")
print(names(which.min(aic_vec)))


cat("\n15. Theoretical ARMA autocorrelations with ARMAacf()\n")

true_acf <- ARMAacf(
  ar = c(0.7, -0.2),
  ma = 0.5,
  lag.max = 10
)

cat("Theoretical ACF for the simulated ARMA(2,1):\n")
print(cbind(
  lag = 0:10,
  theoretical_acf = round(true_acf, 4)
))


cat("\n16. Compare sample ACF with theoretical ACF\n")

sample_acf <- as.vector(acf(x, lag.max = 10, plot = FALSE)$acf)

acf_compare <- cbind(
  lag = 0:10,
  sample_acf = round(sample_acf, 4),
  theoretical_acf = round(true_acf, 4)
)

print(acf_compare)


cat("\n17. Simulate a pure AR(2) process and fit AR(2)\n")

x_ar <- as.vector(arima.sim(
  model = list(ar = c(0.6, -0.3)),
  n = n
))

fit_ar2 <- arima(x_ar, order = c(2, 0, 0), include.mean = TRUE)

cat("AR(2) fit using arima():\n")
print(fit_ar2)

cat("True AR coefficients:\n")
print(c(0.6, -0.3))

cat("Estimated AR coefficients:\n")
print(round(fit_ar2$coef[1:2], 4))


cat("\n18. Simulate a pure MA(1) process and fit MA(1)\n")

x_ma <- as.vector(arima.sim(
  model = list(ma = 0.8),
  n = n
))

fit_ma1 <- arima(x_ma, order = c(0, 0, 1), include.mean = TRUE)

cat("MA(1) fit using arima():\n")
print(fit_ma1)

cat("True MA coefficient:\n")
print(0.8)

cat("Estimated MA coefficient:\n")
print(round(fit_ma1$coef["ma1"], 4))


cat("\n19. Simulate an integrated ARIMA(1,1,0) model and fit it\n")

x_i <- as.vector(arima.sim(
  model = list(order = c(1, 1, 0), ar = 0.5),
  n = n
))

fit_i <- arima(x_i, order = c(1, 1, 0), include.mean = FALSE)

cat("ARIMA(1,1,0) fit:\n")
print(fit_i)

cat("Estimated AR coefficient:\n")
print(round(fit_i$coef["ar1"], 4))


cat("\n20. Manual differencing before acf() and pacf()\n")

dx_i <- diff(x_i)

cat("First 10 values of differenced integrated series:\n")
print(round(dx_i[1:10], 4))

a_dx <- acf(dx_i, lag.max = 10, plot = FALSE)
p_dx <- pacf(dx_i, lag.max = 10, plot = FALSE)

cat("ACF of differenced series:\n")
print(cbind(
  lag = as.vector(a_dx$lag),
  acf = round(as.vector(a_dx$acf), 4)
))

cat("PACF of differenced series:\n")
print(cbind(
  lag = as.vector(p_dx$lag),
  pacf = round(as.vector(p_dx$acf), 4)
))


cat("\n21. Forecast-like predictions from an arima() fit\n")

pred <- predict(fit_arima, n.ahead = 5)

cat("Predicted values:\n")
print(round(pred$pred, 4))

cat("Prediction standard errors:\n")
print(round(pred$se, 4))


cat("\n22. Basic residual checks without plotting\n")

res <- residuals(fit_arima)

cat("First 10 residuals:\n")
print(round(res[1:10], 4))

cat("Residual mean:\n")
print(mean(res))

cat("Residual variance:\n")
print(var(res))

res_acf <- acf(res, lag.max = 10, plot = FALSE)

cat("Residual ACF:\n")
print(cbind(
  lag = as.vector(res_acf$lag),
  acf = round(as.vector(res_acf$acf), 4)
))


cat("\nDone.\n")
