# acf_demo_no_plot.R
# Demonstrate the R acf() function without plotting.
#
# acf() computes sample autocorrelations.
#
# Basic form:
#   acf(x, lag.max = NULL, type = "correlation", plot = FALSE)
#
# Important:
#   plot = FALSE prevents the plot and returns the computed values.

cat("\n1. Basic autocorrelation of a vector\n")

x <- c(2.0, 3.1, 2.8, 4.0, 3.7, 5.2, 4.9, 6.1)

a <- acf(x, plot = FALSE)

print(a)

cat("acf values:\n")
print(a$acf)

cat("lags:\n")
print(a$lag)


cat("\n2. Extract acf values as a simple vector\n")

acf_values <- as.vector(a$acf)
lags <- as.vector(a$lag)

print(data.frame(lag = lags, acf = acf_values))


cat("\n3. Specify maximum lag\n")

a <- acf(x, lag.max = 3, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n4. White-noise-like example\n")

x <- c(0.2, -1.1, 0.7, 1.4, -0.5, 0.3, -0.9, 1.0, -0.2, 0.4)

a <- acf(x, lag.max = 5, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n5. Persistent series example\n")

x <- c(1.0, 1.2, 1.5, 1.7, 2.0, 2.2, 2.5, 2.7, 3.0, 3.2)

a <- acf(x, lag.max = 5, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n6. Alternating series example\n")

x <- c(1, -1, 1, -1, 1, -1, 1, -1, 1, -1)

a <- acf(x, lag.max = 6, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n7. Use type = 'covariance'\n")

x <- c(2.0, 3.1, 2.8, 4.0, 3.7, 5.2, 4.9, 6.1)

a <- acf(x, lag.max = 3, type = "covariance", plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  covariance = as.vector(a$acf)
))


cat("\n8. Use type = 'correlation'\n")

a <- acf(x, lag.max = 3, type = "correlation", plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  correlation = as.vector(a$acf)
))


cat("\n9. Autocorrelation of first differences\n")

x <- c(10, 11, 13, 12, 15, 16, 18, 17, 20)

dx <- diff(x)

cat("x:\n")
print(x)

cat("diff(x):\n")
print(dx)

a <- acf(dx, lag.max = 4, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n10. Simulated AR(1) series\n")

set.seed(123)

n <- 100
phi <- 0.8
e <- rnorm(n)
x <- numeric(n)

for (i in 2:n) {
  x[i] <- phi * x[i - 1] + e[i]
}

a <- acf(x, lag.max = 10, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n11. Compare sample ACF to theoretical AR(1) ACF\n")

lags <- as.vector(a$lag)
sample_acf <- as.vector(a$acf)
theoretical_acf <- phi^lags

print(data.frame(
  lag = lags,
  sample_acf = sample_acf,
  theoretical_acf = theoretical_acf
))


cat("\n12. Remove lag 0 from the output\n")

a <- acf(x, lag.max = 10, plot = FALSE)

lags <- as.vector(a$lag)
acf_values <- as.vector(a$acf)

keep <- lags > 0

print(data.frame(
  lag = lags[keep],
  acf = acf_values[keep]
))


cat("\n13. Confidence-band style cutoff\n")

# For a rough white-noise reference, many ACF plots use +/- 1.96 / sqrt(n).

n <- length(x)
cutoff <- 1.96 / sqrt(n)

cat("Approximate white-noise cutoff:\n")
print(cutoff)

print(data.frame(
  lag = lags[keep],
  acf = acf_values[keep],
  outside_cutoff = abs(acf_values[keep]) > cutoff
))


cat("\n14. Missing values: na.action = na.pass\n")

x <- c(1, 2, NA, 4, 5, 6, NA, 8)

# This keeps missing values present. For acf(), this can produce NA results.
a <- acf(x, lag.max = 4, plot = FALSE, na.action = na.pass)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n15. Missing values: remove missing values first\n")

x_clean <- na.omit(x)

a <- acf(x_clean, lag.max = 4, plot = FALSE)

print(data.frame(
  lag = as.vector(a$lag),
  acf = as.vector(a$acf)
))


cat("\n16. Multivariate time series\n")

x1 <- c(1, 2, 3, 4, 5, 6)
x2 <- c(2, 1, 2, 1, 2, 1)

xmat <- cbind(x1 = x1, x2 = x2)

a <- acf(xmat, lag.max = 3, plot = FALSE)

cat("Dimensions of a$acf:\n")
print(dim(a$acf))

cat("Lag values:\n")
print(as.vector(a$lag))

cat("a$acf[lag_index, series_i, series_j]\n")
print(a$acf)

