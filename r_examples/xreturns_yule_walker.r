# return_stats_corr_ar_yw.R
# Read prices, compute log returns, print stats, correlations,
# autocorrelations, and fit AR models using Yule-Walker equations.

return_stats <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)

  if (n > 2 && s > 0.0) {
    skew <- mean(((x - m) / s)^3)
    kurt <- mean(((x - m) / s)^4) - 3.0
  } else {
    skew <- NA_real_
    kurt <- NA_real_
  }

  y <- c(
    nobs = n,
    mean = m,
    sd = s,
    min = min(x),
    max = max(x),
    skew = skew,
    excess_kurtosis = kurt
  )

  return(y)
}

acf_values <- function(x, nacf) {
  x <- x[is.finite(x)]
  x <- x - mean(x)

  n <- length(x)
  denom <- sum(x^2)

  y <- rep(NA_real_, nacf)

  if (denom <= 0.0) {
    return(y)
  }

  for (lag in seq_len(nacf)) {
    if (lag < n) {
      y[lag] <- sum(x[1:(n - lag)] * x[(lag + 1):n]) / denom
    }
  }

  return(y)
}

fit_ar_yw <- function(x, p) {
  x <- x[is.finite(x)]
  xmean <- mean(x)
  xc <- x - xmean

  n <- length(xc)

  acov <- rep(NA_real_, p + 1)

  for (lag in 0:p) {
    acov[lag + 1] <- sum(xc[1:(n - lag)] * xc[(lag + 1):n]) / n
  }

  gamma0 <- acov[1]

  if (gamma0 <= 0.0) {
    stop("zero variance series")
  }

  if (p == 0) {
    phi <- numeric(0)
    intercept <- xmean
    sigma2 <- gamma0
  } else {
    rmat <- toeplitz(acov[1:p])
    rhs <- acov[2:(p + 1)]

    phi <- as.numeric(solve(rmat, rhs))
    intercept <- xmean * (1.0 - sum(phi))
    sigma2 <- gamma0 - sum(phi * rhs)
  }

  y <- list(
    mean = xmean,
    intercept = intercept,
    phi = phi,
    sigma2 = sigma2,
    sd = sqrt(sigma2),
    acov = acov,
    acf = acov[-1] / gamma0
  )

  return(y)
}

print_ar_fit <- function(name, fit) {
  p <- length(fit$phi)

  cat("\n")
  cat(name, "AR(", p, ") Yule-Walker fit\n", sep = "")
  cat("mean      =", fit$mean, "\n")
  cat("intercept =", fit$intercept, "\n")
  cat("sigma2    =", fit$sigma2, "\n")
  cat("sigma     =", fit$sd, "\n")

  if (p > 0) {
    out <- data.frame(
      lag = seq_len(p),
      phi = fit$phi
    )

    print(out, row.names = FALSE)
  }

  return(invisible(NULL))
}

max_assets <- 1

prices <- read.csv("spy_tlt.csv", stringsAsFactors = FALSE)

dates <- as.Date(as.character(prices$Date), format = "%Y%m%d")

price_names <- setdiff(names(prices), "Date")
price_names <- price_names[seq_len(min(max_assets, length(price_names)))]
price_mat <- as.matrix(prices[, price_names])
storage.mode(price_mat) <- "double"

log_price <- log(price_mat)
ret_mat <- diff(log_price)
colnames(ret_mat) <- price_names

ret_dates <- dates[-1]

cat("number of price observations  =", nrow(price_mat), "\n")
cat("number of return observations =", nrow(ret_mat), "\n")
cat("first return date =", as.character(ret_dates[1]), "\n")
cat("last return date  =", as.character(tail(ret_dates, 1)), "\n")

cat("\nreturn stats\n")

stats_mat <- matrix(NA_real_, nrow = length(price_names), ncol = 7)
rownames(stats_mat) <- price_names
colnames(stats_mat) <- c("nobs", "mean", "sd", "min", "max", "skew", "excess_kurtosis")

for (j in seq_along(price_names)) {
  stats_mat[j, ] <- return_stats(ret_mat[, j])
}

print(stats_mat)

cat("\ncorrelation matrix of returns\n")
print(cor(ret_mat, use = "pairwise.complete.obs"))

nacf <- 2

cat("\nfirst", nacf, "autocorrelations of returns\n")

acf_mat <- matrix(NA_real_, nrow = length(price_names), ncol = nacf)
rownames(acf_mat) <- price_names
colnames(acf_mat) <- paste0("lag", seq_len(nacf))

for (j in seq_along(price_names)) {
  acf_mat[j, ] <- acf_values(ret_mat[, j], nacf)
}

print(acf_mat)

ar_order <- nacf

cat("\nAR models fit by Yule-Walker equations\n")
cat("AR order =", ar_order, "\n")

ar_fits <- vector("list", length(price_names))
names(ar_fits) <- price_names

for (j in seq_along(price_names)) {
  asset <- price_names[j]
  ret <- ret_mat[, j]

  ar_fits[[j]] <- fit_ar_yw(ret, p = ar_order)

  print_ar_fit(asset, ar_fits[[j]])
}
