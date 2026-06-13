# vector_arma_returns.R
# Read prices, compute log returns, and fit a vector ARMA(1, 1) model.
# The model dimension is inferred from the number of return columns.

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

logdet_spd <- function(sigma) {
  r <- chol(sigma)
  y <- 2.0 * sum(log(diag(r)))
  return(y)
}

spectral_radius_power <- function(a) {
  k <- ncol(a)
  if (k == 0) {
    return(0.0)
  }

  v <- rep(1.0 / sqrt(k), k)
  rho <- 0.0

  for (iter in 1:80) {
    w <- as.numeric(a %*% v)
    nw <- sqrt(sum(w^2))
    if (nw <= 0.0) {
      return(0.0)
    }
    v <- w / nw
    rho <- nw
  }

  return(rho)
}

varma_residuals <- function(x, mu, a, m) {
  n <- nrow(x)
  k <- ncol(x)

  y <- sweep(x, 2, mu, "-")
  e <- matrix(0.0, nrow = n, ncol = k)

  for (t in seq_len(n)) {
    ar_part <- rep(0.0, k)
    ma_part <- rep(0.0, k)

    if (t > 1) {
      ar_part <- as.numeric(a %*% y[t - 1, ])
      ma_part <- as.numeric(m %*% e[t - 1, ])
    }

    e[t, ] <- y[t, ] - ar_part - ma_part
  }

  return(e)
}

varma_negloglik <- function(par, x) {
  n <- nrow(x)
  k <- ncol(x)

  i0 <- 1
  i1 <- k
  mu <- par[i0:i1]

  i0 <- i1 + 1
  i1 <- i0 + k * k - 1
  a <- matrix(par[i0:i1], nrow = k, ncol = k)

  i0 <- i1 + 1
  i1 <- i0 + k * k - 1
  m <- matrix(par[i0:i1], nrow = k, ncol = k)

  if (spectral_radius_power(a) >= 0.995) {
    return(1.0e100)
  }

  if (spectral_radius_power(m) >= 0.995) {
    return(1.0e100)
  }

  e <- varma_residuals(x, mu, a, m)
  efit <- e[2:n, , drop = FALSE]
  nfit <- nrow(efit)

  sigma <- crossprod(efit) / nfit
  ridge <- 1.0e-10 * mean(diag(sigma))
  if (!is.finite(ridge) || ridge <= 0.0) {
    ridge <- 1.0e-10
  }
  sigma <- sigma + diag(ridge, k)

  y <- 0.5 * nfit * k * log(2.0 * pi) +
    0.5 * nfit * logdet_spd(sigma) +
    0.5 * nfit * k

  if (!is.finite(y)) {
    y <- 1.0e100
  }

  return(y)
}

fit_varma11 <- function(x) {
  x <- as.matrix(x)
  n <- nrow(x)
  k <- ncol(x)

  if (n <= 3) {
    stop("need more observations")
  }

  mu0 <- colMeans(x)
  a0 <- matrix(0.0, nrow = k, ncol = k)
  m0 <- matrix(0.0, nrow = k, ncol = k)

  par0 <- c(mu0, as.numeric(a0), as.numeric(m0))

  fit <- optim(
    par = par0,
    fn = varma_negloglik,
    x = x,
    method = "BFGS",
    control = list(maxit = 1000, reltol = 1.0e-9)
  )

  par <- fit$par

  i0 <- 1
  i1 <- k
  mu <- par[i0:i1]

  i0 <- i1 + 1
  i1 <- i0 + k * k - 1
  a <- matrix(par[i0:i1], nrow = k, ncol = k)

  i0 <- i1 + 1
  i1 <- i0 + k * k - 1
  m <- matrix(par[i0:i1], nrow = k, ncol = k)

  e <- varma_residuals(x, mu, a, m)
  efit <- e[2:n, , drop = FALSE]
  sigma <- crossprod(efit) / nrow(efit)

  npar <- length(par) + k * (k + 1) / 2
  loglik <- -fit$value
  aic <- -2.0 * loglik + 2.0 * npar
  bic <- -2.0 * loglik + log(nrow(efit)) * npar

  y <- list(
    nobs = nrow(efit),
    nseries = k,
    mu = mu,
    ar = a,
    ma = m,
    sigma = sigma,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = fit$convergence
  )

  return(y)
}

ret.scale = 100
cat("\nret.scale:", ret.scale)
infile <- "spy_tlt.csv"
max.assets <- NULL

prices <- read.csv(infile, stringsAsFactors = FALSE)
dates <- as.Date(as.character(prices$Date), format = "%Y%m%d")

all_price_names <- setdiff(names(prices), "Date")

if (!is.null(max.assets)) {
  max.assets <- min(max.assets, length(all_price_names))
  price_names <- all_price_names[seq_len(max.assets)]
} else {
  price_names <- all_price_names
}

price_mat <- as.matrix(prices[, price_names, drop = FALSE])
storage.mode(price_mat) <- "double"

log_price <- log(price_mat)
ret_mat <- ret.scale * diff(log_price)
colnames(ret_mat) <- price_names

ret_dates <- dates[-1]

cat("input file =", infile, "\n")
cat("number of assets read =", length(price_names), "\n")
cat("assets read\n")
print(price_names)

cat("\n")
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

cat("\nVector ARMA(1, 1) fit by conditional Gaussian maximum likelihood\n")

varma_fit <- fit_varma11(ret_mat)

fit_nobs <- varma_fit$nobs
fit_nseries <- varma_fit$nseries
fit_convergence <- varma_fit$convergence
fit_loglik <- varma_fit$loglik
fit_aic <- varma_fit$aic
fit_bic <- varma_fit$bic

cat("number of fitted observations =", fit_nobs, "\n")
cat("number of time series =", fit_nseries, "\n")
cat("convergence =", fit_convergence, "\n")
cat("log likelihood =", fit_loglik, "\n")
cat("AIC =", fit_aic, "\n")
cat("BIC =", fit_bic, "\n")

cat("\nmean vector\n")
names(varma_fit$mu) <- price_names
print(varma_fit$mu)

cat("\nAR coefficient matrix\n")
rownames(varma_fit$ar) <- price_names
colnames(varma_fit$ar) <- price_names
print(varma_fit$ar)

cat("\nMA coefficient matrix\n")
rownames(varma_fit$ma) <- price_names
colnames(varma_fit$ma) <- price_names
print(varma_fit$ma)

cat("\ninnovation covariance matrix\n")
rownames(varma_fit$sigma) <- price_names
colnames(varma_fit$sigma) <- price_names
print(varma_fit$sigma)
