# vector_arma_returns_order.R
# Read prices, compute log returns, and select a vector ARMA(p, q) model.
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
  d <- det(sigma)
  if (!is.finite(d) || d <= 0.0) {
    return(NA_real_)
  }
  y <- log(d)
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

unpack_varma_par <- function(par, k, p, q) {
  i0 <- 1
  i1 <- k
  mu <- par[i0:i1]

  ar <- matrix(0.0, nrow = k, ncol = k * p)
  ma <- matrix(0.0, nrow = k, ncol = k * q)

  if (p > 0) {
    for (lag in 1:p) {
      col1 <- (lag - 1) * k + 1
      col2 <- lag * k
      i0 <- i1 + 1
      i1 <- i0 + k * k - 1
      ar[, col1:col2] <- matrix(par[i0:i1], nrow = k, ncol = k)
    }
  }

  if (q > 0) {
    for (lag in 1:q) {
      col1 <- (lag - 1) * k + 1
      col2 <- lag * k
      i0 <- i1 + 1
      i1 <- i0 + k * k - 1
      ma[, col1:col2] <- matrix(par[i0:i1], nrow = k, ncol = k)
    }
  }

  y <- list(mu = mu, ar = ar, ma = ma)
  return(y)
}

varma_residuals_order <- function(x, mu, ar, ma, p, q) {
  n <- nrow(x)
  k <- ncol(x)

  y <- sweep(x, 2, mu, "-")
  e <- matrix(0.0, nrow = n, ncol = k)

  for (t in seq_len(n)) {
    ar_part <- rep(0.0, k)
    ma_part <- rep(0.0, k)

    if (p > 0) {
      for (lag in 1:p) {
        if (t > lag) {
          col1 <- (lag - 1) * k + 1
          col2 <- lag * k
          ar_part <- ar_part + as.numeric(ar[, col1:col2, drop = FALSE] %*% y[t - lag, ])
        }
      }
    }

    if (q > 0) {
      for (lag in 1:q) {
        if (t > lag) {
          col1 <- (lag - 1) * k + 1
          col2 <- lag * k
          ma_part <- ma_part + as.numeric(ma[, col1:col2, drop = FALSE] %*% e[t - lag, ])
        }
      }
    }

    e[t, ] <- y[t, ] - ar_part - ma_part
  }

  return(e)
}

varma_negloglik_order <- function(par, x, p, q) {
  n <- nrow(x)
  k <- ncol(x)

  theta <- unpack_varma_par(par, k, p, q)
  mu <- theta$mu
  ar <- theta$ar
  ma <- theta$ma

  if (p > 0) {
    for (lag in 1:p) {
      col1 <- (lag - 1) * k + 1
      col2 <- lag * k
      if (spectral_radius_power(ar[, col1:col2, drop = FALSE]) >= 0.995) {
        return(1.0e100)
      }
    }
  }

  if (q > 0) {
    for (lag in 1:q) {
      col1 <- (lag - 1) * k + 1
      col2 <- lag * k
      if (spectral_radius_power(ma[, col1:col2, drop = FALSE]) >= 0.995) {
        return(1.0e100)
      }
    }
  }

  e <- varma_residuals_order(x, mu, ar, ma, p, q)
  start <- max(p, q) + 1
  efit <- e[start:n, , drop = FALSE]
  nfit <- nrow(efit)

  sigma <- crossprod(efit) / nfit
  ridge <- 1.0e-10 * mean(diag(sigma))
  if (!is.finite(ridge) || ridge <= 0.0) {
    ridge <- 1.0e-10
  }
  sigma <- sigma + diag(ridge, k)

  ldet <- logdet_spd(sigma)
  if (!is.finite(ldet)) {
    return(1.0e100)
  }

  y <- 0.5 * nfit * k * log(2.0 * pi) +
    0.5 * nfit * ldet +
    0.5 * nfit * k

  if (!is.finite(y)) {
    y <- 1.0e100
  }

  return(y)
}

fit_varma_order <- function(x, p, q) {
  x <- as.matrix(x)
  n <- nrow(x)
  k <- ncol(x)
  min_n <- max(p, q) + 2

  if (n <= min_n) {
    stop("need more observations")
  }

  mu0 <- colMeans(x)
  ar0 <- rep(0.0, p * k * k)
  ma0 <- rep(0.0, q * k * k)
  par0 <- numeric(k + length(ar0) + length(ma0))
  i0 <- 1
  i1 <- k
  par0[i0:i1] <- mu0
  if (p > 0) {
    i0 <- i1 + 1
    i1 <- i0 + length(ar0) - 1
    par0[i0:i1] <- ar0
  }
  if (q > 0) {
    i0 <- i1 + 1
    i1 <- i0 + length(ma0) - 1
    par0[i0:i1] <- ma0
  }

  fit <- optim(
    par = par0,
    fn = varma_negloglik_order,
    x = x,
    p = p,
    q = q,
    method = "BFGS",
    control = list(maxit = 1000, reltol = 1.0e-9)
  )

  par_vec <- fit$par
  theta <- unpack_varma_par(par_vec, k, p, q)
  mu <- theta$mu
  ar <- theta$ar
  ma <- theta$ma

  e <- varma_residuals_order(x, mu, ar, ma, p, q)
  start <- max(p, q) + 1
  efit <- e[start:n, , drop = FALSE]
  sigma <- crossprod(efit) / nrow(efit)

  npar <- length(par_vec) + k * (k + 1) / 2
  loglik <- -fit$value
  aic <- -2.0 * loglik + 2.0 * npar
  bic <- -2.0 * loglik + log(nrow(efit)) * npar

  y <- list(
    p = p,
    q = q,
    nobs = nrow(efit),
    nseries = k,
    npar = npar,
    mu = mu,
    sigma = sigma,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = fit$convergence
  )

  return(y)
}

fit_varma_orders <- function(x, p.max, q.max) {
  nmodels <- (p.max + 1) * (q.max + 1)
  table <- matrix(NA_real_, nrow = nmodels, ncol = 8)
  colnames(table) <- c("p", "q", "nobs", "npar", "loglik", "aic", "bic", "convergence")
  row <- 0
  aic_best <- Inf
  bic_best <- Inf
  aic_row <- 1
  bic_row <- 1
  aic_p <- 0
  aic_q <- 0
  bic_p <- 0
  bic_q <- 0

  for (p in 0:p.max) {
    for (q in 0:q.max) {
      row <- row + 1
      fit <- fit_varma_order(x, p, q)

      table[row, ] <- c(
        1.0 * p,
        1.0 * q,
        1.0 * fit$nobs,
        1.0 * fit$npar,
        fit$loglik,
        fit$aic,
        fit$bic,
        1.0 * fit$convergence
      )

      if (row == 1 || fit$aic < aic_best) {
        aic_best <- fit$aic
        aic_row <- row
        aic_p <- p
        aic_q <- q
      }

      if (row == 1 || fit$bic < bic_best) {
        bic_best <- fit$bic
        bic_row <- row
        bic_p <- p
        bic_q <- q
      }
    }
  }

  y <- list(
    table = table,
    aic_row = aic_row,
    bic_row = bic_row,
    aic_p = aic_p,
    aic_q = aic_q,
    bic_p = bic_p,
    bic_q = bic_q
  )

  return(y)
}

print_varma_fit <- function(fit, price_names) {
  fit_p <- fit$p
  fit_q <- fit$q
  fit_nobs <- fit$nobs
  fit_nseries <- fit$nseries
  fit_convergence <- fit$convergence
  fit_loglik <- fit$loglik
  fit_aic <- fit$aic
  fit_bic <- fit$bic

  cat("selected order = VARMA(", fit_p, ", ", fit_q, ")\n", sep = "")
  cat("number of fitted observations =", fit_nobs, "\n")
  cat("number of time series =", fit_nseries, "\n")
  cat("convergence =", fit_convergence, "\n")
  cat("log likelihood =", fit_loglik, "\n")
  cat("AIC =", fit_aic, "\n")
  cat("BIC =", fit_bic, "\n")

  cat("\nmean vector\n")
  names(fit$mu) <- price_names
  print(fit$mu)

  cat("\ninnovation covariance matrix\n")
  rownames(fit$sigma) <- price_names
  colnames(fit$sigma) <- price_names
  print(fit$sigma)
}

ret.scale = 100.0
cat("\nret.scale:", ret.scale)
infile <- "spy_tlt.csv"
max.assets <- NULL
max.returns <- 1000
p.max <- 1
q.max <- 1

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

if (!is.null(max.returns)) {
  max.returns <- min(max.returns, nrow(ret_mat))
  start.row <- nrow(ret_mat) - max.returns + 1
  ret_mat <- ret_mat[start.row:nrow(ret_mat), , drop = FALSE]
  ret_dates <- ret_dates[start.row:length(ret_dates)]
}

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

cat("\nVector ARMA order search by conditional Gaussian maximum likelihood\n")
cat("AR orders: 0 through", p.max, "\n")
cat("MA orders: 0 through", q.max, "\n")

order_search <- fit_varma_orders(ret_mat, p.max, q.max)
order_table <- order_search$table

cat("\norder selection table\n")
print(order_table)

aic_p <- order_search$aic_p
aic_q <- order_search$aic_q
bic_p <- order_search$bic_p
bic_q <- order_search$bic_q

cat("\nAIC chooses VARMA(", aic_p, ", ", aic_q, ")\n", sep = "")
cat("BIC chooses VARMA(", bic_p, ", ", bic_q, ")\n", sep = "")

aic_fit <- fit_varma_order(ret_mat, aic_p, aic_q)
bic_fit <- fit_varma_order(ret_mat, bic_p, bic_q)

cat("\nAIC-selected fit\n")
print_varma_fit(aic_fit, price_names)
cat("\nBIC-selected fit\n")
print_varma_fit(bic_fit, price_names)
