# return_stats_corr_ar_var_yw.R
# Read prices, compute log returns, print stats, correlations,
# autocorrelations, cross-correlations, AR fits, and optional VAR Yule-Walker fit.

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

ccf_matrix <- function(x, lag) {
  n <- nrow(x)

  if (lag == 0) {
    y <- cor(x, use = "pairwise.complete.obs")
  } else if (lag < n) {
    y <- cor(x[(lag + 1):n, , drop = FALSE],
             x[1:(n - lag), , drop = FALSE],
             use = "pairwise.complete.obs")
  } else {
    y <- matrix(NA_real_, nrow = ncol(x), ncol = ncol(x))
    rownames(y) <- colnames(x)
    colnames(y) <- colnames(x)
  }

  return(y)
}

autocov_matrices <- function(x, maxlag) {
  x <- x[complete.cases(x), , drop = FALSE]

  n <- nrow(x)
  p <- ncol(x)

  x <- sweep(x, 2, colMeans(x), "-")

  gamma <- vector("list", maxlag + 1)

  for (lag in 0:maxlag) {
    gamma[[lag + 1]] <- t(x[(lag + 1):n, , drop = FALSE]) %*%
      x[1:(n - lag), , drop = FALSE] / n
  }

  names(gamma) <- paste0("lag", 0:maxlag)

  return(gamma)
}

gamma_lag <- function(gamma, lag) {
  if (lag >= 0) {
    y <- gamma[[lag + 1]]
  } else {
    y <- t(gamma[[-lag + 1]])
  }

  return(y)
}

fit_var_yw <- function(x, p) {
  x <- x[complete.cases(x), , drop = FALSE]

  n <- nrow(x)
  m <- ncol(x)

  if (p < 0) {
    stop("VAR order must be nonnegative")
  }

  if (p >= n) {
    stop("VAR order must be less than the number of observations")
  }

  xmean <- colMeans(x)
  gamma <- autocov_matrices(x, p)

  if (p == 0) {
    a <- vector("list", 0)
    intercept <- xmean
    sigma <- gamma[[1]]
  } else {
    rmat <- matrix(0.0, nrow = p * m, ncol = p * m)
    cmat <- matrix(0.0, nrow = p * m, ncol = m)

    for (i in seq_len(p)) {
      rows <- ((i - 1) * m + 1):(i * m)

      cmat[rows, ] <- t(gamma[[i + 1]])

      for (j in seq_len(p)) {
        cols <- ((j - 1) * m + 1):(j * m)
        rmat[rows, cols] <- gamma_lag(gamma, j - i)
      }
    }

    beta <- solve(rmat, cmat)

    a <- vector("list", p)

    for (i in seq_len(p)) {
      rows <- ((i - 1) * m + 1):(i * m)
      a[[i]] <- t(beta[rows, , drop = FALSE])
      rownames(a[[i]]) <- colnames(x)
      colnames(a[[i]]) <- colnames(x)
    }

    asum <- matrix(0.0, nrow = m, ncol = m)

    for (i in seq_len(p)) {
      asum <- asum + a[[i]]
    }

    intercept <- as.numeric((diag(m) - asum) %*% xmean)
    names(intercept) <- colnames(x)

    sigma <- gamma[[1]] - t(cmat) %*% beta
    sigma <- 0.5 * (sigma + t(sigma))
  }

  eig <- eigen(sigma, symmetric = TRUE, only.values = TRUE)$values

  if (min(eig) <= 0.0) {
    sigma <- sigma + (abs(min(eig)) + 1.0e-10) * diag(m)
  }

  logdet <- determinant(sigma, logarithm = TRUE)$modulus[1]
  loglik <- -0.5 * n * (m * log(2.0 * pi) + logdet + m)

  npar <- m + p * m * m + m * (m + 1) / 2
  aic <- -2.0 * loglik + 2.0 * npar
  bic <- -2.0 * loglik + log(n) * npar

  rownames(sigma) <- colnames(x)
  colnames(sigma) <- colnames(x)

  y <- list(
    order = p,
    nobs = n,
    mean = xmean,
    intercept = intercept,
    a = a,
    sigma = sigma,
    loglik = loglik,
    aic = aic,
    bic = bic
  )

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

  sigma2 <- max(sigma2, .Machine$double.eps)

  npar <- p + 2
  aic <- n * log(sigma2) + 2.0 * npar
  bic <- n * log(sigma2) + log(n) * npar

  y <- list(
    order = p,
    nobs = n,
    mean = xmean,
    intercept = intercept,
    phi = phi,
    sigma2 = sigma2,
    sd = sqrt(sigma2),
    aic = aic,
    bic = bic
  )

  return(y)
}

coef_row <- function(fit, nacf) {
  y <- rep(NA_real_, nacf + 5)
  names(y) <- c("order", "intercept", paste0("phi", seq_len(nacf)),
                "sigma2", "aic", "bic")

  y["order"] <- fit$order
  y["intercept"] <- fit$intercept

  if (fit$order > 0) {
    y[paste0("phi", seq_len(fit$order))] <- fit$phi
  }

  y["sigma2"] <- fit$sigma2
  y["aic"] <- fit$aic
  y["bic"] <- fit$bic

  return(y)
}

print_var_yw_fit <- function(fit) {
  cat("\nVAR(", fit$order, ") Yule-Walker fit\n", sep = "")
  cat("number of observations =", fit$nobs, "\n")
  cat("loglik =", fit$loglik, "\n")
  cat("aic    =", fit$aic, "\n")
  cat("bic    =", fit$bic, "\n")

  cat("\nVAR intercept\n")
  print(fit$intercept)

  if (fit$order > 0) {
    for (i in seq_len(fit$order)) {
      cat("\nVAR coefficient matrix A", i, "\n", sep = "")
      cat("rows are dependent variables, columns are lagged variables\n")
      print(fit$a[[i]])
    }
  }

  cat("\nVAR residual covariance matrix\n")
  print(fit$sigma)

  return(invisible(NULL))
}

infile <- "spy_tlt.csv"
nacf <- 10
nccf <- 5
max.assets <- NULL
ret.scale = 100.0
cat("ret.scale:", ret.scale, "\n")
# max.assets <- 2

fit.var.yw <- TRUE
var.order <- 1

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

cat("\nfirst", nacf, "autocorrelations of returns\n")

acf_mat <- matrix(NA_real_, nrow = length(price_names), ncol = nacf)
rownames(acf_mat) <- price_names
colnames(acf_mat) <- paste0("lag", seq_len(nacf))

for (j in seq_along(price_names)) {
  acf_mat[j, ] <- acf_values(ret_mat[, j], nacf)
}

print(acf_mat)

if (nccf > 0) {
  cat("\nlagged cross-correlations of returns\n")
  cat("entry i,j at lag k is cor(asset_i[t], asset_j[t-k])\n")

  for (lag in 0:nccf) {
    cat("\nlag", lag, "cross-correlation matrix\n")
    print(ccf_matrix(ret_mat, lag))
  }
}

cat("\nAR models fit by Yule-Walker equations\n")
cat("orders fit: 0 through", nacf, "\n")

for (j in seq_along(price_names)) {
  asset <- price_names[j]
  ret <- ret_mat[, j]

  cat("\n")
  cat(asset, "AR coefficient table\n")

  coef_names <- c("order", "intercept", paste0("phi", seq_len(nacf)),
                  "sigma2", "aic", "bic")

  coef_mat <- matrix(NA_real_, nrow = nacf + 1, ncol = length(coef_names))
  colnames(coef_mat) <- coef_names

  for (p in 0:nacf) {
    fit <- fit_ar_yw(ret, p)
    coef_mat[p + 1, ] <- coef_row(fit, nacf)
  }

  print(coef_mat, row.names = FALSE)

  aic_order <- coef_mat[which.min(coef_mat[, "aic"]), "order"]
  bic_order <- coef_mat[which.min(coef_mat[, "bic"]), "order"]

  cat("\n")
  cat(asset, "AIC chooses AR order", aic_order, "\n")
  cat(asset, "BIC chooses AR order", bic_order, "\n")
}

if (fit.var.yw) {
  cat("\nVAR model fit by Yule-Walker equations\n")
  cat("VAR order =", var.order, "\n")

  var_fit <- fit_var_yw(ret_mat, p = var.order)
  print_var_yw_fit(var_fit)
}
