# fit_mvnormal_mixture_returns.R
# Read prices, compute returns, correlations, and fit multivariate normal mixtures.

declare(type(
  aic.comp = double(),
  all_price_names = integer(),
  bic.comp = double(),
  dates = double(),
  fits = double(),
  infile = double(),
  k = integer(),
  log_price = double(),
  max.assets = integer(),
  max.comp = integer(),
  price_mat = double(),
  price_names = integer(),
  prices = double(),
  ret_dates = double(),
  ret_mat = double(),
  selected_orders = double(),
  summary_mat = double()
))

dmvnorm_log_chol <- function(x, mu, sigma) {
  declare(type(
    x = double(),
    mu = double(),
    sigma = double(),
    logdet = double(),
    n = integer(),
    p = integer(),
    q = double(),
    r = double(),
    xc = double(),
    y = double(),
    z = double()
  ))
  n <- nrow(x)
  p <- ncol(x)

  r <- try(chol(sigma), silent = TRUE)

  if (inherits(r, "try-error")) {
    return(rep(-Inf, n))
  }

  xc <- sweep(x, 2, mu, "-")
  z <- backsolve(r, t(xc), transpose = TRUE)
  q <- colSums(z * z)

  logdet <- 2.0 * sum(log(diag(r)))
  y <- -0.5 * (p * log(2.0 * pi) + logdet + q)

  return(y)
}

log_sum_exp <- function(x) {
  declare(type(
    x = double(),
    xmax = double(),
    y = double()
  ))
  xmax <- max(x)

  if (!is.finite(xmax)) {
    return(-Inf)
  }

  y <- xmax + log(sum(exp(x - xmax)))
  return(y)
}

fit_mvnormal_mixture <- function(x, ncomp, max.iter = 500, tol = 1.0e-8,
                                 ridge = 1.0e-8, seed = 123) {
  declare(type(
    x = double(),
    ncomp = integer(),
    max.iter = integer(),
    tol = double(),
    ridge = double(),
    seed = integer(),
    aic = double(),
    bic = double(),
    idx = integer(),
    iter = integer(),
    k = integer(),
    km = double(),
    logden = double(),
    logdens = double(),
    loglik = double(),
    loglik.old = double(),
    mu = double(),
    n = integer(),
    nk = double(),
    npar = double(),
    p = integer(),
    prob = double(),
    resp = double(),
    sigma = double(),
    wk = double(),
    xc = double()
  ))
# fit multivariate mixture of normals
  set.seed(seed)

  x <- x[complete.cases(x), , drop = FALSE]

  n <- nrow(x)
  p <- ncol(x)

  if (ncomp < 1) {
    stop("ncomp must be positive")
  }

  if (ncomp > n) {
    stop("ncomp must not exceed number of observations")
  }

  prob <- rep(1.0 / ncomp, ncomp)
  mu <- vector("list", ncomp)
  sigma <- vector("list", ncomp)

  if (ncomp == 1) {
    mu[[1]] <- colMeans(x)
    sigma[[1]] <- cov(x) * (n - 1.0) / n + ridge * diag(p)
  } else {
    km <- kmeans(x, centers = ncomp, nstart = 10)

    for (k in seq_len(ncomp)) {
      idx <- which(km$cluster == k)

      if (length(idx) == 0) {
        idx <- sample(seq_len(n), 1)
      }

      prob[k] <- length(idx) / n
      mu[[k]] <- colMeans(x[idx, , drop = FALSE])

      if (length(idx) > 1) {
        sigma[[k]] <- cov(x[idx, , drop = FALSE]) * (length(idx) - 1.0) / length(idx)
      } else {
        sigma[[k]] <- cov(x) * (n - 1.0) / n
      }

      sigma[[k]] <- sigma[[k]] + ridge * diag(p)
    }
  }

  logdens <- matrix(0.0, nrow = n, ncol = ncomp)
  resp <- matrix(0.0, nrow = n, ncol = ncomp)

  loglik.old <- -Inf

  for (iter in seq_len(max.iter)) {
    for (k in seq_len(ncomp)) {
      logdens[, k] <- log(prob[k]) + dmvnorm_log_chol(x, mu[[k]], sigma[[k]])
    }

    logden <- apply(logdens, 1, log_sum_exp)
    loglik <- sum(logden)

    if (!is.finite(loglik)) {
      stop("non-finite log likelihood")
    }

    for (k in seq_len(ncomp)) {
      resp[, k] <- exp(logdens[, k] - logden)
    }

    nk <- colSums(resp)
    prob <- nk / n

    for (k in seq_len(ncomp)) {
      if (nk[k] <= 0.0) {
        prob[k] <- 1.0 / n
        mu[[k]] <- x[sample(seq_len(n), 1), ]
        sigma[[k]] <- cov(x) * (n - 1.0) / n + ridge * diag(p)
      } else {
        wk <- resp[, k]

        mu[[k]] <- as.numeric(colSums(x * wk) / nk[k])

        xc <- sweep(x, 2, mu[[k]], "-")
        sigma[[k]] <- t(xc) %*% (xc * wk) / nk[k]
        sigma[[k]] <- sigma[[k]] + ridge * diag(p)
      }
    }

    if (abs(loglik - loglik.old) < tol * (1.0 + abs(loglik.old))) {
      break
    }

    loglik.old <- loglik
  }

  npar <- (ncomp - 1) + ncomp * p + ncomp * p * (p + 1) / 2
  aic <- -2.0 * loglik + 2.0 * npar
  bic <- -2.0 * loglik + log(n) * npar

  y <- list(
    ncomp = ncomp,
    nobs = n,
    ndim = p,
    prob = prob,
    mu = mu,
    sigma = sigma,
    resp = resp,
    loglik = loglik,
    npar = npar,
    aic = aic,
    bic = bic,
    iter = iter
  )

  return(y)
}

print_mixture_fit <- function(fit, asset_names) {
  declare(type(
    fit = double(),
    asset_names = double(),
    k = integer()
  ))
  cat("\n")
  cat("mixture with", fit$ncomp, "component(s)\n")
  cat("iterations =", fit$iter, "\n")
  cat("loglik     =", fit$loglik, "\n")
  cat("npar       =", fit$npar, "\n")
  cat("aic        =", fit$aic, "\n")
  cat("bic        =", fit$bic, "\n")

  cat("\nmixture probabilities\n")
  print(fit$prob)

  for (k in seq_len(fit$ncomp)) {
    cat("\ncomponent", k, "mean\n")
    names(fit$mu[[k]]) <- asset_names
    print(fit$mu[[k]])

    cat("\ncomponent", k, "covariance matrix\n")
    rownames(fit$sigma[[k]]) <- asset_names
    colnames(fit$sigma[[k]]) <- asset_names
    print(fit$sigma[[k]])
  }

  return(invisible(NULL))
}

infile <- "spy_tlt.csv"
max.comp <- 3
max.assets <- NULL
# max.assets <- 2

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
ret_mat <- diff(log_price)
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

cat("\ncorrelation matrix of returns\n")
print(cor(ret_mat, use = "pairwise.complete.obs"))

if (max.comp > 0) {
  cat("\nmultivariate normal mixture fits\n")
  cat("components fit: 1 through", max.comp, "\n")

  fits <- vector("list", max.comp)

  summary_mat <- matrix(NA_real_, nrow = max.comp, ncol = 5)
  colnames(summary_mat) <- c("ncomp", "loglik", "npar", "aic", "bic")

  for (k in seq_len(max.comp)) {
    fits[[k]] <- fit_mvnormal_mixture(ret_mat, ncomp = k)

    summary_mat[k, ] <- c(
      ncomp = fits[[k]]$ncomp,
      loglik = fits[[k]]$loglik,
      npar = fits[[k]]$npar,
      aic = fits[[k]]$aic,
      bic = fits[[k]]$bic
    )
  }

  cat("\nmixture model selection table\n")
  print(summary_mat, row.names = FALSE)

  aic.comp <- summary_mat[which.min(summary_mat[, "aic"]), "ncomp"]
  bic.comp <- summary_mat[which.min(summary_mat[, "bic"]), "ncomp"]

  cat("\nAIC chooses", aic.comp, "component(s)\n")
  cat("BIC chooses", bic.comp, "component(s)\n")

  selected_orders <- data.frame(
    criterion = c("AIC", "BIC"),
    selected_ncomp = c(aic.comp, bic.comp)
  )

  cat("\nselected mixture orders\n")
  print(selected_orders, row.names = FALSE)

  cat("\nAIC-selected fit\n")
  print_mixture_fit(fits[[aic.comp]], price_names)

  if (bic.comp != aic.comp) {
    cat("\nBIC-selected fit\n")
    print_mixture_fit(fits[[bic.comp]], price_names)
  }
} else {
  cat("\nmultivariate mixture fitting skipped because max.comp = 0\n")
}
