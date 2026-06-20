# Fit GARCH(1,1) and NAGARCH(1,1) with Student-t noise to log returns
# in base R, compare models by AIC/BIC, then build CCC and optionally
# DCC multivariate GARCH models from selected standardized residuals.
#
# Input file format assumed:
# Date,SPY,EFA,EEM,...
#
# Symmetric GARCH:
#   h_t = omega + alpha * e_{t-1}^2 + beta * h_{t-1}
#
# NAGARCH:
#   h_t = omega + alpha * (e_{t-1} - theta * sqrt(h_{t-1}))^2 + beta * h_{t-1}
#
# Student-t noise is standardized to have mean 0 and variance 1.
#
# CCC-GARCH:
#   H_t = D_t R D_t
#
# DCC-GARCH:
#   H_t = D_t R_t D_t
#   Q_t = (1 - a - b) Qbar + a z_{t-1} z_{t-1}' + b Q_{t-1}
#   R_t = diag(Q_t)^(-1/2) Q_t diag(Q_t)^(-1/2)

# -----------------------------
# User settings
# -----------------------------

price_file <- "asset_class_etf_prices.csv"
scale.ret <- 100

# Optional price date range.
# Use NA to leave an endpoint unrestricted.
date.min <- as.Date("2010-01-01")
date.max <- as.Date("2024-12-31")

# Maximum number of asset columns to read.
# Set max.assets <= 0 to read all assets.
max.assets <- 2

maxit <- 2000
print_each_fit <- TRUE

# Choose which univariate selection criterion feeds CCC/DCC:
# "aic" or "bic"
select.criterion <- "aic"

# Multivariate model toggles
fit.ccc <- TRUE
fit.dcc <- TRUE
fit.dcc.t <- TRUE

dcc.maxit <- 2000
dcc.t.maxit <- 2000

elapsed_start <- proc.time()[["elapsed"]]

# -----------------------------
# Read prices
# -----------------------------

dat <- read.csv(price_file, stringsAsFactors = FALSE)

if (!("Date" %in% names(dat))) {
  stop("The input file must have a Date column.")
}

dates <- as.Date(dat$Date)

if (any(is.na(dates))) {
  stop("Some entries in the Date column could not be converted to Date.")
}

ord_date <- order(dates)
dat <- dat[ord_date, , drop = FALSE]
dates <- dates[ord_date]

use_date <- rep(TRUE, length(dates))

if (!is.na(date.min)) {
  use_date <- use_date & dates >= date.min
}

if (!is.na(date.max)) {
  use_date <- use_date & dates <= date.max
}

dat <- dat[use_date, , drop = FALSE]
dates <- dates[use_date]

if (nrow(dat) < 2) {
  stop("Fewer than 2 price observations remain after applying date.min/date.max.")
}

all_price_names <- setdiff(names(dat), "Date")

if (length(all_price_names) == 0) {
  stop("No asset price columns found.")
}

if (max.assets > 0) {
  nread <- min(max.assets, length(all_price_names))
  price_names <- all_price_names[1:nread]
} else {
  price_names <- all_price_names
}

prices <- as.matrix(dat[, price_names, drop = FALSE])
storage.mode(prices) <- "double"

if (any(prices <= 0, na.rm = TRUE)) {
  stop("Prices must be positive to compute log returns.")
}

cat("\nPrice file:", price_file, "\n")
cat("date.min:", ifelse(is.na(date.min), "none", as.character(date.min)), "\n")
cat("date.max:", ifelse(is.na(date.max), "none", as.character(date.max)), "\n")

cat("\nAsset columns available:", length(all_price_names), "\n")
cat("Asset columns read     :", length(price_names), "\n")
cat("Assets read:\n")
print(price_names)

cat("\nFirst price date:", as.character(dates[1]), "\n")
cat("Last price date :", as.character(dates[length(dates)]), "\n")

# -----------------------------
# Compute scaled log returns
# -----------------------------

ret_dates <- dates[-1]
rets <- scale.ret * diff(log(prices))

colnames(rets) <- price_names

good <- complete.cases(rets)

rets <- rets[good, , drop = FALSE]
ret_dates <- ret_dates[good]

cat("\nNumber of price observations:", nrow(prices), "\n")
cat("Number of return observations:", nrow(rets), "\n")
cat("First return date:", as.character(ret_dates[1]), "\n")
cat("Last return date :", as.character(ret_dates[length(ret_dates)]), "\n")

# -----------------------------
# Return statistics
# -----------------------------

return_stats <- function(x) {
  c(
    mean = mean(x),
    sd = sd(x),
    min = min(x),
    max = max(x)
  )
}

stats <- t(apply(rets, 2, return_stats))

cat("\nReturn statistics, scaled by scale.ret =", scale.ret, "\n")
print(round(stats, 6))

elapsed_after_data <- proc.time()[["elapsed"]]

# -----------------------------
# Standardized Student-t density
# -----------------------------

log_dstdt <- function(z, nu) {
  s <- sqrt((nu - 2) / nu)
  dt(z / s, df = nu, log = TRUE) - log(s)
}

# -----------------------------
# Shared helpers
# -----------------------------

soft_ab <- function(a_raw, b_raw, persistence_max = 0.999) {
  ea <- exp(a_raw)
  eb <- exp(b_raw)
  denom <- 1 + ea + eb

  c(
    alpha_star = unname(persistence_max * ea / denom),
    beta = unname(persistence_max * eb / denom)
  )
}

make_ab_start <- function(alpha_star0, beta0, persistence_max = 0.999) {
  rem <- persistence_max - alpha_star0 - beta0

  if (rem <= 0) {
    alpha_star0 <- 0.05
    beta0 <- 0.90
    rem <- persistence_max - alpha_star0 - beta0
  }

  c(
    log(alpha_star0 / rem),
    log(beta0 / rem)
  )
}

# -----------------------------
# GARCH parameter transform
# -----------------------------

unpack_garch_par <- function(par) {
  mu <- par[1]
  omega <- exp(par[2])

  ab <- soft_ab(par[3], par[4])

  alpha <- unname(ab["alpha_star"])
  beta <- unname(ab["beta"])

  nu <- 2.01 + exp(par[5])

  list(
    mu = unname(mu),
    omega = unname(omega),
    alpha = alpha,
    beta = beta,
    nu = unname(nu),
    persistence = unname(alpha + beta)
  )
}

make_garch_start_par <- function(x) {
  mu0 <- mean(x)
  v0 <- var(x)

  alpha0 <- 0.05
  beta0 <- 0.90
  persistence0 <- alpha0 + beta0

  omega0 <- max(v0 * (1 - persistence0), 1e-8)

  ab0 <- make_ab_start(alpha0, beta0)

  nu0 <- 8

  c(
    mu0,
    log(omega0),
    ab0[1],
    ab0[2],
    log(nu0 - 2.01)
  )
}

# -----------------------------
# NAGARCH parameter transform
# -----------------------------

unpack_nagarch_par <- function(par) {
  mu <- par[1]
  omega <- exp(par[2])
  theta <- par[3]

  ab <- soft_ab(par[4], par[5])

  alpha_star <- unname(ab["alpha_star"])
  beta <- unname(ab["beta"])

  alpha <- alpha_star / (1 + theta^2)

  nu <- 2.01 + exp(par[6])

  list(
    mu = unname(mu),
    omega = unname(omega),
    alpha = unname(alpha),
    theta = unname(theta),
    beta = beta,
    nu = unname(nu),
    persistence = unname(alpha * (1 + theta^2) + beta)
  )
}

make_nagarch_start_par <- function(x) {
  mu0 <- mean(x)
  v0 <- var(x)

  theta0 <- 0.0
  alpha0 <- 0.05
  beta0 <- 0.90

  persistence0 <- alpha0 * (1 + theta0^2) + beta0

  omega0 <- max(v0 * (1 - persistence0), 1e-8)

  alpha_star0 <- alpha0 * (1 + theta0^2)
  ab0 <- make_ab_start(alpha_star0, beta0)

  nu0 <- 8

  c(
    mu0,
    log(omega0),
    theta0,
    ab0[1],
    ab0[2],
    log(nu0 - 2.01)
  )
}

# -----------------------------
# GARCH filter
# -----------------------------

garch_filter <- function(x, p) {
  mu <- p$mu
  omega <- p$omega
  alpha <- p$alpha
  beta <- p$beta

  n <- length(x)
  e <- x - mu
  h <- numeric(n)

  denom <- 1 - alpha - beta

  if (denom <= 0 || !is.finite(denom)) {
    stop("Invalid GARCH persistence in filter.")
  }

  h[1] <- max(omega / denom, 1e-8)

  for (t in 2:n) {
    h[t] <- omega + alpha * e[t - 1]^2 + beta * h[t - 1]

    if (!is.finite(h[t]) || h[t] <= 0) {
      stop("Invalid GARCH variance in filter.")
    }
  }

  list(
    resid = e,
    h = h,
    z = e / sqrt(h)
  )
}

# -----------------------------
# NAGARCH filter
# -----------------------------

nagarch_filter <- function(x, p) {
  mu <- p$mu
  omega <- p$omega
  alpha <- p$alpha
  theta <- p$theta
  beta <- p$beta

  n <- length(x)
  e <- x - mu
  h <- numeric(n)

  denom <- 1 - alpha * (1 + theta^2) - beta

  if (denom <= 0 || !is.finite(denom)) {
    stop("Invalid NAGARCH persistence in filter.")
  }

  h[1] <- max(omega / denom, 1e-8)

  for (t in 2:n) {
    h[t] <- omega +
      alpha * (e[t - 1] - theta * sqrt(h[t - 1]))^2 +
      beta * h[t - 1]

    if (!is.finite(h[t]) || h[t] <= 0) {
      stop("Invalid NAGARCH variance in filter.")
    }
  }

  list(
    resid = e,
    h = h,
    z = e / sqrt(h)
  )
}

# -----------------------------
# GARCH Student-t negative log likelihood
# -----------------------------

garch_negloglik <- function(par, x) {
  p <- unpack_garch_par(par)

  filt <- try(garch_filter(x, p), silent = TRUE)

  if (inherits(filt, "try-error")) {
    return(1e100)
  }

  z <- filt$z
  h <- filt$h
  nu <- p$nu

  loglik <- sum(log_dstdt(z, nu) - 0.5 * log(h))

  if (!is.finite(loglik)) {
    return(1e100)
  }

  -loglik
}

# -----------------------------
# NAGARCH Student-t negative log likelihood
# -----------------------------

nagarch_negloglik <- function(par, x) {
  p <- unpack_nagarch_par(par)

  filt <- try(nagarch_filter(x, p), silent = TRUE)

  if (inherits(filt, "try-error")) {
    return(1e100)
  }

  z <- filt$z
  h <- filt$h
  nu <- p$nu

  loglik <- sum(log_dstdt(z, nu) - 0.5 * log(h))

  if (!is.finite(loglik)) {
    return(1e100)
  }

  -loglik
}

# -----------------------------
# Fit one GARCH series
# -----------------------------

fit_garch_t <- function(x, maxit = 2000) {
  start <- make_garch_start_par(x)

  opt <- optim(
    par = start,
    fn = garch_negloglik,
    x = x,
    method = "BFGS",
    control = list(
      maxit = maxit,
      reltol = 1e-10
    ),
    hessian = FALSE
  )

  p <- unpack_garch_par(opt$par)
  filt <- garch_filter(x, p)

  n <- length(x)
  k <- length(opt$par)
  loglik <- -opt$value

  aic <- -2 * loglik + 2 * k
  bic <- -2 * loglik + log(n) * k

  coef <- c(
    mu = p$mu,
    omega = p$omega,
    alpha = p$alpha,
    theta = NA_real_,
    beta = p$beta,
    nu = p$nu,
    persistence = p$persistence,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = opt$convergence
  )

  list(
    model = "GARCH",
    coef = coef,
    resid = filt$resid,
    h = filt$h,
    z = filt$z,
    opt = opt
  )
}

# -----------------------------
# Fit one NAGARCH series
# -----------------------------

fit_nagarch_t <- function(x, maxit = 2000) {
  start <- make_nagarch_start_par(x)

  opt <- optim(
    par = start,
    fn = nagarch_negloglik,
    x = x,
    method = "BFGS",
    control = list(
      maxit = maxit,
      reltol = 1e-10
    ),
    hessian = FALSE
  )

  p <- unpack_nagarch_par(opt$par)
  filt <- nagarch_filter(x, p)

  n <- length(x)
  k <- length(opt$par)
  loglik <- -opt$value

  aic <- -2 * loglik + 2 * k
  bic <- -2 * loglik + log(n) * k

  coef <- c(
    mu = p$mu,
    omega = p$omega,
    alpha = p$alpha,
    theta = p$theta,
    beta = p$beta,
    nu = p$nu,
    persistence = p$persistence,
    loglik = loglik,
    aic = aic,
    bic = bic,
    convergence = opt$convergence
  )

  list(
    model = "NAGARCH",
    coef = coef,
    resid = filt$resid,
    h = filt$h,
    z = filt$z,
    opt = opt
  )
}

# -----------------------------
# DCC Gaussian helpers
# -----------------------------

dcc_unpack <- function(par) {
  ea <- exp(par[1])
  eb <- exp(par[2])
  denom <- 1 + ea + eb

  a <- 0.999 * ea / denom
  b <- 0.999 * eb / denom

  c(
    a = unname(a),
    b = unname(b),
    persistence = unname(a + b)
  )
}

dcc_start_par <- function(a0 = 0.03, b0 = 0.94) {
  persistence_max <- 0.999
  rem <- persistence_max - a0 - b0

  if (rem <= 0) {
    a0 <- 0.03
    b0 <- 0.90
    rem <- persistence_max - a0 - b0
  }

  c(
    log(a0 / rem),
    log(b0 / rem)
  )
}

dcc_negloglik <- function(par, Z) {
  ab <- dcc_unpack(par)

  a <- unname(ab["a"])
  b <- unname(ab["b"])

  n <- nrow(Z)

  Qbar <- cov(Z)
  Qt <- Qbar

  nll <- 0

  for (t in 2:n) {
    zlag <- matrix(Z[t - 1, ], ncol = 1)

    Qt <- (1 - a - b) * Qbar + a * (zlag %*% t(zlag)) + b * Qt

    d <- sqrt(diag(Qt))

    if (any(!is.finite(d)) || any(d <= 0)) {
      return(1e100)
    }

    Rt <- Qt / outer(d, d)

    chol_R <- try(chol(Rt), silent = TRUE)

    if (inherits(chol_R, "try-error")) {
      return(1e100)
    }

    logdet_R <- 2 * sum(log(diag(chol_R)))

    zt <- Z[t, ]

    qform <- sum(backsolve(chol_R, zt, transpose = TRUE)^2)

    nll <- nll + 0.5 * (logdet_R + qform)
  }

  if (!is.finite(nll)) {
    return(1e100)
  }

  nll
}

dcc_filter <- function(Z, par) {
  ab <- dcc_unpack(par)

  a <- unname(ab["a"])
  b <- unname(ab["b"])

  n <- nrow(Z)
  k <- ncol(Z)

  Qbar <- cov(Z)
  Qt <- Qbar

  R_array <- array(NA_real_, dim = c(k, k, n))
  dimnames(R_array) <- list(colnames(Z), colnames(Z), rownames(Z))

  R_array[, , 1] <- cov2cor(Qbar)

  for (t in 2:n) {
    zlag <- matrix(Z[t - 1, ], ncol = 1)

    Qt <- (1 - a - b) * Qbar + a * (zlag %*% t(zlag)) + b * Qt

    d <- sqrt(diag(Qt))
    Rt <- Qt / outer(d, d)

    R_array[, , t] <- Rt
  }

  list(
    a = a,
    b = b,
    persistence = unname(a + b),
    Qbar = Qbar,
    R_array = R_array,
    R_last = R_array[, , n]
  )
}

fit_dcc <- function(Z, maxit = 2000) {
  start <- dcc_start_par()

  opt <- optim(
    par = start,
    fn = dcc_negloglik,
    Z = Z,
    method = "BFGS",
    control = list(
      maxit = maxit,
      reltol = 1e-10
    ),
    hessian = FALSE
  )

  filt <- dcc_filter(Z, opt$par)

  n <- nrow(Z)
  kpar <- length(opt$par)
  loglik <- -opt$value

  coef <- c(
    a = filt$a,
    b = filt$b,
    persistence = filt$persistence,
    loglik = loglik,
    aic = -2 * loglik + 2 * kpar,
    bic = -2 * loglik + log(n) * kpar,
    convergence = opt$convergence
  )

  list(
    coef = coef,
    R_array = filt$R_array,
    R_last = filt$R_last,
    Qbar = filt$Qbar,
    opt = opt
  )
}

# -----------------------------
# DCC Student-t helpers
# -----------------------------

dcc_t_unpack <- function(par) {
  ab <- dcc_unpack(par[1:2])

  nu <- 2.01 + exp(par[3])

  c(
    a = unname(ab["a"]),
    b = unname(ab["b"]),
    persistence = unname(ab["persistence"]),
    nu = unname(nu)
  )
}

dcc_t_start_par <- function(a0 = 0.03, b0 = 0.94, nu0 = 8) {
  c(
    dcc_start_par(a0 = a0, b0 = b0),
    log(nu0 - 2.01)
  )
}

dcc_t_negloglik <- function(par, Z) {
  pp <- dcc_t_unpack(par)

  a <- unname(pp["a"])
  b <- unname(pp["b"])
  nu <- unname(pp["nu"])

  n <- nrow(Z)
  k <- ncol(Z)

  Qbar <- cov(Z)
  Qt <- Qbar

  nll <- 0

  const <- lgamma((nu + k) / 2) -
    lgamma(nu / 2) -
    0.5 * k * log((nu - 2) * pi)

  for (t in 2:n) {
    zlag <- matrix(Z[t - 1, ], ncol = 1)

    Qt <- (1 - a - b) * Qbar + a * (zlag %*% t(zlag)) + b * Qt

    d <- sqrt(diag(Qt))

    if (any(!is.finite(d)) || any(d <= 0)) {
      return(1e100)
    }

    Rt <- Qt / outer(d, d)

    chol_R <- try(chol(Rt), silent = TRUE)

    if (inherits(chol_R, "try-error")) {
      return(1e100)
    }

    logdet_R <- 2 * sum(log(diag(chol_R)))

    zt <- Z[t, ]

    qform <- sum(backsolve(chol_R, zt, transpose = TRUE)^2)

    loglik_t <- const -
      0.5 * logdet_R -
      0.5 * (nu + k) * log(1 + qform / (nu - 2))

    nll <- nll - loglik_t
  }

  if (!is.finite(nll)) {
    return(1e100)
  }

  nll
}

fit_dcc_t <- function(Z, maxit = 2000) {
  start <- dcc_t_start_par()

  opt <- optim(
    par = start,
    fn = dcc_t_negloglik,
    Z = Z,
    method = "BFGS",
    control = list(
      maxit = maxit,
      reltol = 1e-10
    ),
    hessian = FALSE
  )

  pp <- dcc_t_unpack(opt$par)

  filt <- dcc_filter(Z, opt$par[1:2])

  n <- nrow(Z)
  kpar <- length(opt$par)
  loglik <- -opt$value

  coef <- c(
    a = unname(pp["a"]),
    b = unname(pp["b"]),
    persistence = unname(pp["persistence"]),
    nu = unname(pp["nu"]),
    loglik = loglik,
    aic = -2 * loglik + 2 * kpar,
    bic = -2 * loglik + log(n) * kpar,
    convergence = opt$convergence
  )

  list(
    coef = coef,
    R_array = filt$R_array,
    R_last = filt$R_last,
    Qbar = filt$Qbar,
    opt = opt
  )
}

# -----------------------------
# Conditional covariance helpers
# -----------------------------

make_ccc_covariances <- function(hmat, R) {
  n <- nrow(hmat)
  k <- ncol(hmat)

  H_array <- array(NA_real_, dim = c(k, k, n))
  dimnames(H_array) <- list(colnames(hmat), colnames(hmat), rownames(hmat))

  for (t in seq_len(n)) {
    D_t <- diag(sqrt(hmat[t, ]), nrow = k, ncol = k)
    H_array[, , t] <- D_t %*% R %*% D_t
  }

  H_array
}

make_dcc_covariances <- function(hmat, R_array) {
  n <- nrow(hmat)
  k <- ncol(hmat)

  H_array <- array(NA_real_, dim = c(k, k, n))
  dimnames(H_array) <- list(colnames(hmat), colnames(hmat), rownames(hmat))

  for (t in seq_len(n)) {
    D_t <- diag(sqrt(hmat[t, ]), nrow = k, ncol = k)
    H_array[, , t] <- D_t %*% R_array[, , t] %*% D_t
  }

  H_array
}

# -----------------------------
# Fit all selected series
# -----------------------------

fit_cols <- c(
  "mu",
  "omega",
  "alpha",
  "theta",
  "beta",
  "nu",
  "persistence",
  "loglik",
  "aic",
  "bic",
  "convergence"
)

nasset <- ncol(rets)
asset_names <- colnames(rets)

garch_fit <- matrix(NA_real_, nrow = nasset, ncol = length(fit_cols))
nagarch_fit <- matrix(NA_real_, nrow = nasset, ncol = length(fit_cols))

rownames(garch_fit) <- asset_names
colnames(garch_fit) <- fit_cols

rownames(nagarch_fit) <- asset_names
colnames(nagarch_fit) <- fit_cols

garch_objects <- vector("list", nasset)
nagarch_objects <- vector("list", nasset)

names(garch_objects) <- asset_names
names(nagarch_objects) <- asset_names

elapsed_univar_start <- proc.time()[["elapsed"]]

for (j in seq_len(nasset)) {
  sym <- asset_names[j]
  x <- rets[, j]

  if (print_each_fit) {
    cat("\nFitting", sym, "with symmetric GARCH Student-t\n")
  }

  garch_objects[[j]] <- fit_garch_t(x, maxit = maxit)
  garch_fit[j, ] <- garch_objects[[j]]$coef

  if (print_each_fit) {
    print(round(garch_fit[j, ], 6))
  }

  if (print_each_fit) {
    cat("\nFitting", sym, "with NAGARCH Student-t\n")
  }

  nagarch_objects[[j]] <- fit_nagarch_t(x, maxit = maxit)
  nagarch_fit[j, ] <- nagarch_objects[[j]]$coef

  if (print_each_fit) {
    print(round(nagarch_fit[j, ], 6))
  }
}

elapsed_univar_end <- proc.time()[["elapsed"]]

cat("\nSymmetric GARCH(1,1) Student-t fit summary\n")
print(round(garch_fit, 6))

cat("\nNAGARCH(1,1) Student-t fit summary\n")
print(round(nagarch_fit, 6))

cat("\nConvergence code 0 usually means optim converged successfully.\n")

# -----------------------------
# Model comparison by AIC and BIC
# -----------------------------

chosen_by_aic <- ifelse(
  garch_fit[, "aic"] <= nagarch_fit[, "aic"],
  "GARCH",
  "NAGARCH"
)

chosen_by_bic <- ifelse(
  garch_fit[, "bic"] <= nagarch_fit[, "bic"],
  "GARCH",
  "NAGARCH"
)

comparison <- data.frame(
  asset = asset_names,
  garch_aic = garch_fit[, "aic"],
  nagarch_aic = nagarch_fit[, "aic"],
  chosen_by_aic = chosen_by_aic,
  garch_bic = garch_fit[, "bic"],
  nagarch_bic = nagarch_fit[, "bic"],
  chosen_by_bic = chosen_by_bic,
  stringsAsFactors = FALSE
)

cat("\nModel comparison by AIC and BIC\n")
print(comparison, row.names = FALSE)

garch_aic_assets <- asset_names[chosen_by_aic == "GARCH"]
nagarch_aic_assets <- asset_names[chosen_by_aic == "NAGARCH"]

garch_bic_assets <- asset_names[chosen_by_bic == "GARCH"]
nagarch_bic_assets <- asset_names[chosen_by_bic == "NAGARCH"]

cat("\nAssets for which GARCH is chosen by AIC:\n")
print(garch_aic_assets)
cat("Number of assets for which GARCH is chosen by AIC:", length(garch_aic_assets), "\n")

cat("\nAssets for which NAGARCH is chosen by AIC:\n")
print(nagarch_aic_assets)
cat("Number of assets for which NAGARCH is chosen by AIC:", length(nagarch_aic_assets), "\n")

cat("\nAssets for which GARCH is chosen by BIC:\n")
print(garch_bic_assets)
cat("Number of assets for which GARCH is chosen by BIC:", length(garch_bic_assets), "\n")

cat("\nAssets for which NAGARCH is chosen by BIC:\n")
print(nagarch_bic_assets)
cat("Number of assets for which NAGARCH is chosen by BIC:", length(nagarch_bic_assets), "\n")

cat("\nAIC choice counts:\n")
print(table(chosen_by_aic))

cat("\nBIC choice counts:\n")
print(table(chosen_by_bic))

# -----------------------------
# Build selected univariate filters
# -----------------------------

if (!(select.criterion %in% c("aic", "bic"))) {
  stop("select.criterion must be either 'aic' or 'bic'.")
}

if (select.criterion == "aic") {
  chosen_model <- chosen_by_aic
} else {
  chosen_model <- chosen_by_bic
}

selected_objects <- vector("list", nasset)
names(selected_objects) <- asset_names

selected_h <- matrix(NA_real_, nrow = nrow(rets), ncol = nasset)
selected_z <- matrix(NA_real_, nrow = nrow(rets), ncol = nasset)
selected_resid <- matrix(NA_real_, nrow = nrow(rets), ncol = nasset)

colnames(selected_h) <- asset_names
colnames(selected_z) <- asset_names
colnames(selected_resid) <- asset_names

rownames(selected_h) <- as.character(ret_dates)
rownames(selected_z) <- as.character(ret_dates)
rownames(selected_resid) <- as.character(ret_dates)

for (j in seq_len(nasset)) {
  if (chosen_model[j] == "GARCH") {
    selected_objects[[j]] <- garch_objects[[j]]
  } else {
    selected_objects[[j]] <- nagarch_objects[[j]]
  }

  selected_h[, j] <- selected_objects[[j]]$h
  selected_z[, j] <- selected_objects[[j]]$z
  selected_resid[, j] <- selected_objects[[j]]$resid
}

cat("\nSelected univariate models used for CCC/DCC, criterion =", select.criterion, "\n")

selected_table <- data.frame(
  asset = asset_names,
  selected_model = chosen_model,
  stringsAsFactors = FALSE
)

print(selected_table, row.names = FALSE)

# -----------------------------
# Build standardized residual matrix Z
# -----------------------------

Z <- selected_z

good_z <- complete.cases(Z)

for (j in seq_len(ncol(Z))) {
  good_z <- good_z & is.finite(Z[, j])
}

Z_ccc <- Z[good_z, , drop = FALSE]
ret_dates_ccc <- ret_dates[good_z]

if (nrow(Z_ccc) < 2) {
  stop("Fewer than 2 valid standardized residual observations for multivariate GARCH.")
}

selected_h_ccc <- selected_h[good_z, , drop = FALSE]

cat("\nStandardized residual matrix Z for multivariate GARCH\n")
cat("Rows:", nrow(Z_ccc), "\n")
cat("Columns:", ncol(Z_ccc), "\n")
cat("First multivariate date:", as.character(ret_dates_ccc[1]), "\n")
cat("Last multivariate date :", as.character(ret_dates_ccc[length(ret_dates_ccc)]), "\n")

cat("\nStandardized residual summary\n")
z_stats <- t(apply(Z_ccc, 2, return_stats))
print(round(z_stats, 6))

# -----------------------------
# Fit multivariate models
# -----------------------------

elapsed_multivar_start <- proc.time()[["elapsed"]]

elapsed_ccc_start <- NA_real_
elapsed_ccc_end <- NA_real_

elapsed_dcc_start <- NA_real_
elapsed_dcc_end <- NA_real_

elapsed_dcc_t_start <- NA_real_
elapsed_dcc_t_end <- NA_real_

if (nasset < 2) {
  cat("\nOnly one asset selected, so CCC/DCC correlation models are skipped.\n")
} else {
  if (fit.ccc) {
    elapsed_ccc_start <- proc.time()[["elapsed"]]

    R_ccc <- cor(Z_ccc)

    H_ccc_array <- make_ccc_covariances(selected_h_ccc, R_ccc)

    elapsed_ccc_end <- proc.time()[["elapsed"]]

    cat("\nCCC correlation matrix of selected standardized residuals\n")
    print(round(R_ccc, 6))

    H_ccc_last <- H_ccc_array[, , dim(H_ccc_array)[3]]

    cat("\nLast CCC conditional covariance matrix H_t\n")
    print(round(H_ccc_last, 6))

    ccc_last_vol <- sqrt(diag(H_ccc_last))
    names(ccc_last_vol) <- asset_names

    cat("\nLast conditional standard deviations from CCC model\n")
    print(round(ccc_last_vol, 6))

    cat("\nCCC covariance array dimensions:\n")
    print(dim(H_ccc_array))
  }

  if (fit.dcc) {
    cat("\nFitting DCC(1,1) Gaussian pseudo-likelihood model\n")

    elapsed_dcc_start <- proc.time()[["elapsed"]]

    dcc_fit <- fit_dcc(Z_ccc, maxit = dcc.maxit)

    H_dcc_array <- make_dcc_covariances(selected_h_ccc, dcc_fit$R_array)

    elapsed_dcc_end <- proc.time()[["elapsed"]]

    cat("\nDCC(1,1) Gaussian fit summary\n")
    print(round(dcc_fit$coef, 6))

    cat("\nLast DCC Gaussian conditional correlation matrix R_t\n")
    print(round(dcc_fit$R_last, 6))

    H_dcc_last <- H_dcc_array[, , dim(H_dcc_array)[3]]

    cat("\nLast DCC Gaussian conditional covariance matrix H_t\n")
    print(round(H_dcc_last, 6))

    dcc_last_vol <- sqrt(diag(H_dcc_last))
    names(dcc_last_vol) <- asset_names

    cat("\nLast conditional standard deviations from DCC Gaussian model\n")
    print(round(dcc_last_vol, 6))

    cat("\nDCC Gaussian covariance array dimensions:\n")
    print(dim(H_dcc_array))
  }

  if (fit.dcc.t) {
    cat("\nFitting DCC(1,1) Student-t pseudo-likelihood model\n")

    elapsed_dcc_t_start <- proc.time()[["elapsed"]]

    dcc_t_fit <- fit_dcc_t(Z_ccc, maxit = dcc.t.maxit)

    H_dcc_t_array <- make_dcc_covariances(selected_h_ccc, dcc_t_fit$R_array)

    elapsed_dcc_t_end <- proc.time()[["elapsed"]]

    cat("\nDCC(1,1) Student-t fit summary\n")
    print(round(dcc_t_fit$coef, 6))

    cat("\nLast DCC Student-t conditional correlation matrix R_t\n")
    print(round(dcc_t_fit$R_last, 6))

    H_dcc_t_last <- H_dcc_t_array[, , dim(H_dcc_t_array)[3]]

    cat("\nLast DCC Student-t conditional covariance matrix H_t\n")
    print(round(H_dcc_t_last, 6))

    dcc_t_last_vol <- sqrt(diag(H_dcc_t_last))
    names(dcc_t_last_vol) <- asset_names

    cat("\nLast conditional standard deviations from DCC Student-t model\n")
    print(round(dcc_t_last_vol, 6))

    cat("\nDCC Student-t covariance array dimensions:\n")
    print(dim(H_dcc_t_array))
  }
}

elapsed_multivar_end <- proc.time()[["elapsed"]]

# -----------------------------
# Timing
# -----------------------------

elapsed_end <- proc.time()[["elapsed"]]

cat("\nElapsed time (seconds)\n")
cat("read data:", elapsed_after_data - elapsed_start, "\n")
cat("fit univariate garch models:", elapsed_univar_end - elapsed_univar_start, "\n")
cat("fit multivariate garch models:", elapsed_multivar_end - elapsed_multivar_start, "\n")

if (is.finite(elapsed_ccc_start) && is.finite(elapsed_ccc_end)) {
  cat("fit CCC model:", elapsed_ccc_end - elapsed_ccc_start, "\n")
}

if (is.finite(elapsed_dcc_start) && is.finite(elapsed_dcc_end)) {
  cat("fit DCC Gaussian model:", elapsed_dcc_end - elapsed_dcc_start, "\n")
}

if (is.finite(elapsed_dcc_t_start) && is.finite(elapsed_dcc_t_end)) {
  cat("fit DCC Student-t model:", elapsed_dcc_t_end - elapsed_dcc_t_start, "\n")
}

cat("overall:", elapsed_end - elapsed_start, "\n")
