# Simulate European call option prices assuming daily returns follow a GARCH(1,1)
# process over 20 trading days.
#
# No plotting and no xtabs.
#
# The script:
# 1. Simulates npaths risk-neutral GARCH(1,1) price paths.
# 2. Computes Monte Carlo European call prices by strike.
# 3. Converts those prices to Black-Scholes implied vols.
# 4. Prints the excess kurtosis of the terminal log price distribution.

set.seed(123)

cat("European call prices and implied vols from GARCH(1,1) Monte Carlo\n\n")

# -----------------------------
# Parameters
# -----------------------------

s0 <- 100.0

r <- 0.04
q <- 0.015

trading_days_per_year <- 252
n_days <- 50
npaths <- 100000
dt <- 1.0 / trading_days_per_year
tau <- n_days / trading_days_per_year

strikes <- c(80, 90, 95, 100, 105, 110, 120)

# Annualized target volatility used to calibrate the unconditional daily variance.
sigma_annual <- 0.25
var_daily_uncond <- sigma_annual^2 / trading_days_per_year

# GARCH(1,1) parameters for daily returns:
#
# h_t = omega + alpha * eps_{t-1}^2 + beta * h_{t-1}
# eps_t = sqrt(h_t) * z_t
#
# Need alpha + beta < 1 for covariance stationarity.

alpha <- 0.06
beta <- 0.92
omega <- var_daily_uncond * (1.0 - alpha - beta)

h0 <- var_daily_uncond

cat("GARCH parameters:\n")
cat("omega =", omega, "\n")
cat("alpha =", alpha, "\n")
cat("beta  =", beta, "\n")
cat("alpha + beta =", alpha + beta, "\n")
cat("unconditional daily variance =", omega / (1 - alpha - beta), "\n")
cat("unconditional annual vol =", sqrt(252 * omega / (1 - alpha - beta)), "\n\n")


# -----------------------------
# Black-Scholes-Merton functions
# -----------------------------

bs_call <- function(s, k, tau, r, q, sigma) {
  if (tau <= 0) {
    return(max(s - k, 0))
  }

  if (sigma <= 0) {
    forward <- s * exp((r - q) * tau)
    return(exp(-r * tau) * max(forward - k, 0))
  }

  d1 <- (log(s / k) + (r - q + 0.5 * sigma^2) * tau) /
    (sigma * sqrt(tau))
  d2 <- d1 - sigma * sqrt(tau)

  s * exp(-q * tau) * pnorm(d1) - k * exp(-r * tau) * pnorm(d2)
}


implied_vol_call <- function(price, s, k, tau, r, q,
                             lower = 1.0e-8,
                             upper = 5.0,
                             tol = 1.0e-10) {
  if (tau <= 0) {
    return(NA_real_)
  }

  intrinsic <- max(s * exp(-q * tau) - k * exp(-r * tau), 0)
  upper_bound <- s * exp(-q * tau)

  if (price < intrinsic - 1.0e-10 || price > upper_bound + 1.0e-10) {
    return(NA_real_)
  }

  f <- function(sig) {
    bs_call(s, k, tau, r, q, sig) - price
  }

  f_lower <- f(lower)
  f_upper <- f(upper)

  if (abs(f_lower) < tol) {
    return(lower)
  }

  if (f_lower * f_upper > 0) {
    return(NA_real_)
  }

  uniroot(f, lower = lower, upper = upper, tol = tol)$root
}


# -----------------------------
# Kurtosis helper
# -----------------------------

excess_kurtosis <- function(x) {
  x_centered <- x - mean(x)
  mean(x_centered^4) / mean(x_centered^2)^2 - 3.0
}


# -----------------------------
# Simulate risk-neutral GARCH(1,1) paths
# -----------------------------

simulate_garch_paths <- function(s0, r, q, omega, alpha, beta, h0,
                                 n_days, npaths, dt) {
  if (alpha < 0 || beta < 0 || omega <= 0 || alpha + beta >= 1) {
    stop("Require omega > 0, alpha >= 0, beta >= 0, and alpha + beta < 1.")
  }

  z <- matrix(rnorm(n_days * npaths), nrow = n_days, ncol = npaths)

  h <- matrix(NA_real_, nrow = n_days, ncol = npaths)
  eps <- matrix(NA_real_, nrow = n_days, ncol = npaths)
  log_ret <- matrix(NA_real_, nrow = n_days, ncol = npaths)

  # Risk-neutral daily log return:
  #
  # log(S_t / S_{t-1}) = (r - q) * dt - 0.5 * h_t + eps_t
  #
  # Conditional on h_t, eps_t is N(0, h_t), so:
  #
  # E[S_t / S_{t-1} | h_t] = exp((r - q) * dt)

  h_prev <- rep(h0, npaths)
  eps_prev <- rep(0.0, npaths)

  for (t in seq_len(n_days)) {
    h_t <- omega + alpha * eps_prev^2 + beta * h_prev
    eps_t <- sqrt(h_t) * z[t, ]

    log_ret_t <- (r - q) * dt - 0.5 * h_t + eps_t

    h[t, ] <- h_t
    eps[t, ] <- eps_t
    log_ret[t, ] <- log_ret_t

    h_prev <- h_t
    eps_prev <- eps_t
  }

  log_prices <- rbind(
    rep(log(s0), npaths),
    log(s0) + apply(log_ret, 2, cumsum)
  )

  spot_paths <- exp(log_prices)

  list(
    spot_paths = spot_paths,
    log_prices = log_prices,
    log_returns = log_ret,
    innovations = eps,
    variances = h
  )
}


sim <- simulate_garch_paths(
  s0 = s0,
  r = r,
  q = q,
  omega = omega,
  alpha = alpha,
  beta = beta,
  h0 = h0,
  n_days = n_days,
  npaths = npaths,
  dt = dt
)

spot_paths <- sim$spot_paths
log_prices <- sim$log_prices
log_returns <- sim$log_returns
h <- sim$variances

cat("Dimensions of spot_paths:\n")
print(dim(spot_paths))

cat("\nFirst few simulated paths:\n")
print(spot_paths[1:6, 1:5])

cat("\nSummary of terminal spot prices:\n")
s_exp <- spot_paths[n_days + 1, ]
print(summary(s_exp))

cat("\n")


# -----------------------------
# Terminal log price distribution
# -----------------------------

terminal_log_prices <- log_prices[n_days + 1, ]

mean_log_price <- mean(terminal_log_prices)
var_log_price <- var(terminal_log_prices)

terminal_log_price_kurtosis <-
  mean((terminal_log_prices - mean_log_price)^4) /
  mean((terminal_log_prices - mean_log_price)^2)^2

terminal_log_price_excess_kurtosis <- terminal_log_price_kurtosis - 3.0

cat("Terminal log price distribution:\n")

cat("Mean log price:\n")
print(mean_log_price)

cat("\nVariance of log price:\n")
print(var_log_price)

cat("\nKurtosis of terminal log price distribution:\n")
print(terminal_log_price_kurtosis)

cat("\nExcess kurtosis of terminal log price distribution:\n")
print(terminal_log_price_excess_kurtosis)

cat("\nExcess kurtosis from helper function:\n")
print(excess_kurtosis(terminal_log_prices))

cat("\n")


# -----------------------------
# Monte Carlo call prices by strike
# -----------------------------

call_prices_by_strike <- numeric(length(strikes))

for (i in seq_along(strikes)) {
  k <- strikes[i]
  payoff <- pmax(s_exp - k, 0)
  call_prices_by_strike[i] <- exp(-r * tau) * mean(payoff)
}

names(call_prices_by_strike) <- paste0("K=", strikes)

cat("Monte Carlo call prices by strike:\n")
print(call_prices_by_strike)

cat("\n")


# -----------------------------
# Implied vols by strike
# -----------------------------

implied_vols_by_strike <- numeric(length(strikes))

for (i in seq_along(strikes)) {
  k <- strikes[i]

  implied_vols_by_strike[i] <- implied_vol_call(
    price = call_prices_by_strike[i],
    s = s0,
    k = k,
    tau = tau,
    r = r,
    q = q
  )
}

names(implied_vols_by_strike) <- paste0("K=", strikes)

cat("Implied vols by strike:\n")
print(implied_vols_by_strike)

cat("\n")


# -----------------------------
# Compare with Black-Scholes using unconditional volatility
# -----------------------------

bs_prices_by_strike <- numeric(length(strikes))

for (i in seq_along(strikes)) {
  bs_prices_by_strike[i] <- bs_call(
    s = s0,
    k = strikes[i],
    tau = tau,
    r = r,
    q = q,
    sigma = sigma_annual
  )
}

names(bs_prices_by_strike) <- paste0("K=", strikes)

cat("Black-Scholes prices using unconditional annual vol:\n")
print(bs_prices_by_strike)

cat("\nGARCH Monte Carlo price minus Black-Scholes price:\n")
print(call_prices_by_strike - bs_prices_by_strike)

cat("\n")


# -----------------------------
# Compact results
# -----------------------------

results <- data.frame(
  strike = strikes,
  garch_mc_call_price = as.numeric(call_prices_by_strike),
  bs_call_price_uncond_vol = as.numeric(bs_prices_by_strike),
  price_difference = as.numeric(call_prices_by_strike - bs_prices_by_strike),
  implied_vol = as.numeric(implied_vols_by_strike)
)

cat("Results:\n")
print(results)

cat("\n")


# -----------------------------
# Variance and realized vol summaries
# -----------------------------

realized_vol_by_path <- apply(log_returns, 2, sd) * sqrt(trading_days_per_year)

avg_daily_variance_by_day <- rowMeans(h)
avg_annual_vol_by_day <- sqrt(trading_days_per_year * avg_daily_variance_by_day)

cat("Average annualized conditional vol by day:\n")
print(avg_annual_vol_by_day)

cat("\nSummary of realized vol by path:\n")
print(summary(realized_vol_by_path))

cat("\nMean realized vol across paths:\n")
print(mean(realized_vol_by_path))

cat("\nMean implied vol across strikes:\n")
print(mean(implied_vols_by_strike, na.rm = TRUE))

cat("\n")


# -----------------------------
# Risk-neutral mean check
# -----------------------------

expected_forward <- s0 * exp((r - q) * tau)
mc_mean_terminal <- mean(s_exp)

cat("Risk-neutral mean check:\n")
cat("Expected forward E[S_T] =", expected_forward, "\n")
cat("Monte Carlo mean S_T   =", mc_mean_terminal, "\n")
cat("Difference             =", mc_mean_terminal - expected_forward, "\n")

cat("\nSummary\n")
cat("This script simulates terminal stock prices under a risk-neutral GARCH(1,1)\n")
cat("return process, prices European calls by discounted Monte Carlo payoffs,\n")
cat("converts those prices into Black-Scholes implied volatilities, and prints\n")
cat("the excess kurtosis of the terminal log price distribution.\n")
