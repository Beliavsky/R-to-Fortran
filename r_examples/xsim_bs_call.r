# Simulate npaths GBM paths over 20 trading days.
# At expiration, compute European call payoffs by strike.
# Also compute Black-Scholes call prices and implied vols one day before expiration.
# No plotting and no xtabs.

set.seed(123)

cat("European call prices and implied vols from npaths GBM paths\n\n")

# -----------------------------
# Parameters
# -----------------------------

s0 <- 100.0
mu <- 0.08
sigma_true <- 0.25

r <- 0.04
q <- 0.015

trading_days_per_year <- 252
n_days <- 20
npaths <- 10000
dt <- 1.0 / trading_days_per_year

strikes <- c(80, 90, 95, 100, 105, 110, 120)


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
# Simulate GBM paths
# -----------------------------

z <- matrix(rnorm(n_days * npaths), nrow = n_days, ncol = npaths)

log_returns <- (mu - 0.5 * sigma_true^2) * dt +
  sigma_true * sqrt(dt) * z

log_prices <- rbind(
  rep(log(s0), npaths),
  log(s0) + apply(log_returns, 2, cumsum)
)

spot_paths <- exp(log_prices)

cat("Dimensions of spot_paths:\n")
print(dim(spot_paths))

cat("\nFirst few simulated paths:\n")
print(spot_paths[1:6, 1:5])

cat("\n")


# -----------------------------
# Terminal prices
# -----------------------------

s_exp <- spot_paths[n_days + 1, ]

cat("Summary of terminal spot prices:\n")
print(summary(s_exp))

cat("\n")


# -----------------------------
# Monte Carlo call prices by strike at expiration
# -----------------------------

call_prices_by_strike <- numeric(length(strikes))

for (i in seq_along(strikes)) {
  k <- strikes[i]
  payoff <- pmax(s_exp - k, 0)
  call_prices_by_strike[i] <- exp(-r * n_days / trading_days_per_year) *
    mean(payoff)
}

names(call_prices_by_strike) <- paste0("K=", strikes)

cat("Monte Carlo call prices by strike using terminal payoffs:\n")
print(call_prices_by_strike)

cat("\n")


# -----------------------------
# Implied vols by strike from Monte Carlo prices
# -----------------------------

tau0 <- n_days / trading_days_per_year

implied_vols_by_strike <- numeric(length(strikes))

for (i in seq_along(strikes)) {
  k <- strikes[i]

  implied_vols_by_strike[i] <- implied_vol_call(
    price = call_prices_by_strike[i],
    s = s0,
    k = k,
    tau = tau0,
    r = r,
    q = q,
    lower = 1.0e-8,
    upper = 5.0
  )
}

names(implied_vols_by_strike) <- paste0("K=", strikes)

cat("Implied vols by strike from Monte Carlo call prices:\n")
print(implied_vols_by_strike)

cat("\n")


# -----------------------------
# Exact Black-Scholes prices for comparison
# -----------------------------

bs_prices_by_strike <- numeric(length(strikes))

for (i in seq_along(strikes)) {
  bs_prices_by_strike[i] <- bs_call(
    s = s0,
    k = strikes[i],
    tau = tau0,
    r = r,
    q = q,
    sigma = sigma_true
  )
}

names(bs_prices_by_strike) <- paste0("K=", strikes)

cat("Exact Black-Scholes call prices by strike:\n")
print(bs_prices_by_strike)

cat("\nMonte Carlo price error by strike:\n")
print(call_prices_by_strike - bs_prices_by_strike)

cat("\n")


# -----------------------------
# Put results in a compact data frame
# -----------------------------

results <- data.frame(
  strike = strikes,
  mc_call_price = as.numeric(call_prices_by_strike),
  bs_call_price = as.numeric(bs_prices_by_strike),
  price_error = as.numeric(call_prices_by_strike - bs_prices_by_strike),
  implied_vol = as.numeric(implied_vols_by_strike)
)

cat("Results:\n")
print(results)

cat("\n")


# -----------------------------
# Realized volatility across paths
# -----------------------------

realized_vol_by_path <- apply(log_returns, 2, sd) * sqrt(trading_days_per_year)

cat("Summary of realized vol by path:\n")
print(summary(realized_vol_by_path))

cat("\nTrue sigma used in simulation:\n")
print(sigma_true)

cat("\nMean realized vol across paths:\n")
print(mean(realized_vol_by_path))

cat("\nMean implied vol across strikes:\n")
print(mean(implied_vols_by_strike, na.rm = TRUE))

cat("\nMaximum absolute implied-vol error:\n")
print(max(abs(implied_vols_by_strike - sigma_true), na.rm = TRUE))
