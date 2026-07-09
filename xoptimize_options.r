# Mean-variance optimization of a long-only option portfolio.

source("option_stats.r")
options(width = 10000)
start_time <- proc.time()

S0 <- 100
mu <- 0.03
realized_sigma <- 0.20
T <- 1.0
r <- 0.03
q <- 0.00
risk_aversion <- 4.0
budget <- 1000.0
optimization_objective <- "sharpe"
max_invested_weight <- 1.0
force_full_investment <- TRUE # FALSE
max_iter <- 2000
print_zero_weight_options <- TRUE
zero_weight_tol <- 1e-12
cat("S0:", S0, "\nmu:", mu, "\nrealized_vol", realized_sigma, "\nT:", T, "\nr:", r,
	"\nq:", q, "\nrisk_aversion:", risk_aversion, "\nbudget:", budget,
	"\noptimization_objective:", optimization_objective,
	"\nmax_invested_weight:", max_invested_weight,
	"\nforce_full_investment:", force_full_investment, "\nmax_iter:",
	max_iter, "\nprint_zero_weight_options:", print_zero_weight_options,
	"\nzero_weight_tol:", zero_weight_tol, "\n\n")
base_strikes <-  c(  80,   90,  100,  110,  120)
implied_sigma <- c(0.18, 0.18, 0.18, 0.18, 0.18)
stopifnot(length(implied_sigma) == length(base_strikes))
K <- c(0, base_strikes, base_strikes)
option_type <- c("call", rep("call", length(base_strikes)), rep("put", length(base_strikes)))
instrument_implied_sigma <- c(NA, implied_sigma, implied_sigma)
tol <- 1e-10

par <- ln_params_from_gbm(S0, mu, realized_sigma, T)
m <- par$m
v <- par$v

payoff_stats <- option_payoff_stats_table(m, v, K, option_type)
expected_payoff <- payoff_stats$mean
cov_payoff <- option_payoff_cov_mat(m, v, K, option_type)
option_price <- bs_option_price_vec(
  S0,
  K,
  r,
  q,
  instrument_implied_sigma,
  T,
  option_type
)
rf_growth <- exp(r * T)
payoff_per_dollar <- expected_payoff / option_price
edge <- payoff_per_dollar - rf_growth
cov_per_dollar <- cov_payoff / outer(option_price, option_price)
sd_per_dollar <- payoff_stats$sd / option_price
sharpe <- edge / sd_per_dollar
adj_sharpe <- modified_sharpe_ratio(sharpe, payoff_stats$skew, payoff_stats$ex.kurt)

if (optimization_objective == "mean_variance") {
  opt <- long_only_mean_variance_opt(
    edge = edge,
    covmat = cov_per_dollar,
    risk_aversion = risk_aversion,
    max_weight = max_invested_weight,
    force_full_investment = force_full_investment,
    tol = tol
  )
  weights <- opt$weights
  old_obj <- opt$objective
  reported_objective <- old_obj
  reported_objective_name <- "mean_variance_objective"
} else if (optimization_objective == "sharpe") {
  opt <- long_only_max_sharpe_opt(
    edge = edge,
    covmat = cov_per_dollar,
    max_weight = max_invested_weight,
    tol = tol
  )
  weights <- opt$weights
  old_obj <- sum(edge * weights) -
    risk_aversion * as.numeric(t(weights) %*% cov_per_dollar %*% weights)
  reported_objective <- opt$sharpe
  reported_objective_name <- "sharpe_objective"
} else {
  stop("optimization_objective must be 'mean_variance' or 'sharpe'")
}

contracts <- budget * weights / option_price
invested_weight <- sum(weights)
cash_weight <- budget * (1.0 - invested_weight)
expected_terminal_wealth <- cash_weight * rf_growth +
  budget * sum(weights * payoff_per_dollar)
terminal_variance <- budget^2 * as.numeric(t(weights) %*% cov_per_dollar %*% weights)
terminal_sd <- sqrt(max(terminal_variance, 0.0))
terminal_edge <- expected_terminal_wealth - budget * rf_growth
terminal_sharpe <- terminal_edge / terminal_sd
portfolio_cov_per_dollar <- as.numeric(cov_per_dollar %*% weights)
portfolio_sd_per_dollar <- terminal_sd / budget
portfolio_corr <- portfolio_cov_per_dollar / (sd_per_dollar * portfolio_sd_per_dollar)
contracts_for_moments <- contracts
portfolio_payoff_stats <- option_portfolio_stats(
  m,
  v,
  K,
  option_type,
  contracts_for_moments
)
portfolio_adj_sharpe <- modified_sharpe_ratio(
  terminal_sharpe,
  portfolio_payoff_stats["skew"],
  portfolio_payoff_stats["ex.kurt"]
)
out <- data.frame(
  type = option_type,
  strike = K,
  implied_vol = instrument_implied_sigma,
  price = round(option_price, 3),
  payoff_per_dollar = round(payoff_per_dollar, 3),
  edge = round(edge, 3),
  sharpe = round(sharpe, 3),
  adj_sharpe = round(adj_sharpe, 3),
  port_corr = round(portfolio_corr, 3),
  weight = weights,
  contracts = contracts
)
print_out <- out
if (!print_zero_weight_options) {
  print_out <- out[abs(out$weight) > zero_weight_tol, , drop = FALSE]
}

cat("Long-only option solution\n")
cat("optimization_objective:", optimization_objective, "\n")
cat("rf_growth:", round(rf_growth, 6), "\n")
print(print_out, row.names = FALSE)
cat("\ninvested_weight:", round(invested_weight, 6), "\n")
cat("cash_weight:", round(cash_weight, 6), "\n")
cat("cash_fraction:", round(1.0 - invested_weight, 6), "\n")
cat("expected_terminal_wealth:", round(expected_terminal_wealth, 6), "\n")
cat("terminal_sd:", round(terminal_sd, 6), "\n")
cat("terminal_sharpe:", round(terminal_sharpe, 6), "\n")
cat("terminal_adj_sharpe:", round(portfolio_adj_sharpe, 6), "\n")
cat("terminal_skew:", round(portfolio_payoff_stats["skew"], 6), "\n")
cat("terminal_ex.kurt:", round(portfolio_payoff_stats["ex.kurt"], 6), "\n")
cat(reported_objective_name, ":", round(reported_objective, 6), "\n", sep = "")
cat("mean_variance_objective:", round(old_obj, 6), "\n")
elapsed <- proc.time() - start_time
cat("elapsed_seconds:", round(elapsed["elapsed"], 6), "\n")
