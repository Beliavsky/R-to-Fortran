# Optimize a single-underlying option portfolio under multiple objectives and
# constraint modes.
#
# The instrument universe contains the stock, reported as a call with strike 0,
# plus calls and puts at base_strikes. Calls and puts use the same strike-level
# implied volatility vector. Payoff moments are computed under a lognormal
# terminal stock distribution with user-specified realized volatility.
#
# For each constraint mode and objective, the script prints an option table with
# executable weights/contracts, single-instrument payoff statistics, and
# portfolio correlation. It also prints portfolio wealth statistics: mean, sd,
# Sharpe, skew, excess kurtosis, minimum terminal wealth, tail slope, and ES
# losses at the configured CVaR tail probabilities.
#
# Supported objectives include Sharpe, mean-variance, adjusted Sharpe, and
# expected utility over risk_aversion_utility values. Supported constraint modes
# include long-only and nonnegative terminal wealth; the latter also enforces
# min_terminal_wealth and configured CVaR/ES loss caps by scaling the optimized
# risky overlay toward cash when needed.
#
# Option transaction costs can be modeled with bid/ask implied vols. The default
# spread model widens vol spreads quadratically in log-moneyness, leaving the
# stock row unchanged. If initial_contracts is nonzero, transaction costs are
# charged only on trades away from the initial portfolio.
#
# The final Optimization objective summary can optionally be written to CSV via
# write_summary_csv and summary_csv_file.

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
optimization_objectives <- c("sharpe", "mean_variance", "expected_utility")
risk_aversion_utility <- c(0.0, 0.001, 1.0, 2.0)
min_terminal_wealth <- 1e-6 * budget
cvar_constraints <- data.frame(
  tail_prob = c(0.01, 0.05),
  max_loss = c(200.0, 100.0)
)
cvar_n_scenarios <- 401
constraint_modes <- c("long_only", "nonnegative_terminal")
use_bid_ask_vols <- TRUE
vol_spread_atm <- 0.01
vol_spread_quad <- 0.10
min_bid_vol <- 0.001
max_invested_weight <- 1.0
force_full_investment <- TRUE # FALSE
max_iter <- 2000
print_zero_weight_options <- TRUE
zero_weight_tol <- 1e-12
write_summary_csv <- FALSE
summary_csv_file <- "optimization_objective_summary.csv"
cat("S0:", S0, "\nmu:", mu, "\nrealized_vol", realized_sigma, "\nT:", T, "\nr:", r,
	"\nq:", q, "\nrisk_aversion:", risk_aversion, "\nbudget:", budget,
	"\noptimization_objectives:", optimization_objectives,
	"\nrisk_aversion_utility:", risk_aversion_utility,
	"\nmin_terminal_wealth:", min_terminal_wealth,
	"\ncvar_n_scenarios:", cvar_n_scenarios,
	"\nconstraint_modes:", constraint_modes,
	"\nuse_bid_ask_vols:", use_bid_ask_vols,
	"\nvol_spread_atm:", vol_spread_atm,
	"\nvol_spread_quad:", vol_spread_quad,
	"\nmin_bid_vol:", min_bid_vol,
	"\nmax_invested_weight:", max_invested_weight,
	"\nforce_full_investment:", force_full_investment, "\nmax_iter:",
	max_iter, "\nprint_zero_weight_options:", print_zero_weight_options,
	"\nzero_weight_tol:", zero_weight_tol,
	"\nwrite_summary_csv:", write_summary_csv,
	"\nsummary_csv_file:", summary_csv_file, "\n\n")
cat("cvar_constraints:\n")
print(cvar_constraints, row.names = FALSE)
cat("\n")
base_strikes <-  c(  80,   90,  100,  110,  120)
implied_sigma <- c(0.18, 0.18, 0.18, 0.18, 0.18)
stopifnot(length(implied_sigma) == length(base_strikes))
K <- c(0, base_strikes, base_strikes)
option_type <- c("call", rep("call", length(base_strikes)), rep("put", length(base_strikes)))
instrument_implied_sigma <- c(NA, implied_sigma, implied_sigma)
initial_contracts <- rep(0.0, length(K))
tol <- 1e-10

par <- ln_params_from_gbm(S0, mu, realized_sigma, T)
m <- par$m
v <- par$v

payoff_stats <- option_payoff_stats_table(m, v, K, option_type)
expected_payoff <- payoff_stats$mean
cov_payoff <- option_payoff_cov_mat(m, v, K, option_type)
state_grid <- sort(unique(c(0.0, K)))
payoff_grid <- option_payoff_matrix(state_grid, K, option_type)
tail_slope <- option_tail_slope(K, option_type)
cvar_z <- qnorm(seq(0.001, 0.999, length.out = cvar_n_scenarios))
cvar_scenarios <- exp(m + sqrt(v) * cvar_z)
cvar_payoff_scenarios <- option_payoff_matrix(cvar_scenarios, K, option_type)
vol_quotes <- vol_bid_ask_from_quadratic_spread(
  S0 = S0,
  K = K,
  T = T,
  r = r,
  q = q,
  mid_vol = instrument_implied_sigma,
  spread_atm = if (use_bid_ask_vols) vol_spread_atm else 0.0,
  spread_quad = if (use_bid_ask_vols) vol_spread_quad else 0.0,
  min_bid_vol = min_bid_vol
)
mid_price <- bs_option_price_vec(
  S0,
  K,
  r,
  q,
  vol_quotes$mid_vol,
  T,
  option_type
)
bid_price <- bs_option_price_vec(S0, K, r, q, vol_quotes$bid_vol, T, option_type)
ask_price <- bs_option_price_vec(S0, K, r, q, vol_quotes$ask_vol, T, option_type)
option_price <- ask_price
cat("initial_contracts:\n")
print(
  data.frame(
    type = option_type,
    strike = K,
    initial_contracts = initial_contracts
  ),
  row.names = FALSE
)
cat("\n")
rf_growth <- exp(r * T)
payoff_per_dollar <- expected_payoff / option_price
edge <- payoff_per_dollar - rf_growth
cov_per_dollar <- cov_payoff / outer(option_price, option_price)
sd_per_dollar <- payoff_stats$sd / option_price
sharpe <- edge / sd_per_dollar
adj_sharpe <- modified_sharpe_ratio(sharpe, payoff_stats$skew, payoff_stats$ex.kurt)

summary_rows <- data.frame()
objective_runs <- data.frame(objective = character(), utility_gamma = numeric())
for (obj in optimization_objectives) {
  if (obj == "expected_utility") {
    objective_runs <- rbind(
      objective_runs,
      data.frame(objective = rep(obj, length(risk_aversion_utility)),
                 utility_gamma = risk_aversion_utility)
    )
  } else {
    objective_runs <- rbind(
      objective_runs,
      data.frame(objective = obj, utility_gamma = NA_real_)
    )
  }
}

for (constraint_mode in constraint_modes) {
for (run_idx in seq_len(nrow(objective_runs))) {
  optimization_objective <- objective_runs$objective[run_idx]
  utility_gamma <- objective_runs$utility_gamma[run_idx]
  method_start_time <- proc.time()

  if (constraint_mode == "nonnegative_terminal") {
    if (optimization_objective == "expected_utility") {
      opt <- expected_utility_option_opt(
        m = m,
        v = v,
        K = K,
        type = option_type,
        prices = option_price,
        bid_prices = bid_price,
        ask_prices = ask_price,
        budget = budget,
        rf_growth = rf_growth,
        gamma = utility_gamma,
        min_terminal_wealth = min_terminal_wealth,
        initial_contracts = initial_contracts,
        cvar_constraints = cvar_constraints,
        cvar_scenarios = cvar_scenarios,
        max_abs_position_weight = max_invested_weight,
        tol = tol
      )
      contracts <- opt$contracts
      weights <- opt$weights
      old_obj <- sum(edge * weights) -
        risk_aversion * as.numeric(t(weights) %*% cov_per_dollar %*% weights)
      reported_objective <- opt$objective
      reported_objective_name <- "expected_utility_objective"
    } else {
      opt <- nonnegative_terminal_option_opt(
        m = m,
        v = v,
        K = K,
        type = option_type,
        prices = option_price,
        bid_prices = bid_price,
        ask_prices = ask_price,
        expected_payoff = expected_payoff,
        cov_payoff = cov_payoff,
        budget = budget,
        rf_growth = rf_growth,
        objective = optimization_objective,
        risk_aversion = risk_aversion,
        min_terminal_wealth = min_terminal_wealth,
        initial_contracts = initial_contracts,
        cvar_constraints = cvar_constraints,
        cvar_scenarios = cvar_scenarios,
        max_abs_position_weight = max_invested_weight,
        tol = tol
      )
      contracts <- opt$contracts
      weights <- opt$weights
      old_obj <- sum(edge * weights) -
        risk_aversion * as.numeric(t(weights) %*% cov_per_dollar %*% weights)
      reported_objective <- opt$objective
      if (optimization_objective == "mean_variance") {
        reported_objective_name <- "mean_variance_objective"
      } else if (optimization_objective == "sharpe") {
        reported_objective_name <- "sharpe_objective"
      } else {
        reported_objective_name <- "adjusted_sharpe_objective"
      }
    }
  } else if (constraint_mode == "long_only" && optimization_objective == "mean_variance") {
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
  } else if (optimization_objective == "adjusted_sharpe" ||
             optimization_objective == "sharpe_adj") {
    opt <- long_only_max_adjusted_sharpe_opt(
      m = m,
      v = v,
      K = K,
      type = option_type,
      prices = option_price,
      edge = edge,
      covmat = cov_per_dollar,
      max_weight = max_invested_weight,
      tol = tol
    )
    weights <- opt$weights
    old_obj <- sum(edge * weights) -
      risk_aversion * as.numeric(t(weights) %*% cov_per_dollar %*% weights)
    reported_objective <- opt$adjusted_sharpe
    reported_objective_name <- "adjusted_sharpe_objective"
  } else if (constraint_mode == "long_only" && optimization_objective == "expected_utility") {
    opt <- long_only_expected_utility_opt(
      m = m,
      v = v,
      K = K,
      type = option_type,
      prices = option_price,
      budget = budget,
      rf_growth = rf_growth,
      gamma = utility_gamma,
      initial_contracts = initial_contracts,
      bid_prices = bid_price,
      ask_prices = ask_price,
      max_weight = max_invested_weight,
      force_full_investment = force_full_investment
    )
    weights <- opt$weights
    contracts <- opt$contracts
    old_obj <- sum(edge * weights) -
      risk_aversion * as.numeric(t(weights) %*% cov_per_dollar %*% weights)
    reported_objective <- opt$objective
    reported_objective_name <- "expected_utility_objective"
  } else {
    stop("optimization_objective must be 'mean_variance', 'sharpe', 'adjusted_sharpe', 'sharpe_adj', or 'expected_utility'")
  }

  if (constraint_mode == "long_only") {
    contracts <- budget * weights / option_price
  } else if (constraint_mode == "nonnegative_terminal") {
    contracts <- scale_contracts_to_terminal_constraints(
      contracts = contracts,
      prices = option_price,
      budget = budget,
      rf_growth = rf_growth,
      payoff_grid = payoff_grid,
      min_terminal_wealth = min_terminal_wealth,
      cvar_constraints = cvar_constraints,
      cvar_payoff_scenarios = cvar_payoff_scenarios,
      initial_contracts = initial_contracts,
      bid_prices = bid_price,
      ask_prices = ask_price,
      tol = 1e-8
    )
    weights <- position_exec_prices(contracts, bid_price, ask_price) * contracts / budget
  }
  invested_weight <- sum(weights)
  trade_contracts <- rebalance_trade_contracts(contracts, initial_contracts)
  trade_cost <- executable_trade_cost(trade_contracts, bid_price, ask_price)
  cash_weight <- budget - trade_cost
  expected_terminal_wealth <- cash_weight * rf_growth + sum(expected_payoff * contracts)
  terminal_variance <- as.numeric(t(contracts) %*% cov_payoff %*% contracts)
  terminal_sd <- sqrt(max(terminal_variance, 0.0))
  terminal_edge <- expected_terminal_wealth - budget * rf_growth
  terminal_sharpe <- terminal_edge / terminal_sd
  portfolio_cov <- as.numeric(cov_payoff %*% contracts)
  portfolio_corr <- portfolio_cov / (payoff_stats$sd * terminal_sd)
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
  min_terminal_value <- min(cash_weight * rf_growth + as.numeric(payoff_grid %*% contracts))
  terminal_tail_slope <- sum(tail_slope * contracts)
  cvar_terminal_wealth <- cash_weight * rf_growth +
    as.numeric(cvar_payoff_scenarios %*% contracts)
  es_loss <- cvar_loss_values(cvar_terminal_wealth, budget, cvar_constraints$tail_prob)
  es_names <- es_loss_col_names(cvar_constraints$tail_prob)
  old_obj <- terminal_edge / budget - risk_aversion * terminal_variance / budget^2
  if (optimization_objective == "mean_variance") {
    reported_objective <- old_obj
  } else if (optimization_objective == "sharpe") {
    reported_objective <- terminal_sharpe
  } else if (optimization_objective == "adjusted_sharpe" ||
             optimization_objective == "sharpe_adj") {
    reported_objective <- as.numeric(portfolio_adj_sharpe)
  } else if (optimization_objective == "expected_utility") {
    reported_objective <- expected_utility_for_contracts(
      m = m,
      v = v,
      K = K,
      type = option_type,
      prices = option_price,
      budget = budget,
      rf_growth = rf_growth,
      contracts = contracts,
      gamma = utility_gamma,
      initial_contracts = initial_contracts,
      bid_prices = bid_price,
      ask_prices = ask_price
    )
  }
  exec_price <- position_exec_prices(trade_contracts, bid_price, ask_price)
  display_payoff_per_dollar <- expected_payoff / exec_price
  display_edge <- display_payoff_per_dollar - rf_growth
  display_sd_per_dollar <- payoff_stats$sd / exec_price
  display_sharpe <- display_edge / display_sd_per_dollar
  display_adj_sharpe <- modified_sharpe_ratio(
    display_sharpe,
    payoff_stats$skew,
    payoff_stats$ex.kurt
  )
  out <- data.frame(
    type = option_type,
    strike = K,
    mid_vol = round(vol_quotes$mid_vol, 4),
    bid_vol = round(vol_quotes$bid_vol, 4),
    ask_vol = round(vol_quotes$ask_vol, 4),
    mid_price = round(mid_price, 3),
    bid_price = round(bid_price, 3),
    ask_price = round(ask_price, 3),
    exec_price = round(exec_price, 3),
    payoff_per_dollar = round(display_payoff_per_dollar, 3),
    edge = round(display_edge, 3),
    sharpe = round(display_sharpe, 3),
    adj_sharpe = round(display_adj_sharpe, 3),
    port_corr = round(portfolio_corr, 3),
    initial_contracts = initial_contracts,
    trade_contracts = trade_contracts,
    weight = weights,
    contracts = contracts
  )
  print_out <- out
  if (!print_zero_weight_options) {
    print_out <- out[abs(out$weight) > zero_weight_tol, , drop = FALSE]
  }
  print_out$weight <- sprintf("%.4f", print_out$weight)
  print_out$initial_contracts <- sprintf("%.4f", print_out$initial_contracts)
  print_out$trade_contracts <- sprintf("%.4f", print_out$trade_contracts)
  print_out$contracts <- sprintf("%.4f", print_out$contracts)

  cat("Option solution\n")
  cat("constraint_mode:", constraint_mode, "\n")
  cat("optimization_objective:", optimization_objective, "\n")
  if (optimization_objective == "expected_utility") {
    cat("utility_gamma:", utility_gamma, "\n")
  }
  cat("rf_growth:", round(rf_growth, 6), "\n")
  print(print_out, row.names = FALSE)
  cat("\ninvested_weight:", round(invested_weight, 6), "\n")
  cat("trade_cost:", round(trade_cost, 6), "\n")
  cat("cash_weight:", round(cash_weight, 6), "\n")
  cat("cash_fraction:", round(cash_weight / budget, 6), "\n")
  cat("mean_wealth:", round(expected_terminal_wealth, 6), "\n")
  cat("sd_wealth:", round(terminal_sd, 6), "\n")
  cat("sharpe:", round(terminal_sharpe, 6), "\n")
  cat("adj_sharpe:", round(portfolio_adj_sharpe, 6), "\n")
  cat("skew:", round(portfolio_payoff_stats["skew"], 6), "\n")
  cat("ex.kurt:", round(portfolio_payoff_stats["ex.kurt"], 6), "\n")
  cat("min_wealth:", round(min_terminal_value, 6), "\n")
  cat("tail_slope:", round(terminal_tail_slope, 6), "\n")
  for (i in seq_along(es_loss)) {
    cat(es_names[i], ":", round(es_loss[i], 6), "\n")
  }
  cat(reported_objective_name, ":", round(reported_objective, 6), "\n")
  if (reported_objective_name != "mean_variance_objective") {
    cat("mean_variance_objective:", round(old_obj, 6), "\n")
  }
  method_elapsed <- proc.time() - method_start_time
  method_elapsed_seconds <- as.numeric(method_elapsed["elapsed"])
  cat("method_elapsed_seconds:", round(method_elapsed_seconds, 6), "\n")
  cat("\n")

  summary_row <- data.frame(
      constraint_mode = constraint_mode,
      objective = optimization_objective,
      utility_gamma = utility_gamma,
      obj_value = reported_objective,
      mean_variance_objective = old_obj,
      invested_weight = invested_weight,
      trade_cost = trade_cost,
      cash_weight = cash_weight,
      cash_fraction = cash_weight / budget,
      mean_wealth = expected_terminal_wealth,
      sd_wealth = terminal_sd,
      sharpe = terminal_sharpe,
      adj_sharpe = as.numeric(portfolio_adj_sharpe),
      skew = as.numeric(portfolio_payoff_stats["skew"]),
      ex.kurt = as.numeric(portfolio_payoff_stats["ex.kurt"]),
      min_wealth = min_terminal_value,
      tail_slope = terminal_tail_slope,
      method_elapsed_seconds = method_elapsed_seconds
  )
  for (i in seq_along(es_loss)) {
    summary_row[[es_names[i]]] <- es_loss[i]
  }
  summary_rows <- rbind(summary_rows, summary_row)
}
}

cat("Optimization objective summary\n")
summary_print <- summary_rows
num_cols <- vapply(summary_print, is.numeric, logical(1))
summary_print[num_cols] <- lapply(summary_print[num_cols], round, 6)
wealth_cols <- c(
  "mean_wealth",
  "sd_wealth",
  "sharpe",
  "adj_sharpe",
  "skew",
  "ex.kurt",
  "min_wealth",
  "tail_slope",
  grep("^es_loss_", names(summary_rows), value = TRUE)
)
for (col in wealth_cols) {
  if (col %in% names(summary_print)) {
    summary_print[[col]] <- round(summary_rows[[col]], 3)
  }
}
print(summary_print, row.names = FALSE)
if (write_summary_csv) {
  write.csv(summary_print, summary_csv_file, row.names = FALSE)
  cat("summary_csv_file:", summary_csv_file, "\n")
}

elapsed <- proc.time() - start_time
cat("\nelapsed_seconds:", round(elapsed["elapsed"], 6), "\n")
