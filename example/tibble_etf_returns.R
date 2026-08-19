args <- commandArgs(trailingOnly = TRUE)
csv_path <- if (length(args) >= 1) args[[1]] else "asset_class_etf_prices.csv"

if (!file.exists(csv_path)) {
  cat("ETF return example skipped: CSV file not found\n")
  cat("Pass its path as the first argument, for example:\n")
  cat("Rscript example/tibble_etf_returns.R asset_class_etf_prices.csv\n")
  quit(status = 0)
}

if (!requireNamespace("tibble", quietly = TRUE)) {
  stop("This example requires the tibble package.")
}

csv_tbl <- tibble::as_tibble(read.csv(csv_path, check.names = FALSE))
if (!("Date" %in% names(csv_tbl))) {
  stop("tibble_etf_returns: expected a Date column")
}

asset_names <- c("SPY", "EFA", "EEM", "EMB")
if (nrow(csv_tbl) < 2) {
  stop("tibble_etf_returns: expected dated price columns")
}

price_tbl <- csv_tbl[, asset_names]
price_matrix <- as.matrix(price_tbl)
return_matrix <- 100 * log(
  price_matrix[-1, , drop = FALSE] /
    price_matrix[-nrow(price_matrix), , drop = FALSE]
)
keep <- rowSums(is.finite(return_matrix)) == ncol(return_matrix)
return_matrix <- return_matrix[keep, , drop = FALSE]
colnames(return_matrix) <- asset_names
return_dates <- csv_tbl$Date[-1]
return_dates <- return_dates[keep]

return_tbl <- tibble::as_tibble(return_matrix)
return_tbl <- tibble::add_column(
  return_tbl,
  Date = return_dates,
  .before = 1
)

stats_matrix <- rbind(
  n = 1.0 * colSums(is.finite(return_matrix)),
  mean = colMeans(return_matrix),
  sd = apply(return_matrix, 2, sd),
  minimum = apply(return_matrix, 2, min),
  maximum = apply(return_matrix, 2, max)
)
colnames(stats_matrix) <- asset_names
rownames(stats_matrix) <- c("n", "mean", "sd", "minimum", "maximum")
stats_tbl <- tibble::as_tibble(stats_matrix, rownames = "statistic")

cat("\n", strrep("=", 72), "\n", sep = "")
cat("R example: ETF log returns in a tibble\n")
cat(strrep("=", 72), "\n\n", sep = "")
cat("Price file:", csv_path, "\n")
cat("Price observations:", nrow(price_tbl), "\n")
cat("Complete return observations:", nrow(return_tbl), "\n")
cat("Assets:", length(asset_names), "\n")
cat("\nFirst five scaled log returns:\n")
print(head(return_tbl, 5))

cat("\nReturn statistics (returns are multiplied by 100):\n")
cat("Statistic rows:", nrow(stats_tbl), "\n")
print(stats_tbl)
