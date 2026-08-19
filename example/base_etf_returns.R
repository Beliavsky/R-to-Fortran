args <- commandArgs(trailingOnly = TRUE)
csv_path <- if (length(args) >= 1) args[[1]] else "asset_class_etf_prices.csv"

if (!file.exists(csv_path)) {
  cat("ETF return example skipped: CSV file not found\n")
  cat("Pass its path as the first argument, for example:\n")
  cat("Rscript example/base_etf_returns.R asset_class_etf_prices.csv\n")
  quit(status = 0)
}

csv_data <- read.csv(
  csv_path,
  stringsAsFactors = FALSE,
  check.names = FALSE
)
if (!("Date" %in% names(csv_data))) {
  stop("base_etf_returns: expected a Date column")
}

asset_names <- c("SPY", "EFA", "EEM", "EMB")
if (nrow(csv_data) < 2) {
  stop("base_etf_returns: expected dated price columns")
}

price_data <- csv_data[, asset_names, drop = FALSE]
price_matrix <- as.matrix(price_data)
return_matrix <- 100 * log(
  price_matrix[-1, , drop = FALSE] /
    price_matrix[-nrow(price_matrix), , drop = FALSE]
)
keep <- rowSums(is.finite(return_matrix)) == ncol(return_matrix)
return_matrix <- return_matrix[keep, , drop = FALSE]
colnames(return_matrix) <- asset_names
return_dates <- csv_data$Date[-1]
return_dates <- return_dates[keep]

return_data <- data.frame(
  Date = return_dates,
  return_matrix,
  check.names = FALSE,
  row.names = NULL
)

stats_matrix <- rbind(
  n = 1.0 * colSums(is.finite(return_matrix)),
  mean = colMeans(return_matrix),
  sd = apply(return_matrix, 2, sd),
  minimum = apply(return_matrix, 2, min),
  maximum = apply(return_matrix, 2, max)
)
colnames(stats_matrix) <- asset_names
stats_data <- data.frame(
  statistic = c("n", "mean", "sd", "minimum", "maximum"),
  stats_matrix,
  check.names = FALSE,
  row.names = NULL
)

cat("\n", strrep("=", 72), "\n", sep = "")
cat("Base R example: ETF log returns in a data frame\n")
cat(strrep("=", 72), "\n\n", sep = "")
cat("Price file:", csv_path, "\n")
cat("Price observations:", nrow(price_data), "\n")
cat("Complete return observations:", nrow(return_data), "\n")
cat("Assets:", length(asset_names), "\n")
cat("\nFirst five scaled log returns:\n")
print(head(return_data, 5))

cat("\nReturn statistics (returns are multiplied by 100):\n")
cat("Statistic rows:", nrow(stats_data), "\n")
print(stats_data)
