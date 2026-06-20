# 07_apply_t_named_columns.R
# t(apply(...)) with named c() return should print original columns as rows and c() names as columns.
x <- matrix(c(
  1.0, 2.0, 3.0,
  2.0, 1.5, 3.5,
  3.0, 2.5, 4.0,
  4.0, 3.0, 5.0
), nrow = 4, byrow = TRUE)
colnames(x) <- c("SPY", "EFA", "EEM")

stats <- t(apply(x, 2, function(v) {
  c(mean = mean(v), sd = sd(v), min = min(v), max = max(v))
}))
cat("apply stats labels:\n")
print(round(stats, 4))
