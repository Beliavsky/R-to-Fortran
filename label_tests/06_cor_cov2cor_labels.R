# 06_cor_cov2cor_labels.R
# cor() and cov2cor() should use source matrix column labels for rows and columns.
x <- matrix(c(
  1.0, 2.0, 3.0,
  2.0, 1.5, 3.5,
  3.0, 2.5, 4.0,
  4.0, 3.0, 5.0
), nrow = 4, byrow = TRUE)
colnames(x) <- c("SPY", "EFA", "EEM")

cat("cor labels:\n")
print(round(cor(x), 4))

s <- cov(x)
cat("cov labels via assignment:\n")
print(round(s, 4))

cat("cov2cor labels:\n")
print(round(cov2cor(s), 4))
