# 05_transpose_labels.R
# t(m) should swap row and column labels.
m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, byrow = TRUE)
rownames(m) <- c("SPY", "EFA", "EEM")
colnames(m) <- c("mean", "sd")
cat("transpose labels:\n")
print(t(m))
