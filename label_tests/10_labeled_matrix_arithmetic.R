# 10_labeled_matrix_arithmetic.R
# Elementwise arithmetic should preserve labels when shape is unchanged.
m <- matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE)
rownames(m) <- c("SPY", "EFA")
colnames(m) <- c("ret", "vol")
z <- 2 * m + 1
cat("arithmetic labels:\n")
print(z)
