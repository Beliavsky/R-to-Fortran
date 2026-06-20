# 02_matrix_rownames_colnames.R
# Matrix rownames/colnames should be preserved in print(round(m, ...)).
m <- matrix(c(1.23456, 2.34567, 3.45678, 4.56789, 5.67891, 6.78912), nrow = 3, byrow = TRUE)
rownames(m) <- c("SPY", "EFA", "EEM")
colnames(m) <- c("mean", "sd")
cat("labeled matrix:\n")
print(round(m, 3))
