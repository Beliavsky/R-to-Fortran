# 03_dimnames_matrix.R
# dimnames(list(rows, cols)) should be equivalent to rownames/colnames.
m <- matrix(1:9, nrow = 3)
dimnames(m) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
cat("dimnames matrix:\n")
print(m)
