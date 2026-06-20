# 04_label_alias_round_subset.R
# Labels should propagate through aliases, round(), and drop=FALSE subsets.
m <- matrix(c(10, 20, 30, 40, 50, 60), nrow = 3, byrow = TRUE)
rownames(m) <- c("SPY", "EFA", "EEM")
colnames(m) <- c("ret", "vol")

z <- m
cat("aliased matrix:\n")
print(z)

cat("rounded alias:\n")
print(round(z, 1))

cat("row subset drop false:\n")
print(z[c(1, 3), , drop = FALSE])

cat("col subset drop false:\n")
print(z[, "vol", drop = FALSE])
