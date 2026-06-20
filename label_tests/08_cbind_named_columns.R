# 08_cbind_named_columns.R
# cbind named arguments should create column labels and preserve vector row names when available.
a <- c(SPY = 1.0, EFA = 2.0, EEM = 3.0)
b <- c(SPY = 4.0, EFA = 5.0, EEM = 6.0)
m <- cbind(alpha = a, beta = b)
cat("cbind labels:\n")
print(m)
