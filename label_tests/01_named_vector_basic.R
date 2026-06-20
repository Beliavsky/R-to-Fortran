# 01_named_vector_basic.R
# Named numeric vectors should print labels after direct assignment and round().
x <- c(SPY = 0.2, EFA = 0.3, EEM = 0.5)
cat("named vector:\n")
print(x)
cat("rounded named vector:\n")
print(round(x, 2))

y <- x
cat("aliased named vector:\n")
print(y)
