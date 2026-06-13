# Permutation test for difference in means in base R

set.seed(123)
x <- rnorm(25, mean = 0.0, sd = 1)
y <- rnorm(25, mean = 0.7, sd = 1)

observed <- mean(y) - mean(x)

combined <- c(x, y)
n_x <- length(x)
B <- 5000
perm_stat <- numeric(B)

for (b in 1:B) {
  shuffled <- sample(combined)
  xb <- shuffled[1:n_x]
  yb <- shuffled[(n_x + 1):length(combined)]
  perm_stat[b] <- mean(yb) - mean(xb)
}

p_value <- mean(abs(perm_stat) >= abs(observed))

cat("Observed difference in means =", observed, "\n")
cat("Two-sided permutation p-value =", p_value, "\n")
