# Bootstrap confidence interval for a mean in base R

set.seed(123)
x <- rexp(80, rate = 1)

B <- 5000
boot_mean <- numeric(B)

for (b in 1:B) {
  xb <- sample(x, size = length(x), replace = TRUE)
  boot_mean[b] <- mean(xb)
}

ci <- quantile(boot_mean, probs = c(0.025, 0.975))

cat("Sample mean =", mean(x), "\n")
cat("Bootstrap percentile 95% CI:\n")
print(ci)

cat("\nBootstrap standard error =", sd(boot_mean), "\n")
