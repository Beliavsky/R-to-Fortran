# Two-way ANOVA in base R

set.seed(123)
A <- factor(rep(c("A1", "A2"), each = 60))
B <- factor(rep(rep(c("B1", "B2", "B3"), each = 20), times = 2))

mu <- 10 +
  ifelse(A == "A2", 2, 0) +
  ifelse(B == "B2", 1, ifelse(B == "B3", 3, 0)) +
  ifelse(A == "A2" & B == "B3", 2, 0)

y <- rnorm(length(mu), mean = mu, sd = 2)
dat <- data.frame(y = y, A = A, B = B)

fit <- aov(y ~ A * B, data = dat)

print(summary(fit))
cat("\nCell means:\n")
print(with(dat, tapply(y, list(A, B), mean)))
