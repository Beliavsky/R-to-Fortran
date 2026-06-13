# One-way ANOVA in base R

set.seed(123)
group <- factor(rep(c("A", "B", "C"), each = 30))
y <- c(rnorm(30, mean = 10, sd = 2),
       rnorm(30, mean = 12, sd = 2),
       rnorm(30, mean = 14, sd = 2))

dat <- data.frame(group = group, y = y)

fit <- aov(y ~ group, data = dat)

print(summary(fit))

cat("\nTukey HSD comparisons:\n")
print(TukeyHSD(fit))
