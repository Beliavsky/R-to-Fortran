# Nonparametric tests in base R

set.seed(123)
x <- rexp(30, rate = 1)
y <- rexp(35, rate = 0.8)

cat("Wilcoxon rank-sum test:\n")
print(wilcox.test(x, y))

before <- rnorm(20)
after <- before + rexp(20, rate = 2) - 0.2

cat("\nWilcoxon signed-rank test:\n")
print(wilcox.test(before, after, paired = TRUE))

g <- factor(rep(c("A", "B", "C"), each = 20))
z <- c(rexp(20, 1), rexp(20, 0.8), rexp(20, 0.5))

cat("\nKruskal-Wallis test:\n")
print(kruskal.test(z ~ g))
