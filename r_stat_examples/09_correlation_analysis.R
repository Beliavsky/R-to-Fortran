# Correlation analysis in base R

set.seed(123)
x <- rnorm(100)
y <- 0.7 * x + rnorm(100, sd = 0.7)
z <- rnorm(100)

dat <- data.frame(x = x, y = y, z = z)

cat("Correlation matrix:\n")
print(cor(dat))

cat("\nPearson correlation test for x and y:\n")
print(cor.test(x, y, method = "pearson"))

cat("\nSpearman correlation test for x and y:\n")
print(cor.test(x, y, method = "spearman"))
