# Multiple linear regression from a headed text file.

dat <- read.table(
  "multiple_regression_data.txt",
  header = TRUE
)
y <- dat[, 1]
x1 <- dat[, 2]
x2 <- dat[, 3]
x3B <- dat[, 4]

fit <- lm(y ~ x1 + x2 + x3B)

print(summary(fit))
cat("\nANOVA table:\n")
print(anova(fit))

cat("\nModel matrix first six rows:\n")
print(head(model.matrix(fit)))
