# Logistic regression in base R (using a shared dataset file so R and
# Fortran use the exact same inputs).

dat <- read.table(
  "logistic_regression_data.txt",
  header = TRUE
)
y <- dat[, 1]
x1 <- dat[, 2]
x2 <- dat[, 3]

fit <- glm(y ~ x1 + x2, data = dat, family = binomial())

print(summary(fit))

cat("\nOdds ratios:\n")
print(exp(coef(fit)))

cat("\nPredicted probabilities for first six rows:\n")
print(head(predict(fit, type = "response")))
