# Poisson regression from a headed text file.

dat <- read.table(
  "poisson_regression_data.txt",
  header = TRUE
)
y <- dat[, 1]
x <- dat[, 2]
exposure <- dat[, 3]

fit <- glm(y ~ x + offset(log(exposure)), family = poisson())

print(summary(fit))

cat("\nRate ratio for x:\n")
print(exp(coef(fit)["x"]))

cat("\nCheck for overdispersion:\n")
dispersion <- sum(resid(fit, type = "pearson")^2) / fit$df.residual
cat("Pearson dispersion =", dispersion, "\n")
