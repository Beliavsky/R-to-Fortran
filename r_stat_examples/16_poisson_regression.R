# Poisson regression in base R (using a shared dataset file so R and
# Fortran use the exact same inputs).

dat <- read.table(
  "poisson_regression_data.txt",
  header = TRUE
)
y <- dat[, 1]
x <- dat[, 2]
exposure <- dat[, 3]

fit <- glm(y ~ x + offset(log(exposure)), data = dat, family = poisson())

print(summary(fit))

cat("\nRate ratio for x:\n")
print(exp(coef(fit)["x"]))

cat("\nCheck for overdispersion:\n")
dispersion <- sum(resid(fit, type = "pearson")^2) / fit$df.residual
cat("Pearson dispersion =", dispersion, "\n")
