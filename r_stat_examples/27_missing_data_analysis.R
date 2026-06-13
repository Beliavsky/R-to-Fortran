# Missing data handling in base R

set.seed(123)
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- 1 + 2 * x1 - x2 + rnorm(n)

x1[sample(1:n, 10)] <- NA
x2[sample(1:n, 15)] <- NA

dat <- data.frame(y = y, x1 = x1, x2 = x2)

cat("Missing counts:\n")
print(colSums(is.na(dat)))

cat("\nComplete-case regression:\n")
fit_cc <- lm(y ~ x1 + x2, data = dat, na.action = na.omit)
print(summary(fit_cc))

cat("\nMean-imputation example:\n")
dat_imp <- dat
dat_imp$x1[is.na(dat_imp$x1)] <- mean(dat_imp$x1, na.rm = TRUE)
dat_imp$x2[is.na(dat_imp$x2)] <- mean(dat_imp$x2, na.rm = TRUE)
fit_imp <- lm(y ~ x1 + x2, data = dat_imp)
print(summary(fit_imp))
