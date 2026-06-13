# Simple distribution fitting in base R using optim()

set.seed(123)
x <- rexp(200, rate = 0.7)

# Exponential MLE by formula
rate_hat_formula <- 1 / mean(x)

# Exponential MLE by numerical optimization
negloglik_exp <- function(log_rate) {
  rate <- exp(log_rate)
  -sum(dexp(x, rate = rate, log = TRUE))
}

opt <- optim(par = log(1), fn = negloglik_exp)
rate_hat_optim <- exp(opt$par)

cat("Exponential rate MLE by formula =", rate_hat_formula, "\n")
cat("Exponential rate MLE by optim   =", rate_hat_optim, "\n")

# Compare exponential and normal AIC
ll_exp <- sum(dexp(x, rate = rate_hat_formula, log = TRUE))
ll_norm <- sum(dnorm(x, mean = mean(x), sd = sqrt(mean((x - mean(x))^2)), log = TRUE))

aic_exp <- -2 * ll_exp + 2 * 1
aic_norm <- -2 * ll_norm + 2 * 2

cat("\nAIC exponential =", aic_exp, "\n")
cat("AIC normal      =", aic_norm, "\n")
