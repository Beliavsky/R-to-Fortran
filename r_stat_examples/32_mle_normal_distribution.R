# Maximum likelihood estimation for a normal distribution in base R

set.seed(123)
x <- rnorm(200, mean = 3, sd = 2)

negloglik_norm <- function(par) {
  mu <- par[1]
  sigma <- exp(par[2])
  -sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}

opt <- optim(par = c(mean(x), log(sd(x))), fn = negloglik_norm,
             hessian = TRUE)

mu_hat <- opt$par[1]
sigma_hat <- exp(opt$par[2])

cat("MLE mu =", mu_hat, "\n")
cat("MLE sigma =", sigma_hat, "\n")

# For comparison, formula MLE uses denominator n for sigma.
cat("Formula mu =", mean(x), "\n")
cat("Formula sigma =", sqrt(mean((x - mean(x))^2)), "\n")
