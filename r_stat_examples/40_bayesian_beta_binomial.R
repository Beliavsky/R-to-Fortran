# Simple Bayesian beta-binomial update in base R

# Prior: p ~ Beta(alpha0, beta0)
alpha0 <- 2
beta0 <- 2

# Data: x successes in n trials
x <- 18
n <- 25

alpha1 <- alpha0 + x
beta1 <- beta0 + n - x

cat("Posterior alpha =", alpha1, "\n")
cat("Posterior beta  =", beta1, "\n")
cat("Posterior mean  =", alpha1 / (alpha1 + beta1), "\n")

ci <- qbeta(c(0.025, 0.975), alpha1, beta1)
cat("Posterior 95% credible interval:\n")
print(ci)

# Posterior predictive probability that next trial is success
cat("Posterior predictive P(success next) =", alpha1 / (alpha1 + beta1), "\n")
