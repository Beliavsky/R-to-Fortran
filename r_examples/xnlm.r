# nlm_demo.R
# Demonstrate the R nlm() function.
#
# nlm() minimizes a nonlinear function.
#
# Basic form:
#   nlm(f, p, ...)
#
# where:
#   f = objective function to minimize
#   p = starting parameter vector
#
# nlm() returns a list with components such as:
#   minimum    objective value at optimum
#   estimate   parameter estimate
#   gradient   gradient at optimum
#   code       convergence code
#   iterations number of iterations

cat("\n1. Minimize a simple quadratic function\n")

f1 <- function(x) {
  (x - 3)^2
}

fit <- nlm(f1, p = 0)

print(fit)

cat("Estimated minimum point:\n")
print(fit$estimate)

cat("Minimum value:\n")
print(fit$minimum)


cat("\n2. Minimize a two-variable quadratic function\n")

f2 <- function(par) {
  x <- par[1]
  y <- par[2]

  (x - 2)^2 + (y + 4)^2
}

fit <- nlm(f2, p = c(0, 0))

print(fit)

cat("Estimated minimum point:\n")
print(fit$estimate)


cat("\n3. Rosenbrock banana function\n")

rosenbrock <- function(par) {
  x <- par[1]
  y <- par[2]

  100 * (y - x^2)^2 + (1 - x)^2
}

fit <- nlm(rosenbrock, p = c(-1.2, 1))

print(fit)

cat("True minimum is near c(1, 1).\n")
cat("Estimated minimum point:\n")
print(fit$estimate)


cat("\n4. Least squares fit of a nonlinear model\n")

# Model:
#   y = a * exp(b * x) + error

x <- 0:10
y <- c(2.1, 2.7, 3.8, 5.1, 6.8, 9.1, 12.2, 16.4, 22.0, 29.5, 39.6)

ss_exp <- function(par, x, y) {
  a <- par[1]
  b <- par[2]

  yhat <- a * exp(b * x)

  sum((y - yhat)^2)
}

fit <- nlm(ss_exp, p = c(1, 0.2), x = x, y = y)

print(fit)

cat("Estimated a and b:\n")
print(fit$estimate)

a_hat <- fit$estimate[1]
b_hat <- fit$estimate[2]

cat("Fitted values:\n")
print(a_hat * exp(b_hat * x))


cat("\n5. Maximum likelihood estimation for a normal distribution\n")

# Simulate non-random-looking data.
x <- c(4.2, 5.1, 4.8, 5.4, 6.0, 4.9, 5.7, 5.2, 4.6, 5.5)

# Negative log-likelihood.
# Parameterization:
#   par[1] = mu
#   par[2] = log(sigma)
#
# Using log(sigma) keeps sigma positive.

nll_normal <- function(par, x) {
  mu <- par[1]
  sigma <- exp(par[2])

  -sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}

fit <- nlm(nll_normal, p = c(0, 0), x = x)

print(fit)

mu_hat <- fit$estimate[1]
sigma_hat <- exp(fit$estimate[2])

cat("Estimated mu:\n")
print(mu_hat)

cat("Estimated sigma:\n")
print(sigma_hat)

cat("Compare with mean(x) and MLE sd:\n")
print(mean(x))
print(sqrt(mean((x - mean(x))^2)))


cat("\n6. Maximum likelihood estimation for an exponential distribution\n")

# Exponential density:
#   f(x) = lambda * exp(-lambda * x)
#
# Parameterization:
#   par[1] = log(lambda)

x <- c(0.4, 1.2, 0.7, 2.3, 1.8, 0.5, 0.9, 3.1, 0.2, 1.4)

nll_exp <- function(par, x) {
  lambda <- exp(par[1])

  -sum(dexp(x, rate = lambda, log = TRUE))
}

fit <- nlm(nll_exp, p = 0, x = x)

print(fit)

lambda_hat <- exp(fit$estimate[1])

cat("Estimated lambda:\n")
print(lambda_hat)

cat("Closed-form MLE, 1 / mean(x):\n")
print(1 / mean(x))


cat("\n7. Linear regression by minimizing sum of squared errors\n")

x <- c(1, 2, 3, 4, 5, 6)
y <- c(1.2, 1.9, 3.2, 3.8, 5.1, 5.9)

sse_lm <- function(par, x, y) {
  beta0 <- par[1]
  beta1 <- par[2]

  yhat <- beta0 + beta1 * x

  sum((y - yhat)^2)
}

fit <- nlm(sse_lm, p = c(0, 0), x = x, y = y)

print(fit)

cat("nlm estimates:\n")
print(fit$estimate)

cat("lm estimates:\n")
print(coef(lm(y ~ x)))


cat("\n8. Logistic regression by minimizing negative log-likelihood\n")

x <- c(-3, -2, -1, 0, 1, 2, 3, 4)
y <- c(0,  0,  0, 0, 1, 1, 1, 1)

nll_logistic <- function(par, x, y) {
  beta0 <- par[1]
  beta1 <- par[2]

  eta <- beta0 + beta1 * x

  # Stable log-likelihood calculation:
  # log p     = -log(1 + exp(-eta))
  # log(1-p) = -log(1 + exp( eta))
  logp1 <- -log1p(exp(-eta))
  logp0 <- -log1p(exp( eta))

  -sum(y * logp1 + (1 - y) * logp0)
}

fit <- nlm(nll_logistic, p = c(0, 0), x = x, y = y)

print(fit)

cat("Estimated coefficients:\n")
print(fit$estimate)


cat("\n9. Supplying an analytic gradient as an attribute\n")

# nlm() can use gradient information if the objective function
# returns an attribute named "gradient".

f_with_grad <- function(par) {
  x <- par[1]
  y <- par[2]

  value <- (x - 2)^2 + (y + 4)^2

  grad <- c(
    2 * (x - 2),
    2 * (y + 4)
  )

  attr(value, "gradient") <- grad

  value
}

fit <- nlm(f_with_grad, p = c(0, 0))

print(fit)

cat("Estimated minimum point:\n")
print(fit$estimate)


cat("\n10. Supplying analytic gradient and Hessian\n")

# nlm() can also use a Hessian attribute.

f_with_grad_hess <- function(par) {
  x <- par[1]
  y <- par[2]

  value <- (x - 2)^2 + (y + 4)^2

  grad <- c(
    2 * (x - 2),
    2 * (y + 4)
  )

  hess <- matrix(
    c(2, 0,
      0, 2),
    nrow = 2,
    byrow = TRUE
  )

  attr(value, "gradient") <- grad
  attr(value, "hessian") <- hess

  value
}

fit <- nlm(f_with_grad_hess, p = c(0, 0), hessian = TRUE)

print(fit)

cat("Estimated Hessian at optimum:\n")
print(fit$hessian)


cat("\n11. Ask nlm() to return a numerical Hessian\n")

f2 <- function(par) {
  x <- par[1]
  y <- par[2]

  (x - 2)^2 + (y + 4)^2
}

fit <- nlm(f2, p = c(0, 0), hessian = TRUE)

print(fit)

cat("Numerical Hessian:\n")
print(fit$hessian)


cat("\n12. Using stepmax to limit step sizes\n")

f3 <- function(x) {
  (x - 100)^2
}

fit1 <- nlm(f3, p = 0)
fit2 <- nlm(f3, p = 0, stepmax = 1)

cat("Without small stepmax:\n")
print(fit1)

cat("With stepmax = 1:\n")
print(fit2)


cat("\n13. Example with parameter transformation for positivity\n")

# Minimize over sigma > 0 by optimizing log_sigma over all real numbers.
#
# Objective:
#   (sigma - 2)^2

f_positive <- function(log_sigma) {
  sigma <- exp(log_sigma)

  (sigma - 2)^2
}

fit <- nlm(f_positive, p = 0)

print(fit)

cat("Estimate on log scale:\n")
print(fit$estimate)

cat("Estimate on original positive scale:\n")
print(exp(fit$estimate))


cat("\n14. Example with a local minimum issue\n")

# nlm() is a local optimizer.
# Different starting values can lead to different local minima.

f_wavy <- function(x) {
  sin(5 * x) + 0.1 * x^2
}

starts <- c(-3, -1, 0, 1, 3)

for (s in starts) {
  fit <- nlm(f_wavy, p = s)

  cat("\nStarting point:", s, "\n")
  cat("estimate =", fit$estimate, "\n")
  cat("minimum  =", fit$minimum, "\n")
  cat("code     =", fit$code, "\n")
}


cat("\n15. Check convergence code\n")

f1 <- function(x) {
  (x - 3)^2
}

fit <- nlm(f1, p = 0)

cat("code =", fit$code, "\n")

if (fit$code %in% c(1, 2)) {
  cat("Usually this indicates successful convergence.\n")
} else {
  cat("Check the fit carefully.\n")
}


cat("\nDone.\n")
