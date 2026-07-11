quad1 <- function(x) {
  (x - 3)^2
}

quad2 <- function(par) {
  x <- par[1]
  y <- par[2]
  (x - 2)^2 + (y + 4)^2
}

rosen <- function(par) {
  x <- par[1]
  y <- par[2]
  100 * (y - x^2)^2 + (1 - x)^2
}

nll_normal <- function(par, x) {
  mu <- par[1]
  sigma <- exp(par[2])
  -sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}

quad_hess <- function(par) {
  x <- par[1]
  y <- par[2]
  (x - 2)^2 + 3 * (y + 1)^2
}

fit <- nlm(quad1, p = 0)
err <- max(abs(fit$estimate - 3))
grad_norm <- sqrt(sum(fit$gradient^2))
cat("quad1", err, fit$minimum, grad_norm, fit$code, fit$iterations, "\n")

fit <- nlm(quad2, p = c(0, 0))
err <- max(abs(fit$estimate - c(2, -4)))
grad_norm <- sqrt(sum(fit$gradient^2))
cat("quad2", err, fit$minimum, grad_norm, fit$code, fit$iterations, "\n")

fit <- nlm(rosen, p = c(-1.2, 1))
err <- max(abs(fit$estimate - c(1, 1)))
grad_norm <- sqrt(sum(fit$gradient^2))
cat("rosen", err, fit$minimum, grad_norm, fit$code, fit$iterations, "\n")

xdat <- c(4.2, 5.1, 4.8, 5.4, 6.0, 4.9, 5.7, 5.2, 4.6, 5.5)
mu_mle <- mean(xdat)
sig_mle <- sqrt(mean((xdat - mu_mle)^2))
fit <- nlm(nll_normal, p = c(0, 0), x = xdat)
normal_err <- max(abs(c(fit$estimate[1], exp(fit$estimate[2])) - c(mu_mle, sig_mle)))
cat("normal", normal_err, fit$minimum, sqrt(sum(fit$gradient^2)), fit$code, fit$iterations, "\n")

fit <- nlm(quad_hess, p = c(0, 0), hessian = TRUE)
hess_err <- max(abs(fit$hessian - matrix(c(2, 0, 0, 6), nrow = 2, byrow = TRUE)))
cat("hessian", hess_err, fit$minimum, sqrt(sum(fit$gradient^2)), fit$code, fit$iterations, "\n")
