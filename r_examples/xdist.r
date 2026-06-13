# distribution_pdqr_demo.R
# Test built-in R probability distribution functions:
#   d* density / mass
#   p* distribution function
#   q* quantile function
#   r* random generation
#
# Uses base R / stats functions only.

set.seed(123)

cat("\nProbability distribution p, d, q, r function demo\n")

print_header <- function(title) {
  cat("\n", paste(rep("=", nchar(title)), collapse = ""), "\n", sep = "")
  cat(title, "\n")
  cat(paste(rep("=", nchar(title)), collapse = ""), "\n", sep = "")
}

show_continuous <- function(name, dfun, pfun, qfun, rfun,
                            x, probs, nsim = 5, ...) {
  print_header(name)

  cat("\nx values:\n")
  print(x)

  cat("\ndensity d*:\n")
  print(dfun(x, ...))

  cat("\nCDF p*:\n")
  print(pfun(x, ...))

  cat("\nprobabilities:\n")
  print(probs)

  cat("\nquantiles q*:\n")
  print(qfun(probs, ...))

  cat("\nrandom sample r*:\n")
  print(rfun(nsim, ...))

  cat("\nCheck p(q(p)) approximately equals p:\n")
  q <- qfun(probs, ...)
  print(pfun(q, ...))
}

show_discrete <- function(name, dfun, pfun, qfun, rfun,
                          x, probs, nsim = 10, ...) {
  print_header(name)

  cat("\nx values:\n")
  print(x)

  cat("\nprobability mass d*:\n")
  print(dfun(x, ...))

  cat("\nCDF p*:\n")
  print(pfun(x, ...))

  cat("\nprobabilities:\n")
  print(probs)

  cat("\nquantiles q*:\n")
  print(qfun(probs, ...))

  cat("\nrandom sample r*:\n")
  print(rfun(nsim, ...))

  cat("\nCheck p(q(p)) >= p for discrete distributions:\n")
  q <- qfun(probs, ...)
  print(pfun(q, ...))
}


# 1. Normal distribution
show_continuous(
  name = "Normal: dnorm, pnorm, qnorm, rnorm",
  dfun = dnorm,
  pfun = pnorm,
  qfun = qnorm,
  rfun = rnorm,
  x = c(-2, -1, 0, 1, 2),
  probs = c(0.01, 0.05, 0.5, 0.95, 0.99),
  nsim = 5,
  mean = 0,
  sd = 1
)


# 2. Uniform distribution
show_continuous(
  name = "Uniform: dunif, punif, qunif, runif",
  dfun = dunif,
  pfun = punif,
  qfun = qunif,
  rfun = runif,
  x = c(0, 0.25, 0.5, 0.75, 1),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  min = 0,
  max = 1
)


# 3. Exponential distribution
show_continuous(
  name = "Exponential: dexp, pexp, qexp, rexp",
  dfun = dexp,
  pfun = pexp,
  qfun = qexp,
  rfun = rexp,
  x = c(0, 0.5, 1, 2, 4),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  rate = 2
)


# 4. Gamma distribution
show_continuous(
  name = "Gamma: dgamma, pgamma, qgamma, rgamma",
  dfun = dgamma,
  pfun = pgamma,
  qfun = qgamma,
  rfun = rgamma,
  x = c(0, 0.5, 1, 2, 4),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  shape = 2,
  rate = 1
)


# 5. Beta distribution
show_continuous(
  name = "Beta: dbeta, pbeta, qbeta, rbeta",
  dfun = dbeta,
  pfun = pbeta,
  qfun = qbeta,
  rfun = rbeta,
  x = c(0, 0.1, 0.25, 0.5, 0.9, 1),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  shape1 = 2,
  shape2 = 5
)


# 6. Chi-square distribution
show_continuous(
  name = "Chi-square: dchisq, pchisq, qchisq, rchisq",
  dfun = dchisq,
  pfun = pchisq,
  qfun = qchisq,
  rfun = rchisq,
  x = c(0, 1, 2, 5, 10),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  df = 4
)


# 7. Student t distribution
show_continuous(
  name = "Student t: dt, pt, qt, rt",
  dfun = dt,
  pfun = pt,
  qfun = qt,
  rfun = rt,
  x = c(-3, -1, 0, 1, 3),
  probs = c(0.01, 0.05, 0.5, 0.95, 0.99),
  nsim = 5,
  df = 5
)


# 8. F distribution
show_continuous(
  name = "F: df, pf, qf, rf",
  dfun = df,
  pfun = pf,
  qfun = qf,
  rfun = rf,
  x = c(0, 0.5, 1, 2, 5),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  df1 = 5,
  df2 = 10
)


# 9. Logistic distribution
show_continuous(
  name = "Logistic: dlogis, plogis, qlogis, rlogis",
  dfun = dlogis,
  pfun = plogis,
  qfun = qlogis,
  rfun = rlogis,
  x = c(-3, -1, 0, 1, 3),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  location = 0,
  scale = 1
)


# 10. Lognormal distribution
show_continuous(
  name = "Lognormal: dlnorm, plnorm, qlnorm, rlnorm",
  dfun = dlnorm,
  pfun = plnorm,
  qfun = qlnorm,
  rfun = rlnorm,
  x = c(0.1, 0.5, 1, 2, 5),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  meanlog = 0,
  sdlog = 1
)


# 11. Weibull distribution
show_continuous(
  name = "Weibull: dweibull, pweibull, qweibull, rweibull",
  dfun = dweibull,
  pfun = pweibull,
  qfun = qweibull,
  rfun = rweibull,
  x = c(0, 0.5, 1, 2, 4),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  shape = 2,
  scale = 1
)


# 12. Cauchy distribution
show_continuous(
  name = "Cauchy: dcauchy, pcauchy, qcauchy, rcauchy",
  dfun = dcauchy,
  pfun = pcauchy,
  qfun = qcauchy,
  rfun = rcauchy,
  x = c(-5, -1, 0, 1, 5),
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 5,
  location = 0,
  scale = 1
)


# 13. Binomial distribution
show_discrete(
  name = "Binomial: dbinom, pbinom, qbinom, rbinom",
  dfun = dbinom,
  pfun = pbinom,
  qfun = qbinom,
  rfun = rbinom,
  x = 0:10,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  size = 10,
  prob = 0.4
)


# 14. Poisson distribution
show_discrete(
  name = "Poisson: dpois, ppois, qpois, rpois",
  dfun = dpois,
  pfun = ppois,
  qfun = qpois,
  rfun = rpois,
  x = 0:10,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  lambda = 3
)


# 15. Geometric distribution
show_discrete(
  name = "Geometric: dgeom, pgeom, qgeom, rgeom",
  dfun = dgeom,
  pfun = pgeom,
  qfun = qgeom,
  rfun = rgeom,
  x = 0:10,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  prob = 0.3
)


# 16. Negative binomial distribution
show_discrete(
  name = "Negative binomial: dnbinom, pnbinom, qnbinom, rnbinom",
  dfun = dnbinom,
  pfun = pnbinom,
  qfun = qnbinom,
  rfun = rnbinom,
  x = 0:15,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  size = 5,
  prob = 0.4
)


# 17. Hypergeometric distribution
show_discrete(
  name = "Hypergeometric: dhyper, phyper, qhyper, rhyper",
  dfun = dhyper,
  pfun = phyper,
  qfun = qhyper,
  rfun = rhyper,
  x = 0:10,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  m = 20,
  n = 30,
  k = 10
)


# 18. Wilcoxon rank-sum distribution
show_discrete(
  name = "Wilcoxon rank-sum: dwilcox, pwilcox, qwilcox, rwilcox",
  dfun = dwilcox,
  pfun = pwilcox,
  qfun = qwilcox,
  rfun = rwilcox,
  x = 0:30,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  m = 5,
  n = 6
)


# 19. Wilcoxon signed-rank distribution
show_discrete(
  name = "Wilcoxon signed-rank: dsignrank, psignrank, qsignrank, rsignrank",
  dfun = dsignrank,
  pfun = psignrank,
  qfun = qsignrank,
  rfun = rsignrank,
  x = 0:21,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9),
  nsim = 10,
  n = 6
)


cat("\nAdditional checks\n")

cat("\nLower tail versus upper tail for normal:\n")
x <- c(-1, 0, 1)

print(cbind(
  x = x,
  lower_tail = pnorm(x),
  upper_tail = pnorm(x, lower.tail = FALSE),
  sum = pnorm(x) + pnorm(x, lower.tail = FALSE)
))

cat("\nLog density and log probability examples:\n")
print(dnorm(0, log = TRUE))
print(pnorm(0, log.p = TRUE))

cat("\nQuantiles using lower.tail = FALSE:\n")
print(qnorm(c(0.025, 0.05), lower.tail = FALSE))

cat("\nDiscrete quantile behavior example:\n")
p <- c(0.1, 0.25, 0.5, 0.75, 0.9)
q <- qpois(p, lambda = 3)

print(cbind(
  p = p,
  qpois = q,
  ppois_q = ppois(q, lambda = 3),
  ppois_q_minus_1 = ppois(q - 1, lambda = 3)
))

cat("\nFor a discrete distribution, q(p) is the smallest integer q such that P(X <= q) >= p.\n")
