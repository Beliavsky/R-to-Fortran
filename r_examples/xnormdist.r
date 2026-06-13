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
