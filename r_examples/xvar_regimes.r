# ms_varp_sim_base_r.R
# Simulate from a Markov regime-switching VAR model using base R.
# The VAR order can differ across regimes.

rmvnorm_chol <- function(n, mu, sigma) {
  p <- length(mu)
  z <- matrix(rnorm(n * p), nrow = n, ncol = p)
  y <- sweep(z %*% chol(sigma), 2, mu, "+")
  return(y)
}

simulate_markov_chain <- function(n, pmat, s0 = NULL) {
  k <- nrow(pmat)

  if (!all(dim(pmat) == c(k, k))) {
    stop("pmat must be a square matrix")
  }

  if (max(abs(rowSums(pmat) - 1.0)) > 1.0e-12) {
    stop("each row of pmat must sum to 1")
  }

  s <- integer(n)

  if (is.null(s0)) {
    s[1] <- sample.int(k, size = 1)
  } else {
    s[1] <- s0
  }

  for (t in 2:n) {
    s[t] <- sample.int(k, size = 1, prob = pmat[s[t - 1], ])
  }

  return(s)
}

simulate_ms_varp <- function(n, pmat, mu_list, a_list, sigma_list,
                             x0 = NULL, s0 = NULL) {
  k <- length(mu_list)
  p <- length(mu_list[[1]])

  if (!all(dim(pmat) == c(k, k))) {
    stop("pmat must be k by k, where k = length(mu_list)")
  }

  if (length(a_list) != k) {
    stop("a_list must have length k")
  }

  if (length(sigma_list) != k) {
    stop("sigma_list must have length k")
  }

  order_vec <- integer(k)

  for (j in seq_len(k)) {
    if (length(mu_list[[j]]) != p) {
      stop("all mean vectors must have the same length")
    }

    if (length(sigma_list[[j]]) != p * p) {
      stop("each covariance matrix must be p by p")
    }

    if (!all(dim(sigma_list[[j]]) == c(p, p))) {
      stop("each covariance matrix must be p by p")
    }

    order_vec[j] <- length(a_list[[j]])

    if (order_vec[j] < 1) {
      stop("each regime must have at least one AR lag matrix")
    }

    for (lag in seq_len(order_vec[j])) {
      if (!all(dim(a_list[[j]][[lag]]) == c(p, p))) {
        stop("each AR lag matrix must be p by p")
      }
    }
  }

  max_order <- max(order_vec)

  if (n <= max_order) {
    stop("n must be greater than the largest regime order")
  }

  state <- simulate_markov_chain(n, pmat, s0)

  x <- matrix(NA_real_, nrow = n, ncol = p)

  if (is.null(x0)) {
    for (t in seq_len(max_order)) {
      x[t, ] <- mu_list[[state[t]]]
    }
  } else {
    if (!all(dim(x0) == c(max_order, p))) {
      stop("x0 must be a max_order by p matrix")
    }

    x[1:max_order, ] <- x0
  }

  for (t in (max_order + 1):n) {
    j <- state[t]

    mu <- mu_list[[j]]
    sigma <- sigma_list[[j]]
    order_j <- order_vec[j]

    mean_t <- mu

    for (lag in seq_len(order_j)) {
      a <- a_list[[j]][[lag]]
      mean_t <- mean_t + as.numeric(a %*% (x[t - lag, ] - mu))
    }

    x[t, ] <- rmvnorm_chol(1, mean_t, sigma)
  }

  colnames(x) <- paste0("x", seq_len(p))

  y <- list(
    x = x,
    state = state,
    order = order_vec
  )

  return(y)
}

set.seed(123)

n <- 1000

# Transition matrix.
# Row i gives probabilities of moving from state i to states 1, ..., k.
pmat <- matrix(c(0.95, 0.05,
                 0.10, 0.90), nrow = 2, byrow = TRUE)

mu_list <- list(
  c(0.0, 0.0),
  c(2.0, -1.0)
)

# Regime 1 is VAR(1).
# Regime 2 is VAR(2).
a_list <- list(

  list(
    matrix(c(0.50, 0.10,
             0.05, 0.40), nrow = 2, byrow = TRUE)
  ),

  list(
    matrix(c(0.60, -0.15,
             0.10,  0.45), nrow = 2, byrow = TRUE),

    matrix(c(0.20,  0.05,
            -0.05,  0.10), nrow = 2, byrow = TRUE)
  )
)

sigma_list <- list(
  matrix(c(1.0, 0.2,
           0.2, 1.0), nrow = 2, byrow = TRUE),

  matrix(c(2.0, 0.6,
           0.6, 1.5), nrow = 2, byrow = TRUE)
)

sim <- simulate_ms_varp(
  n = n,
  pmat = pmat,
  mu_list = mu_list,
  a_list = a_list,
  sigma_list = sigma_list
)

x <- sim$x
state <- sim$state

cat("transition matrix:\n")
print(pmat)

cat("\nregime VAR orders:\n")
print(sim$order)

cat("\nstate counts:\n")
print(table(state))

cat("\nfirst 10 states:\n")
print(state[1:10])

cat("\nfirst 10 observations:\n")
print(x[1:10, ])

cat("\nsample mean by state:\n")
for (j in seq_along(mu_list)) {
  cat("\nstate", j, "\n")
  print(colMeans(x[state == j, , drop = FALSE]))
}

cat("\nsample covariance by state:\n")
for (j in seq_along(mu_list)) {
  cat("\nstate", j, "\n")
  print(cov(x[state == j, , drop = FALSE]))
}
