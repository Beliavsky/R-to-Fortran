# Simple life-table style survival analysis in base R
# This does not use the survival package.

set.seed(123)
n <- 100
true_time <- rexp(n, rate = 0.1)
censor_time <- rexp(n, rate = 0.05)
time <- pmin(true_time, censor_time)
status <- as.integer(true_time <= censor_time)  # 1 = event, 0 = censored

dat <- data.frame(time = time, status = status)
dat <- dat[order(dat$time), ]

event_times <- sort(unique(dat$time[dat$status == 1]))

surv <- numeric(length(event_times))
at_risk <- numeric(length(event_times))
events <- numeric(length(event_times))
s <- 1

for (i in seq_along(event_times)) {
  t <- event_times[i]
  at_risk[i] <- sum(dat$time >= t)
  events[i] <- sum(dat$time == t & dat$status == 1)
  s <- s * (1 - events[i] / at_risk[i])
  surv[i] <- s
}

life_table <- data.frame(time = event_times,
                         at_risk = at_risk,
                         events = events,
                         survival = surv)

cat("First rows of Kaplan-Meier-style estimate:\n")
print(head(life_table, 12))
