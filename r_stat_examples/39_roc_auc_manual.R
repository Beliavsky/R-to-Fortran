# Manual ROC AUC calculation in base R

set.seed(123)
n <- 200
actual <- rbinom(n, 1, 0.4)
score <- rnorm(n, mean = actual * 0.8, sd = 1)

# AUC equals the probability that a random positive has a higher score
# than a random negative, with ties getting half credit.
pos <- score[actual == 1]
neg <- score[actual == 0]

count <- 0
total <- length(pos) * length(neg)

for (i in seq_along(pos)) {
  count <- count + sum(pos[i] > neg) + 0.5 * sum(pos[i] == neg)
}

auc <- count / total

cat("Manual AUC =", auc, "\n")

# A few ROC points
thresholds <- quantile(score, probs = seq(0, 1, by = 0.1))
roc <- data.frame(threshold = thresholds, tpr = NA_real_, fpr = NA_real_)

for (i in seq_along(thresholds)) {
  pred <- as.integer(score >= thresholds[i])
  roc$tpr[i] <- sum(pred == 1 & actual == 1) / sum(actual == 1)
  roc$fpr[i] <- sum(pred == 1 & actual == 0) / sum(actual == 0)
}

print(roc)
