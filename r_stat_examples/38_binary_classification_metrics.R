# Binary classification metrics in base R

set.seed(123)
n <- 200
actual <- rbinom(n, 1, 0.4)
score <- 0.3 + 0.5 * actual + rnorm(n, sd = 0.4)
prob <- 1 / (1 + exp(-score))

pred <- as.integer(prob >= 0.5)

tab <- table(predicted = pred, actual = actual)
print(tab)

accuracy <- mean(pred == actual)
sensitivity <- sum(pred == 1 & actual == 1) / sum(actual == 1)
specificity <- sum(pred == 0 & actual == 0) / sum(actual == 0)
precision <- sum(pred == 1 & actual == 1) / sum(pred == 1)

cat("accuracy =", accuracy, "\n")
cat("sensitivity =", sensitivity, "\n")
cat("specificity =", specificity, "\n")
cat("precision =", precision, "\n")
