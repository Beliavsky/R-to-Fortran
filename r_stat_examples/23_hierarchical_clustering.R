# Hierarchical clustering in base R

set.seed(123)
x <- rbind(
  cbind(rnorm(10, 0, 0.3), rnorm(10, 0, 0.3)),
  cbind(rnorm(10, 3, 0.3), rnorm(10, 3, 0.3)),
  cbind(rnorm(10, 0, 0.3), rnorm(10, 4, 0.3))
)

d <- dist(x)
fit <- hclust(d, method = "complete")

cat("Merge matrix first six rows:\n")
print(head(fit$merge))

groups <- cutree(fit, k = 3)
cat("\nCluster assignments:\n")
print(groups)

cat("\nCluster sizes:\n")
print(table(groups))
