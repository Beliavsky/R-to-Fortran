# K-means clustering in base R

set.seed(123)
x <- rbind(
  cbind(rnorm(50, 0, 0.5), rnorm(50, 0, 0.5)),
  cbind(rnorm(50, 3, 0.5), rnorm(50, 3, 0.5)),
  cbind(rnorm(50, 0, 0.5), rnorm(50, 4, 0.5))
)

colnames(x) <- c("x1", "x2")

fit <- kmeans(x, centers = 3, nstart = 20)

cat("Cluster sizes:\n")
print(fit$size)

cat("\nCluster centers:\n")
print(fit$centers)

cat("\nWithin-cluster sum of squares:\n")
print(fit$withinss)
