# Hierarchical clustering from file

x <- as.matrix(read.table(
  "kmeans_clustering_data.txt",
  header = TRUE
))
set.seed(123)

input_file <- "kmeans_clustering_data.txt"
d <- dist(x)
fit <- hclust(d, method = "complete")

cat("Data file: ", input_file, "\n")
cat("Number of observations: ", nrow(x), "\n")
cat("Merge matrix first six rows:\n")
print(head(fit$merge, 6))

groups <- cutree(fit, k = 3)
cat("\nCluster assignments:\n")
print(groups)

cat("\nCluster sizes:\n")
print(table(groups))
