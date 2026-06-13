# Generate k-means clustering data and write it as a headed text file.

set.seed(123)
x <- rbind(
  cbind(rnorm(50, 0, 0.5), rnorm(50, 0, 0.5)),
  cbind(rnorm(50, 3, 0.5), rnorm(50, 3, 0.5)),
  cbind(rnorm(50, 0, 0.5), rnorm(50, 4, 0.5))
)

colnames(x) <- c("x1", "x2")

write.table(
  x,
  file = "kmeans_clustering_data.txt",
  row.names = FALSE,
  quote = FALSE
)

cat("wrote kmeans_clustering_data.txt\n")
