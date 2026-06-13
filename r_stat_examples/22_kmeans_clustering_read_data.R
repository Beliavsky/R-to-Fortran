# K-means clustering in base R from a headed text file.

dat <- read.table("kmeans_clustering_data.txt", header = TRUE)
x <- as.matrix(dat)

km_fit <- kmeans(x, centers = 3, nstart = 20)

cat("Cluster sizes:\n")
print(km_fit$size)

cat("\nCluster centers:\n")
print(km_fit$centers)

cat("\nWithin-cluster sum of squares:\n")
print(km_fit$withinss)
