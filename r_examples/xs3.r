# Minimal S3 class target for initial R-to-Fortran support.

stat_point <- function(x) {
  obj <- list(x = as.numeric(x))
  class(obj) <- "stat_point"
  obj
}

center <- function(object) {
  UseMethod("center")
}

center.default <- function(object) {
  mean(object)
}

center.stat_point <- function(object) {
  mean(object$x)
}

a <- stat_point(c(2, 4, 6, 8))
b <- c(1, 3, 5)

cat("class:", class(a), "\n")
cat("s3 center:", center(a), "\n")
cat("default center:", center(b), "\n")
