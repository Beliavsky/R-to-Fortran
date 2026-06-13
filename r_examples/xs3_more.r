# Additional S3 class coverage for static R-to-Fortran lowering.

stat_point <- function(x) {
  obj <- list(x = as.numeric(x))
  class(obj) <- "stat_point"
  obj
}

weighted_point <- function(x, weight) {
  obj <- list(
    x = as.numeric(x),
    weight = as.numeric(weight)
  )
  class(obj) <- c("weighted_point", "stat_point")
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

center.weighted_point <- function(object) {
  sum(object$x * object$weight) / sum(object$weight)
}

spread <- function(object) {
  UseMethod("spread")
}

spread.default <- function(object) {
  max(object) - min(object)
}

spread.stat_point <- function(object) {
  max(object$x) - min(object$x)
}

a <- stat_point(c(2, 4, 6, 8))
w <- weighted_point(c(2, 4, 6, 8), c(1, 1, 2, 4))
b <- c(1, 3, 5)

cat("stat class:", class(a), "\n")
cat("weighted class:", class(w), "\n")
cat("stat center:", center(a), "\n")
cat("weighted center:", center(w), "\n")
cat("inherited spread:", spread(w), "\n")
cat("default spread:", spread(b), "\n")
