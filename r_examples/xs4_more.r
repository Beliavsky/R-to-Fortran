setClass("stat_point", slots = c(x = "numeric"))
setClass("weighted_point", contains = "stat_point", slots = c(weight = "numeric"))

invisible(setGeneric("center", function(object) standardGeneric("center")))
setMethod("center", "stat_point", function(object) mean(object@x))
setMethod("center", "weighted_point", function(object) sum(object@x * object@weight) / sum(object@weight))

invisible(setGeneric("spread", function(object) standardGeneric("spread")))
setMethod("spread", "stat_point", function(object) max(object@x) - min(object@x))

invisible(setGeneric("total_weight", function(object) standardGeneric("total_weight")))
setMethod("total_weight", "weighted_point", function(object) sum(object@weight))

a <- new("stat_point", x = c(2, 4, 6, 8))
w <- new("weighted_point", x = c(2, 4, 6, 8), weight = c(1, 1, 2, 4))

cat("stat class:", class(a), "\n")
cat("weighted class:", class(w), "\n")
cat("weighted is stat:", is(w, "stat_point"), "\n")
cat("stat center:", center(a), "\n")
cat("weighted center:", center(w), "\n")
cat("inherited spread:", spread(w), "\n")
cat("total weight:", total_weight(w), "\n")
