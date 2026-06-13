setClass("stat_point", slots = c(x = "numeric"))

invisible(setGeneric("center", function(object) standardGeneric("center")))
setMethod("center", "stat_point", function(object) mean(object@x))

invisible(setGeneric("spread", function(object) standardGeneric("spread")))
setMethod("spread", "stat_point", function(object) max(object@x) - min(object@x))

a <- new("stat_point", x = c(2, 4, 6, 8))

cat("s4 center:", center(a), "\n")
cat("s4 spread:", spread(a), "\n")
