square <- function(x) x^2

x <- c(1.0, 2.0, 3.0, 4.0)
xi <- 1:4

doubled <- purrr::map_dbl(x, function(value) value * 2.0)
squared <- purrr::map_dbl(x, square)
shifted <- purrr::map_int(xi, \(value) value + 1L)
even <- purrr::map_lgl(xi, function(value) value %% 2L == 0L)

cat("Doubled values:\n")
print(doubled)
cat("Squared values:\n")
print(squared)
cat("Shifted integers:\n")
print(shifted)
cat("Even-number mask:\n")
print(even)
