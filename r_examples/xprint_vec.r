print_vec <- function(name, x, digits = 6) {
  cat("\n", name, ":\n", sep = "")
  print(round(as.vector(x), digits))
}

x = sqrt(1:20)
print_vec("x", x)
