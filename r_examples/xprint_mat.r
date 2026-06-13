print_mat <- function(name, x, digits = 6) {
  cat("\n", name, ":\n", sep = "")
  print(round(x, digits))
}

x = sqrt(1:10)
print_mat("x", x)
