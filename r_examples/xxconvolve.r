slow_convolve <- function(a, b) {
  declare(type(a = double(NA)),
          type(b = double(NA)))
  ab <- double(length(a) + length(b) - 1)
  for (i in seq_along(a)) {
    for (j in seq_along(b)) {
      ab[i+j-1] <- ab[i+j-1] + a[i] * b[j]
    }
  }
  ab
}

a <- (1:100000) * 1.0
b <- (1:1000) * 1.0
ab = slow_convolve(a,b)
print(mean(ab))
