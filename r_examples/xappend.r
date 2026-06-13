# Demonstrate append() in base R.
# append(x, values, after = length(x)) inserts values into a vector.

cat("Basic append\n")

x <- c(10, 20, 30)

cat("x:\n")
print(x)

cat("append(x, 40):\n")
print(append(x, 40))

cat("\n")


cat("Append multiple values\n")

cat("append(x, c(40, 50, 60)):\n")
print(append(x, c(40, 50, 60)))

cat("\n")


cat("Insert after a specified position\n")

cat("append(x, 15, after = 1):\n")
print(append(x, 15, after = 1))

cat("append(x, c(25, 27), after = 2):\n")
print(append(x, c(25, 27), after = 2))

cat("\n")


cat("Insert at the beginning\n")

cat("append(x, 5, after = 0):\n")
print(append(x, 5, after = 0))

cat("\n")


cat("Append at the end explicitly\n")

cat("append(x, 40, after = length(x)):\n")
print(append(x, 40, after = length(x)))

cat("\n")


cat("append() does not modify the original vector unless assigned\n")

y <- append(x, 40)

cat("Original x:\n")
print(x)

cat("New y:\n")
print(y)

x <- append(x, 40)

cat("After assigning x <- append(x, 40), x is:\n")
print(x)

cat("\n")


cat("Appending character vectors\n")

s <- c("alpha", "beta", "gamma")

cat("s:\n")
print(s)

cat("append(s, 'delta'):\n")
print(append(s, "delta"))

cat("append(s, c('x', 'y'), after = 1):\n")
print(append(s, c("x", "y"), after = 1))

cat("\n")


cat("Appending logical vectors\n")

b <- c(TRUE, FALSE, TRUE)

cat("b:\n")
print(b)

cat("append(b, FALSE):\n")
print(append(b, FALSE))

cat("append(b, c(FALSE, FALSE), after = 2):\n")
print(append(b, c(FALSE, FALSE), after = 2))

cat("\n")


cat("Appending to an empty vector\n")

empty_num <- numeric(0)

cat("empty_num:\n")
print(empty_num)

cat("append(empty_num, c(1, 2, 3)):\n")
print(append(empty_num, c(1, 2, 3)))

cat("\n")


cat("Appending lists\n")

lst <- list(a = 1, b = 2)

cat("lst:\n")
print(lst)

cat("append(lst, list(c = 3)):\n")
print(append(lst, list(c = 3)))

cat("append(lst, list(x = 100, y = 200), after = 1):\n")
print(append(lst, list(x = 100, y = 200), after = 1))

cat("\n")


cat("Type coercion example\n")
cat("Appending character values to numeric values coerces the result to character.\n")

num <- c(1, 2, 3)

cat("num:\n")
print(num)

cat("append(num, 'four'):\n")
print(append(num, "four"))

cat("class(append(num, 'four')):\n")
print(class(append(num, "four")))

cat("\n")


cat("Comparison with c()\n")

x <- c(10, 20, 30)

cat("c(x, 40) appends to the end:\n")
print(c(x, 40))

cat("append(x, 40) gives the same result by default:\n")
print(append(x, 40))

cat("append(x, 15, after = 1) inserts in the middle, which c() does not do directly:\n")
print(append(x, 15, after = 1))
