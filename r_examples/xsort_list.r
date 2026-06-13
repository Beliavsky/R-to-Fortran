# Demonstrate sort.list() in base R.
# No plotting is used.

cat("sort.list() in base R\n\n")

cat("sort.list(x) returns the permutation of indices that sorts x.\n")
cat("That is, x[sort.list(x)] gives sorted x.\n\n")


cat("1. Basic numeric example\n\n")

x <- c(30, 10, 40, 20)

cat("x:\n")
print(x)

idx <- sort.list(x)

cat("\nsort.list(x):\n")
print(idx)

cat("\nx[sort.list(x)]:\n")
print(x[idx])

cat("\nCompare with sort(x):\n")
print(sort(x))

cat("\n")


cat("2. Decreasing order\n\n")

x <- c(30, 10, 40, 20)

cat("x:\n")
print(x)

idx <- sort.list(x, decreasing = TRUE)

cat("\nsort.list(x, decreasing = TRUE):\n")
print(idx)

cat("\nx[idx]:\n")
print(x[idx])

cat("\n")


cat("3. Character sorting\n\n")

s <- c("pear", "apple", "orange", "banana")

cat("s:\n")
print(s)

idx <- sort.list(s)

cat("\nsort.list(s):\n")
print(idx)

cat("\ns[idx]:\n")
print(s[idx])

cat("\n")


cat("4. Sorting one vector and applying the order to another\n\n")

names <- c("Ann", "Bob", "Cal", "Dee")
scores <- c(88, 95, 72, 91)

cat("names:\n")
print(names)

cat("\nscores:\n")
print(scores)

idx <- sort.list(scores, decreasing = TRUE)

cat("\nIndices that sort scores from high to low:\n")
print(idx)

cat("\nNames in descending score order:\n")
print(names[idx])

cat("\nScores in descending order:\n")
print(scores[idx])
