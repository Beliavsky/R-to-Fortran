# Demonstrate rle() on vectors
#
# rle means "run length encoding".
# It records consecutive runs of equal values.

# 1. Basic numeric example
x <- c(1, 1, 1, 2, 2, 5, 5, 5, 5, 1)

r <- rle(x)

r
# Run Length Encoding
#   lengths: int [1:4] 3 2 4 1
#   values : num [1:4] 1 2 5 1

r$lengths
# [1] 3 2 4 1

r$values
# [1] 1 2 5 1


# 2. Convert back to the original vector
inverse.rle(r)
# [1] 1 1 1 2 2 5 5 5 5 1

identical(x, inverse.rle(r))
# [1] TRUE


# 3. Character vector
x <- c("a", "a", "b", "b", "b", "a", "c", "c")

rle(x)
# Run Length Encoding
#   lengths: int [1:4] 2 3 1 2
#   values : chr [1:4] "a" "b" "a" "c"


# 4. Logical vector
x <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE)

r <- rle(x)

r$lengths
# [1] 2 3 1

r$values
# [1]  TRUE FALSE  TRUE


# 5. Count runs
x <- c(4, 4, 4, 2, 2, 9, 9, 4)

r <- rle(x)

length(r$lengths)
# [1] 4


# 6. Longest run
x <- c("up", "up", "down", "down", "down", "up", "up", "up", "up")

r <- rle(x)

max(r$lengths)
# [1] 4

r$values[which.max(r$lengths)]
# [1] "up"


# 7. Find all runs of length at least 3
x <- c(1, 1, 1, 2, 2, 3, 3, 3, 3, 2)

r <- rle(x)

r$values[r$lengths >= 3]
# [1] 1 3

r$lengths[r$lengths >= 3]
# [1] 3 4


# 8. Find repeated consecutive values
x <- c(10, 10, 20, 30, 30, 30, 40, 50, 50)

r <- rle(x)

r$values[r$lengths > 1]
# [1] 10 30 50

