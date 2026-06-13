# Demonstrate replace() on vectors

# Basic form:
# replace(x, list, values)
#
# It returns a modified copy of x.
# It does not modify x in place.


# 1. Replace one element by position
x <- c(10, 20, 30, 40, 50)

y <- replace(x, 3, 999)

x
# [1] 10 20 30 40 50

y
# [1]  10  20 999  40  50


# 2. Replace several elements by position
x <- c(10, 20, 30, 40, 50)

replace(x, c(2, 5), c(200, 500))
# [1]  10 200  30  40 500


# 3. Replace several elements with one repeated value
x <- c(10, 20, 30, 40, 50)

replace(x, c(2, 5), 0)
# [1] 10  0 30 40  0


# 4. Replace using a logical condition
x <- c(3, -1, 5, -7, 2)

replace(x, x < 0, 0)
# [1] 3 0 5 0 2


# 5. Replace values larger than a cutoff
x <- c(4, 12, 7, 20, 3)

replace(x, x > 10, 10)
# [1]  4 10  7 10  3


# 6. Replace NA values
x <- c(1, NA, 3, NA, 5)

replace(x, is.na(x), 0)
# [1] 1 0 3 0 5


# 7. Replace NaN values
x <- c(1, NaN, 3, NaN, 5)

replace(x, is.nan(x), 0)
# [1] 1 0 3 0 5


# 8. Replace both NA and NaN
x <- c(1, NA, NaN, 4)

replace(x, is.na(x), 0)
# [1] 1 0 0 4


# 9. Replace character values
x <- c("red", "blue", "green", "blue")

replace(x, x == "blue", "cyan")
# [1] "red"   "cyan"  "green" "cyan"


# 10. Replace duplicate entries after first occurrence
x <- c("a", "b", "a", "c", "b", "a")

replace(x, duplicated(x), "dup")
# [1] "a"   "b"   "dup" "c"   "dup" "dup"


# 11. Replace non-duplicates
x <- c("a", "b", "a", "c", "b", "a")

replace(x, !duplicated(x), "first")
# [1] "first" "first" "a"     "first" "b"     "a"


# 12. Replace using which()
x <- c(5, 8, 12, 3, 20)

replace(x, which(x > 10), 999)
# [1]   5   8 999   3 999


# 13. Replace with a sequence of values
x <- c(10, 20, 30, 40, 50)

replace(x, c(2, 4), c(200, 400))
# [1]  10 200  30 400  50


# 14. Replacement values can be recycled
x <- c(10, 20, 30, 40, 50, 60)

replace(x, c(2, 4, 6), c(100, 200))
# [1]  10 100  30 200  50 100


# 15. Replace elements in an integer vector
x <- 1:6

typeof(x)
# [1] "integer"

y <- replace(x, c(2, 4), c(20L, 40L))

y
# [1]  1 20  3 40  5  6

typeof(y)
# [1] "integer"


# 16. Replacement may change the vector type
# x <- 1:5
# 
# y <- replace(x, 3, 3.5)
# 
# y
# # [1] 1.0 2.0 3.5 4.0 5.0
# 
# typeof(y)
# [1] "double"


# 17. Replace in a logical vector
x <- c(TRUE, FALSE, TRUE, FALSE)

replace(x, x == FALSE, TRUE)
# [1] TRUE TRUE TRUE TRUE


# 18. Equivalent direct indexing form
x <- c(10, 20, 30, 40, 50)

y <- x
y[c(2, 5)] <- c(200, 500)

y
# [1]  10 200  30  40 500

replace(x, c(2, 5), c(200, 500))
# [1]  10 200  30  40 500
