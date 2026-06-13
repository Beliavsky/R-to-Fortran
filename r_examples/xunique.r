# unique() examples on vectors

# 1. Numeric vector
x <- c(3, 1, 2, 3, 2, 4, 1)
unique(x)
# [1] 3 1 2 4


# 2. Character vector
names <- c("Ann", "Bob", "Ann", "Carl", "Bob")
unique(names)
# [1] "Ann"  "Bob"  "Carl"


# 3. Logical vector
b <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
unique(b)
# [1]  TRUE FALSE


# 4. Integer vector
i <- c(1L, 2L, 1L, 3L, 2L)
unique(i)
# [1] 1 2 3

typeof(unique(i))
# [1] "integer"


# 5. Double vector
d <- c(1, 2, 1, 3, 2)
unique(d)
# [1] 1 2 3

typeof(unique(d))
# [1] "double"


# 6. unique() preserves first occurrence order
x <- c(5, 2, 5, 3, 2, 1)
unique(x)
# [1] 5 2 3 1


# 7. With NA values
x <- c(1, NA, 2, NA, 1, 3)
unique(x)
# [1]  1 NA  2  3


# 8. With NaN values
x <- c(1, NaN, 2, NaN, 1)
unique(x)
# [1]   1 NaN   2


# 9. Mixed types are coerced before unique()
x <- c(1, "1", 2, "2", 1)
x
# [1] "1" "1" "2" "2" "1"

unique(x)
# [1] "1" "2"


# 10. Using unique() to count distinct values
x <- c("red", "blue", "red", "green", "blue")

u <- unique(x)
u
# [1] "red"   "blue"  "green"

length(u)
# [1] 3


# 11. Sort the unique values
x <- c(4, 2, 3, 2, 1, 4)

unique(x)
# [1] 4 2 3 1

sort(unique(x))
# [1] 1 2 3 4
