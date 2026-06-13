# string_functions_fortran_like_demo.R
# Demonstrate base R string functions that have close Fortran counterparts
# or could be easily coded in standard Fortran.
#
# This avoids regular expressions and approximate matching.
# The examples use simple literal string operations.

cat("\n1. Character vector\n")

x <- c("apple", "banana", "pear", "apricot", "blueberry")

cat("x:\n")
print(x)


cat("\n2. nchar(): string lengths\n")

# Fortran counterparts:
#   len(s)       gives declared length
#   len_trim(s)  gives length excluding trailing blanks

cat("nchar(x):\n")
print(nchar(x))


cat("\n3. paste0(): concatenate with no separator\n")

# Fortran counterpart:
#   a // b

a <- "abc"
b <- "def"

cat("paste0(a, b):\n")
print(paste0(a, b))


cat("\n4. paste(): concatenate with a separator\n")

# Fortran equivalent:
#   trim(a) // sep // trim(b)

cat("paste(a, b):\n")
print(paste(a, b))

cat("paste(a, b, sep = '_'):\n")
print(paste(a, b, sep = "_"))


cat("\n5. paste() over vectors\n")

first <- c("x", "y", "z")
second <- c("10", "20", "30")

cat("paste0(first, second):\n")
print(paste0(first, second))

cat("paste(first, second, sep = '='):\n")
print(paste(first, second, sep = "="))


cat("\n6. collapse a vector into one string\n")

# Easy to code in Fortran with a loop and delimiter.

cat("paste(x, collapse = ', '):\n")
print(paste(x, collapse = ", "))

cat("toString(x):\n")
print(toString(x))


cat("\n7. substr(): extract substrings\n")

# Fortran counterpart:
#   s(i:j)

s <- "abcdef"

cat("s:\n")
print(s)

cat("substr(s, 2, 4):\n")
print(substr(s, 2, 4))

cat("substr(s, 1, 3):\n")
print(substr(s, 1, 3))

cat("substr(s, 4, 6):\n")
print(substr(s, 4, 6))


cat("\n8. substring(): extract from a start position\n")

cat("substring(s, 3):\n")
print(substring(s, 3))

cat("substring(s, 3, 5):\n")
print(substring(s, 3, 5))


cat("\n9. Assign into substrings\n")

# Fortran counterpart:
#   s(i:j) = replacement

s2 <- "abcdef"

cat("Before:\n")
print(s2)

substr(s2, 2, 4) <- "XYZ"

cat("After substr(s2, 2, 4) <- 'XYZ':\n")
print(s2)


cat("\n10. toupper() and tolower()\n")

# Standard Fortran has no intrinsic toupper/tolower,
# but ASCII versions are easy to code using iachar() and achar().

s <- "AbcDef123"

cat("s:\n")
print(s)

cat("toupper(s):\n")
print(toupper(s))

cat("tolower(s):\n")
print(tolower(s))


cat("\n11. casefold()\n")

cat("casefold(s, upper = TRUE):\n")
print(casefold(s, upper = TRUE))

cat("casefold(s, upper = FALSE):\n")
print(casefold(s, upper = FALSE))


cat("\n12. startsWith(): prefix test\n")

# Easy Fortran equivalent:
#   s(1:len(prefix)) == prefix

cat("startsWith(x, 'ap'):\n")
print(startsWith(x, "ap"))

cat("startsWith(x, 'ba'):\n")
print(startsWith(x, "ba"))


cat("\n13. endsWith(): suffix test\n")

# Easy Fortran equivalent:
#   s(len_trim(s)-len_trim(suffix)+1:len_trim(s)) == suffix

cat("endsWith(x, 'e'):\n")
print(endsWith(x, "e"))

cat("endsWith(x, 'ry'):\n")
print(endsWith(x, "ry"))


cat("\n14. grepl() with fixed = TRUE: literal substring search\n")

# Fortran counterpart:
#   index(s, pattern) > 0

cat("grepl('ap', x, fixed = TRUE):\n")
print(grepl("ap", x, fixed = TRUE))

cat("grepl('err', x, fixed = TRUE):\n")
print(grepl("err", x, fixed = TRUE))


cat("\n15. grep() with fixed = TRUE: indices of literal matches\n")

cat("grep('ap', x, fixed = TRUE):\n")
print(grep("ap", x, fixed = TRUE))

cat("x[grep('ap', x, fixed = TRUE)]:\n")
print(x[grep("ap", x, fixed = TRUE)])


cat("\n16. regexpr() with fixed = TRUE: first match position\n")

# Fortran counterpart:
#   index(s, pattern)

s <- "abc_def_abc"

cat("s:\n")
print(s)

cat("regexpr('def', s, fixed = TRUE):\n")
print(regexpr("def", s, fixed = TRUE))

cat("as.integer(regexpr('def', s, fixed = TRUE)):\n")
print(as.integer(regexpr("def", s, fixed = TRUE)))

cat("regexpr('xyz', s, fixed = TRUE):\n")
print(regexpr("xyz", s, fixed = TRUE))

cat("as.integer(regexpr('xyz', s, fixed = TRUE)):\n")
print(as.integer(regexpr("xyz", s, fixed = TRUE)))


cat("\n17. sub() with fixed = TRUE: replace first literal match\n")

# Easy to code in Fortran using index() and concatenation.

s <- "abc-def-def"

cat("s:\n")
print(s)

cat("sub('def', 'XYZ', s, fixed = TRUE):\n")
print(sub("def", "XYZ", s, fixed = TRUE))


cat("\n18. gsub() with fixed = TRUE: replace all literal matches\n")

# Easy to code in Fortran with a loop using index().

cat("gsub('def', 'XYZ', s, fixed = TRUE):\n")
print(gsub("def", "XYZ", s, fixed = TRUE))


cat("\n19. strsplit() with fixed = TRUE: split on a simple delimiter\n")

# Easy to code in Fortran using repeated index() calls.

s <- "alpha,beta,gamma"

cat("s:\n")
print(s)

cat("strsplit(s, ',', fixed = TRUE):\n")
parts <- strsplit(s, ",", fixed = TRUE)
print(parts)

cat("First split result as a character vector:\n")
print(parts[[1]])


cat("\n20. strsplit() on a vector of strings\n")

svec <- c("a,b,c", "d,e", "f")

cat("svec:\n")
print(svec)

cat("strsplit(svec, ',', fixed = TRUE):\n")
print(strsplit(svec, ",", fixed = TRUE))


cat("\n21. match(): exact string matching\n")

# Easy to code in Fortran with nested loops.

table <- c("red", "green", "blue")
query <- c("blue", "red", "black", "green")

cat("table:\n")
print(table)

cat("query:\n")
print(query)

cat("match(query, table):\n")
print(match(query, table))


cat("\n22. %in%: membership test\n")

# Easy to code in Fortran with loops.

cat("query %in% table:\n")
print(query %in% table)


cat("\n23. Sorting strings\n")

# Standard Fortran has lexical comparison operators/intrinsics,
# but no built-in sort. A simple sort is easy to code.

words <- c("banana", "apple", "pear", "apricot")

cat("words:\n")
print(words)

cat("sort(words):\n")
print(sort(words))

cat("order(words):\n")
print(order(words))

cat("words[order(words)]:\n")
print(words[order(words)])


cat("\n24. sprintf(): formatted strings\n")

# Fortran counterpart:
#   internal write, for example:
#   write(s, '(\"i = \", i0, \", x = \", f8.3)') i, x

i <- 7
y <- 3.14159

cat("sprintf('i = %d, y = %.3f', i, y):\n")
print(sprintf("i = %d, y = %.3f", i, y))


cat("\n25. format(): convert values to character form\n")

# Fortran counterpart:
#   formatted internal write.

nums <- c(1.2, 123.456, 10000)

cat("nums:\n")
print(nums)

cat("format(nums):\n")
print(format(nums))

cat("format(nums, scientific = FALSE):\n")
print(format(nums, scientific = FALSE))

cat("format(nums, digits = 4):\n")
print(format(nums, digits = 4))


cat("\n26. as.character(): convert non-strings to strings\n")

# Fortran counterpart:
#   internal write.

cat("as.character(1:5):\n")
print(as.character(1:5))

cat("as.character(c(TRUE, FALSE)):\n")
print(as.character(c(TRUE, FALSE)))


cat("\n27. is.character(): test whether an object is character\n")

cat("is.character(x):\n")
print(is.character(x))

cat("is.character(1:3):\n")
print(is.character(1:3))


cat("\n28. trimws(): trim leading and trailing whitespace\n")

# Fortran has trim() for trailing blanks.
# Leading trim is easy using adjustl() plus trim().
#
# R trimws() trims both leading and trailing whitespace.

s <- "   abc def   "

cat("Original string with spaces shown using brackets:\n")
print(paste0("[", s, "]"))

cat("trimws(s):\n")
print(paste0("[", trimws(s), "]"))

cat("trimws(s, which = 'left'):\n")
print(paste0("[", trimws(s, which = "left"), "]"))

cat("trimws(s, which = 'right'):\n")
print(paste0("[", trimws(s, which = "right"), "]"))


cat("\n29. chartr(): character-by-character translation\n")

# Similar functionality could be coded in Fortran by looping over characters.

s <- "abc-123-xyz"

cat("s:\n")
print(s)

cat("chartr('abcxyz', 'ABCXYZ', s):\n")
print(chartr("abcxyz", "ABCXYZ", s))


cat("\n30. Simple helper: literal string replacement without regex\n")

replace_literal_first <- function(s, old, new) {
  pos <- regexpr(old, s, fixed = TRUE)

  if (as.integer(pos) < 0) {
    return(s)
  }

  first <- as.integer(pos)
  last <- first + nchar(old) - 1

  paste0(
    substr(s, 1, first - 1),
    new,
    substr(s, last + 1, nchar(s))
  )
}

s <- "abc-def-def"

cat("s:\n")
print(s)

cat("replace_literal_first(s, 'def', 'XYZ'):\n")
print(replace_literal_first(s, "def", "XYZ"))


cat("\n31. Simple helper: starts_with coded from substr() and nchar()\n")

starts_with_simple <- function(s, prefix) {
  n <- nchar(prefix)
  substr(s, 1, n) == prefix
}

cat("starts_with_simple(x, 'ap'):\n")
print(starts_with_simple(x, "ap"))

cat("Compare with startsWith(x, 'ap'):\n")
print(startsWith(x, "ap"))


cat("\n32. Simple helper: ends_with coded from substr() and nchar()\n")

ends_with_simple <- function(s, suffix) {
  ns <- nchar(s)
  nf <- nchar(suffix)

  ok <- nf <= ns
  ans <- rep(FALSE, length(s))

  ans[ok] <- substr(s[ok], ns[ok] - nf + 1, ns[ok]) == suffix

  ans
}

cat("ends_with_simple(x, 'e'):\n")
print(ends_with_simple(x, "e"))

cat("Compare with endsWith(x, 'e'):\n")
print(endsWith(x, "e"))


cat("\nDone.\n")
