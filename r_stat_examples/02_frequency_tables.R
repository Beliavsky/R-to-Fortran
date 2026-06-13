# Frequency tables in base R

set.seed(123)
group <- sample(c("A", "B", "C"), size = 100, replace = TRUE,
                prob = c(0.30, 0.50, 0.20))
outcome <- sample(c("success", "failure"), size = 100, replace = TRUE)

cat("One-way frequency table:\n")
print(table(group))

cat("\nProportions:\n")
print(prop.table(table(group)))

cat("\nTwo-way table:\n")
tab <- table(group, outcome)
print(tab)

cat("\nRow proportions:\n")
print(prop.table(tab, margin = 1))

cat("\nColumn proportions:\n")
print(prop.table(tab, margin = 2))
