# Fisher exact test in base R

tab <- matrix(c(8, 2,
                1, 5),
              nrow = 2, byrow = TRUE)
rownames(tab) <- c("Treatment", "Control")
colnames(tab) <- c("Success", "Failure")

print(tab)
print(fisher.test(tab))
