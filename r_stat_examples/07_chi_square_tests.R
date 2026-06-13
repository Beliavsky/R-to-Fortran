# Chi-square tests in base R

# Goodness-of-fit test
observed <- c(50, 30, 20)
expected_probs <- c(0.40, 0.40, 0.20)

cat("Chi-square goodness-of-fit test:\n")
print(chisq.test(observed, p = expected_probs))

# Test of independence
tab <- matrix(c(30, 20, 15,
                25, 35, 20),
              nrow = 2, byrow = TRUE)
rownames(tab) <- c("Male", "Female")
colnames(tab) <- c("Low", "Medium", "High")

cat("\nChi-square test of independence:\n")
print(tab)
print(chisq.test(tab))
