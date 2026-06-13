# Proportion tests in base R

# One-sample proportion test:
# 62 successes in 100 trials, H0: p = 0.50
cat("One-sample proportion test:\n")
print(prop.test(x = 62, n = 100, p = 0.50, correct = FALSE))

# Two-sample proportion test:
# group 1: 50 successes out of 100
# group 2: 65 successes out of 120
cat("\nTwo-sample proportion test:\n")
print(prop.test(x = c(50, 65), n = c(100, 120), correct = FALSE))
