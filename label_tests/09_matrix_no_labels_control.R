# 09_matrix_no_labels_control.R
# Unlabeled matrices should still print generic R-style indices.
m <- matrix(1:6, nrow = 2)
cat("unlabeled matrix control:\n")
print(m)
