x <- matrix(
  c(
    1, 2, 3, 4,
    10, 20, 30, 40,
    5, 4, 3, 2
  ),
  nrow = 4
)
colnames(x) <- c("id", "value", "weight")
tbl <- tibble::as_tibble(x)

cat("Source tibble:\n")
print(tbl)

cat("\nSelected columns:\n")
selected <- dplyr::select(tbl, weight, value)
print(selected)

cat("\nSelection using all_of():\n")
wanted <- c("id", "value")
selected_all <- dplyr::select(tbl, all_of(wanted))
print(selected_all)

cat("\nFiltered and mutated rows:\n")
threshold <- 20
filtered <- dplyr::filter(tbl, value >= threshold, weight < 5)
mutated <- dplyr::mutate(
  filtered,
  spread = value - weight,
  doubled = spread * 2
)
print(mutated)

cat("\nNative pipe workflow:\n")
piped <- tbl |>
  dplyr::filter(value > 10) |>
  dplyr::mutate(ratio = value / weight) |>
  dplyr::select(id, ratio)
print(piped)
