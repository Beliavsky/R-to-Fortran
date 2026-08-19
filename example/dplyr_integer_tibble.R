x <- matrix(
  c(
    1L, 2L, 3L, 4L,
    10L, 20L, 30L, 40L,
    5L, 4L, 3L, 2L
  ),
  nrow = 4
)
colnames(x) <- c("id", "value", "weight")
tbl <- tibble::as_tibble(x)

cat("Integer source tibble:\n")
print(tbl)

cat("\nInteger filter, mutate, and select pipeline:\n")
integer_result <- tbl |>
  dplyr::filter(value >= 20L, weight < 5L) |>
  dplyr::mutate(doubled = value * 2L) |>
  dplyr::select(id, doubled)
print(integer_result)

cat("\nReal-valued mutation promotes the result:\n")
promoted <- dplyr::mutate(tbl, ratio = value / weight)
print(promoted)
