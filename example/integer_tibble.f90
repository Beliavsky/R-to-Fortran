program integer_tibble
use r_mod, only: dp, print_tibble, r_tibble_integer_t, r_tibble_real_t, &
   tibble_integer, tibble_integer_col, tibble_integer_filter, &
   tibble_integer_mutate, tibble_integer_select
implicit none

integer :: columns(4, 3)
type(r_tibble_integer_t) :: tbl, filtered, selected, mutated
type(r_tibble_real_t) :: promoted

columns = reshape([ &
   1, 2, 3, 4, &
   10, 20, 30, 40, &
   5, 4, 3, 2 &
], [4, 3])

tbl = tibble_integer( &
   [character(len=6) :: "id", "value", "weight"], &
   columns, &
   [character(len=4) :: "row1", "row2", "row3", "row4"], &
   row_label_name="key" &
)

write(*, '(a)') "Integer source tibble:"
call print_tibble(tbl)

filtered = tibble_integer_filter(tbl, &
   tibble_integer_col(tbl, "value") >= 20 .and. &
   tibble_integer_col(tbl, "weight") < 5)
mutated = tibble_integer_mutate(filtered, "doubled", &
   2 * tibble_integer_col(filtered, "value"))
selected = tibble_integer_select(mutated, &
   [character(len=7) :: "id", "doubled"])

write(*, '(/,a)') "Filtered, mutated, and selected integer tibble:"
call print_tibble(selected)

promoted = tibble_integer_mutate(tbl, "ratio", &
   real(tibble_integer_col(tbl, "value"), kind=dp) / &
   real(tibble_integer_col(tbl, "weight"), kind=dp))

write(*, '(/,a)') "Real-valued mutation promotes the tibble:"
call print_tibble(promoted)
end program integer_tibble
