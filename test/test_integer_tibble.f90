program test_integer_tibble
use r_mod, only: dp, print_tibble, read_csv_tibble_integer, r_tibble_integer_t, &
   r_tibble_real_t, tempfile, tibble_integer, tibble_integer_col, &
   tibble_integer_filter, tibble_integer_mutate, tibble_integer_select, &
   tibble_ncol, tibble_nrow
implicit none

integer :: unit
integer :: columns(4, 3)
character(len=:), allocatable :: csv_path
type(r_tibble_integer_t) :: tbl, filtered, selected, mutated, from_csv
type(r_tibble_real_t) :: promoted

columns = reshape([1, 2, 3, 4, 10, 20, 30, 40, 5, 4, 3, 2], [4, 3])
tbl = tibble_integer([character(len=6) :: "id", "value", "weight"], columns, &
   [character(len=4) :: "row1", "row2", "row3", "row4"], row_label_name="key")
if (tibble_nrow(tbl) /= 4 .or. tibble_ncol(tbl) /= 3) error stop "integer tibble dimensions"
if (any(tibble_integer_col(tbl, "value") /= columns(:, 2))) error stop "integer tibble column"

filtered = tibble_integer_filter(tbl, columns(:, 2) >= 20 .and. columns(:, 3) < 5)
if (tibble_nrow(filtered) /= 3) error stop "integer tibble filter"
if (any(filtered%row_labels /= [character(len=4) :: "row2", "row3", "row4"])) &
   error stop "integer tibble filtered labels"
filtered = tibble_integer_filter(tbl, [.false., .false., .false., .false.])
if (tibble_nrow(filtered) /= 0 .or. tibble_ncol(filtered) /= 3) &
   error stop "integer tibble empty filter"

selected = tibble_integer_select(tbl, [character(len=6) :: "weight", "value"])
if (tibble_ncol(selected) /= 2) error stop "integer tibble select"
if (any(selected%integer_cols(:, 1) /= columns(:, 3))) error stop "integer tibble select values"

mutated = tibble_integer_mutate(tbl, "doubled", 2 * columns(:, 2))
mutated = tibble_integer_mutate(mutated, "weight", 99)
if (any(tibble_integer_col(mutated, "doubled") /= [20, 40, 60, 80])) &
   error stop "integer tibble vector mutation"
if (any(tibble_integer_col(mutated, "weight") /= 99)) error stop "integer tibble scalar mutation"
if (any(tibble_integer_col(tbl, "weight") /= columns(:, 3))) error stop "integer tibble mutation alias"

promoted = tibble_integer_mutate(tbl, "ratio", real(columns(:, 2), dp) / real(columns(:, 3), dp))
if (tibble_ncol(promoted) /= 4) error stop "integer tibble real promotion dimensions"
if (maxval(abs(promoted%real_cols(:, 4) - &
   real(columns(:, 2), dp) / real(columns(:, 3), dp))) > 1.0e-12_dp) &
   error stop "integer tibble real promotion values"

csv_path = tempfile("integer_tibble_")
open(newunit=unit, file=csv_path, status="replace", action="write")
write(unit, '(a)') "Date,count,group"
write(unit, '(a)') "2026-01-01,10,1"
write(unit, '(a)') "2026-01-02,20,2"
close(unit)
from_csv = read_csv_tibble_integer(csv_path, index_col="Date")
if (tibble_nrow(from_csv) /= 2 .or. tibble_ncol(from_csv) /= 2) &
   error stop "integer tibble CSV dimensions"
if (any(tibble_integer_col(from_csv, "count") /= [10, 20])) error stop "integer tibble CSV values"
if (trim(from_csv%row_label_name) /= "Date") error stop "integer tibble CSV index name"

call print_tibble(tbl, n=2)
end program test_integer_tibble
