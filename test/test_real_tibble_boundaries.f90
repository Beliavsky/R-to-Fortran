program test_real_tibble_boundaries
use r_mod, only: dp, print_tibble, r_tibble_real_t, tibble_ncol, &
   tibble_nrow, tibble_real, tibble_real_filter, tibble_real_mutate, &
   tibble_real_select
implicit none

real(kind=dp) :: columns(3, 2)
type(r_tibble_real_t) :: tbl, empty_rows, empty_columns, result

columns(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp]
columns(:, 2) = [4.0_dp, 5.0_dp, 6.0_dp]
tbl = tibble_real([character(len=1) :: "x", "y"], columns)

empty_rows = tibble_real([character(len=1) :: "x", "y"], &
   reshape([real(kind=dp) ::], [0, 2]))
if (tibble_nrow(empty_rows) /= 0 .or. tibble_ncol(empty_rows) /= 2) &
   error stop "zero-row real tibble failed"

empty_columns = tibble_real([character(len=1) ::], &
   reshape([real(kind=dp) ::], [3, 0]))
if (tibble_nrow(empty_columns) /= 3 .or. tibble_ncol(empty_columns) /= 0) &
   error stop "zero-column real tibble failed"

result = tibble_real_filter(tbl, [.false., .false., .false.])
if (tibble_nrow(result) /= 0 .or. tibble_ncol(result) /= 2) &
   error stop "all-false real tibble filter failed"

result = tibble_real_select(tbl, [character(len=1) ::])
if (tibble_nrow(result) /= 3 .or. tibble_ncol(result) /= 0) &
   error stop "empty real tibble selection failed"

result = tibble_real_mutate(empty_columns, "constant", 7.0_dp)
if (tibble_nrow(result) /= 3 .or. tibble_ncol(result) /= 1) &
   error stop "mutating zero-column real tibble failed"
if (any(abs(result%real_cols(:, 1) - 7.0_dp) > 1.0e-12_dp)) &
   error stop "zero-column real tibble scalar recycling failed"

result = tibble_real_mutate(empty_rows, "z", 1.0_dp)
if (tibble_nrow(result) /= 0 .or. tibble_ncol(result) /= 3) &
   error stop "mutating zero-row real tibble failed"

call print_tibble(empty_rows)
call print_tibble(empty_columns)
end program test_real_tibble_boundaries
