program tibble_etf_returns
use r_mod, only: dp, print_tibble, read_csv_tibble_real, r_tibble_real_t, &
   tibble_ncol, tibble_nrow, tibble_real_log_returns, tibble_real_stats
implicit none

character(len=512) :: csv_path
type(r_tibble_real_t) :: csv_tbl, price_tbl, return_tbl, stats_tbl
logical :: exists

csv_path = "asset_class_etf_prices.csv"
if (command_argument_count() >= 1) call get_command_argument(1, csv_path)
inquire(file=trim(csv_path), exist=exists)
if (.not. exists) then
   write(*, '(a)') "ETF return example skipped: CSV file not found"
   write(*, '(a)') "Pass its path after --, for example:"
   write(*, '(a)') "fpm run --example tibble_etf_returns -- asset_class_etf_prices.csv"
   stop
end if

csv_tbl = read_csv_tibble_real(trim(csv_path), max_cols=4, index_col="Date")
if (tibble_ncol(csv_tbl) < 1 .or. tibble_nrow(csv_tbl) < 2) &
   error stop "tibble_etf_returns: expected dated price columns"
price_tbl = csv_tbl
return_tbl = tibble_real_log_returns(price_tbl, scale=100.0_dp)
stats_tbl = tibble_real_stats(return_tbl)

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: ETF log returns in a real-only tibble"
write(*, '(a, /)') repeat("=", 72)
write(*, '(a, 1x, a)') "Price file:", trim(csv_path)
write(*, '(a, i0)') "Price observations: ", tibble_nrow(price_tbl)
write(*, '(a, i0)') "Complete return observations: ", tibble_nrow(return_tbl)
write(*, '(a, i0)') "Assets: ", tibble_ncol(return_tbl)
write(*, '(/, a)') "First five scaled log returns:"
call print_tibble(return_tbl, 5)

write(*, '(/, a)') "Return statistics (returns are multiplied by 100):"
call print_tibble(stats_tbl, integer_row_labels=[character(len=1) :: "n"], &
   decimal_places=4, row_numbers=.false.)
end program tibble_etf_returns
