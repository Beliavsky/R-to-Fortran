program test_real_tibble_workflow
use r_mod, only: dp, file_exists, file_remove, print_tibble, &
   read_csv_tibble_real, r_tibble_real_t, tempfile, tibble_ncol, &
   tibble_nrow, tibble_real_log_returns, tibble_real_stats
implicit none

character(len=:), allocatable :: csv_path
type(r_tibble_real_t) :: csv_tbl, limited, prices, returns, stats
real(kind=dp) :: aaa_return, bbb_return_1, bbb_return_2
integer :: unit
logical :: removed

csv_path = tempfile("r_mod_tibble_prices_")
if (file_exists(csv_path)) then
   removed = file_remove(csv_path)
   if (.not. removed) error stop "stale tibble workflow file cleanup failed"
end if
open(newunit=unit, file=csv_path, status="replace", action="write")
write(unit, '(a)') "Date,AAA,BBB"
write(unit, '(a)') "2026-01-01,100.0,50.0"
write(unit, '(a)') "2026-01-02,110.0,45.0"
write(unit, '(a)') "2026-01-03,121.0,49.5"
close(unit)

csv_tbl = read_csv_tibble_real(csv_path)
if (tibble_nrow(csv_tbl) /= 3 .or. tibble_ncol(csv_tbl) /= 3) &
   error stop "CSV tibble dimensions failed"

limited = read_csv_tibble_real(csv_path, max_rows=2, max_cols=2)
if (tibble_nrow(limited) /= 2 .or. tibble_ncol(limited) /= 2) &
   error stop "limited CSV tibble dimensions failed"
if (trim(limited%names(1)) /= "Date" .or. trim(limited%names(2)) /= "AAA") &
   error stop "limited CSV tibble names failed"
call assert_close(limited%real_cols(2, 2), 110.0_dp, &
   "limited CSV tibble last value")

limited = read_csv_tibble_real(csv_path, max_rows=0, max_cols=0)
if (tibble_nrow(limited) /= 0 .or. tibble_ncol(limited) /= 0) &
   error stop "zero-limit CSV tibble dimensions failed"

limited = read_csv_tibble_real(csv_path, max_rows=99, max_cols=99)
if (tibble_nrow(limited) /= 3 .or. tibble_ncol(limited) /= 3) &
   error stop "oversized-limit CSV tibble dimensions failed"

limited = read_csv_tibble_real(csv_path, max_rows=2, max_cols=1, &
   index_col="Date")
if (tibble_nrow(limited) /= 2 .or. tibble_ncol(limited) /= 1) &
   error stop "limited indexed CSV tibble dimensions failed"
if (trim(limited%names(1)) /= "AAA" .or. &
    trim(limited%row_labels(2)) /= "2026-01-02") &
   error stop "limited indexed CSV tibble values failed"

prices = read_csv_tibble_real(csv_path, max_cols=2, index_col="Date")
if (tibble_nrow(prices) /= 3 .or. tibble_ncol(prices) /= 2) &
   error stop "indexed price tibble dimensions failed"
if (trim(prices%names(1)) /= "AAA" .or. trim(prices%names(2)) /= "BBB") &
   error stop "indexed price tibble names failed"
if (.not. allocated(prices%row_labels)) error stop "price index missing"
if (.not. allocated(prices%row_label_name)) error stop "price index name missing"
if (trim(prices%row_label_name) /= "Date") error stop "price index name failed"
if (trim(prices%row_labels(1)) /= "2026-01-01" .or. &
    trim(prices%row_labels(3)) /= "2026-01-03") &
   error stop "price index values failed"

returns = tibble_real_log_returns(prices, scale=100.0_dp)
if (tibble_nrow(returns) /= 2 .or. tibble_ncol(returns) /= 2) &
   error stop "tibble log-return dimensions failed"
aaa_return = 100.0_dp * log(1.1_dp)
bbb_return_1 = 100.0_dp * log(0.9_dp)
bbb_return_2 = aaa_return
call assert_close(returns%real_cols(1, 1), aaa_return, "first AAA return")
call assert_close(returns%real_cols(2, 1), aaa_return, "second AAA return")
call assert_close(returns%real_cols(1, 2), bbb_return_1, "first BBB return")
call assert_close(returns%real_cols(2, 2), bbb_return_2, "second BBB return")
if (trim(returns%row_labels(1)) /= "2026-01-02" .or. &
    trim(returns%row_labels(2)) /= "2026-01-03") &
   error stop "return index values failed"
if (.not. allocated(returns%row_label_name)) error stop "return index name missing"
if (trim(returns%row_label_name) /= "Date") error stop "return index name failed"

stats = tibble_real_stats(returns)
if (tibble_nrow(stats) /= 5 .or. tibble_ncol(stats) /= 2) &
   error stop "tibble statistics dimensions failed"
if (trim(stats%names(1)) /= "AAA" .or. trim(stats%names(2)) /= "BBB") &
   error stop "tibble statistics asset names failed"
call assert_row_labels(stats)
if (.not. allocated(stats%row_label_name)) error stop "statistics row-label name missing"
if (trim(stats%row_label_name) /= "statistic") error stop "statistics row-label name failed"
call assert_close(stats%real_cols(1, 1), 2.0_dp, "AAA count")
call assert_close(stats%real_cols(2, 1), aaa_return, "AAA mean")
call assert_close(stats%real_cols(3, 1), 0.0_dp, "AAA sd")
call assert_close(stats%real_cols(4, 1), aaa_return, "AAA minimum")
call assert_close(stats%real_cols(5, 1), aaa_return, "AAA maximum")
call assert_close(stats%real_cols(2, 2), &
   0.5_dp * (bbb_return_1 + bbb_return_2), "BBB mean")
call assert_close(stats%real_cols(3, 2), &
   abs(bbb_return_2 - bbb_return_1) / sqrt(2.0_dp), "BBB sd")

write(*, '(a)') "Imported numeric CSV:"
call print_tibble(csv_tbl)
write(*, '(/, a)') "Price tibble indexed by Date:"
call print_tibble(prices)
write(*, '(/, a)') "Scaled log returns:"
call print_tibble(returns)
write(*, '(/, a)') "Return statistics (assets are columns; statistics are row labels):"
call print_tibble(stats, integer_row_labels=[character(len=1) :: "n"], &
   decimal_places=4, row_numbers=.false.)
write(*, '(/, a)') "All real-tibble workflow checks passed."

removed = file_remove(csv_path)
if (.not. removed) error stop "tibble workflow file removal failed"

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > 1.0e-11_dp) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_row_labels(tbl)
type(r_tibble_real_t), intent(in) :: tbl
character(len=7), parameter :: expected(5) = &
   ["n      ", "mean   ", "sd     ", "minimum", "maximum"]
integer :: i

if (.not. allocated(tbl%row_labels)) error stop "statistics row labels missing"
if (size(tbl%row_labels) /= size(expected)) error stop "statistics row-label count failed"
do i = 1, size(expected)
   if (trim(tbl%row_labels(i)) /= trim(expected(i))) &
      error stop "statistics row-label value failed"
end do
end subroutine assert_row_labels
end program test_real_tibble_workflow
