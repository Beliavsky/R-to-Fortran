program test_real_tibble
use r_mod, only: dp, print_tibble, r_tibble_real_t, tibble_ncol, &
   tibble_nrow, tibble_real, tibble_real_col, tibble_real_filter, &
   tibble_real_mutate, tibble_real_select
implicit none

real(kind=dp) :: columns(4, 3)
type(r_tibble_real_t) :: tbl, filtered, selected, mutated

columns(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
columns(:, 2) = [10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp]
columns(:, 3) = [5.0_dp, 4.0_dp, 3.0_dp, 2.0_dp]
tbl = tibble_real([character(len=6) :: "id", "value", "weight"], columns, &
   [character(len=5) :: "row1", "row2", "row3", "row4"])

if (tibble_nrow(tbl) /= 4 .or. tibble_ncol(tbl) /= 3) &
   error stop "real tibble dimensions failed"
call assert_vector(tibble_real_col(tbl, "value"), columns(:, 2), &
   "named real tibble column")

filtered = tibble_real_filter(tbl, [.true., .false., .true., .false.])
if (tibble_nrow(filtered) /= 2 .or. tibble_ncol(filtered) /= 3) &
   error stop "real tibble filter dimensions failed"
if (trim(filtered%row_labels(1)) /= "row1" .or. &
   trim(filtered%row_labels(2)) /= "row3") &
   error stop "real tibble filter row labels failed"
call assert_vector(tibble_real_col(filtered, "id"), [1.0_dp, 3.0_dp], &
   "real tibble filter values")

filtered = tibble_real_filter(tbl, [.false., .false., .false., .false.])
if (tibble_nrow(filtered) /= 0 .or. tibble_ncol(filtered) /= 3) &
   error stop "real tibble zero-row filter dimensions failed"
if (.not. allocated(filtered%row_labels) .or. size(filtered%row_labels) /= 0) &
   error stop "real tibble zero-row filter labels failed"

selected = tibble_real_select(tbl, [character(len=6) :: "weight", "value"])
if (tibble_nrow(selected) /= 4 .or. tibble_ncol(selected) /= 2) &
   error stop "real tibble select dimensions failed"
if (trim(selected%names(1)) /= "weight" .or. trim(selected%names(2)) /= "value") &
   error stop "real tibble select names failed"
if (any(selected%row_labels /= tbl%row_labels)) &
   error stop "real tibble select row labels failed"
call assert_vector(selected%real_cols(:, 1), columns(:, 3), &
   "real tibble select order")

mutated = tibble_real_mutate(tbl, "scaled", columns(:, 2) / 10.0_dp)
if (tibble_ncol(mutated) /= 4) error stop "real tibble append mutate failed"
if (any(mutated%row_labels /= tbl%row_labels)) &
   error stop "real tibble mutate row labels failed"
call assert_vector(tibble_real_col(mutated, "scaled"), &
   [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], "real tibble append values")

mutated = tibble_real_mutate(mutated, "weight", 99.0_dp)
if (tibble_ncol(mutated) /= 4) error stop "real tibble replacement added a column"
call assert_vector(tibble_real_col(mutated, "weight"), &
   [99.0_dp, 99.0_dp, 99.0_dp, 99.0_dp], "real tibble scalar recycling")
call assert_vector(tibble_real_col(tbl, "weight"), columns(:, 3), &
   "real tibble mutation changed input")

call print_tibble(tbl, 2)

contains

subroutine assert_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " size failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " failed"
end subroutine assert_vector
end program test_real_tibble
