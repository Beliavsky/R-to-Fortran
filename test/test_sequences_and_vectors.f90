program test_sequences_and_vectors
use, intrinsic :: ieee_arithmetic, only: ieee_is_negative, ieee_quiet_nan, ieee_value
use r_mod, only: cummax, cumprod, cumsum, diff, dp, r_seq_int, r_seq_int_by, &
   r_inf, r_is_nan, r_na_real, r_seq_int_length, r_seq_len, rev_int, rev_real, is_na
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: matrix_values(3, 2), expected_difference(2, 2), ordinary_nan, na_value
integer, allocatable :: empty_integer(:)
real(kind=dp), allocatable :: empty_real(:), cumulative(:), missing_differences(:,:)

call assert_integer_vector(r_seq_int(1, 5), [1, 2, 3, 4, 5], "ascending sequence")
call assert_integer_vector(r_seq_int(3, -1), [3, 2, 1, 0, -1], "descending sequence")
call assert_integer_vector(r_seq_len(5), [1, 2, 3, 4, 5], "sequence length")
call assert_integer_vector(r_seq_int_by(2, 10, 3), [2, 5, 8], "stepped sequence")
call assert_integer_vector(r_seq_int_length(0, 10, 6), [0, 2, 4, 6, 8, 10], &
   "fixed-length sequence")
if (size(r_seq_len(0)) /= 0) error stop "zero-length sequence failed"
if (size(r_seq_int_by(3, 1, 1)) /= 0) error stop "wrong-direction sequence failed"
if (size(r_seq_int_by(1, 3, 0)) /= 0) error stop "zero-step sequence failed"

call assert_integer_vector(cumsum([1, 2, 3, 4]), [1, 3, 6, 10], "integer cumulative sum")
call assert_integer_vector(cumprod([1, 2, 3, 4]), [1, 2, 6, 24], "integer cumulative product")
call assert_integer_vector(cummax([3, 1, 4, 2]), [3, 3, 4, 4], "integer cumulative maximum")
call assert_integer_vector(diff([1, 4, 9, 16]), [3, 5, 7], "integer differences")
call assert_integer_vector(rev_int([1, 2, 3, 4]), [4, 3, 2, 1], "integer reversal")
call assert_integer_vector(cumsum([1, -huge(0), 2]), [1, -huge(0), -huge(0)], &
   "integer cumulative sum NA propagation")
call assert_integer_vector(cumprod([1, -huge(0), 2]), [1, -huge(0), -huge(0)], &
   "integer cumulative product NA propagation")
call assert_integer_vector(cummax([1, -huge(0), 2]), [1, -huge(0), -huge(0)], &
   "integer cumulative maximum NA propagation")
call assert_integer_vector(diff([1, -huge(0), 2]), [-huge(0), -huge(0)], &
   "integer difference NA propagation")
call assert_integer_vector(cumsum([-huge(0), 2, 3]), [-huge(0), -huge(0), -huge(0)], &
   "leading integer cumulative NA propagation")
call assert_integer_vector(diff([-huge(0), 2, -huge(0)]), [-huge(0), -huge(0)], &
   "integer differences around NA")

call assert_real_vector(cumsum([0.5_dp, 1.5_dp, -2.0_dp]), [0.5_dp, 2.0_dp, 0.0_dp], &
   "real cumulative sum")
call assert_real_vector(cumprod([0.5_dp, 2.0_dp, -3.0_dp]), [0.5_dp, 1.0_dp, -3.0_dp], &
   "real cumulative product")
call assert_real_vector(cummax([-2.0_dp, -3.0_dp, 1.0_dp]), [-2.0_dp, -2.0_dp, 1.0_dp], &
   "real cumulative maximum")
call assert_real_vector(diff([1.0_dp, 1.5_dp, 3.5_dp]), [0.5_dp, 2.0_dp], "real differences")
call assert_real_vector(rev_real([1.5_dp, 2.5_dp, 3.5_dp]), [3.5_dp, 2.5_dp, 1.5_dp], &
   "real reversal")

ordinary_nan = ieee_value(0.0_dp, ieee_quiet_nan)
na_value = r_na_real()
call assert_cumulative_missing(cumsum([1.0_dp, ordinary_nan, na_value]), "cumsum")
call assert_cumulative_missing(cumprod([1.0_dp, ordinary_nan, na_value]), "cumprod")
call assert_cumulative_missing(cummax([1.0_dp, ordinary_nan, na_value]), "cummax")
cumulative = cumsum([-0.0_dp, 0.0_dp, -0.0_dp])
if (any(ieee_is_negative(cumulative))) error stop "cumsum signed zero failed"
cumulative = cumprod([-0.0_dp, 0.0_dp, -0.0_dp])
if (.not. ieee_is_negative(cumulative(1)) .or. .not. ieee_is_negative(cumulative(2)) .or. &
   ieee_is_negative(cumulative(3))) error stop "cumprod signed zero failed"
cumulative = cummax([-0.0_dp, 0.0_dp, -0.0_dp])
if (.not. ieee_is_negative(cumulative(1)) .or. ieee_is_negative(cumulative(2)) .or. &
   .not. ieee_is_negative(cumulative(3))) error stop "cummax signed zero failed"
cumulative = cumsum([-r_inf(), 1.0_dp, r_inf()])
if (cumulative(1) /= -r_inf() .or. cumulative(2) /= -r_inf() .or. &
   .not. r_is_nan(cumulative(3))) error stop "cumsum infinity handling failed"
call assert_real_vector(cumprod([-r_inf(), 1.0_dp, r_inf()]), &
   [-r_inf(), -r_inf(), -r_inf()], "cumprod infinities")
call assert_real_vector(cummax([-r_inf(), 1.0_dp, r_inf()]), &
   [-r_inf(), 1.0_dp, r_inf()], "cummax infinities")
cumulative = diff([1.0_dp, na_value, 3.0_dp])
if (.not. all(is_na(cumulative)) .or. any(r_is_nan(cumulative))) &
   error stop "diff NA propagation failed"
cumulative = diff([1.0_dp, ordinary_nan, 3.0_dp])
if (.not. all(r_is_nan(cumulative))) error stop "diff NaN propagation failed"
cumulative = diff([ordinary_nan, na_value])
if (r_is_nan(cumulative(1))) error stop "diff left NA precedence failed"
cumulative = diff([na_value, ordinary_nan])
if (.not. r_is_nan(cumulative(1))) error stop "diff left NaN precedence failed"
cumulative = diff([-0.0_dp, 0.0_dp, -0.0_dp])
if (ieee_is_negative(cumulative(1)) .or. .not. ieee_is_negative(cumulative(2))) &
   error stop "diff signed zero failed"
missing_differences = diff(reshape([1.0_dp, na_value, 3.0_dp, ordinary_nan, 5.0_dp, 7.0_dp], [3, 2]))
if (any(shape(missing_differences) /= [2, 2])) error stop "matrix missing diff shape failed"
if (r_is_nan(missing_differences(1, 1)) .or. r_is_nan(missing_differences(2, 1)) .or. &
   .not. r_is_nan(missing_differences(1, 2)) .or. &
   abs(missing_differences(2, 2) - 2.0_dp) > tolerance) error stop "matrix missing diff failed"

matrix_values(1, :) = [1.0_dp, 10.0_dp]
matrix_values(2, :) = [3.0_dp, 15.0_dp]
matrix_values(3, :) = [8.0_dp, 21.0_dp]
expected_difference(1, :) = [2.0_dp, 5.0_dp]
expected_difference(2, :) = [5.0_dp, 6.0_dp]
call assert_real_matrix(diff(matrix_values), expected_difference, "matrix row differences")

allocate(empty_integer(0), empty_real(0))
if (size(cumsum(empty_integer)) /= 0) error stop "empty integer cumulative sum failed"
if (size(cumprod(empty_real)) /= 0) error stop "empty real cumulative product failed"
if (size(diff(empty_integer)) /= 0) error stop "empty integer differences failed"
if (size(diff([1.0_dp])) /= 0) error stop "singleton real differences failed"
if (size(rev_real(empty_real)) /= 0) error stop "empty real reversal failed"

contains

subroutine assert_cumulative_missing(actual, label)
real(kind=dp), intent(in) :: actual(:)
character(len=*), intent(in) :: label

if (size(actual) /= 3) error stop trim(label) // " missing-value shape failed"
if (actual(1) /= 1.0_dp) error stop trim(label) // " finite prefix failed"
if (.not. r_is_nan(actual(2))) error stop trim(label) // " NaN propagation failed"
if (.not. is_na(actual(3))) error stop trim(label) // " NA precedence failed"
end subroutine assert_cumulative_missing

subroutine assert_integer_vector(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(actual /= expected)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_integer_vector

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_real_vector

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) then
   write(*, '(a)') trim(label) // " shape failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_real_matrix
end program test_sequences_and_vectors
