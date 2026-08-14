program test_real_sequences
use r_mod, only: dp, r_seq_real_by, r_seq_real_length
implicit none

call assert_close(r_seq_real_by(1.0_dp, 2.0_dp, 0.25_dp), &
   [1.0_dp, 1.25_dp, 1.5_dp, 1.75_dp, 2.0_dp], "ascending real sequence")
call assert_close(r_seq_real_by(2.0_dp, 0.5_dp, -0.5_dp), &
   [2.0_dp, 1.5_dp, 1.0_dp, 0.5_dp], "descending real sequence")
call assert_close(r_seq_real_by(0.0_dp, 0.3_dp, 0.1_dp), &
   [0.0_dp, 0.1_dp, 0.2_dp, 0.3_dp], "roundoff-tolerant real sequence")
if (size(r_seq_real_by(2.0_dp, 1.0_dp, 0.5_dp)) /= 0) error stop "invalid ascending direction failed"
if (size(r_seq_real_by(1.0_dp, 2.0_dp, -0.5_dp)) /= 0) error stop "invalid descending direction failed"
if (size(r_seq_real_by(1.0_dp, 2.0_dp, 0.0_dp)) /= 0) error stop "zero step failed"
call assert_close(r_seq_real_by(1.0_dp, 1.0_dp, -2.0_dp), [1.0_dp], &
   "equal endpoints with negative step")
call assert_close(r_seq_real_by(1.0_dp, 1.1_dp, 2.0_dp), [1.0_dp], &
   "step larger than interval")

call assert_close(r_seq_real_length(0.0_dp, 1.0_dp, 5), &
   [0.0_dp, 0.25_dp, 0.5_dp, 0.75_dp, 1.0_dp], "fixed-length real sequence")
call assert_close(r_seq_real_length(3.0_dp, -1.0_dp, 3), &
   [3.0_dp, 1.0_dp, -1.0_dp], "descending fixed-length sequence")
call assert_close(r_seq_real_length(4.0_dp, 9.0_dp, 1), [4.0_dp], "single-value sequence")
if (size(r_seq_real_length(1.0_dp, 2.0_dp, 0)) /= 0) error stop "zero sequence length failed"
if (size(r_seq_real_length(1.0_dp, 2.0_dp, -1)) /= 0) error stop "negative sequence length failed"
call assert_close(r_seq_real_length(-2.0_dp, 2.0_dp, 2), [-2.0_dp, 2.0_dp], &
   "two-value fixed-length sequence")

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_close
end program test_real_sequences
