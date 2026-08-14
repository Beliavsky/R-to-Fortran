program test_empirical_distributions
use r_mod, only: cut, cut_n, dp, ecdf_eval, findInterval, hist, hist_result_t
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
type(hist_result_t) :: h
real(kind=dp), allocatable :: probabilities(:)
integer, allocatable :: bins(:)

h = hist([0.0_dp, 0.5_dp, 1.0_dp, 1.5_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   [0.0_dp, 1.0_dp, 2.0_dp, 4.0_dp], plot=.false.)
call assert_real_vector_close(h%breaks, [0.0_dp, 1.0_dp, 2.0_dp, 4.0_dp], &
   "explicit histogram breaks")
call assert_real_vector_close(h%mids, [0.5_dp, 1.5_dp, 3.0_dp], &
   "explicit histogram mids")
call assert_integer_vector_equal(h%counts, [3, 2, 2], "explicit histogram counts")
call assert_real_vector_close(h%density, [3.0_dp / 7.0_dp, 2.0_dp / 7.0_dp, 1.0_dp / 7.0_dp], &
   "explicit histogram density")
call assert_close(sum(h%density * [1.0_dp, 1.0_dp, 2.0_dp]), 1.0_dp, &
   "histogram density normalization")

h = hist([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], breaks=2, plot=.false.)
call assert_real_vector_close(h%breaks, [0.0_dp, 2.0_dp, 4.0_dp], &
   "automatic histogram breaks")
call assert_integer_vector_equal(h%counts, [2, 2], "automatic histogram counts")
call assert_real_vector_close(h%density, [0.25_dp, 0.25_dp], "automatic histogram density")

probabilities = ecdf_eval([1.0_dp, 2.0_dp, 2.0_dp, 4.0_dp], &
   [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp])
call assert_real_vector_close(probabilities, [0.0_dp, 0.25_dp, 0.75_dp, 0.75_dp, 1.0_dp, 1.0_dp], &
   "empirical cdf")

bins = findInterval([-1.0_dp, 0.0_dp, 0.5_dp, 1.0_dp, 2.0_dp, 3.0_dp], &
   [0.0_dp, 1.0_dp, 2.0_dp])
call assert_integer_vector_equal(bins, [0, 1, 1, 2, 3, 3], "interval lookup")
call assert_integer_vector_equal(findInterval([-1.0_dp, 0.0_dp, 1.0_dp], &
   [real(kind=dp) ::]), [0, 0, 0], "interval lookup with empty breakpoints")
if (size(findInterval([real(kind=dp) ::], [0.0_dp, 1.0_dp])) /= 0) &
   error stop "empty interval queries failed"

bins = cut([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   [0.0_dp, 2.0_dp, 4.0_dp], include_lowest=.true., labels=.false.)
call assert_integer_vector_equal(bins, [1, 1, 1, 2, 2], "cut including lowest boundary")
bins = cut([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   [0.0_dp, 2.0_dp, 4.0_dp], include_lowest=.false., labels=.false.)
call assert_integer_vector_equal(bins, [0, 1, 1, 2, 2], "cut excluding lowest boundary")
bins = cut([-1.0_dp, 0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp], &
   [0.0_dp, 2.0_dp], include_lowest=.true., labels=.false.)
call assert_integer_vector_equal(bins, [0, 1, 1, 1, 0], "cut outside break range")
bins = cut_n([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], 2, labels=.false.)
call assert_integer_vector_equal(bins, [1, 1, 1, 2, 2], "cut by bin count")

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_real_vector_close(actual, expected, label)
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
end subroutine assert_real_vector_close

subroutine assert_integer_vector_equal(actual, expected, label)
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
end subroutine assert_integer_vector_equal
end program test_empirical_distributions
