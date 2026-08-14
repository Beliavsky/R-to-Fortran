program test_empirical_distribution_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_quiet_nan, ieee_value
use r_mod, only: cut, cut_n, dp, ecdf_eval, findInterval, hist, &
   hist_result_t, quantile, r_inf, r_is_nan, r_na_real
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp), allocatable :: values(:)
integer, allocatable :: bins(:)
type(hist_result_t) :: histogram

histogram = hist([0.0_dp, 1.0_dp], breaks=1, plot=.false.)
if (sum(histogram%counts) /= 2) error stop "automatic histogram lowest boundary failed"
if (abs(sum(histogram%density * &
   (histogram%breaks(2:) - histogram%breaks(:size(histogram%breaks) - 1))) - &
   1.0_dp) > tolerance) error stop "automatic histogram normalization failed"

histogram = hist([-1.0_dp, 0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp], &
   [0.0_dp, 1.0_dp, 2.0_dp], plot=.false.)
if (any(histogram%counts /= [2, 1])) error stop "explicit histogram range handling failed"
histogram = hist([real(kind=dp) ::], breaks=4, plot=.false.)
if (size(histogram%counts) /= 4 .or. any(histogram%counts /= 0)) &
   error stop "empty automatic histogram failed"
histogram = hist([1.0_dp, 2.0_dp], [real(kind=dp) ::], plot=.false.)
if (size(histogram%counts) /= 0 .or. size(histogram%mids) /= 0) &
   error stop "empty explicit breaks failed"

values = quantile([4.0_dp, 1.0_dp, 3.0_dp, 2.0_dp], &
   [-1.0_dp, 0.5_dp, 2.0_dp], names=.false., type=7)
call assert_vector_close(values, [1.0_dp, 2.5_dp, 4.0_dp], &
   "quantile probability clamping")
values = quantile([real(kind=dp) ::], [0.0_dp, 0.5_dp, 1.0_dp])
if (any(.not. ieee_is_nan(values))) error stop "empty quantiles failed"
if (size(quantile([1.0_dp, 2.0_dp], [real(kind=dp) ::])) /= 0) &
   error stop "empty quantile probabilities failed"

values = ecdf_eval([1.0_dp, 1.0_dp, 2.0_dp], &
   [-huge(1.0_dp), 1.0_dp, huge(1.0_dp)])
call assert_vector_close(values, [0.0_dp, 2.0_dp / 3.0_dp, 1.0_dp], &
   "ECDF extreme queries")
values = ecdf_eval([real(kind=dp) ::], [0.0_dp, 1.0_dp])
if (any(.not. ieee_is_nan(values))) error stop "empty ECDF failed"
values = ecdf_eval([1.0_dp, r_na_real(), 2.0_dp, &
   ieee_value(0.0_dp, ieee_quiet_nan), 2.0_dp, 4.0_dp], &
   [-r_inf(), 1.0_dp, 2.0_dp, r_inf(), r_na_real(), &
   ieee_value(0.0_dp, ieee_quiet_nan)])
call assert_vector_close(values(:4), [0.0_dp, 0.25_dp, 0.75_dp, 1.0_dp], &
   "ECDF missing observations")
if (.not. ieee_is_nan(values(5)) .or. r_is_nan(values(5))) &
   error stop "ECDF NA query failed"
if (.not. r_is_nan(values(6))) error stop "ECDF NaN query failed"
values = ecdf_eval([r_na_real(), ieee_value(0.0_dp, ieee_quiet_nan)], [0.0_dp])
if (.not. r_is_nan(values(1))) error stop "all-missing ECDF sample failed"

bins = findInterval([-1.0_dp, 0.0_dp, 1.0_dp], [real(kind=dp) ::])
if (any(bins /= 0)) error stop "interval lookup without breakpoints failed"
if (size(findInterval([real(kind=dp) ::], [0.0_dp])) /= 0) &
   error stop "empty interval queries failed"
bins = findInterval([-r_inf(), -0.0_dp, 0.0_dp, 1.0_dp, r_inf(), &
   ieee_value(0.0_dp, ieee_quiet_nan), r_na_real()], &
   [-r_inf(), 0.0_dp, 0.0_dp, r_inf()])
if (any(bins /= [1, 3, 3, 3, 4, -huge(0), -huge(0)])) &
   error stop "interval repeated, infinite, and missing boundaries failed"

bins = cut([-1.0_dp, 0.0_dp, 0.5_dp, 1.0_dp, 2.0_dp], &
   [0.0_dp, 1.0_dp], include_lowest=.true., labels=.false.)
if (any(bins /= [0, 1, 1, 1, 0])) error stop "cut range boundaries failed"
bins = cut([1.0_dp, 1.0_dp, 1.0_dp], [0.0_dp, 1.0_dp, 1.0_dp, 2.0_dp], &
   include_lowest=.true., labels=.false.)
if (any(bins /= 1)) error stop "cut repeated boundaries failed"
bins = cut_n([3.0_dp, 3.0_dp, 3.0_dp], 3, labels=.false.)
if (any(bins < 1) .or. any(bins > 3)) error stop "constant cut-by-count failed"
bins = cut_n([1.0_dp, 2.0_dp], 0, labels=.false.)
if (any(bins /= 0)) error stop "invalid cut count failed"
bins = cut_n([real(kind=dp) ::], 2, labels=.false.)
if (size(bins) /= 0) error stop "empty cut-by-count failed"

contains

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_vector_close
end program test_empirical_distribution_variants
