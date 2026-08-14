program test_smoothing_variants
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan
use r_mod, only: dp, ksmooth, loess_fit, loess_fit_t, lowess, &
   predict_loess, predict_smooth_spline, r_filter_linear, runmed, smooth, &
   smooth_spline, smooth_spline_fit_t, smooth_xy_t
implicit none

real(kind=dp), parameter :: tolerance = 2.0e-9_dp
real(kind=dp), parameter :: x(5) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
real(kind=dp), parameter :: quadratic(5) = [0.0_dp, 1.0_dp, 4.0_dp, 9.0_dp, 16.0_dp]
real(kind=dp), allocatable :: values(:)
type(loess_fit_t) :: loess_model, empty_loess
type(smooth_spline_fit_t) :: spline_model, empty_spline
type(smooth_xy_t) :: smooth_result

values = r_filter_linear([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], &
   [0.5_dp, 0.5_dp], sides=1)
if (.not. ieee_is_nan(values(1))) error stop "one-sided filter boundary failed"
call assert_vector_close(values(2:5), [1.5_dp, 2.5_dp, 3.5_dp, 4.5_dp], &
   "one-sided filter values")
values = r_filter_linear([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], &
   [0.25_dp, 0.5_dp, 0.25_dp])
if (.not. ieee_is_nan(values(1)) .or. .not. ieee_is_nan(values(5))) &
   error stop "centered filter boundaries failed"
call assert_vector_close(values(2:4), [2.0_dp, 3.0_dp, 4.0_dp], &
   "centered filter values")
if (size(r_filter_linear([real(kind=dp) ::], [1.0_dp])) /= 0) &
   error stop "empty filter failed"

values = runmed([1.0_dp, 100.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], 3, &
   endrule="keep")
call assert_vector_close(values, [1.0_dp, 2.0_dp, 3.0_dp, 3.0_dp, 4.0_dp], &
   "running median keep endpoints")
values = runmed([1.0_dp, 100.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], 3, &
   endrule="constant")
call assert_vector_close(values, [2.0_dp, 2.0_dp, 3.0_dp, 3.0_dp, 3.0_dp], &
   "running median constant endpoints")
call assert_vector_close(runmed(x, 2, endrule="keep"), &
   runmed(x, 3, endrule="keep"), "even running-median width")
call assert_vector_close(runmed(x, 99, endrule="keep"), &
   [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], "clamped running-median width")

values = smooth([1.0_dp, 100.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], &
   kind="3RS3R", twiceit=.true., endrule="copy")
if (size(values) /= 5 .or. any(.not. ieee_is_finite(values))) &
   error stop "recursive smoother failed"
if (size(smooth([real(kind=dp) ::])) /= 0) error stop "empty smoother failed"

smooth_result = ksmooth(x, quadratic, kernel="normal", bandwidth=1.0_dp, &
   range_x=[0.0_dp, 4.0_dp], n_points=5)
call assert_vector_close(smooth_result%x, x, "normal-kernel grid")
if (any(.not. ieee_is_finite(smooth_result%y))) error stop "normal-kernel values failed"
smooth_result = ksmooth(x, quadratic, x_points=[3.0_dp, 1.0_dp])
call assert_vector_close(smooth_result%x, [1.0_dp, 3.0_dp], &
   "kernel explicit-point sorting")
smooth_result = ksmooth([real(kind=dp) ::], [real(kind=dp) ::], n_points=2)
if (size(smooth_result%x) /= 2 .or. any(.not. ieee_is_nan(smooth_result%y))) &
   error stop "empty kernel smoothing failed"

smooth_result = lowess([1.0_dp, 1.0_dp, 2.0_dp, 3.0_dp], &
   [2.0_dp, 2.0_dp, 4.0_dp, 6.0_dp], f=1.0_dp, iter=2)
if (any(.not. ieee_is_finite(smooth_result%y))) error stop "duplicate LOWESS values failed"
smooth_result = lowess([real(kind=dp) ::], [real(kind=dp) ::])
if (size(smooth_result%x) /= 0) error stop "empty LOWESS failed"

loess_model = loess_fit(x, quadratic, span=1.0_dp, degree=2)
call assert_vector_close(predict_loess(loess_model, [1.0_dp, 2.0_dp, 3.0_dp]), &
   [1.0_dp, 4.0_dp, 9.0_dp], "quadratic LOESS reproduction")
loess_model = loess_fit(x, spread(7.0_dp, 1, 5), span=1.0_dp, degree=0)
call assert_vector_close(predict_loess(loess_model, [0.5_dp, 2.5_dp]), &
   [7.0_dp, 7.0_dp], "constant LOESS reproduction")
if (size(predict_loess(empty_loess, [1.0_dp, 2.0_dp])) /= 2) &
   error stop "empty LOESS prediction shape failed"

spline_model = smooth_spline(x, quadratic, df=3.0_dp)
if (abs(spline_model%df - 3.0_dp) > tolerance .or. &
   any(.not. ieee_is_finite(spline_model%y))) error stop "df-controlled spline failed"
spline_model = smooth_spline(x, quadratic, spar=0.8_dp)
if (spline_model%df < 2.0_dp .or. spline_model%df > 5.0_dp .or. &
   any(.not. ieee_is_finite(spline_model%y))) error stop "spar-controlled spline failed"
smooth_result = predict_smooth_spline(empty_spline, [0.0_dp, 1.0_dp])
if (any(.not. ieee_is_nan(smooth_result%y))) error stop "empty spline prediction failed"

contains

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_vector_close
end program test_smoothing_variants
