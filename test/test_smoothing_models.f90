program test_smoothing_models
use r_mod, only: dp, ksmooth, loess_fit, loess_fit_t, lowess, predict_loess, &
   predict_smooth_spline, smooth_spline, smooth_spline_fit_t, smooth_xy_t
implicit none

real(kind=dp), parameter :: tolerance = 2.0e-10_dp
type(smooth_xy_t) :: smoothed, predicted
type(loess_fit_t) :: loess_model
type(smooth_spline_fit_t) :: spline_model
real(kind=dp), parameter :: x(5) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
real(kind=dp), parameter :: y(5) = [1.0_dp, 3.0_dp, 5.0_dp, 7.0_dp, 9.0_dp]

smoothed = ksmooth([0.0_dp, 1.0_dp, 2.0_dp], [0.0_dp, 10.0_dp, 20.0_dp], &
   kernel="box", bandwidth=2.0_dp, x_points=[2.0_dp, 0.0_dp, 1.0_dp])
call assert_vector_close(smoothed%x, [0.0_dp, 1.0_dp, 2.0_dp], "kernel prediction ordering")
call assert_vector_close(smoothed%y, [5.0_dp, 10.0_dp, 15.0_dp], "box-kernel averages")
call assert_close(smoothed%df, 3.0_dp, "kernel smoother degrees of freedom")

smoothed = lowess([4.0_dp, 1.0_dp, 3.0_dp, 0.0_dp, 2.0_dp], &
   [9.0_dp, 3.0_dp, 7.0_dp, 1.0_dp, 5.0_dp], f=1.0_dp, iter=0, delta=0.0_dp)
call assert_vector_close(smoothed%x, x, "LOWESS sorted predictors")
call assert_vector_close(smoothed%y, y, "LOWESS linear reproduction")
call assert_close(smoothed%df, 5.0_dp, "LOWESS neighborhood size")

loess_model = loess_fit(x, y, span=1.0_dp, degree=1)
if (loess_model%degree /= 1) error stop "LOESS degree storage failed"
call assert_close(loess_model%span, 1.0_dp, "LOESS span storage")
call assert_vector_close(predict_loess(loess_model, [0.5_dp, 2.5_dp, 3.5_dp]), &
   [2.0_dp, 6.0_dp, 8.0_dp], "LOESS linear predictions")

spline_model = smooth_spline(x, y)
call assert_vector_close(spline_model%x, x, "default spline predictors")
call assert_vector_close(spline_model%y, y, "default spline fitted values")
call assert_close(spline_model%df, 5.0_dp, "default spline degrees of freedom")
predicted = predict_smooth_spline(spline_model, [-1.0_dp, 0.5_dp, 2.5_dp, 5.0_dp])
call assert_vector_close(predicted%x, [-1.0_dp, 0.5_dp, 2.5_dp, 5.0_dp], "spline prediction points")
call assert_vector_close(predicted%y, [1.0_dp, 2.0_dp, 6.0_dp, 9.0_dp], &
   "spline interpolation and clamping")
call assert_close(predicted%df, spline_model%df, "spline prediction degrees of freedom")

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " values failed"
   error stop 1
end if
end subroutine assert_vector_close
end program test_smoothing_models
