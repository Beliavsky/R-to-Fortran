program test_result_printing_smoke
use r_mod, only: acf, acf_fit_t, arima_fit, arima_fit_t, dp, eigen, &
   eigen_result_t, hist, hist_result_t, integrate, integrate_result_t, &
   nlm_optimize_scalar, nlm_result_t, prcomp, prcomp_fit_t, print_acf, &
   print_arima_fit, print_eigen, print_hist, print_integrate_result, &
   print_nlm_result, print_prcomp_summary, print_qr, print_rle, qr, &
   qr_fit_t, rle, rle_int_t
implicit none

type(acf_fit_t) :: acf_result
type(arima_fit_t) :: arima_result
type(eigen_result_t) :: eigen_result
type(hist_result_t) :: histogram
type(integrate_result_t) :: integral
type(nlm_result_t) :: nlm_result
type(prcomp_fit_t) :: pca
type(qr_fit_t) :: qr_result
type(rle_int_t) :: runs
real(kind=dp) :: x(6, 2)

integral = integrate(square, 0.0_dp, 1.0_dp)
call print_integrate_result(integral)

nlm_result = nlm_optimize_scalar(quadratic, 8.0_dp)
call print_nlm_result(nlm_result)

histogram = hist([1.0_dp, 2.0_dp, 2.0_dp, 3.0_dp], 3)
call print_hist(histogram)

eigen_result = eigen(reshape([2.0_dp, 0.0_dp, 0.0_dp, 3.0_dp], [2, 2]))
call print_eigen(eigen_result)

x(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
x(:, 2) = [2.0_dp, 1.0_dp, 4.0_dp, 3.0_dp, 6.0_dp, 5.0_dp]
pca = prcomp(x)
call print_prcomp_summary(pca)

acf_result = acf(x(:, 1), lag_max=2, plot=.false.)
call print_acf(acf_result, digits=3, series_name="x")

arima_result = arima_fit(x(:, 1), [0, 0, 0])
call print_arima_fit(arima_result)

qr_result = qr(x)
call print_qr(qr_result)

runs = rle([1, 1, 2, 3, 3])
call print_rle(runs)

contains

pure function square(value) result(out)
real(kind=dp), intent(in) :: value
real(kind=dp) :: out
out = value * value
end function square

pure function quadratic(value) result(out)
real(kind=dp), intent(in) :: value
real(kind=dp) :: out
out = (value - 2.0_dp)**2
end function quadratic
end program test_result_printing_smoke
