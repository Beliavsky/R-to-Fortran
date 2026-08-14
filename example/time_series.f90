program time_series
use r_mod, only: acf_fit_t, dp, print_real_vector, r_acf, &
   r_filter_linear, runmed
implicit none

real(kind=dp) :: series(10)
real(kind=dp), allocatable :: moving_average(:), running_median(:)
type(acf_fit_t) :: autocorrelation

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: Time series"
write(*, '(a, /)') repeat("=", 72)

series = [1.0_dp, 2.0_dp, 8.0_dp, 4.0_dp, 5.0_dp, &
   6.0_dp, 7.0_dp, 3.0_dp, 9.0_dp, 10.0_dp]

autocorrelation = r_acf(series, lag_max=4, plot=.false.)
write(*, '(a)') "Autocorrelation lags:"
call print_real_vector(autocorrelation%lag)
write(*, '(a)') "Autocorrelation values:"
call print_real_vector(autocorrelation%acf(:, 1, 1))

moving_average = r_filter_linear(series, &
   [1.0_dp, 1.0_dp, 1.0_dp] / 3.0_dp, sides=2)
write(*, '(a)') "Centered three-point moving average:"
call print_real_vector(moving_average)

running_median = runmed(series, 3, endrule="keep")
write(*, '(a)') "Three-point running median:"
call print_real_vector(running_median)
end program time_series
