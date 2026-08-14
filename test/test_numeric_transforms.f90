program test_numeric_transforms
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_is_negative, &
   ieee_quiet_nan, ieee_value
use r_mod, only: dp, pmax, r_inf, r_is_nan, r_log, r_na_real, r_round
implicit none

real(kind=dp), allocatable :: values(:), empty_matrix(:,:)
real(kind=dp) :: matrix_values(2, 2), ordinary_nan, na_value

call assert_real_vector(r_round([0.5_dp, 1.5_dp, 2.5_dp, 3.5_dp, &
   -0.5_dp, -1.5_dp, -2.5_dp], 0), [0.0_dp, 2.0_dp, 2.0_dp, 4.0_dp, &
   0.0_dp, -2.0_dp, -2.0_dp], "ties-to-even rounding")
call assert_real_vector(r_round([1.25_dp, 1.35_dp, 2.25_dp, -1.25_dp], 1), &
   [1.2_dp, 1.4_dp, 2.2_dp, -1.2_dp], "decimal rounding")
call assert_real_vector(r_round([125.0_dp, 135.0_dp, 250.0_dp], -1), &
   [120.0_dp, 140.0_dp, 250.0_dp], "negative-digit rounding")
if (abs(r_round(1.23456_dp, 3) - 1.235_dp) > 1.0e-12_dp) error stop "scalar rounding failed"
if (.not. ieee_is_nan(r_round(ieee_value(0.0_dp, ieee_quiet_nan), 2))) &
   error stop "NaN rounding failed"
if (ieee_is_finite(r_round(r_inf(), 2))) error stop "infinity rounding failed"
if (r_round(1.2345_dp, 400) /= 1.2345_dp) error stop "large positive digits rounding failed"
if (r_round(1.0e300_dp, 400) /= 1.0e300_dp) error stop "large-magnitude positive digits failed"
if (r_round(1.2345_dp, -400) /= 0.0_dp) error stop "large negative digits rounding failed"
if (r_round(1.0e300_dp, -400) /= 0.0_dp) error stop "large-magnitude negative digits failed"
if (ieee_is_negative(r_round(-0.5_dp, -400))) error stop "extreme rounded zero sign failed"
if (size(r_round([real(kind=dp) ::], 2)) /= 0) error stop "empty rounding failed"

call assert_real_vector(pmax([1.0_dp, 4.0_dp, -2.0_dp], [2.0_dp, 3.0_dp, -3.0_dp]), &
   [2.0_dp, 4.0_dp, -2.0_dp], "elementwise maximum")
if (abs(pmax(2.0_dp, 3.0_dp) - 3.0_dp) > 1.0e-12_dp) error stop "scalar maximum failed"
ordinary_nan = ieee_value(0.0_dp, ieee_quiet_nan)
na_value = r_na_real()
if (.not. r_is_nan(pmax(ordinary_nan, 2.0_dp))) error stop "left NaN maximum failed"
if (.not. r_is_nan(pmax(2.0_dp, ordinary_nan))) error stop "right NaN maximum failed"
if (.not. r_is_nan(pmax(na_value, ordinary_nan))) error stop "right NaN precedence failed"
if (r_is_nan(pmax(ordinary_nan, na_value))) error stop "right NA precedence failed"
if (.not. ieee_is_negative(pmax(-0.0_dp, 0.0_dp))) error stop "negative-zero tie failed"
if (ieee_is_negative(pmax(0.0_dp, -0.0_dp))) error stop "positive-zero tie failed"
if (pmax(-r_inf(), 2.0_dp) /= 2.0_dp) error stop "negative-infinity maximum failed"
if (pmax(2.0_dp, r_inf()) /= r_inf()) error stop "positive-infinity maximum failed"
if (size(pmax([real(kind=dp) ::], [real(kind=dp) ::])) /= 0) &
   error stop "empty vector maximum failed"

values = r_log([1.0_dp, exp(1.0_dp), 0.0_dp, -1.0_dp])
if (size(values) /= 4) error stop "log vector shape failed"
if (abs(values(1)) > 1.0e-12_dp .or. abs(values(2) - 1.0_dp) > 1.0e-12_dp) &
   error stop "log vector finite values failed"
if (ieee_is_finite(values(3)) .or. values(3) >= 0.0_dp) error stop "log zero failed"
if (.not. ieee_is_nan(values(4))) error stop "negative log failed"
matrix_values = reshape([1.0_dp, exp(1.0_dp), exp(2.0_dp), exp(3.0_dp)], [2, 2])
call assert_real_matrix(r_log(matrix_values), reshape([0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp], [2, 2]), &
   "matrix logarithm")
values = r_log([-r_inf(), -0.0_dp, 0.0_dp, r_inf(), ordinary_nan, na_value])
if (.not. r_is_nan(values(1))) error stop "negative-infinity logarithm failed"
if (ieee_is_finite(values(2)) .or. values(2) >= 0.0_dp) error stop "negative-zero logarithm failed"
if (ieee_is_finite(values(3)) .or. values(3) >= 0.0_dp) error stop "positive-zero logarithm failed"
if (ieee_is_finite(values(4)) .or. values(4) <= 0.0_dp) error stop "positive-infinity logarithm failed"
if (.not. r_is_nan(values(5))) error stop "ordinary NaN logarithm failed"
if (r_is_nan(values(6))) error stop "NA payload logarithm failed"
if (size(r_log([real(kind=dp) ::])) /= 0) error stop "empty vector logarithm failed"
empty_matrix = r_log(reshape([real(kind=dp) ::], [0, 3]))
if (any(shape(empty_matrix) /= [0, 3])) error stop "empty matrix logarithm shape failed"

contains

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix
end program test_numeric_transforms
