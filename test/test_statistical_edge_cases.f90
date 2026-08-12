program test_statistical_edge_cases
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: cor, cov, cov2cor, dp, median, quantile, sd, summary, var
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp), allocatable :: empty(:), values(:), matrix_result(:,:)
real(kind=dp) :: one_row(1, 2), constant_column(4, 2), invalid_covariance(2, 2)
real(kind=dp) :: empty_matrix(0, 0)

allocate(empty(0))

if (.not. ieee_is_nan(median(empty))) error stop "empty median should be NaN"
if (.not. ieee_is_nan(sd(empty))) error stop "empty sd should be NaN"
if (.not. ieee_is_nan(var(empty))) error stop "empty variance should be NaN"
if (.not. ieee_is_nan(sd([1.0_dp]))) error stop "singleton sd should be NaN"
if (.not. ieee_is_nan(var([1.0_dp]))) error stop "singleton variance should be NaN"
if (.not. ieee_is_nan(cov([1.0_dp], [2.0_dp]))) error stop "singleton covariance should be NaN"
if (.not. ieee_is_nan(cor([1.0_dp, 1.0_dp], [2.0_dp, 3.0_dp]))) &
   error stop "constant-vector correlation should be NaN"

values = quantile(empty, [0.25_dp, 0.5_dp, 0.75_dp])
if (size(values) /= 3 .or. .not. all(ieee_is_nan(values))) error stop "empty quantiles should be NaN"
values = quantile([1.0_dp, 2.0_dp, 3.0_dp], [-1.0_dp, 0.5_dp, 2.0_dp])
call assert_vector_close(values, [1.0_dp, 2.0_dp, 3.0_dp], "clamped quantiles")

values = summary(empty)
if (size(values) /= 6 .or. .not. all(ieee_is_nan(values))) error stop "empty summary should be NaN"

one_row(1, :) = [1.0_dp, 2.0_dp]
matrix_result = cov(one_row)
if (any(shape(matrix_result) /= [2, 2]) .or. .not. all(ieee_is_nan(matrix_result))) &
   error stop "one-row covariance should be NaN"

constant_column(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
constant_column(:, 2) = 5.0_dp
matrix_result = cor(constant_column)
call assert_close(matrix_result(1, 1), 1.0_dp, "variable-column self correlation")
if (.not. ieee_is_nan(matrix_result(1, 2)) .or. .not. ieee_is_nan(matrix_result(2, 1)) .or. &
   .not. ieee_is_nan(matrix_result(2, 2))) error stop "constant-column correlations should be NaN"

invalid_covariance(1, :) = [4.0_dp, 2.0_dp]
invalid_covariance(2, :) = [2.0_dp, 0.0_dp]
matrix_result = cov2cor(invalid_covariance)
call assert_close(matrix_result(1, 1), 1.0_dp, "valid covariance diagonal")
if (.not. ieee_is_nan(matrix_result(1, 2)) .or. .not. ieee_is_nan(matrix_result(2, 1)) .or. &
   .not. ieee_is_nan(matrix_result(2, 2))) error stop "invalid covariance diagonal should produce NaN"

matrix_result = cov2cor(empty_matrix)
if (any(shape(matrix_result) /= [0, 0])) error stop "empty covariance conversion shape failed"

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

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_vector_close
end program test_statistical_edge_cases
