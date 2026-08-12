program test_descriptive_statistics
use r_mod, only: cor, cov, dp, median, quantile, sd, var
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: x(4), y(4), samples(4, 2)
real(kind=dp), allocatable :: actual_vector(:), actual_matrix(:,:)

x = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
y = [2.0_dp, 4.0_dp, 6.0_dp, 8.0_dp]
samples = reshape([x, y], shape(samples))

call assert_close(median(x), 2.5_dp, "median")
call assert_close(sd(x), sqrt(5.0_dp / 3.0_dp), "sd")
call assert_close(var(x), 5.0_dp / 3.0_dp, "var")
call assert_close(cov(x, y), 10.0_dp / 3.0_dp, "cov")
call assert_close(cor(x, y), 1.0_dp, "cor")

actual_vector = quantile(x, [0.0_dp, 0.25_dp, 0.5_dp, 0.75_dp, 1.0_dp])
call assert_vector_close(actual_vector, [1.0_dp, 1.75_dp, 2.5_dp, 3.25_dp, 4.0_dp], "quantile")

actual_matrix = cov(samples)
call assert_matrix_close(actual_matrix, reshape([5.0_dp / 3.0_dp, 10.0_dp / 3.0_dp, &
   10.0_dp / 3.0_dp, 20.0_dp / 3.0_dp], [2, 2]), "cov matrix")

actual_matrix = var(samples)
call assert_matrix_close(actual_matrix, reshape([5.0_dp / 3.0_dp, 10.0_dp / 3.0_dp, &
   10.0_dp / 3.0_dp, 20.0_dp / 3.0_dp], [2, 2]), "var matrix")

actual_matrix = cor(samples)
call assert_matrix_close(actual_matrix, reshape([1.0_dp, 1.0_dp, 1.0_dp, 1.0_dp], [2, 2]), "cor matrix")

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

subroutine assert_matrix_close(actual, expected, label)
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
end subroutine assert_matrix_close
end program test_descriptive_statistics
