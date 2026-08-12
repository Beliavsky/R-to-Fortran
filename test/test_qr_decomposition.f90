program test_qr_decomposition
use r_mod, only: dp, qr, qr_coef, qr_fitted, qr_fit_t, qr_pivot, qr_Q, qr_qty, qr_qy, &
   qr_R, qr_rank, qr_resid
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: x(4, 2), y(4), responses(4, 2), identity2(2, 2), identity4(4, 4)
real(kind=dp), allocatable :: q(:,:), q_full(:,:), r(:,:), actual_vector(:), actual_matrix(:,:)
type(qr_fit_t) :: fit
integer, allocatable :: pivot(:)
integer :: i

x(:, 1) = 1.0_dp
x(:, 2) = [0.0_dp, 1.0_dp, 2.0_dp, 3.0_dp]
y = 2.0_dp + 3.0_dp * x(:, 2)
fit = qr(x)

if (qr_rank(fit) /= 2 .or. fit%rank /= 2) error stop "QR rank failed"
pivot = qr_pivot(fit)
if (size(pivot) /= 2 .or. any(pivot /= [1, 2])) error stop "QR pivot failed"

q = qr_Q(fit)
r = qr_R(fit)
if (any(shape(q) /= [4, 2])) error stop "thin Q shape failed"
if (any(shape(r) /= [2, 2])) error stop "thin R shape failed"
identity2 = 0.0_dp
identity2(1, 1) = 1.0_dp
identity2(2, 2) = 1.0_dp
call assert_matrix_close(matmul(transpose(q), q), identity2, "thin Q orthogonality")
call assert_matrix_close(matmul(q, r), x, "QR reconstruction")

q_full = qr_Q(fit, complete=.true.)
r = qr_R(fit, complete=.true.)
if (any(shape(q_full) /= [4, 4])) error stop "complete Q shape failed"
if (any(shape(r) /= [4, 2])) error stop "complete R shape failed"
identity4 = 0.0_dp
do i = 1, 4
   identity4(i, i) = 1.0_dp
end do
call assert_matrix_close(matmul(transpose(q_full), q_full), identity4, &
   "complete Q orthogonality")
call assert_matrix_close(matmul(q_full, r), x, "complete QR reconstruction")

actual_vector = qr_coef(fit, y)
call assert_vector_close(actual_vector, [2.0_dp, 3.0_dp], "QR coefficients")
actual_vector = qr_fitted(fit, y)
call assert_vector_close(actual_vector, y, "QR fitted values")
actual_vector = qr_resid(fit, y)
call assert_vector_close(actual_vector, [0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], &
   "QR residuals")

actual_vector = qr_qty(fit, y)
call assert_vector_close(qr_qy(fit, actual_vector), y, "Q transpose and Q inverse operations")

responses(:, 1) = y
responses(:, 2) = -1.0_dp + 0.5_dp * x(:, 2)
actual_matrix = qr_coef(fit, responses)
call assert_matrix_close(actual_matrix, reshape([2.0_dp, 3.0_dp, -1.0_dp, 0.5_dp], [2, 2]), &
   "QR matrix coefficients")
actual_matrix = qr_fitted(fit, responses)
call assert_matrix_close(actual_matrix, responses, "QR matrix fitted values")
actual_matrix = qr_resid(fit, responses)
call assert_matrix_close(actual_matrix, spread(spread(0.0_dp, 1, 4), 2, 2), &
   "QR matrix residuals")

contains

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
end program test_qr_decomposition
