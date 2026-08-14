program test_arithmetic_helpers
use r_mod, only: dp, r_add, r_div, r_matmul, r_mul, r_sub
implicit none

real(kind=dp) :: a(2, 3), b(3, 2)
integer :: ai(2, 3), bi(3, 2)
complex(kind=dp) :: ac(2, 2), bc(2, 2)

call assert_real_vector(r_add([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp], [10.0_dp, 20.0_dp]), &
   [11.0_dp, 22.0_dp, 13.0_dp, 24.0_dp], "recycled vector addition")
call assert_real_vector(r_add([1.0_dp], [10.0_dp, 20.0_dp, 30.0_dp]), &
   [11.0_dp, 21.0_dp, 31.0_dp], "left scalar-length recycling")
call assert_real_vector(r_sub([1.0_dp, 2.0_dp, 3.0_dp], 1.0_dp), &
   [0.0_dp, 1.0_dp, 2.0_dp], "vector-scalar subtraction")
call assert_real_vector(r_sub(10.0_dp, [1.0_dp, 2.0_dp, 3.0_dp]), &
   [9.0_dp, 8.0_dp, 7.0_dp], "scalar-vector subtraction")
call assert_real_vector(r_mul([1.0_dp, 2.0_dp, 3.0_dp], [2.0_dp]), &
   [2.0_dp, 4.0_dp, 6.0_dp], "vector multiplication")
call assert_real_vector(r_div([2.0_dp, 4.0_dp, 8.0_dp], 2.0_dp), &
   [1.0_dp, 2.0_dp, 4.0_dp], "vector-scalar division")
call assert_real_vector(r_div(8.0_dp, [2.0_dp, 4.0_dp, 8.0_dp]), &
   [4.0_dp, 2.0_dp, 1.0_dp], "scalar-vector division")
if (size(r_add([real(kind=dp) ::], [1.0_dp])) /= 0) error stop "empty addition failed"
if (size(r_sub([1.0_dp], [real(kind=dp) ::])) /= 0) error stop "empty subtraction failed"
if (size(r_mul([real(kind=dp) ::], [1.0_dp])) /= 0) error stop "empty multiplication failed"
if (size(r_div([1.0_dp], [real(kind=dp) ::])) /= 0) error stop "empty division failed"

a = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [2, 3])
call assert_real_matrix(r_add(a, [10.0_dp, 20.0_dp]), &
   reshape([11.0_dp, 22.0_dp, 13.0_dp, 24.0_dp, 15.0_dp, 26.0_dp], [2, 3]), &
   "matrix-vector addition")
call assert_real_matrix(r_sub([10.0_dp, 20.0_dp], a), &
   reshape([9.0_dp, 18.0_dp, 7.0_dp, 16.0_dp, 5.0_dp, 14.0_dp], [2, 3]), &
   "vector-matrix subtraction")
call assert_real_matrix(r_mul(a, [2.0_dp, 3.0_dp]), &
   reshape([2.0_dp, 6.0_dp, 6.0_dp, 12.0_dp, 10.0_dp, 18.0_dp], [2, 3]), &
   "matrix-vector multiplication")
call assert_real_matrix(r_div([12.0_dp, 24.0_dp], a), &
   reshape([12.0_dp, 12.0_dp, 4.0_dp, 6.0_dp, 2.4_dp, 4.0_dp], [2, 3]), &
   "vector-matrix division")
if (size(r_add(a, [real(kind=dp) ::])) /= 0) error stop "empty matrix addition failed"
if (size(r_sub([real(kind=dp) ::], a)) /= 0) error stop "empty matrix subtraction failed"
if (any(shape(r_add(reshape([real(kind=dp) ::], [0, 3]), [1.0_dp])) /= [0, 3])) &
   error stop "zero-row matrix addition shape failed"
if (any(shape(r_sub(reshape([real(kind=dp) ::], [2, 0]), [1.0_dp])) /= [2, 0])) &
   error stop "zero-column matrix subtraction shape failed"
if (any(shape(r_mul([1.0_dp], reshape([real(kind=dp) ::], [0, 3]))) /= [0, 3])) &
   error stop "zero-row matrix multiplication shape failed"
if (any(shape(r_div([1.0_dp], reshape([real(kind=dp) ::], [2, 0]))) /= [2, 0])) &
   error stop "zero-column matrix division shape failed"
if (any(shape(r_add(reshape([real(kind=dp) ::], [0, 3]), [real(kind=dp) ::])) /= [0, 3])) &
   error stop "fully empty matrix-vector addition shape failed"

if (abs(r_matmul([1.0_dp, 2.0_dp, 3.0_dp], [4.0_dp, 5.0_dp, 6.0_dp]) - 32.0_dp) &
   > 1.0e-12_dp) error stop "real dot product failed"
if (abs(r_matmul([1, 2, 3], [4.0_dp, 5.0_dp, 6.0_dp]) - 32.0_dp) > 1.0e-12_dp) &
   error stop "mixed dot product failed"
call assert_real_vector(r_matmul(a, [1.0_dp, 2.0_dp, 3.0_dp]), [22.0_dp, 28.0_dp], &
   "matrix-vector product")
call assert_real_vector(r_matmul([1.0_dp, 2.0_dp], a), [5.0_dp, 11.0_dp, 17.0_dp], &
   "vector-matrix product")
b = reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp], [3, 2])
call assert_real_matrix(r_matmul(a, b), reshape([22.0_dp, 28.0_dp, 49.0_dp, 64.0_dp], [2, 2]), &
   "matrix product")
ai = int(a)
bi = int(b)
call assert_real_matrix(r_matmul(ai, bi), reshape([22.0_dp, 28.0_dp, 49.0_dp, 64.0_dp], [2, 2]), &
   "integer matrix product")
call assert_real_matrix(r_matmul(a, bi), reshape([22.0_dp, 28.0_dp, 49.0_dp, 64.0_dp], [2, 2]), &
   "mixed matrix product")

ac = reshape([cmplx(1.0_dp, 1.0_dp, kind=dp), cmplx(2.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, 0.0_dp, kind=dp), cmplx(1.0_dp, -1.0_dp, kind=dp)], [2, 2])
bc = reshape([cmplx(1.0_dp, 0.0_dp, kind=dp), cmplx(0.0_dp, 1.0_dp, kind=dp), &
   cmplx(2.0_dp, 0.0_dp, kind=dp), cmplx(1.0_dp, 0.0_dp, kind=dp)], [2, 2])
call assert_complex_matrix(r_matmul(ac, bc), matmul(ac, bc), "complex matrix product")
call assert_complex_vector(r_matmul(real(ai(:, 1:2), kind=dp), &
   [cmplx(1.0_dp, 1.0_dp, kind=dp), cmplx(2.0_dp, 0.0_dp, kind=dp)]), &
   matmul(cmplx(real(ai(:, 1:2), kind=dp), 0.0_dp, kind=dp), &
   [cmplx(1.0_dp, 1.0_dp, kind=dp), cmplx(2.0_dp, 0.0_dp, kind=dp)]), &
   "real-complex matrix-vector product")

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

subroutine assert_complex_vector(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_complex_vector

subroutine assert_complex_matrix(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_complex_matrix
end program test_arithmetic_helpers
