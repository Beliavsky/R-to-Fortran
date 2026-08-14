program test_matrix_constructors
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
use r_mod, only: diag, dp, matrix, r_array_char, r_array_int, r_array_real, toeplitz
implicit none

integer :: integer_rect(2, 3)
integer, allocatable :: integer_values(:,:)
real(kind=dp), allocatable :: real_values(:,:)
complex(kind=dp) :: complex_vector(2)

call assert_integer_matrix(matrix([1, 2, 3], 2, 3), &
   reshape([1, 2, 3, 1, 2, 3], [2, 3]), "integer matrix recycling")
call assert_real_matrix(matrix([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], 2), &
   reshape([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 1.0_dp], [2, 3]), &
   "inferred matrix columns")
integer_values = matrix([integer ::], 2, 2)
if (any(shape(integer_values) /= [2, 2])) error stop "empty integer matrix shape failed"
if (any(integer_values /= -huge(0))) error stop "empty integer matrix NA fill failed"
real_values = matrix([real(kind=dp) ::], 2, 2)
if (any(shape(real_values) /= [2, 2])) error stop "empty real matrix shape failed"
if (.not. all(ieee_is_nan(real_values))) error stop "empty real matrix NA fill failed"
if (any(shape(matrix([1, 2], 0, 3)) /= [0, 3])) error stop "zero-row matrix failed"
if (any(shape(matrix([1, 2], 3, 0)) /= [3, 0])) error stop "zero-column matrix failed"
if (any(shape(matrix([integer ::], 2)) /= [2, 0])) error stop "inferred empty matrix failed"
call assert_integer_matrix(matrix([1, 2, 3, 4, 5], 2, 2), &
   reshape([1, 2, 3, 4], [2, 2]), "explicit matrix truncation")

call assert_integer_matrix(r_array_int([1, 2, 3], [2, 2]), reshape([1, 2, 3, 1], [2, 2]), &
   "integer array recycling")
call assert_real_matrix(r_array_real([1.5_dp, 2.5_dp], [2, 3]), &
   reshape([1.5_dp, 2.5_dp, 1.5_dp, 2.5_dp, 1.5_dp, 2.5_dp], [2, 3]), &
   "real array recycling")
call assert_character_matrix(r_array_char([character(len=3) :: "a", "bb"], [2, 2]), &
   reshape([character(len=3) :: "a", "bb", "a", "bb"], [2, 2]), &
   "character array recycling")
integer_values = r_array_int([integer ::], [2, 2])
if (any(integer_values /= -huge(0))) error stop "empty integer array NA fill failed"
real_values = r_array_real([real(kind=dp) ::], [2, 2])
if (.not. all(ieee_is_nan(real_values))) error stop "empty real array NA fill failed"
if (any(shape(r_array_real([1.0_dp], [3])) /= [0, 0])) error stop "short dimensions failed"
if (any(shape(r_array_real([1.0_dp], [0, 3])) /= [0, 3])) error stop "zero-row real array failed"
if (any(shape(r_array_int([1], [3, 0])) /= [3, 0])) error stop "zero-column integer array failed"
if (any(shape(r_array_char([character(len=1) :: "x"], [0, 2])) /= [0, 2])) &
   error stop "zero-row character array failed"

integer_rect = reshape([1, 2, 3, 4, 5, 6], [2, 3])
call assert_integer_vector(diag(integer_rect), [1, 4], "integer diagonal extraction")
call assert_real_matrix(diag([1.0_dp, 2.0_dp]), &
   reshape([1.0_dp, 0.0_dp, 0.0_dp, 2.0_dp], [2, 2]), "real diagonal construction")
call assert_integer_matrix(diag([1, 2], 3), &
   reshape([1, 0, 0, 0, 2, 0, 0, 0, 1], [3, 3]), "resized integer diagonal")
call assert_real_matrix(diag([1.0_dp, 2.0_dp], 3), &
   reshape([1.0_dp, 0.0_dp, 0.0_dp, 0.0_dp, 2.0_dp, 0.0_dp, &
   0.0_dp, 0.0_dp, 1.0_dp], [3, 3]), "resized real diagonal")
call assert_integer_matrix(diag(3), reshape([1, 0, 0, 0, 1, 0, 0, 0, 1], [3, 3]), &
   "integer identity")
call assert_real_matrix(diag(2.5_dp, 2), reshape([2.5_dp, 0.0_dp, 0.0_dp, 2.5_dp], [2, 2]), &
   "real scalar diagonal")
complex_vector = [cmplx(1.0_dp, 2.0_dp, kind=dp), cmplx(3.0_dp, -1.0_dp, kind=dp)]
call assert_complex_matrix(diag(complex_vector, 3), reshape([ &
   complex_vector(1), cmplx(0.0_dp, 0.0_dp, kind=dp), cmplx(0.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, 0.0_dp, kind=dp), complex_vector(2), cmplx(0.0_dp, 0.0_dp, kind=dp), &
   cmplx(0.0_dp, 0.0_dp, kind=dp), cmplx(0.0_dp, 0.0_dp, kind=dp), complex_vector(1)], [3, 3]), &
   "resized complex diagonal")
if (any(shape(diag([integer ::])) /= [0, 0])) error stop "empty integer diagonal failed"
if (any(shape(diag([real(kind=dp) ::], 0)) /= [0, 0])) error stop "empty real diagonal failed"

call assert_real_matrix(toeplitz([1.0_dp, 2.0_dp, 3.0_dp]), &
   reshape([1.0_dp, 2.0_dp, 3.0_dp, 2.0_dp, 1.0_dp, 2.0_dp, &
   3.0_dp, 2.0_dp, 1.0_dp], [3, 3]), "Toeplitz construction")
if (any(shape(toeplitz([real(kind=dp) ::])) /= [0, 0])) error stop "empty Toeplitz failed"

contains

subroutine assert_integer_vector(actual, expected, label)
integer, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_vector

subroutine assert_integer_matrix(actual, expected, label)
integer, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_matrix

subroutine assert_real_matrix(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix

subroutine assert_complex_matrix(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_complex_matrix

subroutine assert_character_matrix(actual, expected, label)
character(len=*), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label
integer :: i, j

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
do j = 1, size(actual, 2)
   do i = 1, size(actual, 1)
      if (trim(actual(i, j)) /= trim(expected(i, j))) error stop trim(label) // " values failed"
   end do
end do
end subroutine assert_character_matrix
end program test_matrix_constructors
