program test_vector_boundary_variants
use r_mod, only: combn, cummax, diff, dp, outer, outer_divide, outer_minus, &
   outer_plus, outer_power, r_head, r_seq_real_by, r_seq_real_length
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
real(kind=dp) :: real_matrix(3, 2)
integer :: integer_matrix(3, 2)
real(kind=dp), allocatable :: real_values(:), real_result(:,:), differences(:,:)
integer, allocatable :: integer_result(:,:), integer_values(:)
character(len=:), allocatable :: character_result(:,:)

call assert_real_vector(r_seq_real_by(0.0_dp, 1.0_dp, 0.3_dp), &
   [0.0_dp, 0.3_dp, 0.6_dp, 0.9_dp], "nondivisible real sequence")
call assert_real_vector(r_seq_real_by(1.0_dp, -0.5_dp, -0.5_dp), &
   [1.0_dp, 0.5_dp, 0.0_dp, -0.5_dp], "descending real sequence")
if (size(r_seq_real_by(1.0_dp, 0.0_dp, 0.25_dp)) /= 0) &
   error stop "wrong-direction real sequence failed"
if (size(r_seq_real_by(0.0_dp, 1.0_dp, 0.0_dp)) /= 0) &
   error stop "zero-step real sequence failed"
call assert_real_vector(r_seq_real_length(2.5_dp, 9.0_dp, 1), [2.5_dp], &
   "singleton real sequence")
if (size(r_seq_real_length(1.0_dp, 2.0_dp, -1)) /= 0) &
   error stop "negative-length real sequence failed"

integer_values = r_head([1, 2, 3, 4], -1)
call assert_integer_vector(integer_values, [1, 2, 3], "negative integer head")
real_values = r_head([1.0_dp, 2.0_dp, 3.0_dp], 9)
call assert_real_vector(real_values, [1.0_dp, 2.0_dp, 3.0_dp], "oversized real head")
if (size(r_head([1, 2, 3], -9)) /= 0) error stop "fully dropped head failed"

integer_matrix = reshape([1, 2, 3, 4, 5, 6], [3, 2])
integer_result = r_head(integer_matrix, -1)
call assert_integer_matrix(integer_result, reshape([1, 2, 4, 5], [2, 2]), &
   "negative matrix head")
real_matrix = real(integer_matrix, kind=dp)
real_result = r_head(real_matrix, 0)
if (any(shape(real_result) /= [0, 2])) error stop "zero-row matrix head shape failed"

if (size(cummax([integer ::])) /= 0) error stop "empty integer cummax failed"
if (size(cummax([real(kind=dp) ::])) /= 0) error stop "empty real cummax failed"
differences = diff(reshape([real(kind=dp) ::], [0, 3]))
if (any(shape(differences) /= [0, 3])) error stop "empty matrix diff shape failed"
differences = diff(reshape([1.0_dp, 2.0_dp], [1, 2]))
if (any(shape(differences) /= [0, 2])) error stop "singleton-row matrix diff shape failed"

real_result = outer_plus([real(kind=dp) ::], [1.0_dp, 2.0_dp])
if (any(shape(real_result) /= [0, 2])) error stop "empty outer-plus rows failed"
real_result = outer_minus([1.0_dp, 2.0_dp], [real(kind=dp) ::])
if (any(shape(real_result) /= [2, 0])) error stop "empty outer-minus columns failed"
real_result = outer_divide([real(kind=dp) ::], [real(kind=dp) ::])
if (any(shape(real_result) /= [0, 0])) error stop "empty outer-divide shape failed"
real_result = outer_power([1.0_dp], [real(kind=dp) ::])
if (any(shape(real_result) /= [1, 0])) error stop "empty outer-power columns failed"
real_result = outer([1.0_dp, 2.0_dp], [real(kind=dp) ::])
if (any(shape(real_result) /= [2, 0])) error stop "empty outer-product columns failed"

integer_result = combn([4, 5, 6], 1)
call assert_integer_matrix(integer_result, reshape([4, 5, 6], [1, 3]), &
   "singleton integer combinations")
character_result = combn([character(len=1) :: "a", "b"], 0)
if (any(shape(character_result) /= [0, 1])) &
   error stop "zero-size character combination shape failed"

contains

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

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
end program test_vector_boundary_variants
