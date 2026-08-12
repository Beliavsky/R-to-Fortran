program test_combinatorics
use r_mod, only: combn, dp, outer, outer_divide, outer_minus, outer_plus, outer_power
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-12_dp
integer, allocatable :: integer_combinations(:,:)
real(kind=dp), allocatable :: real_combinations(:,:), values(:,:)
character(len=:), allocatable :: character_combinations(:,:)

integer_combinations = combn([1, 2, 3, 4], 2)
call assert_integer_matrix_equal(integer_combinations, reshape([1, 2, 1, 3, 1, 4, &
   2, 3, 2, 4, 3, 4], [2, 6]), "integer combinations")

real_combinations = combn([1.5_dp, 2.5_dp, 3.5_dp], 2)
call assert_real_matrix_close(real_combinations, reshape([1.5_dp, 2.5_dp, 1.5_dp, 3.5_dp, &
   2.5_dp, 3.5_dp], [2, 3]), "real combinations")

character_combinations = combn([character(len=1) :: "a", "b", "c"], 2)
call assert_character_matrix_equal(character_combinations, reshape([character(len=1) :: &
   "a", "b", "a", "c", "b", "c"], [2, 3]), "character combinations")

integer_combinations = combn([4, 5, 6], 3)
call assert_integer_matrix_equal(integer_combinations, reshape([4, 5, 6], [3, 1]), &
   "full-size combination")
integer_combinations = combn([4, 5, 6], 0)
if (any(shape(integer_combinations) /= [0, 1])) error stop "zero-size combination shape failed"
integer_combinations = combn([integer ::], 0)
if (any(shape(integer_combinations) /= [0, 1])) error stop "empty combination shape failed"

values = outer([1.0_dp, 2.0_dp], [10.0_dp, 20.0_dp, 30.0_dp])
call assert_real_matrix_close(values, reshape([10.0_dp, 20.0_dp, 20.0_dp, 40.0_dp, &
   30.0_dp, 60.0_dp], [2, 3]), "outer product")
values = outer_plus([1.0_dp, 2.0_dp], [10.0_dp, 20.0_dp, 30.0_dp])
call assert_real_matrix_close(values, reshape([11.0_dp, 12.0_dp, 21.0_dp, 22.0_dp, &
   31.0_dp, 32.0_dp], [2, 3]), "outer sums")
values = outer_minus([1.0_dp, 2.0_dp], [10.0_dp, 20.0_dp, 30.0_dp])
call assert_real_matrix_close(values, reshape([-9.0_dp, -8.0_dp, -19.0_dp, -18.0_dp, &
   -29.0_dp, -28.0_dp], [2, 3]), "outer differences")
values = outer_divide([10.0_dp, 20.0_dp], [2.0_dp, 5.0_dp])
call assert_real_matrix_close(values, reshape([5.0_dp, 10.0_dp, 2.0_dp, 4.0_dp], [2, 2]), &
   "outer quotients")
values = outer_power([2.0_dp, 3.0_dp], [0.0_dp, 2.0_dp, 3.0_dp])
call assert_real_matrix_close(values, reshape([1.0_dp, 1.0_dp, 4.0_dp, 9.0_dp, &
   8.0_dp, 27.0_dp], [2, 3]), "outer powers")

values = outer([real(kind=dp) ::], [1.0_dp, 2.0_dp])
if (any(shape(values) /= [0, 2])) error stop "empty outer-product shape failed"

contains

subroutine assert_integer_matrix_equal(actual, expected, label)
integer, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(actual /= expected)) error stop trim(label) // " values failed"
end subroutine assert_integer_matrix_equal

subroutine assert_real_matrix_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_real_matrix_close

subroutine assert_character_matrix_equal(actual, expected, label)
character(len=*), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label
integer :: i, j

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
do j = 1, size(actual, 2)
   do i = 1, size(actual, 1)
      if (trim(actual(i, j)) /= trim(expected(i, j))) error stop trim(label) // " values failed"
   end do
end do
end subroutine assert_character_matrix_equal
end program test_combinatorics
