program test_remaining_small_helpers
use r_mod, only: dp, grep_value_char, r_digit_power_sum, r_sd, sd
implicit none

character(len=:), allocatable :: matches(:)
integer :: digit_sums(3)

matches = grep_value_char("an", [character(len=7) :: "apple", "banana", "pear", "mango"])
call assert_strings(matches, [character(len=6) :: "banana", "mango"], "substring grep")
matches = grep_value_char("", [character(len=2) :: "a", "bb"])
call assert_strings(matches, [character(len=2) :: "a", "bb"], "empty-pattern grep")
matches = grep_value_char("z", [character(len=3) :: "one", "two"])
if (size(matches) /= 0) error stop "missing-pattern grep failed"

if (r_digit_power_sum(123, 2) /= 14) error stop "digit square sum failed"
if (r_digit_power_sum(-123, 3) /= 36) error stop "negative digit cube sum failed"
if (r_digit_power_sum(0, 5) /= 0) error stop "zero digit power sum failed"
digit_sums = r_digit_power_sum([12, 34, 56], 2)
if (any(digit_sums /= [5, 25, 61])) error stop "elemental digit power sum failed"

call assert_close(r_sd([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]), &
   sd([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]), "standard-deviation alias")

contains

subroutine assert_strings(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:), label
integer :: i
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_strings

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > 1.0e-12_dp) error stop trim(label) // " failed"
end subroutine assert_close
end program test_remaining_small_helpers
