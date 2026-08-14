program test_formatting_and_names
use r_mod, only: ar_coef_names, count_ws_tokens, dp, lag_names, r_format_vec, &
   r_paste0_int, r_paste0_real
implicit none

call assert_string(r_format_vec([1.25_dp, -2.0_dp, 3.5_dp], 2, sep=","), &
   "1.25,-2.00,3.50", "formatted vector with separator")
call assert_string(r_format_vec([1.0_dp, 2.0_dp], 0), "1 2", &
   "formatted vector default separator")
call assert_string(r_format_vec([real(kind=dp) ::], 3), "", "empty formatted vector")

call assert_character_vector(r_paste0_int("item", [1, -2, 30]), &
   [character(len=6) :: "item1", "item-2", "item30"], "integer paste0")
call assert_character_vector(r_paste0_real("x=", [1.0_dp, 1.25_dp, -0.5_dp]), &
   [character(len=6) :: "x=1", "x=1.25", "x=-0.5"], "real paste0")
if (size(r_paste0_int("x", [integer ::])) /= 0) error stop "empty paste0 failed"

call assert_character_vector(ar_coef_names(3), &
   [character(len=9) :: "order", "intercept", "phi1", "phi2", "phi3", &
   "sigma2", "aic", "bic"], "AR coefficient names")
call assert_character_vector(ar_coef_names(-1), &
   [character(len=9) :: "order", "intercept", "sigma2", "aic", "bic"], &
   "zero-order AR coefficient names")
call assert_character_vector(lag_names(3), &
   [character(len=4) :: "lag1", "lag2", "lag3"], "lag names")
if (size(lag_names(-2)) /= 0) error stop "negative lag-name count failed"

if (count_ws_tokens("  alpha beta" // char(9) // "gamma  ") /= 3) &
   error stop "whitespace token count failed"
if (count_ws_tokens("   " // char(9)) /= 0) error stop "blank token count failed"
if (count_ws_tokens("single") /= 1) error stop "single token count failed"

contains

subroutine assert_string(actual, expected, label)
character(len=*), intent(in) :: actual, expected, label

if (actual /= expected) then
   write(*, '(a)') trim(label) // " actual:   [" // actual // "]"
   write(*, '(a)') trim(label) // " expected: [" // expected // "]"
   error stop trim(label) // " failed"
end if
end subroutine assert_string

subroutine assert_character_vector(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector
end program test_formatting_and_names
