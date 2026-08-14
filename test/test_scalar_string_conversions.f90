program test_scalar_string_conversions
use r_mod, only: dp, int_to_string, r_as_real, r_to_string_real, &
   real_to_string_f, real_to_string_g
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
implicit none

character(len=128), allocatable :: strings(:)
real(kind=dp), allocatable :: values(:)

call assert_string(int_to_string(0), "0", "zero integer formatting")
call assert_string(int_to_string(-42), "-42", "negative integer formatting")
call assert_string(real_to_string_f(1.25_dp, 2), "1.25", "fixed real formatting")
call assert_string(real_to_string_f(0.125_dp, 3), "0.125", "fractional fixed real formatting")
call assert_string(real_to_string_f(-0.125_dp, 3), "-0.125", "negative fixed real formatting")

call assert_close(r_as_real(real_to_string_g(12.5_dp, 6)), 12.5_dp, "general real formatting")
strings = real_to_string_g([1.25_dp, -2.5_dp, 30.0_dp], 8)
if (size(strings) /= 3) error stop "vector general formatting shape failed"
values = r_as_real(strings)
call assert_real_vector(values, [1.25_dp, -2.5_dp, 30.0_dp], "vector general formatting")

call assert_string(r_to_string_real(1.25_dp), "1.25", "compact decimal formatting")
call assert_string(r_to_string_real(2.0_dp), "2", "compact integer formatting")
call assert_close(r_as_real("  -3.5e1 "), -35.0_dp, "character real conversion")
call assert_close(r_as_real("0x1.8p1"), 3.0_dp, "hexadecimal character real conversion")
values = r_as_real([character(len=4) :: "1.5", "bad", "-2"])
if (size(values) /= 3) error stop "vector character conversion shape failed"
call assert_close(values(1), 1.5_dp, "first vector character conversion")
if (.not. ieee_is_nan(values(2))) error stop "invalid character conversion failed"
call assert_close(values(3), -2.0_dp, "last vector character conversion")

contains

subroutine assert_string(actual, expected, label)
character(len=*), intent(in) :: actual, expected, label
if (trim(actual) /= expected) error stop trim(label) // " failed"
end subroutine assert_string

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label
if (abs(actual - expected) > 1.0e-10_dp) error stop trim(label) // " failed"
end subroutine assert_close

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-10_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_vector
end program test_scalar_string_conversions
