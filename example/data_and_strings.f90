program data_and_strings
use r_mod, only: char_join, data_frame_real, dataframe_real_col, dp, &
   print_char_vector, print_dataframe, print_real_vector, &
   r_dataframe_t, strsplit_fixed, toupper
implicit none

character(len=:), allocatable :: fields(:)
real(kind=dp) :: columns(4, 2)
type(r_dataframe_t) :: measurements
integer :: i

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: Numeric data frames and strings"
write(*, '(a, /)') repeat("=", 72)

columns(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
columns(:, 2) = [10.5_dp, 11.0_dp, 10.8_dp, 11.4_dp]
measurements = data_frame_real( &
   [character(len=11) :: "observation", "temperature"], columns)

write(*, '(a)') "Numeric data frame:"
call print_dataframe(measurements)
write(*, '(a)') "Named temperature column:"
call print_real_vector(dataframe_real_col(measurements, "temperature"))

fields = strsplit_fixed("alpha,beta,gamma", ",")
write(*, '(a)') "Split fields:"
call print_char_vector(fields)
do i = 1, size(fields)
   fields(i) = toupper(fields(i))
end do
write(*, '(a, 1x, a)') "Upper-case fields:", char_join(fields, " | ")
end program data_and_strings
