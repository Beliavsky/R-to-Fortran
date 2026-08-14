program test_structured_result_printing
use r_mod, only: aggregate, aggregate_result_t, by_matrix_result_t, &
   data_frame_real, date_from_iso, dp, print_aggregate_result, &
   print_by_matrix_result, print_dataframe, print_dataframe_head, &
   print_date, print_date_vector, r_by, r_dataframe_t
implicit none

type(aggregate_result_t) :: aggregate_result, empty_aggregate
type(by_matrix_result_t) :: by_result, empty_by
type(r_dataframe_t) :: dataframe, empty_dataframe
real(kind=dp) :: columns(5, 2)
integer :: epoch, leap_day

columns(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
columns(:, 2) = [10.0_dp, 20.0_dp, 30.0_dp, 40.0_dp, 50.0_dp]
dataframe = data_frame_real([character(len=6) :: "value", "weight"], columns)
call print_dataframe(dataframe)
call print_dataframe(dataframe, 2)
call print_dataframe_head(dataframe)
call print_dataframe_head(dataframe, 3)
call print_dataframe(empty_dataframe)

aggregate_result = aggregate(columns(:, 1), &
   [character(len=1) :: "a", "a", "b", "b", "a"], &
   "group", "value", "mean")
call print_aggregate_result(aggregate_result)
call print_aggregate_result(empty_aggregate)

by_result = r_by(columns, [character(len=1) :: "a", "a", "b", "b", "a"], &
   "colmeans")
call print_by_matrix_result(by_result)
call print_by_matrix_result(empty_by)

epoch = date_from_iso("1970-01-01")
leap_day = date_from_iso("2000-02-29")
call print_date(epoch)
call print_date_vector([epoch - 1, epoch, epoch + 1, leap_day])
call print_date_vector([integer ::])
end program test_structured_result_printing
