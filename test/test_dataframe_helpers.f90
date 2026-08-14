program test_dataframe_helpers
use r_mod, only: data_frame_real, dataframe_real_col, dp, r_dataframe_t
implicit none

type(r_dataframe_t) :: df, empty_df
real(kind=dp) :: columns(3, 2)

columns = reshape([1.0_dp, 2.0_dp, 3.0_dp, 10.0_dp, 20.0_dp, 30.0_dp], [3, 2])
df = data_frame_real([character(len=6) :: "value", "weight"], columns)
if (.not. allocated(df%names) .or. .not. allocated(df%real_cols)) &
   error stop "data frame allocation failed"
if (size(df%names) /= 2 .or. any(shape(df%real_cols) /= [3, 2])) &
   error stop "data frame shape failed"
if (trim(df%names(1)) /= "value" .or. trim(df%names(2)) /= "weight") &
   error stop "data frame names failed"
call assert_real_vector(dataframe_real_col(df, "value"), [1.0_dp, 2.0_dp, 3.0_dp], &
   "first data frame column")
call assert_real_vector(dataframe_real_col(df, "weight"), [10.0_dp, 20.0_dp, 30.0_dp], &
   "second data frame column")

empty_df = data_frame_real([character(len=1) ::], reshape([real(kind=dp) ::], [0, 0]))
if (size(empty_df%names) /= 0 .or. any(shape(empty_df%real_cols) /= [0, 0])) &
   error stop "empty data frame failed"

contains

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_vector
end program test_dataframe_helpers
