program test_text_io_helpers
use r_mod, only: dp, file_exists, file_remove, read_csv_header_names, &
   read_csv_real_matrix, read_real_vector, read_table_real_matrix, scan_real, &
   tempfile, write_table_real_matrix, write_table_real_vector
use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
implicit none

character(len=:), allocatable :: matrix_path, vector_path, csv_path, scan_path
character(len=:), allocatable :: headers(:)
real(kind=dp), allocatable :: actual(:), table(:,:)
real(kind=dp), parameter :: matrix_values(2,3) = reshape([ &
   1.0_dp, 4.0_dp, 2.0_dp, 5.0_dp, 3.0_dp, 6.0_dp], [2, 3])
integer :: unit

matrix_path = tempfile("r_mod_matrix_io_")
vector_path = tempfile("r_mod_vector_io_")
csv_path = tempfile("r_mod_csv_io_")
scan_path = tempfile("r_mod_scan_io_")
call remove_if_present(matrix_path)
call remove_if_present(vector_path)
call remove_if_present(csv_path)
call remove_if_present(scan_path)

call write_table_real_matrix(matrix_path, matrix_values, [character(len=2) :: "a", "b", "c"])
call read_table_real_matrix(matrix_path, table, header=.true.)
call assert_matrix_close(table, matrix_values, "matrix table round trip")

call write_table_real_vector(vector_path, [1.5_dp, -2.0_dp, 3.25_dp], "value")
call read_real_vector(vector_path, actual)
call assert_vector_close(actual, [1.5_dp, -2.0_dp, 3.25_dp], "vector table round trip")

open(newunit=unit, file=scan_path, status="replace", action="write")
write(unit, '(a)') "1.0 2.5"
write(unit, '(a)') "ignored header"
write(unit, '(a)') "-3.0"
close(unit)
actual = scan_real(scan_path)
call assert_vector_close(actual, [1.0_dp, 2.5_dp, -3.0_dp], "numeric scan")

open(newunit=unit, file=csv_path, status="replace", action="write")
write(unit, '(a)') '"Date",value,''other'''
write(unit, '(a)') "2026-08-12,1.5,2"
write(unit, '(a)') "2026-08-13,bad,-4"
close(unit)
headers = read_csv_header_names(csv_path)
call assert_headers(headers, [character(len=5) :: "Date", "value", "other"])
call read_csv_real_matrix(csv_path, table)
if (any(shape(table) /= [2, 3])) error stop "CSV matrix shape failed"
if (table(1, 1) /= 20260812.0_dp .or. table(2, 1) /= 20260813.0_dp) error stop "CSV date parsing failed"
if (abs(table(1, 2) - 1.5_dp) > 1.0e-12_dp) error stop "CSV real parsing failed"
if (.not. ieee_is_nan(table(2, 2))) error stop "CSV invalid-value parsing failed"
if (table(1, 3) /= 2.0_dp .or. table(2, 3) /= -4.0_dp) error stop "CSV integer parsing failed"

call remove_required(matrix_path)
call remove_required(vector_path)
call remove_required(csv_path)
call remove_required(scan_path)

contains

subroutine assert_vector_close(x, expected, label)
real(kind=dp), intent(in) :: x(:), expected(:)
character(len=*), intent(in) :: label
if (size(x) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(x - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_vector_close

subroutine assert_matrix_close(x, expected, label)
real(kind=dp), intent(in) :: x(:,:), expected(:,:)
character(len=*), intent(in) :: label
if (any(shape(x) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(x - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_matrix_close

subroutine assert_headers(x, expected)
character(len=*), intent(in) :: x(:), expected(:)
integer :: i
if (size(x) /= size(expected)) error stop "CSV header shape failed"
do i = 1, size(x)
   if (trim(x(i)) /= trim(expected(i))) error stop "CSV header values failed"
end do
end subroutine assert_headers

subroutine remove_if_present(path)
character(len=*), intent(in) :: path
logical :: removed
if (.not. file_exists(path)) return
removed = file_remove(path)
if (.not. removed) error stop "stale test file cleanup failed"
end subroutine remove_if_present

subroutine remove_required(path)
character(len=*), intent(in) :: path
logical :: removed
removed = file_remove(path)
if (.not. removed) error stop "test file cleanup failed"
end subroutine remove_required
end program test_text_io_helpers
