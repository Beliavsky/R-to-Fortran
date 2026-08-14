program test_types_and_missing_values
use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_is_nan, ieee_quiet_nan, ieee_value
use r_mod, only: dp, is_na, numeric, r_character, r_ifelse_real, r_inf, r_is_nan, &
   r_na_real, r_typeof, tail, r_head
implicit none

real(kind=dp) :: ordinary_nan, na_value
real(kind=dp), allocatable :: values(:)
character(len=:), allocatable :: strings(:)
complex(kind=dp) :: complex_na
logical, allocatable :: missing_matrix(:,:)

values = numeric(4)
if (size(values) /= 4 .or. any(values /= 0.0_dp)) error stop "numeric allocation failed"
if (size(numeric(-2)) /= 0) error stop "negative numeric allocation failed"
strings = r_character(3)
if (size(strings) /= 3) error stop "character allocation shape failed"
if (any(strings /= "")) error stop "character allocation initialization failed"
strings(2) = "retained text"
if (trim(strings(2)) /= "retained text") error stop "character allocation capacity failed"
if (size(r_character(-1)) /= 0) error stop "negative character allocation failed"

ordinary_nan = ieee_value(0.0_dp, ieee_quiet_nan)
na_value = r_na_real()
if (.not. is_na(ordinary_nan) .or. .not. is_na(na_value)) error stop "real NA detection failed"
if (.not. r_is_nan(ordinary_nan)) error stop "ordinary NaN detection failed"
if (r_is_nan(na_value)) error stop "NA payload classified as NaN"
call assert_logical_vector(r_is_nan([1.0_dp, ordinary_nan, na_value]), &
   [.false., .true., .false.], "vector NaN payload distinction")
missing_matrix = r_is_nan(reshape([ordinary_nan, na_value, 1.0_dp, ordinary_nan], [2, 2]))
call assert_logical_matrix(missing_matrix, reshape([.true., .false., .false., .true.], [2, 2]), &
   "matrix NaN payload distinction")
if (ieee_is_finite(r_inf()) .or. r_inf() <= 0.0_dp) error stop "positive infinity failed"
call assert_logical_vector(is_na([1.0_dp, ordinary_nan, na_value]), [.false., .true., .true.], &
   "real-vector NA detection")
call assert_logical_vector(is_na([1, -huge(0), 3]), [.false., .true., .false.], &
   "integer NA detection")
call assert_logical_vector(is_na([character(len=3) :: "a", "", "b"]), &
   [.false., .true., .false.], "character NA detection")
call assert_logical_vector(is_na([.true., .false.]), [.false., .false.], &
   "logical NA detection")
complex_na = cmplx(1.0_dp, ordinary_nan, kind=dp)
if (.not. is_na(complex_na)) error stop "complex NA detection failed"
missing_matrix = is_na(reshape([1.0_dp, ordinary_nan, na_value, 4.0_dp], [2, 2]))
call assert_logical_matrix(missing_matrix, reshape([.false., .true., .true., .false.], [2, 2]), &
   "real-matrix NA detection")
missing_matrix = is_na(reshape([1, -huge(0), 3, 4], [2, 2]))
call assert_logical_matrix(missing_matrix, reshape([.false., .true., .false., .false.], [2, 2]), &
   "integer-matrix NA detection")
missing_matrix = is_na(reshape([.true., .false., .false., .true.], [2, 2]))
call assert_logical_matrix(missing_matrix, reshape([.false., .false., .false., .false.], [2, 2]), &
   "logical-matrix NA detection")
missing_matrix = is_na(reshape([cmplx(1.0_dp, 0.0_dp, kind=dp), complex_na, &
   cmplx(ordinary_nan, 2.0_dp, kind=dp), cmplx(3.0_dp, 4.0_dp, kind=dp)], [2, 2]))
call assert_logical_matrix(missing_matrix, reshape([.false., .true., .true., .false.], [2, 2]), &
   "complex-matrix NA detection")
missing_matrix = is_na(reshape([character(len=3) :: "a", "", "b", ""], [2, 2]))
call assert_logical_matrix(missing_matrix, reshape([.false., .true., .false., .true.], [2, 2]), &
   "character-matrix NA detection")
missing_matrix = is_na(reshape([real(kind=dp) ::], [0, 3]))
if (any(shape(missing_matrix) /= [0, 3])) error stop "empty-row NA matrix shape failed"
missing_matrix = is_na(reshape([integer ::], [3, 0]))
if (any(shape(missing_matrix) /= [3, 0])) error stop "empty-column NA matrix shape failed"

if (r_typeof(1.0_dp) /= "double") error stop "real typeof failed"
if (r_typeof([1.0_dp]) /= "double") error stop "real-vector typeof failed"
if (r_typeof(1) /= "integer") error stop "integer typeof failed"
if (r_typeof([1, 2]) /= "integer") error stop "integer-vector typeof failed"
if (r_typeof(.true.) /= "logical") error stop "logical typeof failed"
if (r_typeof([.true.]) /= "logical") error stop "logical-vector typeof failed"
if (r_typeof("abc") /= "character") error stop "character typeof failed"
if (r_typeof([character(len=1) :: "a"]) /= "character") error stop "character-vector typeof failed"
if (r_typeof(cmplx(1.0_dp, 2.0_dp, kind=dp)) /= "complex") error stop "complex typeof failed"
if (r_typeof(reshape([1.0_dp, 2.0_dp], [1, 2])) /= "double") error stop "real-matrix typeof failed"
if (r_typeof(reshape([1, 2], [2, 1])) /= "integer") error stop "integer-matrix typeof failed"
if (r_typeof(reshape([.true., .false.], [1, 2])) /= "logical") error stop "logical-matrix typeof failed"
if (r_typeof(reshape([character(len=1) :: "a", "b"], [2, 1])) /= "character") &
   error stop "character-matrix typeof failed"
if (r_typeof([cmplx(1.0_dp, 2.0_dp, kind=dp)]) /= "complex") &
   error stop "complex-vector typeof failed"
if (r_typeof(reshape([cmplx(1.0_dp, 2.0_dp, kind=dp)], [1, 1])) /= "complex") &
   error stop "complex-matrix typeof failed"
if (r_typeof(reshape([real(kind=dp) ::], [0, 2])) /= "double") &
   error stop "empty real-matrix typeof failed"
if (r_typeof(reshape([integer ::], [2, 0])) /= "integer") &
   error stop "empty integer-matrix typeof failed"
if (r_typeof([logical ::]) /= "logical") error stop "empty logical-vector typeof failed"
if (r_typeof([character(len=1) ::]) /= "character") error stop "empty character-vector typeof failed"
if (r_typeof([complex(kind=dp) ::]) /= "complex") error stop "empty complex-vector typeof failed"

values = r_ifelse_real([1.0_dp, 0.0_dp, na_value], 1.5_dp, -2.0_dp)
if (size(values) /= 3) error stop "ifelse shape failed"
if (abs(values(1) - 1.5_dp) > 1.0e-12_dp .or. abs(values(2) + 2.0_dp) > 1.0e-12_dp) &
   error stop "ifelse branch values failed"
if (.not. ieee_is_nan(values(3))) error stop "ifelse NA propagation failed"

call assert_real_vector(tail([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], 2), &
   [4.0_dp, 5.0_dp], "positive tail")
call assert_real_vector(tail([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], -2), &
   [3.0_dp, 4.0_dp, 5.0_dp], "negative tail")
call assert_real_vector(r_head([1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp], -2), &
   [1.0_dp, 2.0_dp, 3.0_dp], "negative real head")
if (size(tail([1.0_dp, 2.0_dp], -5)) /= 0) error stop "oversized negative tail failed"

contains

subroutine assert_real_vector(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > 1.0e-12_dp)) error stop trim(label) // " values failed"
end subroutine assert_real_vector

subroutine assert_logical_vector(actual, expected, label)
logical, intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(actual .neqv. expected)) error stop trim(label) // " values failed"
end subroutine assert_logical_vector

subroutine assert_logical_matrix(actual, expected, label)
logical, intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(actual .neqv. expected)) error stop trim(label) // " values failed"
end subroutine assert_logical_matrix
end program test_types_and_missing_values
