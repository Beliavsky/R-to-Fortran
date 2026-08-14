program test_equality_and_factor_helpers
use, intrinsic :: ieee_arithmetic, only: ieee_positive_inf, ieee_quiet_nan, ieee_value
use r_mod, only: all_equal, dp, nested_matrix_list_len, r_factor_labels, r_na_real
implicit none

real(kind=dp) :: slices(2, 2, 4), nan_value, inf
complex(kind=dp) :: complex_nan

inf = ieee_value(0.0_dp, ieee_positive_inf)
nan_value = ieee_value(0.0_dp, ieee_quiet_nan)

if (.not. all_equal(1.0_dp, 1.0_dp + 1.0e-10_dp)) &
   error stop "default real tolerance failed"
if (all_equal(1.0_dp, 1.01_dp, tolerance=1.0e-4_dp)) &
   error stop "explicit real tolerance failed"
if (.not. all_equal([1.0_dp, 2.0_dp], [1.0_dp, 2.000001_dp], tolerance=1.0e-5_dp)) &
   error stop "real-vector equality failed"
if (all_equal([1.0_dp], [1.0_dp, 2.0_dp])) error stop "real-vector shape mismatch failed"
if (.not. all_equal([integer ::], [integer ::])) error stop "empty integer equality failed"
if (.not. all_equal(3, 3)) error stop "integer scalar equality failed"
if (all_equal([1, 2], [1, 3])) error stop "integer-vector inequality failed"
if (.not. all_equal([.true., .false.], [.true., .false.])) &
   error stop "logical equality failed"
if (all_equal([.true.], [.false.])) error stop "logical inequality failed"
if (.not. all_equal(.true., .true.)) error stop "logical scalar equality failed"
if (all_equal(.true., .false., tolerance=0.0_dp)) error stop "logical scalar inequality failed"
if (.not. all_equal("alpha", "alpha")) error stop "character scalar equality failed"
if (all_equal([character(len=1) :: "a"], [character(len=1) :: "b"])) &
   error stop "character-vector inequality failed"
if (.not. all_equal(inf, inf)) error stop "matching positive infinity failed"
if (.not. all_equal(-inf, -inf)) error stop "matching negative infinity failed"
if (all_equal(inf, -inf)) error stop "opposite infinities compared equal"
if (.not. all_equal(nan_value, nan_value)) error stop "matching NaN failed"
if (.not. all_equal(r_na_real(), nan_value)) error stop "NA and NaN all.equal semantics failed"
if (all_equal(nan_value, 1.0_dp)) error stop "NaN compared equal to finite value"
if (.not. all_equal([1.0_dp, inf, nan_value], [1.0_dp, inf, nan_value])) &
   error stop "nonfinite real-vector equality failed"
if (all_equal([1.0_dp, inf], [1.0_dp, -inf])) &
   error stop "nonfinite real-vector inequality failed"
if (.not. all_equal(reshape([0.0_dp, -0.0_dp, inf, nan_value], [2, 2]), &
   reshape([-0.0_dp, 0.0_dp, inf, nan_value], [2, 2]))) &
   error stop "nonfinite real-matrix equality failed"
if (all_equal(reshape([1.0_dp, 2.0_dp], [1, 2]), &
   reshape([1.0_dp, 2.0_dp], [2, 1]))) error stop "real-matrix shape mismatch failed"
if (.not. all_equal([1, -huge(0), 3], [1, -huge(0), 3])) &
   error stop "integer NA-vector equality failed"
if (all_equal([1, -huge(0), 3], [1, 2, 3])) error stop "integer NA-vector inequality failed"
if (.not. all_equal(reshape([1, -huge(0), 3, 4], [2, 2]), &
   reshape([1, -huge(0), 3, 4], [2, 2]))) error stop "integer-matrix equality failed"
if (all_equal(reshape([1, 2], [1, 2]), reshape([1, 2], [2, 1]))) &
   error stop "integer-matrix shape mismatch failed"
if (.not. all_equal(cmplx(1.0_dp, 2.0_dp, kind=dp), &
   cmplx(1.0_dp + 1.0e-10_dp, 2.0_dp - 1.0e-10_dp, kind=dp))) &
   error stop "complex scalar tolerance failed"
if (all_equal(cmplx(1.0_dp, 2.0_dp, kind=dp), &
   cmplx(1.0_dp, 2.1_dp, kind=dp), tolerance=1.0e-4_dp)) &
   error stop "complex scalar inequality failed"
complex_nan = cmplx(nan_value, inf, kind=dp)
if (.not. all_equal(complex_nan, complex_nan)) error stop "complex nonfinite equality failed"
if (.not. all_equal([cmplx(1.0_dp, 2.0_dp, kind=dp), complex_nan], &
   [cmplx(1.0_dp, 2.0_dp, kind=dp), complex_nan])) &
   error stop "complex-vector equality failed"
if (.not. all_equal([complex(kind=dp) ::], [complex(kind=dp) ::])) &
   error stop "empty complex-vector equality failed"
if (all_equal(reshape([cmplx(1.0_dp, 0.0_dp, kind=dp), &
   cmplx(2.0_dp, 0.0_dp, kind=dp)], [1, 2]), &
   reshape([cmplx(1.0_dp, 0.0_dp, kind=dp), &
   cmplx(2.0_dp, 0.0_dp, kind=dp)], [2, 1]))) &
   error stop "complex-matrix shape mismatch failed"
if (.not. all_equal(reshape([complex_nan, cmplx(2.0_dp, 3.0_dp, kind=dp)], [1, 2]), &
   reshape([cmplx(r_na_real(), inf, kind=dp), cmplx(2.0_dp, 3.0_dp, kind=dp)], [1, 2]))) &
   error stop "complex-matrix missing equality failed"
if (.not. all_equal(reshape([.true., .false., .false., .true.], [2, 2]), &
   reshape([.true., .false., .false., .true.], [2, 2]))) error stop "logical-matrix equality failed"
if (all_equal(reshape([.true., .false.], [1, 2]), reshape([.true., .false.], [2, 1]))) &
   error stop "logical-matrix shape mismatch failed"
if (.not. all_equal(reshape([character(len=2) :: "a", "bb", "c", "dd"], [2, 2]), &
   reshape([character(len=2) :: "a", "bb", "c", "dd"], [2, 2]))) &
   error stop "character-matrix equality failed"
if (all_equal(reshape([character(len=1) :: "a", "b"], [1, 2]), &
   reshape([character(len=1) :: "a", "b"], [2, 1]))) error stop "character-matrix shape mismatch failed"

call assert_character_vector(r_factor_labels([1, 3, 2, 0, 4], &
   [character(len=6) :: "low", "medium", "high"]), &
   [character(len=6) :: "low", "high", "medium", "NA", "NA"], &
   "factor labels")
if (size(r_factor_labels([integer ::], [character(len=1) :: "a"])) /= 0) &
   error stop "empty factor labels failed"

slices = nan_value
slices(:, :, 1) = 1.0_dp
slices(1, 1, 3) = 2.0_dp
if (nested_matrix_list_len(slices) /= 3) error stop "nested matrix list length failed"
slices = nan_value
if (nested_matrix_list_len(slices) /= 0) error stop "empty nested matrix list failed"

contains

subroutine assert_character_vector(actual, expected, label)
character(len=*), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label
integer :: i

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
do i = 1, size(actual)
   if (trim(actual(i)) /= trim(expected(i))) error stop trim(label) // " values failed"
end do
end subroutine assert_character_vector
end program test_equality_and_factor_helpers
