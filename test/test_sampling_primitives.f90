program test_sampling_primitives
use r_mod, only: dp, randint_range, random_choice2_prob, rnorm_mat, rnorm_vec, rnorm1, &
   runif_vec, runif1, sample_int, sample_int1, sample_value_int, set_seed_int
implicit none

integer, allocatable :: first_int(:), second_int(:)
real(kind=dp), allocatable :: first_matrix(:,:), second_matrix(:,:)
real(kind=dp) :: first_real, second_real
integer :: i

call set_seed_int(101)
first_real = runif1()
call set_seed_int(101)
second_real = runif1()
if (first_real /= second_real) error stop "scalar uniform reproducibility failed"
if (first_real < 0.0_dp .or. first_real >= 1.0_dp) error stop "scalar uniform support failed"

call set_seed_int(102)
first_real = rnorm1()
call set_seed_int(102)
second_real = rnorm1()
if (first_real /= second_real) error stop "scalar normal reproducibility failed"

call set_seed_int(103)
first_matrix = rnorm_mat(3, 4)
call set_seed_int(103)
second_matrix = rnorm_mat(3, 4)
if (any(shape(first_matrix) /= [3, 4])) error stop "normal matrix shape failed"
if (any(first_matrix /= second_matrix)) error stop "normal matrix reproducibility failed"

if (any(random_choice2_prob(8, 1.0_dp) /= 1)) error stop "first degenerate binary choice failed"
if (any(random_choice2_prob(8, 0.0_dp) /= 2)) error stop "second degenerate binary choice failed"
if (any(randint_range(6, 4, 4) /= 4)) error stop "degenerate integer range failed"
if (size(randint_range(4, 5, 3)) /= 0) error stop "invalid integer range failed"

call set_seed_int(104)
first_int = randint_range(20, -3, 7)
call set_seed_int(104)
second_int = randint_range(20, -3, 7)
if (any(first_int /= second_int)) error stop "integer range reproducibility failed"
if (any(first_int < -3 .or. first_int > 7)) error stop "integer range support failed"

call set_seed_int(105)
first_int = sample_int(8, size_=8)
if (any(first_int < 1 .or. first_int > 8)) error stop "sample support failed"
if (any([(count(first_int == i) /= 1, i=1,8)])) error stop "sample without replacement failed"
if (any(sample_int(3, size_=10, replace=.true., prob=[0.0_dp, 0.0_dp, 1.0_dp]) /= 3)) &
   error stop "weighted replacement sample failed"
if (size(sample_int(5, size_=0)) /= 0) error stop "zero-size sample failed"
if (sample_int1(1) /= 1) error stop "scalar sample failed"
if (sample_value_int([42]) /= 42) error stop "sampled value failed"

call set_seed_int(24680)
first_real = sum(runif_vec(0))
call assert_rng_unchanged("zero-length uniform")
first_real = sum(rnorm_vec(0))
call assert_rng_unchanged("zero-length normal")
first_matrix = rnorm_mat(0, 3)
if (any(shape(first_matrix) /= [0, 3])) error stop "zero-row normal matrix shape failed"
call assert_rng_unchanged("zero-row normal matrix")
first_matrix = rnorm_mat(3, 0)
if (any(shape(first_matrix) /= [3, 0])) error stop "zero-column normal matrix shape failed"
call assert_rng_unchanged("zero-column normal matrix")
first_int = random_choice2_prob(0, 0.5_dp)
if (size(first_int) /= 0) error stop "zero-length binary choice failed"
call assert_rng_unchanged("zero-length binary choice")
first_int = randint_range(0, 2, 8)
if (size(first_int) /= 0) error stop "zero-length integer range failed"
call assert_rng_unchanged("zero-length integer range")
first_int = randint_range(4, 5, 3)
if (size(first_int) /= 0) error stop "invalid integer range shape failed"
call assert_rng_unchanged("invalid integer range")
first_int = sample_int(5, size_=0)
if (size(first_int) /= 0) error stop "zero-size sample shape failed"
call assert_rng_unchanged("zero-size sample")

contains

subroutine assert_rng_unchanged(label)
character(len=*), intent(in) :: label
real(kind=dp), allocatable :: actual_after(:), expected_after(:)
actual_after = runif_vec(4)
call set_seed_int(24680)
expected_after = runif_vec(4)
if (any(actual_after /= expected_after)) then
   write(*, '(a)') trim(label) // " changed RNG state"
   error stop 1
end if
call set_seed_int(24680)
end subroutine assert_rng_unchanged

end program test_sampling_primitives
