program test_optimization
use r_mod, only: dp, nlm_optimize_scalar, nlm_optimize_vec, nlm_result_t, &
   optim_bfgs, optim_lbfgsb, optim_result_t, set_nlm_method
implicit none

real(kind=dp), parameter :: optimizer_tolerance = 2.0e-5_dp
type(optim_result_t) :: optim_fit
type(nlm_result_t) :: nlm_fit

optim_fit = optim_bfgs(quadratic_vec, [8.0_dp, -7.0_dp], gr=quadratic_gradient, &
   reltol=1.0e-10_dp, hessian=.true.)
call assert_optim_success(optim_fit, [2.0_dp, -1.0_dp], "BFGS")
call assert_close(optim_fit%value, 0.0_dp, optimizer_tolerance, "BFGS objective")
call assert_matrix_close(optim_fit%hessian, reshape([2.0_dp, 0.0_dp, 0.0_dp, 4.0_dp], [2, 2]), &
   2.0e-3_dp, "BFGS Hessian")

optim_fit = optim_lbfgsb(quadratic_vec, [8.0_dp, -7.0_dp], lower=[3.0_dp, -2.0_dp], &
   upper=[10.0_dp, 0.0_dp], gr=quadratic_gradient, reltol=1.0e-10_dp)
call assert_optim_success(optim_fit, [3.0_dp, -1.0_dp], "bounded L-BFGS-B")
call assert_close(optim_fit%value, 1.0_dp, optimizer_tolerance, "bounded L-BFGS-B objective")

call set_nlm_method("newton")
nlm_fit = nlm_optimize_scalar(quadratic_scalar, 9.0_dp, hessian=.true.)
call assert_nlm_success(nlm_fit, [3.0_dp], "scalar nlm")
call assert_close(nlm_fit%minimum, 1.0_dp, optimizer_tolerance, "scalar nlm objective")
call assert_close(nlm_fit%hessian(1, 1), 2.0_dp, 2.0e-3_dp, "scalar nlm Hessian")

nlm_fit = nlm_optimize_vec(quadratic_vec, [8.0_dp, -7.0_dp], hessian=.true.)
call assert_nlm_success(nlm_fit, [2.0_dp, -1.0_dp], "vector nlm")
call assert_close(nlm_fit%minimum, 0.0_dp, optimizer_tolerance, "vector nlm objective")
call assert_matrix_close(nlm_fit%hessian, reshape([2.0_dp, 0.0_dp, 0.0_dp, 4.0_dp], [2, 2]), &
   2.0e-3_dp, "vector nlm Hessian")

contains

pure function quadratic_vec(par) result(value)
real(kind=dp), intent(in) :: par(:)
real(kind=dp) :: value

value = (par(1) - 2.0_dp)**2 + 2.0_dp * (par(2) + 1.0_dp)**2
end function quadratic_vec

pure function quadratic_gradient(par) result(value)
real(kind=dp), intent(in) :: par(:)
real(kind=dp), allocatable :: value(:)

value = [2.0_dp * (par(1) - 2.0_dp), 4.0_dp * (par(2) + 1.0_dp)]
end function quadratic_gradient

function quadratic_scalar(x) result(value)
real(kind=dp), intent(in) :: x
real(kind=dp) :: value

value = (x - 3.0_dp)**2 + 1.0_dp
end function quadratic_scalar

subroutine assert_optim_success(actual, expected, label)
type(optim_result_t), intent(in) :: actual
real(kind=dp), intent(in) :: expected(:)
character(len=*), intent(in) :: label

if (actual%convergence /= 0) error stop trim(label) // " convergence failed"
call assert_vector_close(actual%par, expected, optimizer_tolerance, trim(label) // " parameters")
if (actual%counts(1) <= 0 .or. actual%counts(2) <= 0) error stop trim(label) // " counters failed"
end subroutine assert_optim_success

subroutine assert_nlm_success(actual, expected, label)
type(nlm_result_t), intent(in) :: actual
real(kind=dp), intent(in) :: expected(:)
character(len=*), intent(in) :: label

if (actual%code < 1 .or. actual%code > 2) error stop trim(label) // " convergence failed"
call assert_vector_close(actual%estimate, expected, optimizer_tolerance, trim(label) // " estimate")
if (maxval(abs(actual%gradient)) > optimizer_tolerance) error stop trim(label) // " gradient failed"
if (actual%iterations <= 0) error stop trim(label) // " iteration count failed"
end subroutine assert_nlm_success

subroutine assert_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual, expected, tolerance
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_vector_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual(:), expected(:), tolerance
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_vector_close

subroutine assert_matrix_close(actual, expected, tolerance, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:), tolerance
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) error stop trim(label) // " shape failed"
if (any(abs(actual - expected) > tolerance)) error stop trim(label) // " values failed"
end subroutine assert_matrix_close
end program test_optimization
