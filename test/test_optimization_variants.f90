program test_optimization_variants
use r_mod, only: constr_optim_bfgs, constr_optim_nelder_mead, dp, optim_cg, &
   optim_nelder_mead, optim_result_t, optim_sann, set_seed_int
implicit none

real(kind=dp), parameter :: start(2) = [8.0_dp, -7.0_dp]
real(kind=dp), parameter :: ui(1,2) = reshape([1.0_dp, 0.0_dp], [1, 2])
type(optim_result_t) :: fit

fit = optim_cg(quadratic, start, maxit=300, reltol=1.0e-9_dp, ndeps=1.0e-5_dp)
call assert_solution(fit, [2.0_dp, -1.0_dp], 2.0e-3_dp, "conjugate gradient")

fit = optim_nelder_mead(quadratic, start, maxit=500, reltol=1.0e-10_dp)
call assert_solution(fit, [2.0_dp, -1.0_dp], 2.0e-3_dp, "Nelder-Mead")

call set_seed_int(601)
fit = optim_sann(quadratic, start, maxit=1500, parscale=[1.0_dp, 1.0_dp])
if (fit%convergence /= 0) error stop "simulated annealing status failed"
if (fit%value >= quadratic(start)) error stop "simulated annealing improvement failed"
if (any(.not. (fit%par > -huge(1.0_dp) .and. fit%par < huge(1.0_dp)))) &
   error stop "simulated annealing parameter finiteness failed"

fit = constr_optim_bfgs(quadratic, [4.0_dp, -3.0_dp], ui, [3.0_dp], &
   maxit=800, reltol=1.0e-8_dp, ndeps=1.0e-5_dp)
call assert_constrained(fit, "constrained BFGS")

fit = constr_optim_nelder_mead(quadratic, [4.0_dp, -3.0_dp], ui, [3.0_dp], &
   maxit=1200, reltol=1.0e-8_dp)
call assert_constrained(fit, "constrained Nelder-Mead")

fit = constr_optim_bfgs(quadratic, [2.0_dp, -1.0_dp], ui, [3.0_dp])
if (fit%convergence /= 51) error stop "infeasible constrained status failed"
if (trim(fit%message) /= "initial value is not feasible") &
   error stop "infeasible constrained message failed"

contains

pure function quadratic(par) result(value)
real(kind=dp), intent(in) :: par(:)
real(kind=dp) :: value
value = (par(1) - 2.0_dp)**2 + 2.0_dp * (par(2) + 1.0_dp)**2
end function quadratic

subroutine assert_solution(actual, expected, tolerance, label)
type(optim_result_t), intent(in) :: actual
real(kind=dp), intent(in) :: expected(:), tolerance
character(len=*), intent(in) :: label
if (actual%convergence /= 0) error stop trim(label) // " convergence failed"
if (size(actual%par) /= size(expected)) error stop trim(label) // " shape failed"
if (any(abs(actual%par - expected) > tolerance)) error stop trim(label) // " parameters failed"
if (actual%value > tolerance**2) error stop trim(label) // " objective failed"
end subroutine assert_solution

subroutine assert_constrained(actual, label)
type(optim_result_t), intent(in) :: actual
character(len=*), intent(in) :: label
if (size(actual%par) /= 2) error stop trim(label) // " shape failed"
if (actual%par(1) <= 3.0_dp) error stop trim(label) // " feasibility failed"
if (abs(actual%par(2) + 1.0_dp) > 0.1_dp) error stop trim(label) // " second parameter failed"
if (actual%value >= quadratic([4.0_dp, -3.0_dp])) error stop trim(label) // " improvement failed"
end subroutine assert_constrained
end program test_optimization_variants
