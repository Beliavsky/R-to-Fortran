program linear_algebra
use r_mod, only: chol, det_real, dp, eigen, eigen_result_t, &
   print_matrix, print_real_vector, solve_real
implicit none

real(kind=dp) :: coefficient(3, 3), rhs(3)
real(kind=dp), allocatable :: factor(:,:), solution(:)
type(eigen_result_t) :: eig

write(*, '(/, a)') repeat("=", 72)
write(*, '(a)') "r_mod example: Linear algebra"
write(*, '(a, /)') repeat("=", 72)

coefficient(1, :) = [4.0_dp, 2.0_dp, 2.0_dp]
coefficient(2, :) = [2.0_dp, 5.0_dp, 1.0_dp]
coefficient(3, :) = [2.0_dp, 1.0_dp, 3.0_dp]
rhs = [12.0_dp, 15.0_dp, 13.0_dp]

solution = solve_real(coefficient, rhs)
factor = chol(coefficient)
eig = eigen(coefficient, symmetric=.true., only_values=.true.)

write(*, '(a)') "Coefficient matrix:"
call print_matrix(coefficient)
write(*, '(a)') "Solution of A x = b:"
call print_real_vector(solution)
write(*, '(a, f10.4)') "Determinant: ", det_real(coefficient)
write(*, '(a)') "Upper-triangular Cholesky factor:"
call print_matrix(factor)
write(*, '(a)') "Eigenvalues:"
call print_real_vector(real(eig%values, kind=dp))
end program linear_algebra
