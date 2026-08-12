program test_eigen_and_pca
use r_mod, only: diag, dp, eigen, eigen_result_t, prcomp, prcomp_fit_t
implicit none

real(kind=dp), parameter :: tolerance = 1.0e-10_dp
real(kind=dp) :: a(2, 2), data(4, 2), centered(4, 2), identity(2, 2)
real(kind=dp), allocatable :: reconstructed(:,:)
complex(kind=dp), allocatable :: complex_reconstruction(:,:)
type(eigen_result_t) :: eig
type(prcomp_fit_t) :: pca

a = 0.0_dp
a(1, 1) = 5.0_dp
a(2, 2) = 2.0_dp
eig = eigen(a, symmetric=.true.)
call assert_complex_vector_close(eig%values, &
   [cmplx(5.0_dp, 0.0_dp, kind=dp), cmplx(2.0_dp, 0.0_dp, kind=dp)], &
   "symmetric eigenvalues")
identity = 0.0_dp
identity(1, 1) = 1.0_dp
identity(2, 2) = 1.0_dp
call assert_complex_matrix_close(matmul(transpose(conjg(eig%vectors)), eig%vectors), &
   cmplx(identity, 0.0_dp, kind=dp), "eigenvector orthogonality")
complex_reconstruction = matmul(eig%vectors, matmul(diag(eig%values), &
   transpose(conjg(eig%vectors))))
call assert_complex_matrix_close(complex_reconstruction, cmplx(a, 0.0_dp, kind=dp), &
   "eigendecomposition reconstruction")

eig = eigen(a, symmetric=.true., only_values=.true.)
if (any(shape(eig%vectors) /= [0, 0])) error stop "values-only eigenvectors failed"
call assert_complex_vector_close(eig%values, &
   [cmplx(5.0_dp, 0.0_dp, kind=dp), cmplx(2.0_dp, 0.0_dp, kind=dp)], &
   "values-only eigenvalues")

data(:, 1) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]
data(:, 2) = 2.0_dp * data(:, 1)
centered(:, 1) = data(:, 1) - 2.5_dp
centered(:, 2) = data(:, 2) - 5.0_dp
pca = prcomp(data)
call assert_real_vector_close(pca%center, [2.5_dp, 5.0_dp], "PCA center")
call assert_real_vector_close(pca%scale, [1.0_dp, 1.0_dp], "PCA default scale")
call assert_real_vector_close(pca%sdev, [5.0_dp / sqrt(3.0_dp), 0.0_dp], &
   "PCA standard deviations")
call assert_matrix_close(matmul(transpose(pca%rotation), pca%rotation), identity, &
   "PCA rotation orthogonality")
reconstructed = matmul(pca%x, transpose(pca%rotation))
call assert_matrix_close(reconstructed, centered, "PCA score reconstruction")
call assert_close(sum(pca%x(:, 1)**2) / 3.0_dp, 25.0_dp / 3.0_dp, &
   "PCA first component variance")
call assert_close(maxval(abs(pca%x(:, 2))), 0.0_dp, "PCA zero second component")

pca = prcomp(data, scale_=.true.)
call assert_real_vector_close(pca%scale, [sqrt(5.0_dp / 3.0_dp), &
   2.0_dp * sqrt(5.0_dp / 3.0_dp)], "scaled PCA column scales")
call assert_real_vector_close(pca%sdev, [sqrt(2.0_dp), 0.0_dp], &
   "scaled PCA standard deviations")

contains

subroutine assert_close(actual, expected, label)
real(kind=dp), intent(in) :: actual, expected
character(len=*), intent(in) :: label

if (abs(actual - expected) > tolerance) then
   write(*, '(a, 2(1x, es24.16))') trim(label) // " failed:", actual, expected
   error stop 1
end if
end subroutine assert_close

subroutine assert_real_vector_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_real_vector_close

subroutine assert_complex_vector_close(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:), expected(:)
character(len=*), intent(in) :: label

if (size(actual) /= size(expected)) then
   write(*, '(a)') trim(label) // " size failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_complex_vector_close

subroutine assert_matrix_close(actual, expected, label)
real(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) then
   write(*, '(a)') trim(label) // " shape failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_matrix_close

subroutine assert_complex_matrix_close(actual, expected, label)
complex(kind=dp), intent(in) :: actual(:,:), expected(:,:)
character(len=*), intent(in) :: label

if (any(shape(actual) /= shape(expected))) then
   write(*, '(a)') trim(label) // " shape failed"
   error stop 1
end if
if (any(abs(actual - expected) > tolerance)) then
   write(*, '(a)') trim(label) // " failed"
   error stop 1
end if
end subroutine assert_complex_matrix_close
end program test_eigen_and_pca
