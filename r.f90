! helper functions for R-to-Fortran transpiler
module r_mod
use, intrinsic :: iso_fortran_env, only: real64, int64
use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
   & ieee_is_finite
#ifdef XR2F_USE_R_RNG
use, intrinsic :: iso_c_binding, only: c_double, c_int
#endif
implicit none
private
public :: dp, runif1, runif_vec, rnorm1, rnorm_vec, rnorm_mat, rbinom, rpois, random_choice2_prob, &
   & randint_range, sample_int, sample_int1, quantile, median, summary, dnorm, tail, cbind2, cbind, numeric, &
   & pmax, sd, r_sd, var, r_format_vec, colMeans, apply_col_cumsum, apply_col_sd, count_ws_tokens, &
   & besselJ, besselY, besselI, besselK, &
   & read_real_vector, read_table_real_matrix, read_csv_real_matrix, read_csv_header_names, &
   & write_table_real_matrix, lm_fit_t, glm_fit_t, prcomp_fit_t, eigen_result_t, optim_result_t, nlm_result_t, nlm_stub, nlm_optimize_scalar, nlm_optimize_vec, print_nlm_result, decompose_result_t, ks_test_result_t, lm_fit_general, lm_r_squared_general, lm_predict_general, step_lm, &
   & lm_predict_interval, print_lm_prediction_interval, lm_confint, lm_cooks_distance, print_lm_cooks_top, &
   & lm_coef, print_lm_summary, print_lm_coef_rstyle, print_lm_confint, print_lm_anova, pchisq, normal_cdf, qnorm, ppois, qpois, &
   & dunif, punif, qunif, dexp, pexp, qexp, dgamma, pgamma, qgamma, dbeta, pbeta, qbeta, dchisq, qchisq, &
   & dt, pt, qt, df, pf, qf, dlogis, plogis, qlogis, dlnorm, plnorm, qlnorm, dweibull, pweibull, qweibull, &
   & dcauchy, pcauchy, qcauchy, dbinom, pbinom, qbinom, dpois, dgeom, pgeom, qgeom, dnbinom, pnbinom, qnbinom, &
   & dhyper, phyper, qhyper, dwilcox, pwilcox, qwilcox, dsignrank, psignrank, qsignrank, cov, cor, mahalanobis, isSymmetric, scale, all_equal, r_log, r_seq_int, r_seq_len, &
   & glm_binomial_fit, glm_poisson_fit, glm_predict_response, glm_pearson_resid, print_glm_summary, &
   & prcomp, print_prcomp_summary, eigen, print_eigen, arima_fit_t, arima_predict_result_t, arima_sim, arima_fit, arima_predict, arima_predict_result, print_arima_fit, &
   & acf_fit_t, acf, r_acf, r_acf_values, r_ccf, print_acf, ar_fit_t, ar_fit, ARMAacf, &
   & r_seq_int_by, r_seq_int_length, r_seq_real_by, r_seq_real_length, &
   & r_rep_real, r_rep_char, r_rep_int, r_drop_index, r_drop_indices, r_head, r_array_real, r_array_int, r_array_char, matrix, &
   & r_matmul, r_add, r_sub, r_mul, r_div, print_matrix, &
   & print_matrix_rstyle, print_matrix_rstyle_named, print_real_scalar, &
   & print_real_vector, print_char_vector, &
   & print_named_real_vector, print_table1, print_table2, print_summary, set_print_int_like, &
   & set_print_int_like_tol, set_recycle_warn, set_recycle_stop, set_seed_int, &
   & kmeans_result_t, kmeans, rbind, max_col, tabulate, table2, prop_table, match, r_in, unique, duplicated, anyDuplicated, &
   & union, intersect, setdiff, setequal, findInterval, &
   & cumsum, cumprod, diff, diag, toeplitz, chol, chol2inv, forwardsolve, backsolve, sort, sort_list, polyroot, decompose, ecdf_eval, &
   & nchar, char_join, list_files, strsplit_fixed, toupper, tolower, casefold, trimws, replace_first_fixed, replace_all_fixed, chartr, ar_coef_names, lag_names, lower_tri, upper_tri, row_index_mat, col_index_mat, is_na, which, which_arr_ind, replace, rle, inverse_rle, print_rle, r_typeof, r_character, order_real, rank_average, &
   & rank_first, det_real, kappa_real, eigen_sym_values, solve_real, qr_fit_t, qr, qr_Q, qr_R, qr_coef, qr_rank, qr_pivot, qr_fitted, qr_resid, qr_qty, qr_qy, print_qr, &
   & rle_real_t, rle_int_t, rle_char_t, rle_logical_t, &
   & nested_matrix_list_len, r_beta, r_lbeta, r_choose, r_lchoose, r_gamma, r_lgamma, r_psigamma, r_digamma, r_trigamma, &
   & r_factorial, r_lfactorial, t_test_result_t, t_test, t_test_p_value, print_t_test, &
   & chisq_test_result_t, chisq_test, print_chisq_test, prop_test_result_t, &
   & prop_test, print_prop_test, cor_test_result_t, cor_test, print_cor_test, &
   & fisher_test_result_t, fisher_test, print_fisher_test, wilcox_test_result_t, &
   & wilcox_test, print_wilcox_test, kruskal_test_result_t, kruskal_test, ks_test, print_ks_test, print_factanal, &
   & r_filter_linear, smooth_xy_t, loess_fit_t, smooth_spline_fit_t, smooth, &
   & runmed, ksmooth, lowess, loess_fit, predict_loess, smooth_spline, &
   & predict_smooth_spline, dist, hclust_result_t, hclust, cutree, &
   & print_kruskal_test
integer, parameter :: dp = real64
logical :: print_int_like_default = .true.
real(kind=dp) :: print_int_like_tol = 1000.0_dp * epsilon(1.0_dp)
logical :: recycle_warn_default = .false.
logical :: recycle_stop_default = .false.
#ifdef XR2F_USE_R_RNG
interface
   subroutine xr2f_r_set_seed(seed) bind(C, name="xr2f_r_set_seed")
      import :: c_int
      integer(c_int), value :: seed
   end subroutine xr2f_r_set_seed

   function xr2f_r_unif_rand() bind(C, name="xr2f_r_unif_rand") result(x)
      import :: c_double
      real(c_double) :: x
   end function xr2f_r_unif_rand

   function xr2f_r_norm_rand() bind(C, name="xr2f_r_norm_rand") result(x)
      import :: c_double
      real(c_double) :: x
   end function xr2f_r_norm_rand
end interface
#endif
type :: lm_fit_t
   real(kind=dp), allocatable :: coef(:), fitted(:), resid(:), cov_unscaled(:,:), y(:), xpred(:,:)
   real(kind=dp) :: sigma, r_squared, adj_r_squared
   integer :: df = 0
end type lm_fit_t

type :: glm_fit_t
   real(kind=dp), allocatable :: coef(:), se(:), z_value(:), p_value(:), fitted(:), resid(:), y(:), xpred(:,:)
   real(kind=dp), allocatable :: offset(:)
   integer :: df = 0
   integer :: convergence = 1
   integer :: iter = 0
   integer :: family = 1
end type glm_fit_t

type :: prcomp_fit_t
   real(kind=dp), allocatable :: sdev(:), rotation(:,:), x(:,:), center(:), scale(:)
end type prcomp_fit_t

type :: eigen_result_t
   real(kind=dp), allocatable :: values(:), vectors(:,:)
end type eigen_result_t

type :: arima_fit_t
   real(kind=dp), allocatable :: coef(:), resid(:)
   real(kind=dp) :: mean = 0.0_dp
   real(kind=dp) :: aic = 0.0_dp
   real(kind=dp) :: sigma2 = 0.0_dp
   real(kind=dp) :: last_x = 0.0_dp
   real(kind=dp) :: last_resid = 0.0_dp
   integer :: p = 0
   integer :: d = 0
   integer :: q = 0
end type arima_fit_t

type :: arima_predict_result_t
   real(kind=dp), allocatable :: pred(:), se(:)
end type arima_predict_result_t

type :: ar_fit_t
   integer :: order = 0
   real(kind=dp), allocatable :: ar(:), aic(:)
   real(kind=dp) :: var_pred = 0.0_dp
end type ar_fit_t

type :: smooth_xy_t
   real(kind=dp), allocatable :: x(:), y(:)
   real(kind=dp) :: df = 0.0_dp
end type smooth_xy_t

type :: loess_fit_t
   real(kind=dp), allocatable :: x(:), y(:)
   real(kind=dp) :: span = 0.75_dp
   integer :: degree = 2
end type loess_fit_t

type :: smooth_spline_fit_t
   real(kind=dp), allocatable :: x(:), y(:)
   real(kind=dp) :: df = 0.0_dp
end type smooth_spline_fit_t

type :: acf_fit_t
   real(kind=dp), allocatable :: acf(:,:,:)
   real(kind=dp), allocatable :: lag(:)
   integer :: n_used = 0
   integer :: type_code = 1
end type acf_fit_t

type :: qr_fit_t
   real(kind=dp), allocatable :: qr(:,:), q(:,:), r(:,:), qraux(:)
   integer, allocatable :: pivot(:)
   integer :: rank = 0
end type qr_fit_t

type :: rle_real_t
   integer, allocatable :: lengths(:)
   real(kind=dp), allocatable :: values(:)
end type rle_real_t

type :: rle_int_t
   integer, allocatable :: lengths(:)
   integer, allocatable :: values(:)
end type rle_int_t

type :: rle_char_t
   integer, allocatable :: lengths(:)
   character(len=:), allocatable :: values(:)
end type rle_char_t

type :: rle_logical_t
   integer, allocatable :: lengths(:)
   logical, allocatable :: values(:)
end type rle_logical_t

type :: kmeans_result_t
   real(kind=dp), allocatable :: centers(:,:)
   integer, allocatable :: cluster(:)
   integer, allocatable :: size(:)
   real(kind=dp), allocatable :: withinss(:)
end type kmeans_result_t

type :: hclust_result_t
   integer, allocatable :: merge(:,:)
   real(kind=dp), allocatable :: height(:)
   integer, allocatable :: order(:)
   integer, allocatable :: labels(:)
   integer :: method = 1
end type hclust_result_t

type :: optim_result_t
   real(kind=dp), allocatable :: par(:)
   real(kind=dp) :: value
   integer :: convergence
end type optim_result_t

type :: nlm_result_t
   real(kind=dp) :: minimum = 0.0_dp
   real(kind=dp), allocatable :: estimate(:), gradient(:), hessian(:,:)
   integer :: code = 1
   integer :: iterations = 0
end type nlm_result_t

abstract interface
   function nlm_objective_scalar(x) result(v)
      import :: dp
      real(kind=dp), intent(in) :: x
      real(kind=dp) :: v
   end function nlm_objective_scalar
   function nlm_objective_vec(p) result(v)
      import :: dp
      real(kind=dp), intent(in) :: p(:)
      real(kind=dp) :: v
   end function nlm_objective_vec
end interface

type :: decompose_result_t
   real(kind=dp), allocatable :: trend(:), seasonal(:), random(:), figure(:)
end type decompose_result_t

type :: t_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   real(kind=dp) :: parameter = 0.0_dp
   real(kind=dp) :: p_value = 1.0_dp
   real(kind=dp) :: estimate = 0.0_dp
   real(kind=dp) :: estimate2 = 0.0_dp
   real(kind=dp) :: null_value = 0.0_dp
   real(kind=dp) :: conf_low = 0.0_dp
   real(kind=dp) :: conf_high = 0.0_dp
   real(kind=dp) :: stderr = 0.0_dp
   integer :: method = 1
end type t_test_result_t

type :: chisq_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   integer :: parameter = 0
   real(kind=dp) :: p_value = 1.0_dp
   integer :: method = 1
end type chisq_test_result_t

type :: prop_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   integer :: parameter = 0
   real(kind=dp) :: p_value = 1.0_dp
   real(kind=dp) :: estimate = 0.0_dp
   real(kind=dp) :: estimate2 = 0.0_dp
   real(kind=dp) :: null_value = 0.0_dp
   integer :: method = 1
end type prop_test_result_t

type :: cor_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   integer :: parameter = 0
   real(kind=dp) :: p_value = 1.0_dp
   real(kind=dp) :: estimate = 0.0_dp
   integer :: method = 1
end type cor_test_result_t

type :: fisher_test_result_t
   real(kind=dp) :: p_value = 1.0_dp
   real(kind=dp) :: estimate = 0.0_dp
   integer :: method = 1
end type fisher_test_result_t

type :: wilcox_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   real(kind=dp) :: p_value = 1.0_dp
   integer :: method = 1
end type wilcox_test_result_t

type :: kruskal_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   integer :: parameter = 0
   real(kind=dp) :: p_value = 1.0_dp
end type kruskal_test_result_t

type :: ks_test_result_t
   real(kind=dp) :: statistic = 0.0_dp
   real(kind=dp) :: p_value = 1.0_dp
   integer :: n = 0
end type ks_test_result_t

interface cov
   module procedure cov_vec
   module procedure cov_mat
end interface cov

interface var
   module procedure var_vec
   module procedure var_mat
end interface var

interface sd
   module procedure sd_vec
   module procedure sd_mat
end interface sd

interface cor
   module procedure cor_vec
   module procedure cor_mat
   module procedure cor_mat_pair
end interface cor

interface isSymmetric
   module procedure isSymmetric_real
   module procedure isSymmetric_int
end interface isSymmetric

interface scale
   module procedure scale_vec
   module procedure scale_mat
end interface scale

interface all_equal
   module procedure all_equal_real_scalar
   module procedure all_equal_real_vec
   module procedure all_equal_real_mat
   module procedure all_equal_int_scalar
   module procedure all_equal_int_vec
   module procedure all_equal_int_mat
   module procedure all_equal_logical_vec
   module procedure all_equal_logical_mat
end interface all_equal

interface r_log
   module procedure r_log_scalar
   module procedure r_log_vec
   module procedure r_log_mat
end interface r_log

interface det_real
   module procedure det_real_mat
   module procedure det_real_int
end interface det_real

interface kmeans
   module procedure kmeans_vec
   module procedure kmeans_mat
end interface kmeans

interface rbind
   module procedure rbind_vec
   module procedure rbind_mat
   module procedure rbind_vec_mat
   module procedure rbind_mat_vec
end interface rbind

interface t_test
   module procedure t_test_one
   module procedure t_test_two
end interface t_test

interface t_test_p_value
   module procedure t_test_p_value_one
   module procedure t_test_p_value_two
end interface t_test_p_value

interface chisq_test
   module procedure chisq_test_int_vec
   module procedure chisq_test_real_vec
   module procedure chisq_test_int_mat
   module procedure chisq_test_real_mat
end interface chisq_test

interface prop_test
   module procedure prop_test_int_scalar
   module procedure prop_test_real_scalar
   module procedure prop_test_int_vec
   module procedure prop_test_real_vec
end interface prop_test

interface r_head
   module procedure r_head_real
   module procedure r_head_int
   module procedure r_head_real_mat
   module procedure r_head_int_mat
end interface r_head

interface cor_test
   module procedure cor_test_real_vec
   module procedure cor_test_int_vec
end interface cor_test

interface fisher_test
   module procedure fisher_test_int_mat
   module procedure fisher_test_real_mat
end interface fisher_test

interface wilcox_test
   module procedure wilcox_test_two_sample
end interface wilcox_test

interface rbinom
   module procedure rbinom_scalar
   module procedure rbinom_vector_prob
end interface rbinom

interface rpois
   module procedure rpois_scalar
   module procedure rpois_vector
end interface rpois

interface dnorm
   module procedure dnorm_vec
   module procedure dnorm_scalar
   module procedure dnorm_int_scalar
end interface dnorm

interface pchisq
   module procedure pchisq_scalar
   module procedure pchisq_vec
end interface pchisq

interface qchisq
   module procedure qchisq_scalar
   module procedure qchisq_scalar_i
   module procedure qchisq_vec
end interface qchisq

interface glm_binomial_fit
   module procedure glm_binomial_fit_real
   module procedure glm_binomial_fit_int
end interface glm_binomial_fit

interface glm_poisson_fit
   module procedure glm_poisson_fit_real
   module procedure glm_poisson_fit_int
end interface glm_poisson_fit

interface matrix
   module procedure matrix_real
   module procedure matrix_int
end interface matrix

interface summary
   module procedure summary_vec
   module procedure summary_mat
end interface summary

interface print_summary
   module procedure print_summary_vec
   module procedure print_summary_mat
end interface print_summary

interface r_matmul
   module procedure r_matmul_vv_real
   module procedure r_matmul_vv_int
   module procedure r_matmul_vv_real_int
   module procedure r_matmul_vv_int_real
   module procedure r_matmul_mv_real
   module procedure r_matmul_mv_int
   module procedure r_matmul_mv_real_int
   module procedure r_matmul_mv_int_real
   module procedure r_matmul_vm_real
   module procedure r_matmul_vm_int
   module procedure r_matmul_vm_real_int
   module procedure r_matmul_vm_int_real
   module procedure r_matmul_mm_real
   module procedure r_matmul_mm_int
   module procedure r_matmul_mm_real_int
   module procedure r_matmul_mm_int_real
   module procedure r_matmul_mv_complex
   module procedure r_matmul_mm_complex
end interface r_matmul

interface r_add
   module procedure r_add_vv
   module procedure r_add_vs
   module procedure r_add_sv
end interface r_add

interface r_sub
   module procedure r_sub_vv
   module procedure r_sub_vs
   module procedure r_sub_sv
end interface r_sub

interface r_mul
   module procedure r_mul_vv
   module procedure r_mul_vs
   module procedure r_mul_sv
end interface r_mul

interface r_div
   module procedure r_div_vv
   module procedure r_div_vs
   module procedure r_div_sv
end interface r_div

interface r_array
   module procedure r_array_real
   module procedure r_array_int
   module procedure r_array_char
end interface r_array

interface tabulate
   module procedure tabulate_int
   module procedure tabulate_real
end interface tabulate

interface table2
   module procedure table2_int
end interface table2

interface prop_table
   module procedure prop_table_int_vec
   module procedure prop_table_int_mat
end interface prop_table

interface print_table2
   module procedure print_table2_int
   module procedure print_table2_real
end interface print_table2

interface match
   module procedure match_int
   module procedure match_real
   module procedure match_char
end interface match

interface r_in
   module procedure r_in_int
   module procedure r_in_real
   module procedure r_in_int_real
   module procedure r_in_real_int
   module procedure r_in_int_scalar
   module procedure r_in_real_scalar
   module procedure r_in_int_scalar_real
   module procedure r_in_real_scalar_int
   module procedure r_in_char
   module procedure r_in_logical
end interface r_in

interface r_drop_index
   module procedure r_drop_index_real
   module procedure r_drop_index_int
end interface r_drop_index

interface r_drop_indices
   module procedure r_drop_indices_real
   module procedure r_drop_indices_int
end interface r_drop_indices

interface unique
   module procedure unique_int
   module procedure unique_real
   module procedure unique_char
   module procedure unique_logical
end interface unique

interface duplicated
   module procedure duplicated_int
   module procedure duplicated_real
   module procedure duplicated_char
   module procedure duplicated_logical
end interface duplicated

interface anyDuplicated
   module procedure anyDuplicated_int
   module procedure anyDuplicated_real
   module procedure anyDuplicated_char
   module procedure anyDuplicated_logical
end interface anyDuplicated

interface union
   module procedure union_int
   module procedure union_real
   module procedure union_char
end interface union

interface intersect
   module procedure intersect_int
   module procedure intersect_real
   module procedure intersect_char
end interface intersect

interface setdiff
   module procedure setdiff_int
   module procedure setdiff_real
   module procedure setdiff_char
end interface setdiff

interface setequal
   module procedure setequal_int
   module procedure setequal_real
   module procedure setequal_char
end interface setequal

interface print_matrix
   module procedure print_matrix_real
   module procedure print_matrix_int
   module procedure print_matrix_logical
end interface print_matrix

interface print_matrix_rstyle
   module procedure print_matrix_rstyle_real
   module procedure print_matrix_rstyle_int
   module procedure print_matrix_rstyle_logical
end interface print_matrix_rstyle

interface print_matrix_rstyle_named
   module procedure print_matrix_rstyle_named_real
   module procedure print_matrix_rstyle_named_int
end interface print_matrix_rstyle_named

interface cumsum
   module procedure cumsum_real
   module procedure cumsum_int
end interface cumsum

interface cumprod
   module procedure cumprod_real
   module procedure cumprod_int
end interface cumprod

interface diff
   module procedure diff_real
   module procedure diff_mat_real
   module procedure diff_int
end interface diff

interface diag
   module procedure diag_mat_real
   module procedure diag_vec_real
   module procedure diag_mat_int
   module procedure diag_vec_int
   module procedure diag_scalar_int
   module procedure diag_scalar_real_n
end interface diag

interface eigen
   module procedure eigen_real
   module procedure eigen_int
end interface eigen

interface sort
   module procedure sort_real
   module procedure sort_int
   module procedure sort_char
end interface sort

interface sort_list
   module procedure sort_list_real
   module procedure sort_list_int
   module procedure sort_list_char
end interface sort_list

interface solve_real
   module procedure solve_real_vec
   module procedure solve_real_vec_i_r
   module procedure solve_real_vec_i_i
   module procedure solve_real_mat
   module procedure solve_real_mat_r_i
   module procedure solve_real_mat_i_r
   module procedure solve_real_mat_i_i
   module procedure solve_real_vec_r_c
   module procedure solve_real_vec_i_c
   module procedure solve_complex_vec
   module procedure solve_complex_mat
end interface solve_real

interface besselJ
   module procedure besselJ_scalar_i
   module procedure besselJ_scalar_r
   module procedure besselJ_vec_i
   module procedure besselJ_vec_r
end interface besselJ

interface besselY
   module procedure besselY_scalar_i
   module procedure besselY_scalar_r
   module procedure besselY_vec_i
   module procedure besselY_vec_r
end interface besselY

interface besselI
   module procedure besselI_scalar_i
   module procedure besselI_scalar_r
   module procedure besselI_vec_i
   module procedure besselI_vec_r
end interface besselI

interface besselK
   module procedure besselK_scalar_i
   module procedure besselK_scalar_r
   module procedure besselK_vec_i
   module procedure besselK_vec_r
end interface besselK

interface chol
   module procedure chol_real
   module procedure chol_int
end interface chol

interface chol2inv
   module procedure chol2inv_real
   module procedure chol2inv_int
end interface chol2inv

interface forwardsolve
   module procedure forwardsolve_vec
   module procedure forwardsolve_vec_i_r
   module procedure forwardsolve_vec_i_i
   module procedure forwardsolve_mat
   module procedure forwardsolve_mat_r_i
   module procedure forwardsolve_mat_i_r
   module procedure forwardsolve_mat_i_i
end interface forwardsolve

interface backsolve
   module procedure backsolve_vec
   module procedure backsolve_vec_i_r
   module procedure backsolve_vec_i_i
   module procedure backsolve_mat
   module procedure backsolve_mat_r_i
   module procedure backsolve_mat_i_r
   module procedure backsolve_mat_i_i
end interface backsolve

interface qr_coef
   module procedure qr_coef_vec
   module procedure qr_coef_mat
end interface qr_coef

interface qr_fitted
   module procedure qr_fitted_vec
   module procedure qr_fitted_mat
end interface qr_fitted

interface qr_resid
   module procedure qr_resid_vec
   module procedure qr_resid_mat
end interface qr_resid

interface qr_qty
   module procedure qr_qty_vec
   module procedure qr_qty_mat
end interface qr_qty

interface qr_qy
   module procedure qr_qy_vec
   module procedure qr_qy_mat
end interface qr_qy

interface hclust
   module procedure hclust_complete
end interface hclust

interface cutree
   module procedure cutree_f90
end interface cutree

interface dist
   module procedure dist_mat
end interface dist

interface arima_sim
   module procedure arima_sim_scalar
   module procedure arima_sim_vector
end interface arima_sim

interface acf
   module procedure acf_vec
   module procedure acf_mat
end interface acf

interface r_acf
   module procedure acf_vec
   module procedure acf_mat
end interface r_acf

interface r_acf_values
   module procedure acf_values_vec
   module procedure acf_values_mat
end interface r_acf_values

interface r_ccf
   module procedure ccf_vec
end interface r_ccf

interface is_na
   module procedure is_na_real_scalar
   module procedure is_na_real_vec
   module procedure is_na_int_scalar
   module procedure is_na_int_vec
   module procedure is_na_char_scalar
   module procedure is_na_char_vec
end interface is_na

interface which
   module procedure which_logical
end interface which

interface replace
   module procedure replace_real_idx_scalar
   module procedure replace_real_idx_vec
   module procedure replace_real_mask_scalar
   module procedure replace_real_mask_vec
   module procedure replace_int_idx_scalar
   module procedure replace_int_idx_vec
   module procedure replace_int_idx_scalar_real
   module procedure replace_int_idx_vec_real
   module procedure replace_int_mask_scalar
   module procedure replace_int_mask_vec
   module procedure replace_int_mask_scalar_real
   module procedure replace_int_mask_vec_real
   module procedure replace_char_mask_scalar
   module procedure replace_char_mask_vec
   module procedure replace_logical_mask_scalar
end interface replace

interface rle
   module procedure rle_real
   module procedure rle_int
   module procedure rle_char
   module procedure rle_logical
end interface rle

interface inverse_rle
   module procedure inverse_rle_real
   module procedure inverse_rle_int
   module procedure inverse_rle_char
   module procedure inverse_rle_logical
end interface inverse_rle

interface print_rle
   module procedure print_rle_real
   module procedure print_rle_int
   module procedure print_rle_char
   module procedure print_rle_logical
end interface print_rle

interface r_typeof
   module procedure r_typeof_real_scalar
   module procedure r_typeof_real_vec
   module procedure r_typeof_real_mat
   module procedure r_typeof_int_scalar
   module procedure r_typeof_int_vec
   module procedure r_typeof_int_mat
   module procedure r_typeof_char_scalar
   module procedure r_typeof_char_vec
   module procedure r_typeof_char_mat
   module procedure r_typeof_logical_scalar
   module procedure r_typeof_logical_vec
   module procedure r_typeof_logical_mat
end interface r_typeof

contains

function r_character(n) result(out)
! Allocate an R-like character vector initialized to empty strings.
integer, intent(in) :: n
character(len=:), allocatable :: out(:)
allocate(character(len=0) :: out(max(0, n)))
end function r_character

pure function r_drop_index_real(x, k) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: out(:)
logical, allocatable :: keep(:)
integer :: n, m
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(keep(n))
keep = .true.
if (k >= 1 .and. k <= n) keep(k) = .false.
m = count(keep)
allocate(out(m))
if (m > 0) out = pack(x, keep)
end function r_drop_index_real

pure function r_drop_index_int(x, k) result(out)
integer, intent(in) :: x(:)
integer, intent(in) :: k
integer, allocatable :: out(:)
logical, allocatable :: keep(:)
integer :: n, m
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(keep(n))
keep = .true.
if (k >= 1 .and. k <= n) keep(k) = .false.
m = count(keep)
allocate(out(m))
if (m > 0) out = pack(x, keep)
end function r_drop_index_int

pure function r_drop_indices_real(x, drop) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: drop(:)
real(kind=dp), allocatable :: out(:)
logical, allocatable :: keep(:)
integer :: i, n, m
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(keep(n))
keep = .true.
do i = 1, size(drop)
   if (drop(i) >= 1 .and. drop(i) <= n) keep(drop(i)) = .false.
end do
m = count(keep)
allocate(out(m))
if (m > 0) out = pack(x, keep)
end function r_drop_indices_real

pure function r_drop_indices_int(x, drop) result(out)
integer, intent(in) :: x(:)
integer, intent(in) :: drop(:)
integer, allocatable :: out(:)
logical, allocatable :: keep(:)
integer :: i, n, m
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(keep(n))
keep = .true.
do i = 1, size(drop)
   if (drop(i) >= 1 .and. drop(i) <= n) keep(drop(i)) = .false.
end do
m = count(keep)
allocate(out(m))
if (m > 0) out = pack(x, keep)
end function r_drop_indices_int


subroutine set_print_int_like(flag)
! Enable/disable integer-like rendering for real matrix printing.
logical, intent(in) :: flag
print_int_like_default = flag
end subroutine set_print_int_like

subroutine set_print_int_like_tol(tol)
! Set tolerance used for integer-like real rendering.
real(kind=dp), intent(in) :: tol
if (tol > 0.0_dp) print_int_like_tol = tol
end subroutine set_print_int_like_tol

subroutine set_recycle_warn(flag)
! Enable/disable warnings for non-multiple recycling lengths.
logical, intent(in) :: flag
recycle_warn_default = flag
end subroutine set_recycle_warn

subroutine set_recycle_stop(flag)
! Enable/disable error stop for non-multiple recycling lengths.
logical, intent(in) :: flag
recycle_stop_default = flag
end subroutine set_recycle_stop

subroutine set_seed_int(seed)
! Set Fortran RNG seed deterministically from a single integer.
integer, intent(in) :: seed
#ifdef XR2F_USE_R_RNG
call xr2f_r_set_seed(int(seed, kind=c_int))
#else
integer :: n, i
integer, allocatable :: put(:)
integer(kind=int64) :: s, m
call random_seed(size=n)
allocate(put(n))
s = int(abs(seed), kind=int64)
if (s == 0_int64) s = 104729_int64
m = int(huge(0), kind=int64) - 1_int64
do i = 1, n
   put(i) = int(modulo(s + 104729_int64 * int(i, kind=int64), m) + 1_int64)
end do
call random_seed(put=put)
deallocate(put)
#endif
end subroutine set_seed_int

function kmeans_vec(x, centers, nstart) result(out)
! Minimal 1D k-means helper: returns centers and 1-based cluster ids.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: centers
integer, intent(in), optional :: nstart
type(kmeans_result_t) :: out
real(kind=dp), allocatable :: c(:), c_new(:), sums(:), best_withinss(:), withinss(:), best_centers(:)
integer, allocatable :: cnt(:), cl(:), cl_best(:), best_size(:)
integer, allocatable :: size_tot(:)
integer, allocatable :: order_idx(:), remap(:)
integer :: i, j, k, n, it, jbest, nstart_loc, start, idx
integer :: t
real(kind=dp) :: xmin, xmax, scale, d, dbest, u, best_score, score
n = size(x)
k = max(1, centers)
nstart_loc = 1
if (present(nstart)) nstart_loc = max(1, nstart)
allocate(c(k), c_new(k), sums(k), cnt(k), cl(n), cl_best(n))
allocate(withinss(k), best_withinss(k), best_centers(k), best_size(k), size_tot(k))
if (n <= 0) then
   c = 0.0_dp
   sums = 0.0_dp
   cnt = 0
   withinss = 0.0_dp
   out%centers = reshape(c, [k, 1])
   out%cluster = cl
   out%size = cnt
   out%withinss = withinss
   return
end if
xmin = minval(x)
xmax = maxval(x)
best_score = huge(1.0_dp)
best_withinss = 0.0_dp
best_size = 0
cl_best = 1
best_centers = 0.0_dp
if (k > 0) cl_best(1) = 1
do start = 1, nstart_loc
   if (k == 1) then
      if (nstart_loc > 1 .and. start > 1) then
   u = runif1()
         idx = 1 + int(real(n, kind=dp) * u)
         if (idx > n) idx = n
         c(1) = x(idx)
      else
         c(1) = sum(x) / real(n, kind=dp)
      end if
   else
      if (nstart_loc > 1 .and. start > 1) then
         do j = 1, k
   u = runif1()
            idx = 1 + int(real(n, kind=dp) * u)
            if (idx > n) idx = n
            c(j) = x(idx)
         end do
      else
         scale = (xmax - xmin) / real(k - 1, kind=dp)
         do j = 1, k
            c(j) = xmin + real(j - 1, kind=dp) * scale
         end do
      end if
   end if
   do it = 1, 50
      do i = 1, n
         jbest = 1
         dbest = abs(x(i) - c(1))
         do j = 2, k
            d = abs(x(i) - c(j))
            if (d < dbest) then
               dbest = d
               jbest = j
            end if
         end do
         cl(i) = jbest
      end do
      sums = 0.0_dp
      cnt = 0
      do i = 1, n
         j = cl(i)
         sums(j) = sums(j) + x(i)
         cnt(j) = cnt(j) + 1
      end do
      c_new = c
      do j = 1, k
         if (cnt(j) > 0) c_new(j) = sums(j) / real(cnt(j), kind=dp)
      end do
      if (maxval(abs(c_new - c)) <= 1.0e-12_dp * max(1.0_dp, maxval(abs(c)))) exit
      c = c_new
   end do

   size_tot = 0
   withinss = 0.0_dp
   do i = 1, n
      j = cl(i)
      size_tot(j) = size_tot(j) + 1
      d = x(i) - c(j)
      withinss(j) = withinss(j) + d * d
   end do
   score = sum(withinss)
   if (score < best_score) then
      best_score = score
      best_centers = c
      cl_best = cl
      best_withinss = withinss
      best_size = size_tot
   end if
end do
out%centers = reshape(best_centers, [k, 1])
out%cluster = cl_best
out%size = best_size
out%withinss = best_withinss

allocate(order_idx(k), remap(k))
if (k > 1) then
   do i = 1, k
      order_idx(i) = i
   end do
   do i = 1, k - 1
      do j = i + 1, k
         if (best_size(j) > best_size(order_idx(i))) then
            t = order_idx(i)
            order_idx(i) = order_idx(j)
            order_idx(j) = t
         end if
      end do
   end do
end if
if (all(best_size == 0)) order_idx = [(i, i=1,k)]

withinss = best_withinss
sums = best_centers
cnt = best_size
do i = 1, k
   best_size(i) = cnt(order_idx(i))
   best_withinss(i) = withinss(order_idx(i))
   best_centers(i) = sums(order_idx(i))
   remap(order_idx(i)) = i
end do

do i = 1, n
   cl_best(i) = remap(cl_best(i))
end do

out%centers = reshape(best_centers, [k, 1])
out%cluster = cl_best
out%size = best_size
out%withinss = best_withinss
end function kmeans_vec

function kmeans_mat(x, centers, nstart) result(out)
! Minimal row-wise k-means helper for matrix observations.
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in) :: centers
integer, intent(in), optional :: nstart
type(kmeans_result_t) :: out
real(kind=dp), allocatable :: c(:,:), c_new(:,:), sums(:,:), best_centers(:,:), best_withinss(:), withinss(:)
integer, allocatable :: cnt(:), cl(:), cl_best(:), best_size(:), size_tot(:)
integer, allocatable :: order_idx(:), remap(:), ci(:)
integer :: i, j, k, n, p, it, jbest, nstart_loc, start
integer :: idx
integer :: t
real(kind=dp) :: d, dbest, shift, u, best_score, score
n = size(x, 1)
p = size(x, 2)
k = max(1, centers)
nstart_loc = 1
if (present(nstart)) nstart_loc = max(1, nstart)
allocate(c(k, p), c_new(k, p), sums(k, p), best_centers(k, p), withinss(k), best_withinss(k))
allocate(cnt(k), size_tot(k), cl(n), cl_best(n), best_size(k), out%size(k), out%withinss(k))
if (n <= 0 .or. p <= 0) then
   out%centers = 0.0_dp
   out%cluster = cl
   out%size = 0
   out%withinss = 0.0_dp
   return
end if
best_score = huge(1.0_dp)
best_withinss = 0.0_dp
best_size = 0
best_centers = 0.0_dp
cl_best = 1
do start = 1, nstart_loc
   if (k > 1) then
      if (nstart_loc > 1 .and. start > 1) then
         do j = 1, k
   u = runif1()
            idx = 1 + int(real(n, kind=dp) * u)
            if (idx > n) idx = n
            c(j, :) = x(idx, :)
         end do
      else
         do j = 1, k
            idx = 1 + int(real(j - 1, kind=dp) * real(max(1, n - 1), kind=dp) / real(max(1, k - 1), kind=dp))
            c(j, :) = x(idx, :)
         end do
      end if
   else
      c(1, :) = sum(x, dim=1) / real(n, kind=dp)
   end if

   do it = 1, 50
      do i = 1, n
         jbest = 1
         dbest = sum((x(i, :) - c(1, :))**2)
         do j = 2, k
            d = sum((x(i, :) - c(j, :))**2)
            if (d < dbest) then
               dbest = d
               jbest = j
            end if
         end do
         cl(i) = jbest
      end do
      sums = 0.0_dp
      cnt = 0
      do i = 1, n
         j = cl(i)
         sums(j, :) = sums(j, :) + x(i, :)
         cnt(j) = cnt(j) + 1
      end do
      c_new = c
      do j = 1, k
         if (cnt(j) > 0) c_new(j, :) = sums(j, :) / real(cnt(j), kind=dp)
      end do
      shift = maxval(abs(c_new - c))
      if (shift <= 1.0e-12_dp * max(1.0_dp, maxval(abs(c)))) exit
      c = c_new
   end do

   size_tot = 0
   withinss = 0.0_dp
   do i = 1, n
      j = cl(i)
      size_tot(j) = size_tot(j) + 1
      d = sum((x(i, :) - c(j, :))**2)
      withinss(j) = withinss(j) + d
   end do
   score = sum(withinss)
   if (score < best_score) then
      best_score = score
      best_centers = c
      cl_best = cl
      best_size = size_tot
      best_withinss = withinss
   end if
end do
out%centers = best_centers
out%cluster = cl_best
out%size = best_size
out%withinss = best_withinss

allocate(order_idx(k), remap(k), ci(n))
if (k > 1) then
   do i = 1, k
      order_idx(i) = i
   end do
   do i = 1, k - 1
      do j = i + 1, k
         if (best_size(j) > best_size(order_idx(i))) then
            t = order_idx(i)
            order_idx(i) = order_idx(j)
            order_idx(j) = t
         end if
      end do
   end do
end if
if (all(best_size == 0)) order_idx = [(i, i=1,k)]

c_new = best_centers
cnt = best_size
withinss = best_withinss
do i = 1, k
   best_size(i) = cnt(order_idx(i))
   best_withinss(i) = withinss(order_idx(i))
   best_centers(i, :) = c_new(order_idx(i), :)
   remap(order_idx(i)) = i
end do
do i = 1, n
   ci(i) = remap(cl_best(i))
end do
cl_best = ci

out%centers = best_centers
out%cluster = cl_best
out%size = best_size
out%withinss = best_withinss
end function kmeans_mat

pure function dist_mat(x, method) result(out)
! Compute pairwise distance matrix for observations in rows.
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in), optional :: method
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, k, n, p
real(kind=dp) :: acc
character(len=32) :: meth
n = size(x, 1)
p = size(x, 2)
allocate(out(max(0, n), max(0, n)))
if (n <= 0) return
if (present(method)) then
   meth = trim(adjustl(method))
else
   meth = "euclidean"
end if
out = 0.0_dp
do i = 1, n - 1
   out(i, i) = 0.0_dp
   do j = i + 1, n
      acc = 0.0_dp
      if (meth == "manhattan") then
         do k = 1, p
            acc = acc + abs(x(i, k) - x(j, k))
         end do
      else if (meth == "maximum") then
         acc = abs(x(i, 1) - x(j, 1))
         do k = 2, p
            acc = max(acc, abs(x(i, k) - x(j, k)))
         end do
      else if (meth == "canberra") then
         do k = 1, p
            if (abs(x(i, k)) + abs(x(j, k)) /= 0.0_dp) then
               acc = acc + abs(x(i, k) - x(j, k)) / (abs(x(i, k)) + abs(x(j, k)))
            else
               acc = acc + 0.0_dp
            end if
         end do
      else
         do k = 1, p
            acc = acc + (x(i, k) - x(j, k))**2
         end do
         acc = sqrt(acc)
      end if
      out(i, j) = acc
      out(j, i) = acc
   end do
end do
if (n >= 1) out(n, n) = 0.0_dp
end function dist_mat

function hclust_complete(d, method) result(out)
! Minimal hierarchical clustering helper using complete linkage on a distance matrix.
real(kind=dp), intent(in) :: d(:,:)
character(len=*), intent(in), optional :: method
type(hclust_result_t) :: out
logical, allocatable :: alive(:)
real(kind=dp), allocatable :: cdist(:,:)
integer, allocatable :: cluster_rep(:)
integer :: n, step, i, j, k
integer :: a, b, node_count, root_a, root_b, new_node
real(kind=dp) :: best_d, cand_d
integer :: best_a, best_b
character(len=16) :: meth
n = size(d, 1)
if (n <= 1) then
   allocate(out%merge(0, 2))
   allocate(out%height(0))
   allocate(out%order(max(0, n)))
   allocate(out%labels(max(0, n)))
   if (n == 1) then
      out%labels = [1]
      out%order = [1]
   end if
   out%method = 1
   return
end if
if (size(d, 2) /= n) then
   allocate(out%merge(0, 2))
   allocate(out%height(0))
   allocate(out%order(0))
   allocate(out%labels(0))
   out%method = 1
   return
end if
if (present(method)) then
   meth = trim(adjustl(method))
else
   meth = "complete"
end if
if (meth /= "complete") meth = "complete"
allocate(out%merge(max(0, n - 1), 2))
allocate(out%height(max(0, n - 1)))
allocate(out%order(n))
allocate(out%labels(n))
out%labels = [(i, i = 1, n)]
out%order = [(i, i = 1, n)]
out%method = 1
allocate(alive(2 * n - 1))
allocate(cdist(2 * n - 1, 2 * n - 1))
allocate(cluster_rep(max(0, n - 1)))
cdist = 0.0_dp
alive = .false.
cdist(1:n, 1:n) = d
alive(1:n) = .true.
do step = 1, n - 1
   best_d = huge(1.0_dp)
   best_a = -1
   best_b = -1
   node_count = n + step - 1
   do i = 1, node_count
      if (.not. alive(i)) cycle
      do j = i + 1, node_count
         if (.not. alive(j)) cycle
         if (cdist(i, j) < best_d) then
            best_d = cdist(i, j)
            best_a = i
            best_b = j
         end if
      end do
   end do
   if (best_a <= 0 .or. best_b <= 0) exit
   if (best_a <= n) then
      out%merge(step, 1) = -best_a
   else
      out%merge(step, 1) = best_a - n
   end if
   if (best_b <= n) then
      out%merge(step, 2) = -best_b
   else
      out%merge(step, 2) = best_b - n
   end if
   out%height(step) = best_d
   new_node = n + step
   alive(best_a) = .false.
   alive(best_b) = .false.
   alive(new_node) = .true.
   cluster_rep(step) = new_node
   do k = 1, node_count
      if (.not. alive(k) .or. k == new_node) cycle
      if (meth == "complete") then
         cand_d = max(cdist(best_a, k), cdist(best_b, k))
      else
         cand_d = max(cdist(best_a, k), cdist(best_b, k))
      end if
      cdist(new_node, k) = cand_d
      cdist(k, new_node) = cand_d
   end do
   cdist(new_node, new_node) = 0.0_dp
   cdist(:, best_a) = 0.0_dp
   cdist(best_a, :) = 0.0_dp
   cdist(:, best_b) = 0.0_dp
   cdist(best_b, :) = 0.0_dp
end do
end function hclust_complete

function cutree_f90(fit, k) result(group)
! Cut dendrogram at a target number of groups.
type(hclust_result_t), intent(in) :: fit
integer, intent(in), optional :: k
integer, allocatable :: group(:)
integer, allocatable :: rep_map(:), parent(:), merge_rep(:)
integer :: n, nmerge, target_groups, nmerge_apply
integer :: i, a, b, root_a, root_b, ra, rb, r, next_group
nmerge = size(fit%merge, 1)
n = max(0, nmerge) + 1
if (n > 0 .and. size(fit%labels) >= n) n = size(fit%labels)
allocate(group(max(0, n)))
if (n <= 0) then
   return
end if
if (size(fit%merge, 2) /= 2) then
   do i = 1, n
      group(i) = i
   end do
   return
end if
if (nmerge <= 0) then
   do i = 1, n
      group(i) = i
   end do
   return
end if
if (present(k)) then
   target_groups = max(1, min(k, n))
else
   target_groups = 1
end if
if (target_groups >= n) then
   do i = 1, n
      group(i) = i
   end do
   return
end if
nmerge_apply = min(nmerge, n - target_groups)
allocate(parent(n))
allocate(rep_map(n))
allocate(merge_rep(max(0, nmerge)))
do i = 1, n
   parent(i) = i
   rep_map(i) = 0
   group(i) = 0
end do
merge_rep = 0

do i = 1, nmerge_apply
   a = fit%merge(i, 1)
   b = fit%merge(i, 2)
   if (a < 0) then
      root_a = -a
   else if (a >= 1 .and. a <= nmerge) then
      root_a = merge_rep(a)
   else
      root_a = a
   end if
   if (b < 0) then
      root_b = -b
   else if (b >= 1 .and. b <= nmerge) then
      root_b = merge_rep(b)
   else
      root_b = b
   end if
   root_a = max(1, min(n, root_a))
   root_b = max(1, min(n, root_b))
   ra = root_a
   do while (parent(ra) /= ra)
      parent(ra) = parent(parent(ra))
      ra = parent(ra)
   end do
   rb = root_b
   do while (parent(rb) /= rb)
      parent(rb) = parent(parent(rb))
      rb = parent(rb)
   end do
   if (ra /= rb) parent(rb) = ra
   if (i <= nmerge) merge_rep(i) = ra
end do
next_group = 0
do i = 1, n
   r = i
   do while (parent(r) /= r)
      parent(r) = parent(parent(r))
      r = parent(r)
   end do
   if (rep_map(r) == 0) then
      next_group = next_group + 1
      rep_map(r) = next_group
   end if
   group(i) = rep_map(r)
end do
end function cutree_f90

pure function max_col(x, ties_method) result(idx)
! Return 1-based column index of row-wise maxima (ties -> first).
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in), optional :: ties_method
integer, allocatable :: idx(:)
integer :: i, j, n, k, jbest
real(kind=dp) :: vbest
n = size(x, 1)
k = size(x, 2)
allocate(idx(n))
if (present(ties_method)) then
   ! Only "first" tie handling is supported in this subset.
end if
if (k <= 0) then
   idx = 1
   return
end if
do i = 1, n
   jbest = 1
   vbest = x(i, 1)
   do j = 2, k
      if (x(i, j) > vbest) then
         vbest = x(i, j)
         jbest = j
      end if
   end do
   idx(i) = jbest
end do
end function max_col

function tabulate_int(x, nbins) result(out)
! Count occurrences of integer labels 1..nbins.
integer, intent(in) :: x(:)
integer, intent(in) :: nbins
integer, allocatable :: out(:)
integer :: i, b
allocate(out(max(0, nbins)))
if (size(out) > 0) out = 0
do i = 1, size(x)
   b = x(i)
   if (b >= 1 .and. b <= size(out)) out(b) = out(b) + 1
end do
end function tabulate_int

function tabulate_real(x, nbins) result(out)
! Count occurrences after integer-coding real labels.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nbins
integer, allocatable :: out(:)
integer :: i, b
allocate(out(max(0, nbins)))
if (size(out) > 0) out = 0
do i = 1, size(x)
   b = int(nint(x(i)))
   if (b >= 1 .and. b <= size(out)) out(b) = out(b) + 1
end do
end function tabulate_real

function table2_int(x, y, nx, ny) result(out)
! Count paired integer labels into an nx-by-ny contingency table.
integer, intent(in) :: x(:), y(:)
integer, intent(in) :: nx, ny
integer, allocatable :: out(:,:)
integer :: i, a, b, n
allocate(out(max(0, nx), max(0, ny)))
if (size(out) > 0) out = 0
n = min(size(x), size(y))
do i = 1, n
   a = x(i)
   b = y(i)
   if (a >= 1 .and. a <= size(out, 1) .and. b >= 1 .and. b <= size(out, 2)) then
      out(a, b) = out(a, b) + 1
   end if
end do
end function table2_int

function prop_table_int_vec(x, margin) result(out)
! Convert integer counts to proportions.
integer, intent(in) :: x(:)
integer, intent(in), optional :: margin
real(kind=dp), allocatable :: out(:)
integer :: s
allocate(out(size(x)))
s = sum(x)
if (s > 0) then
   out = real(x, kind=dp) / real(s, kind=dp)
else
   out = ieee_value(0.0_dp, ieee_quiet_nan)
end if
end function prop_table_int_vec

function prop_table_int_mat(x, margin) result(out)
! Convert integer contingency tables to overall, row, or column proportions.
integer, intent(in) :: x(:,:)
integer, intent(in), optional :: margin
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, s
allocate(out(size(x, 1), size(x, 2)))
if (present(margin)) then
   if (margin == 1) then
      do i = 1, size(x, 1)
         s = sum(x(i, :))
         if (s > 0) then
            out(i, :) = real(x(i, :), kind=dp) / real(s, kind=dp)
         else
            out(i, :) = ieee_value(0.0_dp, ieee_quiet_nan)
         end if
      end do
      return
   else if (margin == 2) then
      do j = 1, size(x, 2)
         s = sum(x(:, j))
         if (s > 0) then
            out(:, j) = real(x(:, j), kind=dp) / real(s, kind=dp)
         else
            out(:, j) = ieee_value(0.0_dp, ieee_quiet_nan)
         end if
      end do
      return
   end if
end if
s = sum(x)
if (s > 0) then
   out = real(x, kind=dp) / real(s, kind=dp)
else
   out = ieee_value(0.0_dp, ieee_quiet_nan)
end if
end function prop_table_int_mat

pure function nested_matrix_list_len(x) result(n)
! Count non-padding matrix slices in a ragged nested list lowered to rank 3.
real(kind=dp), intent(in) :: x(:,:,:)
integer :: n
integer :: j
n = 0
do j = 1, size(x, 3)
   if (any(ieee_is_finite(x(:,:,j)))) n = j
end do
end function nested_matrix_list_len

pure function match_int(x, table) result(out)
! Return first 1-based match positions, or a sentinel for NA.
integer, intent(in) :: x(:), table(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
out = -huge(0)
do i = 1, size(x)
   do j = 1, size(table)
      if (x(i) == table(j)) then
         out(i) = j
         exit
      end if
   end do
end do
end function match_int

pure function match_real(x, table) result(out)
! Return first 1-based match positions, or a sentinel for NA.
real(kind=dp), intent(in) :: x(:), table(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
out = -huge(0)
do i = 1, size(x)
   do j = 1, size(table)
      if (x(i) == table(j)) then
         out(i) = j
         exit
      end if
   end do
end do
end function match_real

pure function match_char(x, table) result(out)
! Return first 1-based match positions, or a sentinel for NA.
character(len=*), intent(in) :: x(:), table(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
out = -huge(0)
do i = 1, size(x)
   do j = 1, size(table)
      if (trim(x(i)) == trim(table(j))) then
         out(i) = j
         exit
      end if
   end do
end do
end function match_char

pure function r_in_int(x, table) result(out)
integer, intent(in) :: x(:), table(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = has_int_value(table, size(table), x(i))
end do
end function r_in_int

pure function r_in_real(x, table) result(out)
real(kind=dp), intent(in) :: x(:), table(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = has_real_value(table, size(table), x(i))
end do
end function r_in_real

pure function r_in_int_real(x, table) result(out)
integer, intent(in) :: x(:)
real(kind=dp), intent(in) :: table(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = has_real_value(table, size(table), real(x(i), kind=dp))
end do
end function r_in_int_real

pure function r_in_real_int(x, table) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: table(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = has_int_value(table, size(table), nint(x(i))) .and. x(i) == real(nint(x(i)), kind=dp)
end do
end function r_in_real_int

pure function r_in_int_scalar(x, table) result(out)
integer, intent(in) :: x
integer, intent(in) :: table(:)
logical :: out
out = has_int_value(table, size(table), x)
end function r_in_int_scalar

pure function r_in_real_scalar(x, table) result(out)
real(kind=dp), intent(in) :: x
real(kind=dp), intent(in) :: table(:)
logical :: out
out = has_real_value(table, size(table), x)
end function r_in_real_scalar

pure function r_in_int_scalar_real(x, table) result(out)
integer, intent(in) :: x
real(kind=dp), intent(in) :: table(:)
logical :: out
out = has_real_value(table, size(table), real(x, kind=dp))
end function r_in_int_scalar_real

pure function r_in_real_scalar_int(x, table) result(out)
real(kind=dp), intent(in) :: x
integer, intent(in) :: table(:)
logical :: out
out = has_int_value(table, size(table), nint(x)) .and. x == real(nint(x), kind=dp)
end function r_in_real_scalar_int

pure function r_in_char(x, table) result(out)
character(len=*), intent(in) :: x(:), table(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = has_char_value(table, size(table), x(i))
end do
end function r_in_char

pure function r_in_logical(x, table) result(out)
logical, intent(in) :: x(:), table(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = has_logical_value(table, size(table), x(i))
end do
end function r_in_logical

pure logical function has_int_value(x, n, value) result(out)
integer, intent(in) :: x(:), n, value
integer :: i
out = .false.
do i = 1, min(n, size(x))
   if (x(i) == value) then
      out = .true.
      return
   end if
end do
end function has_int_value

pure logical function has_real_value(x, n, value) result(out)
real(kind=dp), intent(in) :: x(:), value
integer, intent(in) :: n
integer :: i
out = .false.
do i = 1, min(n, size(x))
   if (x(i) == value) then
      out = .true.
      return
   end if
end do
end function has_real_value

pure logical function has_char_value(x, n, value) result(out)
character(len=*), intent(in) :: x(:), value
integer, intent(in) :: n
integer :: i
out = .false.
do i = 1, min(n, size(x))
   if (x(i) == value) then
      out = .true.
      return
   end if
end do
end function has_char_value

pure logical function has_logical_value(x, n, value) result(out)
logical, intent(in) :: x(:), value
integer, intent(in) :: n
integer :: i
out = .false.
do i = 1, min(n, size(x))
   if (x(i) .eqv. value) then
      out = .true.
      return
   end if
end do
end function has_logical_value

pure function unique_int(x) result(out)
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer, allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x)))
n = 0
do i = 1, size(x)
   if (.not. has_int_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function unique_int

pure function unique_real(x) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: tmp(:)
logical :: have_nonfinite
integer :: i, n
allocate(tmp(size(x)))
n = 0
have_nonfinite = .false.
do i = 1, size(x)
   if (.not. ieee_is_finite(x(i))) then
      if (.not. have_nonfinite) then
         n = n + 1
         tmp(n) = x(i)
         have_nonfinite = .true.
      end if
   else if (.not. has_real_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function unique_real

pure function unique_char(x) result(out)
character(len=*), intent(in) :: x(:)
character(len=:), allocatable :: out(:)
character(len=:), allocatable :: tmp(:)
integer :: i, n, lch
lch = max(1, len(x))
allocate(character(len=lch) :: tmp(size(x)))
n = 0
do i = 1, size(x)
   if (.not. has_char_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(character(len=lch) :: out(n))
if (n > 0) out = tmp(1:n)
end function unique_char

pure function unique_logical(x) result(out)
logical, intent(in) :: x(:)
logical, allocatable :: out(:)
logical, allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x)))
n = 0
do i = 1, size(x)
   if (.not. has_logical_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function unique_logical

pure function duplicated_int(x, fromLast) result(out)
integer, intent(in) :: x(:)
logical, intent(in), optional :: fromLast
logical, allocatable :: out(:)
integer, allocatable :: seen(:)
integer :: i, n
logical :: rev
allocate(out(size(x)), seen(size(x)))
out = .false.
n = 0
rev = .false.
if (present(fromLast)) rev = fromLast
if (rev) then
   do i = size(x), 1, -1
      out(i) = has_int_value(seen, n, x(i))
      if (.not. out(i)) then
         n = n + 1
         seen(n) = x(i)
      end if
   end do
else
   do i = 1, size(x)
      out(i) = has_int_value(seen, n, x(i))
      if (.not. out(i)) then
         n = n + 1
         seen(n) = x(i)
      end if
   end do
end if
end function duplicated_int

pure function duplicated_real(x, fromLast) result(out)
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: fromLast
logical, allocatable :: out(:)
real(kind=dp), allocatable :: seen(:)
integer :: i, n
logical :: rev, have_nonfinite
allocate(out(size(x)), seen(size(x)))
out = .false.
n = 0
have_nonfinite = .false.
rev = .false.
if (present(fromLast)) rev = fromLast
if (rev) then
   do i = size(x), 1, -1
      if (.not. ieee_is_finite(x(i))) then
         out(i) = have_nonfinite
         if (.not. have_nonfinite) then
            n = n + 1
            seen(n) = x(i)
            have_nonfinite = .true.
         end if
      else
         out(i) = has_real_value(seen, n, x(i))
         if (.not. out(i)) then
            n = n + 1
            seen(n) = x(i)
         end if
      end if
   end do
else
   do i = 1, size(x)
      if (.not. ieee_is_finite(x(i))) then
         out(i) = have_nonfinite
         if (.not. have_nonfinite) then
            n = n + 1
            seen(n) = x(i)
            have_nonfinite = .true.
         end if
      else
         out(i) = has_real_value(seen, n, x(i))
         if (.not. out(i)) then
            n = n + 1
            seen(n) = x(i)
         end if
      end if
   end do
end if
end function duplicated_real

pure function duplicated_char(x, fromLast) result(out)
character(len=*), intent(in) :: x(:)
logical, intent(in), optional :: fromLast
logical, allocatable :: out(:)
character(len=:), allocatable :: seen(:)
integer :: i, n, lch
logical :: rev
lch = max(1, len(x))
allocate(out(size(x)))
allocate(character(len=lch) :: seen(size(x)))
out = .false.
n = 0
rev = .false.
if (present(fromLast)) rev = fromLast
if (rev) then
   do i = size(x), 1, -1
      out(i) = has_char_value(seen, n, x(i))
      if (.not. out(i)) then
         n = n + 1
         seen(n) = x(i)
      end if
   end do
else
   do i = 1, size(x)
      out(i) = has_char_value(seen, n, x(i))
      if (.not. out(i)) then
         n = n + 1
         seen(n) = x(i)
      end if
   end do
end if
end function duplicated_char

pure function duplicated_logical(x, fromLast) result(out)
logical, intent(in) :: x(:)
logical, intent(in), optional :: fromLast
logical, allocatable :: out(:)
logical, allocatable :: seen(:)
integer :: i, n
logical :: rev
allocate(out(size(x)), seen(size(x)))
out = .false.
n = 0
rev = .false.
if (present(fromLast)) rev = fromLast
if (rev) then
   do i = size(x), 1, -1
      out(i) = has_logical_value(seen, n, x(i))
      if (.not. out(i)) then
         n = n + 1
         seen(n) = x(i)
      end if
   end do
else
   do i = 1, size(x)
      out(i) = has_logical_value(seen, n, x(i))
      if (.not. out(i)) then
         n = n + 1
         seen(n) = x(i)
      end if
   end do
end if
end function duplicated_logical

pure function anyDuplicated_int(x, fromLast) result(out)
integer, intent(in) :: x(:)
logical, intent(in), optional :: fromLast
integer :: out, i
logical, allocatable :: dup(:)
dup = duplicated_int(x, fromLast)
out = 0
do i = 1, size(dup)
   if (dup(i)) then
      out = i
      return
   end if
end do
end function anyDuplicated_int

pure function anyDuplicated_real(x, fromLast) result(out)
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: fromLast
integer :: out, i
logical, allocatable :: dup(:)
dup = duplicated_real(x, fromLast)
out = 0
do i = 1, size(dup)
   if (dup(i)) then
      out = i
      return
   end if
end do
end function anyDuplicated_real

pure function anyDuplicated_char(x, fromLast) result(out)
character(len=*), intent(in) :: x(:)
logical, intent(in), optional :: fromLast
integer :: out, i
logical, allocatable :: dup(:)
dup = duplicated_char(x, fromLast)
out = 0
do i = 1, size(dup)
   if (dup(i)) then
      out = i
      return
   end if
end do
end function anyDuplicated_char

pure function anyDuplicated_logical(x, fromLast) result(out)
logical, intent(in) :: x(:)
logical, intent(in), optional :: fromLast
integer :: out, i
logical, allocatable :: dup(:)
dup = duplicated_logical(x, fromLast)
out = 0
do i = 1, size(dup)
   if (dup(i)) then
      out = i
      return
   end if
end do
end function anyDuplicated_logical

pure function union_int(x, y) result(out)
integer, intent(in) :: x(:), y(:)
integer, allocatable :: out(:)
integer, allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x) + size(y)))
n = 0
do i = 1, size(x)
   if (.not. has_int_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
do i = 1, size(y)
   if (.not. has_int_value(tmp, n, y(i))) then
      n = n + 1
      tmp(n) = y(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function union_int

pure function intersect_int(x, y) result(out)
integer, intent(in) :: x(:), y(:)
integer, allocatable :: out(:)
integer, allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x)))
n = 0
do i = 1, size(x)
   if ((.not. has_int_value(tmp, n, x(i))) .and. has_int_value(y, size(y), x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function intersect_int

pure function setdiff_int(x, y) result(out)
integer, intent(in) :: x(:), y(:)
integer, allocatable :: out(:)
integer, allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x)))
n = 0
do i = 1, size(x)
   if ((.not. has_int_value(tmp, n, x(i))) .and. (.not. has_int_value(y, size(y), x(i)))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function setdiff_int

pure function setequal_int(x, y) result(out)
integer, intent(in) :: x(:), y(:)
logical :: out
out = size(setdiff_int(x, y)) == 0 .and. size(setdiff_int(y, x)) == 0
end function setequal_int

pure function union_real(x, y) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x) + size(y)))
n = 0
do i = 1, size(x)
   if (.not. has_real_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
do i = 1, size(y)
   if (.not. has_real_value(tmp, n, y(i))) then
      n = n + 1
      tmp(n) = y(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function union_real

pure function intersect_real(x, y) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x)))
n = 0
do i = 1, size(x)
   if ((.not. has_real_value(tmp, n, x(i))) .and. has_real_value(y, size(y), x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function intersect_real

pure function setdiff_real(x, y) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: tmp(:)
integer :: i, n
allocate(tmp(size(x)))
n = 0
do i = 1, size(x)
   if ((.not. has_real_value(tmp, n, x(i))) .and. (.not. has_real_value(y, size(y), x(i)))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(out(n))
if (n > 0) out = tmp(1:n)
end function setdiff_real

pure function setequal_real(x, y) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
logical :: out
out = size(setdiff_real(x, y)) == 0 .and. size(setdiff_real(y, x)) == 0
end function setequal_real

pure function union_char(x, y) result(out)
character(len=*), intent(in) :: x(:), y(:)
character(len=:), allocatable :: out(:)
character(len=:), allocatable :: tmp(:)
integer :: i, n, lch
lch = max(1, len(x), len(y))
allocate(character(len=lch) :: tmp(size(x) + size(y)))
n = 0
do i = 1, size(x)
   if (.not. has_char_value(tmp, n, x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
do i = 1, size(y)
   if (.not. has_char_value(tmp, n, y(i))) then
      n = n + 1
      tmp(n) = y(i)
   end if
end do
allocate(character(len=lch) :: out(n))
if (n > 0) out = tmp(1:n)
end function union_char

pure function intersect_char(x, y) result(out)
character(len=*), intent(in) :: x(:), y(:)
character(len=:), allocatable :: out(:)
character(len=:), allocatable :: tmp(:)
integer :: i, n, lch
lch = max(1, len(x), len(y))
allocate(character(len=lch) :: tmp(size(x)))
n = 0
do i = 1, size(x)
   if ((.not. has_char_value(tmp, n, x(i))) .and. has_char_value(y, size(y), x(i))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(character(len=lch) :: out(n))
if (n > 0) out = tmp(1:n)
end function intersect_char

pure function setdiff_char(x, y) result(out)
character(len=*), intent(in) :: x(:), y(:)
character(len=:), allocatable :: out(:)
character(len=:), allocatable :: tmp(:)
integer :: i, n, lch
lch = max(1, len(x), len(y))
allocate(character(len=lch) :: tmp(size(x)))
n = 0
do i = 1, size(x)
   if ((.not. has_char_value(tmp, n, x(i))) .and. (.not. has_char_value(y, size(y), x(i)))) then
      n = n + 1
      tmp(n) = x(i)
   end if
end do
allocate(character(len=lch) :: out(n))
if (n > 0) out = tmp(1:n)
end function setdiff_char

pure function setequal_char(x, y) result(out)
character(len=*), intent(in) :: x(:), y(:)
logical :: out
out = size(setdiff_char(x, y)) == 0 .and. size(setdiff_char(y, x)) == 0
end function setequal_char

function r_format_vec(x, digits, sep) result(out)
! Format a real vector like paste(sprintf("%.<digits>f", x), collapse=sep).
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: digits
character(len=*), intent(in), optional :: sep
character(len=:), allocatable :: out
character(len=64) :: fmt, buf
integer :: i, d
d = max(0, min(30, digits))
write(fmt, '("(f0.", i0, ")")') d
out = ""
do i = 1, size(x)
   write(buf, fmt) x(i)
   if (i > 1) then
      if (present(sep)) then
         out = out // sep
      else
         out = out // " "
      end if
   end if
   out = out // trim(adjustl(buf))
end do
end function r_format_vec

subroutine maybe_warn_recycle(op, na, nb)
! Warn/stop when any vector recycling occurs (lengths differ).
character(len=*), intent(in) :: op
integer, intent(in) :: na, nb
integer :: nmax, nmin
if ((.not. recycle_warn_default) .and. (.not. recycle_stop_default)) return
nmax = max(na, nb)
nmin = min(na, nb)
if (nmin <= 0) return
if (nmax /= nmin) then
   if (recycle_stop_default) then
      error stop "recycle-stop: vector recycling occurred (lengths differ)"
   end if
   if (recycle_warn_default) then
      write(*,'(a)') "Warning message:"
      write(*,'(3a)') "In ", trim(op), " : vector recycling occurred (lengths differ)"
   end if
end if
end subroutine maybe_warn_recycle

subroutine print_real_scalar(x, int_like)
! Print one real value using integer format when integer-like.
real(kind=dp), intent(in) :: x
logical, intent(in), optional :: int_like
logical :: use_int_like, as_int
integer(kind=int64) :: k
real(kind=dp) :: tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
as_int = .false.
if (use_int_like) then
   if (ieee_is_finite(x) .and. abs(x) <= real(huge(0_int64), kind=dp)) then
      k = nint(x, kind=int64)
      tol = print_int_like_tol * max(1.0_dp, abs(x))
      as_int = abs(x - real(k, kind=dp)) <= tol
   end if
end if
if (as_int) then
   write(*,"(i0)") k
else
   write(*,"(g0)") x
end if
end subroutine print_real_scalar

subroutine print_real_vector(x, int_like)
! Print one real vector; use integer format when all values are integer-like.
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: int_like
logical :: use_int_like, all_int
integer :: i
integer(kind=int64) :: k
real(kind=dp) :: r, tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
all_int = .false.
if (use_int_like) then
   all_int = .true.
   do i = 1, size(x)
      r = x(i)
      if (.not. ieee_is_finite(r)) then
         all_int = .false.
         exit
      end if
      if (abs(r) > real(huge(0_int64), kind=dp)) then
         all_int = .false.
         exit
      end if
      k = nint(r, kind=int64)
      tol = print_int_like_tol * max(1.0_dp, abs(r))
      if (abs(r - real(k, kind=dp)) > tol) then
         all_int = .false.
         exit
      end if
   end do
end if
if (all_int) then
   do i = 1, size(x)
      k = nint(x(i), kind=int64)
      write(*,"(i0)", advance="no") k
      if (i < size(x)) write(*,"(a)", advance="no") " "
   end do
   write(*,*)
else
   write(*,"(*(g0,1x))") x
end if
end subroutine print_real_vector

subroutine print_char_vector(x)
character(len=*), intent(in) :: x(:)
integer :: i
do i = 1, size(x)
   write(*,"(a)", advance="no") trim(x(i))
   if (i < size(x)) write(*,"(a)", advance="no") " "
end do
write(*,*)
end subroutine print_char_vector

subroutine print_named_real_vector(x, names)
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in) :: names(:)
call print_char_vector(names)
call print_real_vector(x)
end subroutine print_named_real_vector

function nlm_stub(p, hessian) result(out)
real(kind=dp), intent(in) :: p(:)
logical, intent(in), optional :: hessian
type(nlm_result_t) :: out
integer :: n
n = size(p)
allocate(out%estimate(n))
allocate(out%gradient(n))
allocate(out%hessian(n, n))
out%estimate = p
out%gradient = 0.0_dp
out%hessian = 0.0_dp
out%minimum = 0.0_dp
out%code = 1
out%iterations = 0
end function nlm_stub

function nlm_optimize_scalar(fn, p, hessian, stepmax) result(out)
procedure(nlm_objective_scalar) :: fn
real(kind=dp), intent(in) :: p
logical, intent(in), optional :: hessian
real(kind=dp), intent(in), optional :: stepmax
type(nlm_result_t) :: out
real(kind=dp), allocatable :: pv(:)
allocate(pv(1))
pv(1) = p
out = nlm_optimize_scalar_impl(fn, pv, hessian, stepmax)
end function nlm_optimize_scalar

function nlm_optimize_vec(fn, p, hessian, stepmax) result(out)
procedure(nlm_objective_vec) :: fn
real(kind=dp), intent(in) :: p(:)
logical, intent(in), optional :: hessian
real(kind=dp), intent(in), optional :: stepmax
type(nlm_result_t) :: out
integer :: n, iter, i
real(kind=dp), allocatable :: x(:), g(:), trial(:)
real(kind=dp) :: f, f_trial, gnorm, alpha, max_step, step_norm, tol
n = size(p)
allocate(x(n), g(n), trial(n))
x = p
tol = 1.0e-7_dp
max_step = 100.0_dp
if (present(stepmax)) max_step = max(stepmax, 1.0e-12_dp)
f = fn(x)
do iter = 1, 500
   call nlm_fd_grad_vec(fn, x, g)
   gnorm = sqrt(sum(g*g))
   if (gnorm <= tol * max(1.0_dp, sqrt(sum(x*x)))) exit
   alpha = 1.0_dp
   do
      trial = x - alpha * g
      step_norm = sqrt(sum((trial - x) * (trial - x)))
      if (step_norm > max_step) trial = x + (trial - x) * (max_step / step_norm)
      f_trial = fn(trial)
      if (ieee_is_finite(f_trial) .and. f_trial <= f - 1.0e-4_dp * alpha * gnorm * gnorm) exit
      alpha = alpha * 0.5_dp
      if (alpha < 1.0e-10_dp) exit
   end do
   if (.not. ieee_is_finite(f_trial) .or. f_trial >= f) exit
   if (maxval(abs(trial - x)) <= tol * max(1.0_dp, maxval(abs(x)))) then
      x = trial
      f = f_trial
      exit
   end if
   x = trial
   f = f_trial
end do
allocate(out%estimate(n), out%gradient(n), out%hessian(n, n))
out%estimate = x
out%minimum = f
call nlm_fd_grad_vec(fn, x, out%gradient)
out%hessian = 0.0_dp
if (present(hessian)) then
   if (hessian) call nlm_fd_hessian_vec(fn, x, out%hessian)
end if
out%iterations = min(iter, 500)
out%code = merge(1, 4, sqrt(sum(out%gradient*out%gradient)) <= 1.0e-4_dp)
end function nlm_optimize_vec

function nlm_optimize_scalar_impl(fn, p, hessian, stepmax) result(out)
procedure(nlm_objective_scalar) :: fn
real(kind=dp), intent(in) :: p(:)
logical, intent(in), optional :: hessian
real(kind=dp), intent(in), optional :: stepmax
type(nlm_result_t) :: out
integer :: iter
real(kind=dp) :: x, g, f, f_trial, trial, alpha, max_step, tol, h
x = p(1)
tol = 1.0e-7_dp
max_step = 100.0_dp
if (present(stepmax)) max_step = max(stepmax, 1.0e-12_dp)
f = fn(x)
do iter = 1, 500
   g = nlm_fd_grad_scalar(fn, x)
   if (abs(g) <= tol * max(1.0_dp, abs(x))) exit
   alpha = 1.0_dp
   do
      trial = x - alpha * g
      if (abs(trial - x) > max_step) trial = x - sign(max_step, g)
      f_trial = fn(trial)
      if (ieee_is_finite(f_trial) .and. f_trial <= f - 1.0e-4_dp * alpha * g * g) exit
      alpha = alpha * 0.5_dp
      if (alpha < 1.0e-10_dp) exit
   end do
   if (.not. ieee_is_finite(f_trial) .or. f_trial >= f) exit
   if (abs(trial - x) <= tol * max(1.0_dp, abs(x))) then
      x = trial
      f = f_trial
      exit
   end if
   x = trial
   f = f_trial
end do
allocate(out%estimate(1), out%gradient(1), out%hessian(1, 1))
out%estimate(1) = x
out%minimum = f
out%gradient(1) = nlm_fd_grad_scalar(fn, x)
out%hessian = 0.0_dp
if (present(hessian)) then
   if (hessian) then
      h = sqrt(epsilon(1.0_dp)) * max(1.0_dp, abs(x))
      out%hessian(1, 1) = (fn(x + h) - 2.0_dp * fn(x) + fn(x - h)) / (h * h)
   end if
end if
out%iterations = min(iter, 500)
out%code = merge(1, 4, abs(out%gradient(1)) <= 1.0e-4_dp)
end function nlm_optimize_scalar_impl

function nlm_fd_grad_scalar(fn, x) result(g)
procedure(nlm_objective_scalar) :: fn
real(kind=dp), intent(in) :: x
real(kind=dp) :: g, h
h = sqrt(epsilon(1.0_dp)) * max(1.0_dp, abs(x))
g = (fn(x + h) - fn(x - h)) / (2.0_dp * h)
end function nlm_fd_grad_scalar

subroutine nlm_fd_grad_vec(fn, x, g)
procedure(nlm_objective_vec) :: fn
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: g(:)
real(kind=dp), allocatable :: xp(:), xm(:)
integer :: i
real(kind=dp) :: h
allocate(xp(size(x)), xm(size(x)))
do i = 1, size(x)
   xp = x
   xm = x
   h = sqrt(epsilon(1.0_dp)) * max(1.0_dp, abs(x(i)))
   xp(i) = xp(i) + h
   xm(i) = xm(i) - h
   g(i) = (fn(xp) - fn(xm)) / (2.0_dp * h)
end do
end subroutine nlm_fd_grad_vec

subroutine nlm_fd_hessian_vec(fn, x, hess)
procedure(nlm_objective_vec) :: fn
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(out) :: hess(:,:)
real(kind=dp), allocatable :: xpp(:), xpm(:), xmp(:), xmm(:)
integer :: i, j, n
real(kind=dp) :: hi, hj
n = size(x)
allocate(xpp(n), xpm(n), xmp(n), xmm(n))
do i = 1, n
   do j = 1, n
      xpp = x
      xpm = x
      xmp = x
      xmm = x
      hi = sqrt(epsilon(1.0_dp)) * max(1.0_dp, abs(x(i)))
      hj = sqrt(epsilon(1.0_dp)) * max(1.0_dp, abs(x(j)))
      xpp(i) = xpp(i) + hi; xpp(j) = xpp(j) + hj
      xpm(i) = xpm(i) + hi; xpm(j) = xpm(j) - hj
      xmp(i) = xmp(i) - hi; xmp(j) = xmp(j) + hj
      xmm(i) = xmm(i) - hi; xmm(j) = xmm(j) - hj
      hess(i,j) = (fn(xpp) - fn(xpm) - fn(xmp) + fn(xmm)) / (4.0_dp * hi * hj)
   end do
end do
end subroutine nlm_fd_hessian_vec

subroutine print_nlm_result(fit)
type(nlm_result_t), intent(in) :: fit
write(*,"(a)")
write(*,"(a)", advance="no") "$minimum"
write(*,*)
call print_real_scalar(fit%minimum)
write(*,"(a)", advance="no") "$estimate"
write(*,*)
call print_real_vector(fit%estimate)
write(*,"(a)", advance="no") "$gradient"
write(*,*)
call print_real_vector(fit%gradient)
write(*,"(a)", advance="no") "$code"
write(*,*)
write(*,"(i0)") fit%code
write(*,"(a)", advance="no") "$iterations"
write(*,*)
write(*,"(i0)") fit%iterations
end subroutine print_nlm_result

subroutine print_table1(x, names)
integer, intent(in) :: x(:)
character(len=*), intent(in) :: names(:)
integer :: i
do i = 1, min(size(x), size(names))
   write(*,'(a,1x)', advance='no') trim(names(i))
end do
write(*,*)
do i = 1, size(x)
   write(*,'(i0,1x)', advance='no') x(i)
end do
write(*,*)
end subroutine print_table1

subroutine print_table2_int(x, row_names, col_names)
integer, intent(in) :: x(:,:)
character(len=*), intent(in) :: row_names(:), col_names(:)
integer :: i, j
write(*,'(12x)', advance='no')
do j = 1, size(x, 2)
   if (j <= size(col_names)) then
      write(*,'(a12,1x)', advance='no') trim(col_names(j))
   else
      write(*,'(i12,1x)', advance='no') j
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   if (i <= size(row_names)) then
      write(*,'(a12,1x)', advance='no') trim(row_names(i))
   else
      write(*,'(i12,1x)', advance='no') i
   end if
   do j = 1, size(x, 2)
      write(*,'(i12,1x)', advance='no') x(i, j)
   end do
   write(*,*)
end do
end subroutine print_table2_int

subroutine print_table2_real(x, row_names, col_names)
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in) :: row_names(:), col_names(:)
integer :: i, j
write(*,'(12x)', advance='no')
do j = 1, size(x, 2)
   if (j <= size(col_names)) then
      write(*,'(a12,1x)', advance='no') trim(col_names(j))
   else
      write(*,'(i12,1x)', advance='no') j
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   if (i <= size(row_names)) then
      write(*,'(a12,1x)', advance='no') trim(row_names(i))
   else
      write(*,'(i12,1x)', advance='no') i
   end if
   do j = 1, size(x, 2)
      write(*,'(f12.4,1x)', advance='no') x(i, j)
   end do
   write(*,*)
end do
end subroutine print_table2_real

subroutine print_summary_vec(x)
! Print an R-like summary for a numeric vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: s(:)
s = summary_vec(x)
write(*,'(a,1x,g0)') "   Min.:", s(1)
write(*,'(a,1x,g0)') "1st Qu.:", s(2)
write(*,'(a,1x,g0)') " Median:", s(3)
write(*,'(a,1x,g0)') "   Mean:", s(4)
write(*,'(a,1x,g0)') "3rd Qu.:", s(5)
write(*,'(a,1x,g0)') "   Max.:", s(6)
end subroutine print_summary_vec

subroutine print_summary_mat(x)
! Print R-like per-column summaries for a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: s(:,:)
character(len=*), parameter :: labels(6) = [character(len=8) :: "Min.   :", "1st Qu.:", "Median :", "Mean   :", "3rd Qu.:", "Max.   :"]
integer :: j
allocate(s(6, size(x, 2)))
do j = 1, size(x, 2)
   s(:, j) = summary_vec(x(:, j))
end do
write(*,'(9x)', advance='no')
do j = 1, size(x, 2)
   write(*,'(a,i0,13x)', advance='no') "V", j
end do
write(*,*)
do j = 1, 6
   write(*,'(a,1x)', advance='no') labels(j)
   write(*,'(*(es14.6,1x))') s(j, :)
end do
end subroutine print_summary_mat

pure function r_seq_int(a, b) result(out)
! Return integer sequence a, a+step, ..., b with step +/-1.
integer, intent(in) :: a, b
integer, allocatable :: out(:)
integer :: i, n, step
n = abs(b - a) + 1
allocate(out(n))
step = merge(1, -1, a <= b)
do i = 1, n
   out(i) = a + (i - 1) * step
end do
end function r_seq_int

pure function r_seq_len(n) result(out)
! Return integer sequence 1..n (empty for n<=0).
integer, intent(in) :: n
integer, allocatable :: out(:)
integer :: i
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
do i = 1, n
   out(i) = i
end do
end function r_seq_len

pure function r_seq_int_by(a, b, by) result(out)
! Return integer sequence from a to b using explicit integer step by.
integer, intent(in) :: a, b, by
integer, allocatable :: out(:)
integer :: i, n
if (by == 0) then
   allocate(out(0))
   return
end if
if ((by > 0 .and. a > b) .or. (by < 0 .and. a < b)) then
   allocate(out(0))
   return
end if
n = (abs(b - a) / abs(by)) + 1
allocate(out(n))
do i = 1, n
   out(i) = a + (i - 1) * by
end do
end function r_seq_int_by

pure function r_seq_int_length(a, b, n) result(out)
! Return n integer values linearly spaced from a to b.
integer, intent(in) :: a, b, n
integer, allocatable :: out(:)
integer :: i
real(kind=dp) :: t
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
if (n == 1) then
   out(1) = a
   return
end if
do i = 1, n
   t = real(i - 1, kind=dp) / real(n - 1, kind=dp)
   out(i) = nint((1.0_dp - t) * real(a, kind=dp) + t * real(b, kind=dp))
end do
end function r_seq_int_length

pure function r_seq_real_by(a, b, by) result(out)
! Return real sequence from a to b with real step by.
real(kind=dp), intent(in) :: a, b, by
real(kind=dp), allocatable :: out(:)
integer :: i, n
if (by == 0.0_dp) then
   allocate(out(0))
   return
end if
if ((by > 0.0_dp .and. a > b) .or. (by < 0.0_dp .and. a < b)) then
   allocate(out(0))
   return
end if
n = int(floor((b - a) / by + 1.0e-12_dp)) + 1
if (n < 0) n = 0
allocate(out(n))
do i = 1, n
   out(i) = a + real(i - 1, kind=dp) * by
end do
end function r_seq_real_by

pure function r_seq_real_length(a, b, n) result(out)
! Return n real values linearly spaced from a to b.
real(kind=dp), intent(in) :: a, b
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: i
real(kind=dp) :: t
if (n <= 0) then
   allocate(out(0))
   return
end if
allocate(out(n))
if (n == 1) then
   out(1) = a
   return
end if
do i = 1, n
   t = real(i - 1, kind=dp) / real(n - 1, kind=dp)
   out(i) = (1.0_dp - t) * a + t * b
end do
end function r_seq_real_length

pure function r_rep_real(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of a real vector (R-like rep subset).
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(out(0))
      return
   end if
   allocate(out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_real

pure function r_rep_char(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of a character vector (R-like rep subset).
character(len=*), intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
character(len=:), allocatable :: out(:)
character(len=:), allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
integer :: item_len
n = size(x)
if (n <= 0) then
   allocate(character(len=1) :: out(0))
   return
end if
item_len = max(1, len(x(1)))
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(character(len=item_len) :: y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(character(len=item_len) :: z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(character(len=item_len) :: z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(character(len=item_len) :: out(0))
      return
   end if
   allocate(character(len=item_len) :: out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_char

pure function r_rep_int(x, times, each, len_out, times_vec) result(out)
! Repeat elements/blocks of an integer vector (R-like rep subset).
integer, intent(in) :: x(:)
integer, intent(in), optional :: times, each, len_out
integer, intent(in), optional :: times_vec(:)
integer, allocatable :: out(:)
integer, allocatable :: y(:), z(:)
integer :: i, j, n, e, t, k, m, need, c
n = size(x)
if (n <= 0) then
   allocate(out(0))
   return
end if
if (present(each)) then
   e = each
else
   e = 1
end if
if (e < 1) e = 1
allocate(y(n * e))
k = 0
do i = 1, n
   do j = 1, e
      k = k + 1
      y(k) = x(i)
   end do
end do
if (present(times_vec)) then
   m = size(y)
   c = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      if (t > 0) c = c + t
   end do
   allocate(z(c))
   k = 0
   do i = 1, m
      t = times_vec(mod(i - 1, size(times_vec)) + 1)
      do j = 1, max(0, t)
         k = k + 1
         z(k) = y(i)
      end do
   end do
else
   if (present(times)) then
      t = times
   else
      t = 1
   end if
   if (t < 0) t = 0
   allocate(z(size(y) * t))
   k = 0
   do j = 1, t
      do i = 1, size(y)
         k = k + 1
         z(k) = y(i)
      end do
   end do
end if
if (present(len_out)) then
   need = max(0, len_out)
   if (need == 0) then
      allocate(out(0))
      return
   end if
   allocate(out(need))
   if (size(z) > 0) then
      do i = 1, need
         out(i) = z(mod(i - 1, size(z)) + 1)
      end do
   end if
else
   out = z
end if
end function r_rep_int

function runif1() result(u)
! Return one U(0,1) variate.
real(kind=dp) :: u
#ifdef XR2F_USE_R_RNG
u = real(xr2f_r_unif_rand(), kind=dp)
#else
call random_number(u)
#endif
end function runif1

function runif_vec(n) result(x)
! Return a length-n vector of U(0,1) variates.
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
integer :: i
allocate(x(n))
#ifdef XR2F_USE_R_RNG
do i = 1, n
   x(i) = real(xr2f_r_unif_rand(), kind=dp)
end do
#else
call random_number(x)
#endif
end function runif_vec

function rnorm1() result(x)
! Return one N(0,1) variate (Box-Muller).
real(kind=dp) :: x
real(kind=dp) :: u1, u2
#ifdef XR2F_USE_R_RNG
x = real(xr2f_r_norm_rand(), kind=dp)
#else
do
   u1 = runif1()
   u2 = runif1()
   if (u1 > tiny(1.0_dp)) exit
end do
x = sqrt(-2.0_dp * log(u1)) * cos(2.0_dp * acos(-1.0_dp) * u2)
#endif
end function rnorm1

function rnorm_vec(n) result(x)
! Return a length-n vector of N(0,1) variates.
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
integer :: i
real(kind=dp) :: u1, u2, r, t
allocate(x(n))
#ifdef XR2F_USE_R_RNG
do i = 1, n
   x(i) = real(xr2f_r_norm_rand(), kind=dp)
end do
#else
i = 1
do while (i <= n)
   u1 = runif1()
   u2 = runif1()
   if (u1 <= tiny(1.0_dp)) cycle
   r = sqrt(-2.0_dp * log(u1))
   t = 2.0_dp * acos(-1.0_dp) * u2
   x(i) = r * cos(t)
   if (i + 1 <= n) x(i + 1) = r * sin(t)
   i = i + 2
end do
#endif
end function rnorm_vec

function rnorm_mat(nrow, ncol) result(x)
! Return an nrow-by-ncol matrix of N(0,1) variates.
integer, intent(in) :: nrow, ncol
real(kind=dp), allocatable :: x(:,:)
real(kind=dp), allocatable :: v(:)
v = rnorm_vec(nrow * ncol)
x = reshape(v, [nrow, ncol])
end function rnorm_mat

pure function r_filter_linear(x, filt, sides) result(out)
! Minimal stats::filter-style linear convolution for numeric vectors.
real(kind=dp), intent(in) :: x(:), filt(:)
integer, intent(in), optional :: sides
real(kind=dp), allocatable :: out(:)
integer :: n, nf, i, j, idx, side, before, after
real(kind=dp) :: acc, nanv
n = size(x)
nf = size(filt)
side = 2
if (present(sides)) side = sides
nanv = ieee_value(1.0_dp, ieee_quiet_nan)
allocate(out(n))
out = nanv
if (n <= 0 .or. nf <= 0) return
if (side == 1) then
   before = nf - 1
   after = 0
else
   before = nf / 2
   after = nf - before - 1
end if
do i = 1, n
   if (i - before < 1 .or. i + after > n) cycle
   acc = 0.0_dp
   do j = 1, nf
      idx = i + after - j + 1
      acc = acc + filt(j) * x(idx)
   end do
   out(i) = acc
end do
end function r_filter_linear

pure function runmed(x, k) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: k
real(kind=dp), allocatable :: out(:), tmp(:)
integer :: i, n, kk, h
n = size(x)
allocate(out(n))
if (n <= 0) return
kk = max(1, k)
if (mod(kk, 2) == 0) kk = kk + 1
h = kk / 2
if (kk <= 1 .or. n < kk) then
   out = x
   return
end if
out = x
do i = h + 1, n - h
   tmp = sort(x(i - h:i + h))
   out(i) = tmp(h + 1)
end do
end function runmed

pure function smooth(x, kind) result(out)
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in), optional :: kind
real(kind=dp), allocatable :: out(:), prev(:), split(:)
integer :: i, j, n, iter
logical :: use_3rs3r
n = size(x)
allocate(out(n))
if (n <= 0) return
use_3rs3r = .true.
if (present(kind)) use_3rs3r = index(kind, "3RS3R") > 0
out = runmed(x, 3)
if (.not. use_3rs3r) return
if (n < 3) return
do iter = 1, max(1, n)
   prev = out
   out = runmed(out, 3)
   if (all(abs(out - prev) <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, max(maxval(abs(out)), maxval(abs(prev)))))) exit
end do
split = out
i = 2
do while (i <= n - 2)
   j = i
   do while (j < n .and. out(j + 1) == out(i))
      j = j + 1
   end do
   if (j == i + 1 .and. out(i - 1) /= out(i) .and. out(j + 1) /= out(i)) then
      if ((out(i) > out(i - 1) .and. out(i) > out(j + 1)) .or. (out(i) < out(i - 1) .and. out(i) < out(j + 1))) then
         split(i) = out(i - 1)
         split(j) = out(j + 1)
      end if
   end if
   i = j + 1
end do
out = runmed(split, 3)
end function smooth

pure function smooth_kernel_eval(x, y, x0, bandwidth, kernel) result(v)
real(kind=dp), intent(in) :: x(:), y(:), x0, bandwidth
character(len=*), intent(in) :: kernel
real(kind=dp) :: v
real(kind=dp) :: bw, z, w, sw, sy, scale
integer :: i
bw = max(bandwidth, sqrt(tiny(1.0_dp)))
sw = 0.0_dp
sy = 0.0_dp
if (kernel == "box") then
   scale = 0.5_dp * bw
else
   ! R ksmooth bandwidth is the interquartile width of the normal kernel.
   scale = bw * (0.25_dp / 0.6744897501960817_dp)
end if
scale = max(scale, sqrt(tiny(1.0_dp)))
do i = 1, min(size(x), size(y))
   z = (x0 - x(i)) / scale
   if (kernel == "box") then
      if (abs(z) <= 1.0_dp) then
         w = 1.0_dp
      else
         w = 0.0_dp
      end if
   else
      w = exp(-0.5_dp * z * z)
   end if
   sw = sw + w
   sy = sy + w * y(i)
end do
if (sw > 0.0_dp) then
   v = sy / sw
else
   v = ieee_value(1.0_dp, ieee_quiet_nan)
end if
end function smooth_kernel_eval

pure function ksmooth(x, y, kernel, bandwidth, x_points) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
character(len=*), intent(in), optional :: kernel
real(kind=dp), intent(in), optional :: bandwidth
real(kind=dp), intent(in), optional :: x_points(:)
type(smooth_xy_t) :: out
character(len=:), allocatable :: kern
real(kind=dp) :: bw
integer :: i
kern = "normal"
if (present(kernel)) kern = kernel
bw = 0.5_dp
if (present(bandwidth)) bw = bandwidth
if (present(x_points)) then
   out%x = x_points
else
   out%x = x
end if
allocate(out%y(size(out%x)))
do i = 1, size(out%x)
   out%y(i) = smooth_kernel_eval(x, y, out%x(i), bw, kern)
end do
out%df = real(size(out%x), kind=dp)
end function ksmooth

pure function lowess(x, y, f, iter) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), intent(in), optional :: f
integer, intent(in), optional :: iter
type(smooth_xy_t) :: out
real(kind=dp), allocatable :: robust(:), fitted(:), resid(:), abs_resid(:), dist(:), dist_sorted(:)
real(kind=dp) :: frac, h, d, u, w, sw, sx, sy, sxx, sxy, den, beta, alpha, cmad
integer :: n, i, j, ns, pass, niter
n = min(size(x), size(y))
allocate(out%x(n), out%y(n))
if (n <= 0) return
out%x = x(1:n)
frac = 2.0_dp / 3.0_dp
if (present(f)) frac = max(0.01_dp, min(1.0_dp, f))
niter = 3
if (present(iter)) niter = max(0, iter)
ns = max(2, min(n, int(frac * real(n, kind=dp))))
allocate(robust(n), fitted(n), resid(n), abs_resid(n), dist(n), dist_sorted(n))
robust = 1.0_dp
fitted = y(1:n)
do pass = 0, niter
   do i = 1, n
      do j = 1, n
         dist(j) = abs(out%x(j) - out%x(i))
      end do
      dist_sorted = sort(dist)
      h = dist_sorted(ns)
      if (h <= sqrt(tiny(1.0_dp))) h = maxval(dist_sorted)
      sw = 0.0_dp
      sx = 0.0_dp
      sy = 0.0_dp
      sxx = 0.0_dp
      sxy = 0.0_dp
      do j = 1, n
         if (h <= sqrt(tiny(1.0_dp))) then
            w = merge(robust(j), 0.0_dp, dist(j) <= sqrt(tiny(1.0_dp)))
         else if (dist(j) <= h) then
            d = dist(j) / h
            w = (1.0_dp - d**3)**3 * robust(j)
         else
            w = 0.0_dp
         end if
         sw = sw + w
         sx = sx + w * out%x(j)
         sy = sy + w * y(j)
         sxx = sxx + w * out%x(j) * out%x(j)
         sxy = sxy + w * out%x(j) * y(j)
      end do
      if (sw <= sqrt(tiny(1.0_dp))) then
         fitted(i) = y(i)
      else
         den = sw * sxx - sx * sx
         if (abs(den) <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, abs(sw * sxx), abs(sx * sx))) then
            fitted(i) = sy / sw
         else
            beta = (sw * sxy - sx * sy) / den
            alpha = (sy - beta * sx) / sw
            fitted(i) = alpha + beta * out%x(i)
         end if
      end if
   end do
   if (pass >= niter) exit
   resid = y(1:n) - fitted
   abs_resid = abs(resid)
   cmad = median(abs_resid)
   if (cmad <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, maxval(abs(y(1:n))))) exit
   do j = 1, n
      u = abs_resid(j) / (6.0_dp * cmad)
      if (u >= 1.0_dp) then
         robust(j) = 0.0_dp
      else
         robust(j) = (1.0_dp - u * u)**2
      end if
   end do
end do
out%y = fitted
out%df = real(ns, kind=dp)
end function lowess

pure function loess_fit(x, y, span, degree) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), intent(in), optional :: span
integer, intent(in), optional :: degree
type(loess_fit_t) :: out
out%x = x
out%y = y
out%span = 0.75_dp
if (present(span)) out%span = span
out%degree = 2
if (present(degree)) out%degree = degree
end function loess_fit

pure function predict_loess(fit, xnew) result(yhat)
type(loess_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xnew(:)
real(kind=dp), allocatable :: yhat(:)
real(kind=dp), allocatable :: dist(:), dist_sorted(:)
real(kind=dp) :: h, d, w, z, sw, sy, sx, sxx, sxy, sx3, sx4, sx2y
real(kind=dp) :: den, alpha, beta, gamma, det, b0, b1, b2
real(kind=dp) :: m00, m01, m02, m11, m12, m22
integer :: i, j, n, ns, deg
allocate(yhat(size(xnew)))
n = min(size(fit%x), size(fit%y))
if (n <= 0) return
ns = max(2, min(n, int(fit%span * real(n, kind=dp))))
deg = max(0, min(2, fit%degree))
allocate(dist(n), dist_sorted(n))
do i = 1, size(xnew)
   do j = 1, n
      dist(j) = abs(fit%x(j) - xnew(i))
   end do
   dist_sorted = sort(dist)
   h = dist_sorted(ns)
   if (h <= sqrt(tiny(1.0_dp))) h = maxval(dist_sorted)
   sw = 0.0_dp
   sy = 0.0_dp
   sx = 0.0_dp
   sxx = 0.0_dp
   sxy = 0.0_dp
   sx3 = 0.0_dp
   sx4 = 0.0_dp
   sx2y = 0.0_dp
   do j = 1, n
      z = fit%x(j) - xnew(i)
      if (h <= sqrt(tiny(1.0_dp))) then
         w = merge(1.0_dp, 0.0_dp, abs(z) <= sqrt(tiny(1.0_dp)))
      else if (abs(z) <= h) then
         d = abs(z) / h
         w = (1.0_dp - d**3)**3
      else
         w = 0.0_dp
      end if
      sw = sw + w
      sy = sy + w * fit%y(j)
      sx = sx + w * z
      sxx = sxx + w * z * z
      sxy = sxy + w * z * fit%y(j)
      if (deg >= 2) then
         sx3 = sx3 + w * z**3
         sx4 = sx4 + w * z**4
         sx2y = sx2y + w * z * z * fit%y(j)
      end if
   end do
   if (sw <= sqrt(tiny(1.0_dp))) then
      yhat(i) = fit%y(min(max(1, minloc(dist, dim=1)), n))
   else if (deg <= 0) then
      yhat(i) = sy / sw
   else if (deg == 1) then
      den = sw * sxx - sx * sx
      if (abs(den) <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, abs(sw * sxx), abs(sx * sx))) then
         yhat(i) = sy / sw
      else
         beta = (sw * sxy - sx * sy) / den
         alpha = (sy - beta * sx) / sw
         yhat(i) = alpha
      end if
   else
      m00 = sw
      m01 = sx
      m02 = sxx
      m11 = sxx
      m12 = sx3
      m22 = sx4
      det = m00 * (m11 * m22 - m12 * m12) - m01 * (m01 * m22 - m12 * m02) + m02 * (m01 * m12 - m11 * m02)
      if (abs(det) <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, abs(m00*m11*m22))) then
         den = sw * sxx - sx * sx
         if (abs(den) <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, abs(sw * sxx), abs(sx * sx))) then
            yhat(i) = sy / sw
         else
            beta = (sw * sxy - sx * sy) / den
            alpha = (sy - beta * sx) / sw
            yhat(i) = alpha
         end if
      else
         b0 = sy
         b1 = sxy
         b2 = sx2y
         gamma = (m00 * (m11 * b2 - b1 * m12) - m01 * (m01 * b2 - b1 * m02) + b0 * (m01 * m12 - m11 * m02)) / det
         beta = (m00 * (b1 * m22 - m12 * b2) - b0 * (m01 * m22 - m12 * m02) + m02 * (m01 * b2 - b1 * m02)) / det
         alpha = (b0 * (m11 * m22 - m12 * m12) - m01 * (b1 * m22 - m12 * b2) + m02 * (b1 * m12 - m11 * b2)) / det
         yhat(i) = alpha
      end if
   end if
end do
end function predict_loess

pure function smooth_spline(x, y, df, spar) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp), intent(in), optional :: df, spar
type(smooth_spline_fit_t) :: out
type(loess_fit_t) :: lf
real(kind=dp) :: span_eff, df_eff, spar_eff
integer :: n
n = min(size(x), size(y))
out%x = x(1:n)
if (n <= 0) then
   out%df = 0.0_dp
   return
end if
if ((.not. present(df) .or. df <= 0.0_dp) .and. (.not. present(spar) .or. spar < 0.0_dp)) then
   out%df = real(n, kind=dp)
   out%y = y(1:n)
   return
end if
if (present(df) .and. df > 0.0_dp) then
   df_eff = max(2.0_dp, min(real(n, kind=dp), df))
   out%df = df_eff
   span_eff = min(1.0_dp, max(0.08_dp, 1.78_dp / sqrt(df_eff)))
else
   spar_eff = max(0.0_dp, min(1.5_dp, spar))
   out%df = max(2.0_dp, real(n, kind=dp) * exp(-2.836_dp * spar_eff**1.32_dp))
   span_eff = min(1.0_dp, max(0.08_dp, 0.18_dp + 0.82_dp * spar_eff))
end if
lf = loess_fit(out%x, y(1:n), span=span_eff, degree=2)
out%y = predict_loess(lf, out%x)
end function smooth_spline

pure function predict_smooth_spline(fit, xnew) result(out)
type(smooth_spline_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xnew(:)
type(smooth_xy_t) :: out
integer :: i, j
real(kind=dp) :: t
out%x = xnew
out%df = fit%df
allocate(out%y(size(xnew)))
if (size(fit%x) <= 0) then
   out%y = ieee_value(1.0_dp, ieee_quiet_nan)
   return
end if
do i = 1, size(xnew)
   if (xnew(i) <= fit%x(1) .or. size(fit%x) == 1) then
      out%y(i) = fit%y(1)
   else if (xnew(i) >= fit%x(size(fit%x))) then
      out%y(i) = fit%y(size(fit%y))
   else
      j = 1
      do while (j < size(fit%x) - 1 .and. fit%x(j + 1) < xnew(i))
         j = j + 1
      end do
      t = (xnew(i) - fit%x(j)) / max(tiny(1.0_dp), fit%x(j + 1) - fit%x(j))
      out%y(i) = (1.0_dp - t) * fit%y(j) + t * fit%y(j + 1)
   end if
end do
end function predict_smooth_spline

function rbinom_scalar(n, size_, prob) result(x)
! Return n binomial(size, prob) variates.
integer, intent(in) :: n, size_
real(kind=dp), intent(in) :: prob
integer, allocatable :: x(:)
integer :: i, j, s
real(kind=dp) :: u, p
allocate(x(max(0, n)))
p = max(0.0_dp, min(1.0_dp, prob))
do i = 1, size(x)
   s = 0
   do j = 1, max(0, size_)
   u = runif1()
      if (u < p) s = s + 1
   end do
   x(i) = s
end do
end function rbinom_scalar

function rbinom_vector_prob(n, size_, prob) result(x)
! Return n binomial(size, prob(i)) variates.
integer, intent(in) :: n, size_
real(kind=dp), intent(in) :: prob(:)
integer, allocatable :: x(:)
integer :: i, j, s
real(kind=dp) :: u, p
allocate(x(max(0, n)))
do i = 1, size(x)
   if (size(prob) > 0) then
      p = max(0.0_dp, min(1.0_dp, prob(min(i, size(prob)))))
   else
      p = 0.0_dp
   end if
   s = 0
   do j = 1, max(0, size_)
   u = runif1()
      if (u < p) s = s + 1
   end do
   x(i) = s
end do
end function rbinom_vector_prob

function rpois_scalar(n, lambda) result(x)
! Return n Poisson(lambda) variates.
integer, intent(in) :: n
real(kind=dp), intent(in) :: lambda
integer, allocatable :: x(:)
integer :: i
allocate(x(max(0, n)))
do i = 1, size(x)
   x(i) = rpois_one(max(0.0_dp, lambda))
end do
end function rpois_scalar

function rpois_vector(n, lambda) result(x)
! Return n Poisson(lambda(i)) variates.
integer, intent(in) :: n
real(kind=dp), intent(in) :: lambda(:)
integer, allocatable :: x(:)
integer :: i
allocate(x(max(0, n)))
do i = 1, size(x)
   if (size(lambda) > 0) then
      x(i) = rpois_one(max(0.0_dp, lambda(min(i, size(lambda)))))
   else
      x(i) = 0
   end if
end do
end function rpois_vector

function rpois_one(lambda) result(k)
! Draw one Poisson variate; Knuth for small lambda, normal approximation for large lambda.
real(kind=dp), intent(in) :: lambda
integer :: k
real(kind=dp) :: lprob, p, u, z
if (lambda <= 0.0_dp) then
   k = 0
   return
end if
if (lambda < 40.0_dp) then
   lprob = exp(-lambda)
   k = 0
   p = 1.0_dp
   do
   u = runif1()
      p = p * u
      if (p <= lprob) exit
      k = k + 1
   end do
else
   z = rnorm1()
   k = max(0, nint(lambda + sqrt(lambda) * z))
end if
end function rpois_one

function random_choice2_prob(n, p1) result(z)
! Sample n labels in {1,2} with P(label=1)=p1.
integer, intent(in) :: n
real(kind=dp), intent(in) :: p1
integer, allocatable :: z(:)
integer :: i
real(kind=dp) :: u
allocate(z(n))
do i = 1, n
   u = runif1()
   z(i) = merge(1, 2, u < p1)
end do
end function random_choice2_prob

function randint_range(n, lo, hi) result(out)
! Sample n integers uniformly from [lo, hi].
integer, intent(in) :: n, lo, hi
integer, allocatable :: out(:)
integer :: i, span
real(kind=dp) :: u
if (hi < lo) then
   allocate(out(0))
   return
end if
span = hi - lo + 1
allocate(out(n))
do i = 1, n
   u = runif1()
   out(i) = lo + int(u * span)
   if (out(i) > hi) out(i) = hi
end do
end function randint_range

function sample_int(n, size_, replace, prob) result(out)
! R-like sample.int with optional replacement and probabilities.
integer, intent(in) :: n
integer, intent(in), optional :: size_
logical, intent(in), optional :: replace
real(kind=dp), intent(in), optional :: prob(:)
integer, allocatable :: out(:), pool(:)
real(kind=dp), allocatable :: w(:), cdf(:)
integer :: m, i, j, pick, tmp
logical :: rep
real(kind=dp) :: u, s, acc

m = n
if (present(size_)) m = size_
rep = .false.
if (present(replace)) rep = replace

if (n < 1) then
   if (m == 0) then
      allocate(out(0))
      return
   end if
   error stop "sample_int: n must be >= 1"
end if
if (m < 0) error stop "sample_int: size must be >= 0"
if ((.not. rep) .and. (m > n)) error stop &
   & "sample_int: size > n without replacement"
if (present(prob)) then
   if (size(prob) /= n) error stop "sample_int: prob length mismatch"
   if (any(prob < 0.0_dp)) error stop "sample_int: prob must be nonnegative"
   if (sum(prob) <= 0.0_dp) error stop "sample_int: prob sum must be positive"
end if

allocate(out(m))
if (m == 0) return

if (present(prob)) then
   if (rep) then
      allocate(cdf(n))
      s = sum(prob)
      cdf(1) = prob(1) / s
      do i = 2, n
         cdf(i) = cdf(i - 1) + prob(i) / s
      end do
      cdf(n) = 1.0_dp
      do i = 1, m
   u = runif1()
         pick = 1
         do while (pick < n .and. u > cdf(pick))
            pick = pick + 1
         end do
         out(i) = pick
      end do
   else
      allocate(w(n))
      w = prob
      do i = 1, m
         s = sum(w)
         if (s <= 0.0_dp) error stop "sample_int: depleted probability mass"
   u = runif1()
         u = u * s
         acc = 0.0_dp
         pick = n
         do j = 1, n
            acc = acc + w(j)
            if (u <= acc) then
               pick = j
               exit
            end if
         end do
         out(i) = pick
         w(pick) = 0.0_dp
      end do
   end if
else
   if (rep) then
      do i = 1, m
   u = runif1()
         out(i) = 1 + int(u * real(n, kind=dp))
         if (out(i) > n) out(i) = n
      end do
   else
      allocate(pool(n))
      pool = [(j, j=1,n)]
      do i = 1, m
   u = runif1()
         pick = i + int(u * real(n - i + 1, kind=dp))
         if (pick > n) pick = n
         tmp = pool(i)
         pool(i) = pool(pick)
         pool(pick) = tmp
         out(i) = pool(i)
      end do
   end if
end if
end function sample_int

function sample_int1(n, replace, prob) result(out)
! Scalar wrapper for sample_int(..., size_=1).
integer, intent(in) :: n
logical, intent(in), optional :: replace
real(kind=dp), intent(in), optional :: prob(:)
integer :: out
integer, allocatable :: tmp(:)
if (present(replace) .and. present(prob)) then
   tmp = sample_int(n, size_=1, replace=replace, prob=prob)
else if (present(prob)) then
   tmp = sample_int(n, size_=1, prob=prob)
else if (present(replace)) then
    tmp = sample_int(n, size_=1, replace=replace)
else
   tmp = sample_int(n, size_=1)
end if
out = tmp(1)
end function sample_int1

pure subroutine sort_increasing(x)
! Sort a real vector in increasing order (insertion sort).
real(kind=dp), intent(inout) :: x(:)
integer :: i, j
real(kind=dp) :: key
do i = 2, size(x)
   key = x(i)
   j = i - 1
   do while (j >= 1 .and. x(j) > key)
      x(j + 1) = x(j)
      j = j - 1
   end do
   x(j + 1) = key
end do
end subroutine sort_increasing

pure subroutine sort_increasing_int(x)
! Sort an integer vector in increasing order (insertion sort).
integer, intent(inout) :: x(:)
integer :: i, j, key
do i = 2, size(x)
   key = x(i)
   j = i - 1
   do while (j >= 1 .and. x(j) > key)
      x(j + 1) = x(j)
      j = j - 1
   end do
   x(j + 1) = key
end do
end subroutine sort_increasing_int

pure function sort_real(x, decreasing) result(out)
! Return a sorted copy of a real vector.
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: decreasing
real(kind=dp), allocatable :: out(:)
integer :: i, n
out = x
call sort_increasing(out)
if (present(decreasing)) then
   if (decreasing) then
      n = size(out)
      out = out([(i, i = n, 1, -1)])
   end if
end if
end function sort_real

pure function sort_int(x, decreasing) result(out)
! Return a sorted copy of an integer vector.
integer, intent(in) :: x(:)
logical, intent(in), optional :: decreasing
integer, allocatable :: out(:)
integer :: i, n
out = x
call sort_increasing_int(out)
if (present(decreasing)) then
   if (decreasing) then
      n = size(out)
      out = out([(i, i = n, 1, -1)])
   end if
end if
end function sort_int

pure function sort_char(x, decreasing) result(out)
! Return a sorted copy of a character vector.
character(len=*), intent(in) :: x(:)
logical, intent(in), optional :: decreasing
character(len=:), allocatable :: out(:)
integer, allocatable :: idx(:)
idx = sort_list_char(x, decreasing)
allocate(character(len=len(x)) :: out(size(x)))
out = x(idx)
end function sort_char

pure function sort_list_real(x, decreasing) result(idx)
! Return 1-based indices that sort a real vector.
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: decreasing
integer, allocatable :: idx(:)
integer :: i, j, t
logical :: dec
allocate(idx(size(x)))
do i = 1, size(x)
   idx(i) = i
end do
dec = .false.
if (present(decreasing)) dec = decreasing
do i = 2, size(idx)
   t = idx(i)
   j = i - 1
   if (dec) then
      do while (j >= 1 .and. x(idx(j)) < x(t))
         idx(j + 1) = idx(j)
         j = j - 1
      end do
   else
      do while (j >= 1 .and. x(idx(j)) > x(t))
         idx(j + 1) = idx(j)
         j = j - 1
      end do
   end if
   idx(j + 1) = t
end do
end function sort_list_real

pure function sort_list_int(x, decreasing) result(idx)
! Return 1-based indices that sort an integer vector.
integer, intent(in) :: x(:)
logical, intent(in), optional :: decreasing
integer, allocatable :: idx(:)
integer :: i, j, t
logical :: dec
allocate(idx(size(x)))
do i = 1, size(x)
   idx(i) = i
end do
dec = .false.
if (present(decreasing)) dec = decreasing
do i = 2, size(idx)
   t = idx(i)
   j = i - 1
   if (dec) then
      do while (j >= 1 .and. x(idx(j)) < x(t))
         idx(j + 1) = idx(j)
         j = j - 1
      end do
   else
      do while (j >= 1 .and. x(idx(j)) > x(t))
         idx(j + 1) = idx(j)
         j = j - 1
      end do
   end if
   idx(j + 1) = t
end do
end function sort_list_int

pure function sort_list_char(x, decreasing) result(idx)
! Return 1-based indices that sort a character vector.
character(len=*), intent(in) :: x(:)
logical, intent(in), optional :: decreasing
integer, allocatable :: idx(:)
integer :: i, j, t
logical :: dec
allocate(idx(size(x)))
do i = 1, size(x)
   idx(i) = i
end do
dec = .false.
if (present(decreasing)) dec = decreasing
do i = 2, size(idx)
   t = idx(i)
   j = i - 1
   if (dec) then
      do while (j >= 1 .and. x(idx(j)) < x(t))
         idx(j + 1) = idx(j)
         j = j - 1
      end do
   else
      do while (j >= 1 .and. x(idx(j)) > x(t))
         idx(j + 1) = idx(j)
         j = j - 1
      end do
   end if
   idx(j + 1) = t
end do
end function sort_list_char

pure function r_head_real(x, n) result(out)
! Return the first n elements of a real vector.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: m
m = max(0, min(n, size(x)))
allocate(out(m))
if (m > 0) out = x(1:m)
end function r_head_real

pure function r_head_int(x, n) result(out)
! Return the first n elements of an integer vector.
integer, intent(in) :: x(:)
integer, intent(in) :: n
integer, allocatable :: out(:)
integer :: m
m = max(0, min(n, size(x)))
allocate(out(m))
if (m > 0) out = x(1:m)
end function r_head_int

pure function r_head_real_mat(x, n) result(out)
! Return the first n rows of a real matrix.
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:,:)
integer :: m
m = max(0, min(n, size(x, 1)))
allocate(out(m, max(0, size(x, 2))))
if (m > 0) out = x(1:m, :)
end function r_head_real_mat

pure function r_head_int_mat(x, n) result(out)
! Return the first n rows of an integer matrix.
integer, intent(in) :: x(:,:)
integer, intent(in) :: n
integer, allocatable :: out(:,:)
integer :: m
m = max(0, min(n, size(x, 1)))
allocate(out(m, max(0, size(x, 2))))
if (m > 0) out = x(1:m, :)
end function r_head_int_mat

pure function order_real(x) result(idx)
! Return 1-based order indices that sort a real vector increasingly.
real(kind=dp), intent(in) :: x(:)
integer, allocatable :: idx(:)
integer :: i, j, t
allocate(idx(size(x)))
if (size(x) <= 0) return
do i = 1, size(x)
   idx(i) = i
end do
do i = 2, size(idx)
   t = idx(i)
   j = i - 1
   do while (j >= 1 .and. x(idx(j)) > x(t))
      idx(j + 1) = idx(j)
      j = j - 1
   end do
   idx(j + 1) = t
end do
end function order_real

pure function rank_first(x) result(out)
! Return R rank(x, ties.method="first") for a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer, allocatable :: ord(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
ord = order_real(x)
do i = 1, size(ord)
   out(ord(i)) = real(i, kind=dp)
end do
end function rank_first

pure function rank_average(x) result(out)
! Return R rank(x, ties.method="average") for a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer, allocatable :: ord(:)
integer :: first, i, last
real(kind=dp) :: r
allocate(out(size(x)))
if (size(x) <= 0) return
ord = order_real(x)
first = 1
do while (first <= size(ord))
   last = first
   do while (last < size(ord) .and. x(ord(last + 1)) == x(ord(first)))
      last = last + 1
   end do
   r = 0.5_dp * real(first + last, kind=dp)
   do i = first, last
      out(ord(i)) = r
   end do
   first = last + 1
end do
end function rank_average

pure function det_real_mat(x) result(out)
! Return the determinant of a real square matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp) :: out
real(kind=dp), allocatable :: a(:)
integer :: i, j, k, n, p
real(kind=dp) :: fac, piv, t
n = size(x, 1)
out = 0.0_dp
if (n /= size(x, 2)) return
if (n == 0) then
   out = 1.0_dp
   return
end if
allocate(a(n*n))
a = reshape(x, [n*n])
out = 1.0_dp
do k = 1, n
   p = k
   piv = abs(a((k - 1)*n + k))
   do i = k + 1, n
      if (abs(a((i - 1)*n + k)) > piv) then
         p = i
         piv = abs(a((i - 1)*n + k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) then
      out = 0.0_dp
      return
   end if
   if (p /= k) then
      do j = k, n
         t = a((k - 1)*n + j)
         a((k - 1)*n + j) = a((p - 1)*n + j)
         a((p - 1)*n + j) = t
      end do
      out = -out
   end if
   out = out * a((k - 1)*n + k)
   do i = k + 1, n
      fac = a((i - 1)*n + k) / a((k - 1)*n + k)
      do j = k + 1, n
         a((i - 1)*n + j) = a((i - 1)*n + j) - fac * a((k - 1)*n + j)
      end do
   end do
end do
end function det_real_mat

pure function det_real_int(x) result(out)
integer, intent(in) :: x(:,:)
real(kind=dp) :: out
out = det_real_mat(real(x, kind=dp))
end function det_real_int

pure function kappa_real(x) result(out)
! Return a simple 1-norm condition estimate for a square matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp) :: out
real(kind=dp), allocatable :: inv_x(:,:)
integer :: n
n = size(x, 1)
out = huge(1.0_dp)
if (n <= 0 .or. size(x, 2) /= n) return
inv_x = solve_real_mat(x, real(diag(n), kind=dp))
if (all(inv_x == 0.0_dp)) return
out = maxval(sum(abs(x), dim=1)) * maxval(sum(abs(inv_x), dim=1))
end function kappa_real

pure function eigen_sym_values(x) result(vals)
! Return eigenvalues of a real symmetric matrix using Jacobi rotations.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: vals(:)
real(kind=dp), allocatable :: a(:,:)
integer :: i, j, p, q, iter, n, max_iter
real(kind=dp) :: app, aqq, apq, c, off, phi, s, tau, t, tmp
n = size(x, 1)
allocate(vals(n))
vals = 0.0_dp
if (n <= 0 .or. n /= size(x, 2)) return
a = x
max_iter = max(1, 100 * n * n)
do iter = 1, max_iter
   off = 0.0_dp
   p = 1
   q = min(2, n)
   do i = 1, n - 1
      do j = i + 1, n
         if (abs(a(i, j)) > off) then
            off = abs(a(i, j))
            p = i
            q = j
         end if
      end do
   end do
   if (off <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, maxval(abs(a)))) exit
   app = a(p, p)
   aqq = a(q, q)
   apq = a(p, q)
   if (abs(apq) <= tiny(1.0_dp)) cycle
   tau = (aqq - app) / (2.0_dp * apq)
   t = sign(1.0_dp, tau) / (abs(tau) + sqrt(1.0_dp + tau * tau))
   c = 1.0_dp / sqrt(1.0_dp + t * t)
   s = t * c
   do j = 1, n
      if (j /= p .and. j /= q) then
         tmp = a(j, p)
         a(j, p) = c * tmp - s * a(j, q)
         a(p, j) = a(j, p)
         a(j, q) = s * tmp + c * a(j, q)
         a(q, j) = a(j, q)
      end if
   end do
   phi = t * apq
   a(p, p) = app - phi
   a(q, q) = aqq + phi
   a(p, q) = 0.0_dp
   a(q, p) = 0.0_dp
end do
do i = 1, n
   vals(i) = a(i, i)
end do
end function eigen_sym_values

function eigen_int(x, symmetric, only_values) result(fit)
integer, intent(in) :: x(:,:)
logical, intent(in), optional :: symmetric, only_values
type(eigen_result_t) :: fit
if (present(symmetric) .and. present(only_values)) then
   fit = eigen_real(real(x, kind=dp), symmetric=symmetric, only_values=only_values)
else if (present(symmetric)) then
   fit = eigen_real(real(x, kind=dp), symmetric=symmetric)
else if (present(only_values)) then
   fit = eigen_real(real(x, kind=dp), only_values=only_values)
else
   fit = eigen_real(real(x, kind=dp))
end if
end function eigen_int

function eigen_real(x, symmetric, only_values) result(fit)
! Small real eigen decomposition helper for R eigen() examples.
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: symmetric, only_values
type(eigen_result_t) :: fit
real(kind=dp), allocatable :: a(:,:), vecs(:,:), tmpv(:)
real(kind=dp) :: off, app, aqq, apq, tau, t, c, s, phi, tmp, normv
real(kind=dp) :: aa, bb, cc, dd, tr, disc, root, lam
integer :: n, i, j, k, q, iter, max_iter, imax
logical :: do_symmetric, only_vals
n = size(x, 1)
allocate(fit%values(n), fit%vectors(n, n))
fit%values = 0.0_dp
fit%vectors = 0.0_dp
do i = 1, n
   fit%vectors(i, i) = 1.0_dp
end do
if (n <= 0 .or. n /= size(x, 2)) return
do_symmetric = maxval(abs(x - transpose(x))) <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, maxval(abs(x)))
if (present(symmetric)) do_symmetric = symmetric
only_vals = .false.
if (present(only_values)) only_vals = only_values
if ((.not. do_symmetric) .and. n == 2) then
   aa = x(1, 1)
   bb = x(1, 2)
   cc = x(2, 1)
   dd = x(2, 2)
   tr = aa + dd
   disc = (aa - dd) * (aa - dd) + 4.0_dp * bb * cc
   if (disc >= 0.0_dp) then
      root = sqrt(disc)
      fit%values = [(0.5_dp * (tr + root)), (0.5_dp * (tr - root))]
      do j = 1, 2
         lam = fit%values(j)
         if (abs(bb) >= abs(cc) .and. abs(bb) > tiny(1.0_dp)) then
            fit%vectors(:, j) = [bb, lam - aa]
         else if (abs(cc) > tiny(1.0_dp)) then
            fit%vectors(:, j) = [lam - dd, cc]
         else
            fit%vectors(:, j) = 0.0_dp
            fit%vectors(j, j) = 1.0_dp
         end if
         normv = sqrt(max(tiny(1.0_dp), sum(fit%vectors(:, j)**2)))
         fit%vectors(:, j) = fit%vectors(:, j) / normv
      end do
      return
   end if
   error stop "complex eigenvalues are not supported by xr2f eigen() runtime"
end if
a = x
allocate(vecs(n, n))
vecs = 0.0_dp
do i = 1, n
   vecs(i, i) = 1.0_dp
end do
max_iter = max(1, 100 * n * n)
do iter = 1, max_iter
   off = 0.0_dp
   i = 1
   q = min(2, n)
   do j = 1, n - 1
      do k = j + 1, n
         if (abs(a(j, k)) > off) then
            off = abs(a(j, k))
            i = j
            q = k
         end if
      end do
   end do
   if (off <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, maxval(abs(a)))) exit
   app = a(i, i)
   aqq = a(q, q)
   apq = a(i, q)
   if (abs(apq) <= tiny(1.0_dp)) cycle
   tau = (aqq - app) / (2.0_dp * apq)
   t = sign(1.0_dp, tau) / (abs(tau) + sqrt(1.0_dp + tau * tau))
   c = 1.0_dp / sqrt(1.0_dp + t * t)
   s = t * c
   do k = 1, n
      if (k /= i .and. k /= q) then
         tmp = a(k, i)
         a(k, i) = c * tmp - s * a(k, q)
         a(i, k) = a(k, i)
         a(k, q) = s * tmp + c * a(k, q)
         a(q, k) = a(k, q)
      end if
      tmp = vecs(k, i)
      vecs(k, i) = c * tmp - s * vecs(k, q)
      vecs(k, q) = s * tmp + c * vecs(k, q)
   end do
   phi = t * apq
   a(i, i) = app - phi
   a(q, q) = aqq + phi
   a(i, q) = 0.0_dp
   a(q, i) = 0.0_dp
end do
do i = 1, n
   fit%values(i) = a(i, i)
end do
allocate(tmpv(n))
do i = 1, n - 1
   imax = i
   do j = i + 1, n
      if (fit%values(j) > fit%values(imax)) imax = j
   end do
   if (imax /= i) then
      tmp = fit%values(i)
      fit%values(i) = fit%values(imax)
      fit%values(imax) = tmp
      tmpv = vecs(:, i)
      vecs(:, i) = vecs(:, imax)
      vecs(:, imax) = tmpv
   end if
end do
do j = 1, n
   normv = sqrt(max(tiny(1.0_dp), sum(vecs(:, j)**2)))
   fit%vectors(:, j) = vecs(:, j) / normv
   if (fit%vectors(1, j) < 0.0_dp) fit%vectors(:, j) = -fit%vectors(:, j)
end do
if (only_vals) then
   if (allocated(fit%vectors)) deallocate(fit%vectors)
   allocate(fit%vectors(0, 0))
end if
end function eigen_real

subroutine print_eigen(fit)
type(eigen_result_t), intent(in) :: fit
write(*,'(a)') "$values"
call print_real_vector(fit%values)
write(*,*)
write(*,'(a)') "$vectors"
if (allocated(fit%vectors) .and. size(fit%vectors, 1) > 0 .and. size(fit%vectors, 2) > 0) then
   call print_matrix_rstyle(fit%vectors)
else
   write(*,'(a)') "NULL"
end if
end subroutine print_eigen

function prcomp(x, center, scale_) result(fit)
! Principal components for a numeric matrix using a symmetric Jacobi eigensolver.
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: center, scale_
type(prcomp_fit_t) :: fit
real(kind=dp), allocatable :: z(:,:), covm(:,:), vals(:), vecs(:,:), tmpv(:)
real(kind=dp) :: off, app, aqq, apq, tau, t, c, s, phi, tmp, normv
integer :: n, p, i, j, k, q, iter, max_iter, imax
logical :: do_center, do_scale
n = size(x, 1)
p = size(x, 2)
do_center = .true.
do_scale = .false.
if (present(center)) do_center = center
if (present(scale_)) do_scale = scale_
allocate(z(n, p), fit%center(p), fit%scale(p), covm(p, p), vals(p), vecs(p, p), tmpv(p))
z = x
fit%center = 0.0_dp
fit%scale = 1.0_dp
if (do_center .and. n > 0) then
   fit%center = sum(x, dim=1) / real(n, kind=dp)
   do j = 1, p
      z(:, j) = z(:, j) - fit%center(j)
   end do
end if
if (do_scale .and. n > 1) then
   do j = 1, p
      fit%scale(j) = sqrt(max(0.0_dp, sum(z(:, j)**2) / real(n - 1, kind=dp)))
      if (fit%scale(j) > 0.0_dp) z(:, j) = z(:, j) / fit%scale(j)
   end do
end if
if (n > 1) then
   covm = matmul(transpose(z), z) / real(n - 1, kind=dp)
else
   covm = 0.0_dp
end if
vecs = 0.0_dp
do i = 1, p
   vecs(i, i) = 1.0_dp
end do
max_iter = max(1, 100 * p * p)
do iter = 1, max_iter
   off = 0.0_dp
   i = 1
   q = min(2, p)
   do j = 1, p - 1
      do k = j + 1, p
         if (abs(covm(j, k)) > off) then
            off = abs(covm(j, k))
            i = j
            q = k
         end if
      end do
   end do
   if (off <= 100.0_dp * epsilon(1.0_dp) * max(1.0_dp, maxval(abs(covm)))) exit
   app = covm(i, i)
   aqq = covm(q, q)
   apq = covm(i, q)
   if (abs(apq) <= tiny(1.0_dp)) cycle
   tau = (aqq - app) / (2.0_dp * apq)
   t = sign(1.0_dp, tau) / (abs(tau) + sqrt(1.0_dp + tau * tau))
   c = 1.0_dp / sqrt(1.0_dp + t * t)
   s = t * c
   do k = 1, p
      if (k /= i .and. k /= q) then
         tmp = covm(k, i)
         covm(k, i) = c * tmp - s * covm(k, q)
         covm(i, k) = covm(k, i)
         covm(k, q) = s * tmp + c * covm(k, q)
         covm(q, k) = covm(k, q)
      end if
      tmp = vecs(k, i)
      vecs(k, i) = c * tmp - s * vecs(k, q)
      vecs(k, q) = s * tmp + c * vecs(k, q)
   end do
   phi = t * apq
   covm(i, i) = app - phi
   covm(q, q) = aqq + phi
   covm(i, q) = 0.0_dp
   covm(q, i) = 0.0_dp
end do
do i = 1, p
   vals(i) = covm(i, i)
end do
do i = 1, p - 1
   imax = i
   do j = i + 1, p
      if (vals(j) > vals(imax)) imax = j
   end do
   if (imax /= i) then
      tmp = vals(i)
      vals(i) = vals(imax)
      vals(imax) = tmp
      tmpv = vecs(:, i)
      vecs(:, i) = vecs(:, imax)
      vecs(:, imax) = tmpv
   end if
end do
do j = 1, p
   normv = sqrt(max(tiny(1.0_dp), sum(vecs(:, j)**2)))
   vecs(:, j) = vecs(:, j) / normv
   if (vecs(1, j) < 0.0_dp) vecs(:, j) = -vecs(:, j)
end do
fit%sdev = sqrt(max(0.0_dp, vals))
fit%rotation = vecs
fit%x = matmul(z, vecs)
end function prcomp

subroutine print_prcomp_summary(fit)
type(prcomp_fit_t), intent(in) :: fit
real(kind=dp), allocatable :: var(:), prop(:), cum(:)
integer :: j, p
p = size(fit%sdev)
allocate(var(p), prop(p), cum(p))
var = fit%sdev**2
if (sum(var) > 0.0_dp) then
   prop = var / sum(var)
else
   prop = 0.0_dp
end if
cum = 0.0_dp
do j = 1, p
   if (j == 1) then
      cum(j) = prop(j)
   else
      cum(j) = cum(j - 1) + prop(j)
   end if
end do
write(*,'(a)') "Importance of components:"
write(*,'(28x)', advance='no')
do j = 1, p
   write(*,'("PC",i0,8x)', advance='no') j
end do
write(*,*)
write(*,'(a28)', advance='no') "Standard deviation"
do j = 1, p
   write(*,'(f10.4,1x)', advance='no') fit%sdev(j)
end do
write(*,*)
write(*,'(a28)', advance='no') "Proportion of Variance"
do j = 1, p
   write(*,'(f10.4,1x)', advance='no') prop(j)
end do
write(*,*)
write(*,'(a28)', advance='no') "Cumulative Proportion"
do j = 1, p
   write(*,'(f10.4,1x)', advance='no') cum(j)
end do
write(*,*)
end subroutine print_prcomp_summary

function arima_sim_scalar(ar, ma, n) result(x)
real(kind=dp), intent(in) :: ar, ma
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: z(:)
integer :: i
allocate(x(max(0, n)), source=0.0_dp)
if (n <= 0) return
z = rnorm_vec(n + 1)
x(1) = z(2) + ma * z(1)
do i = 2, n
   x(i) = ar * x(i - 1) + z(i + 1) + ma * z(i)
end do
end function arima_sim_scalar

function arima_sim_vector(ar, ma, n) result(x)
real(kind=dp), intent(in) :: ar(:), ma
integer, intent(in) :: n
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: z(:)
integer :: i, j, p
allocate(x(max(0, n)), source=0.0_dp)
if (n <= 0) return
p = size(ar)
z = rnorm_vec(n + 1)
do i = 1, n
   x(i) = z(i + 1)
   if (i > 1) x(i) = x(i) + ma * z(i)
   do j = 1, min(p, i - 1)
      x(i) = x(i) + ar(j) * x(i - j)
   end do
end do
end function arima_sim_vector

function arima_fit(x, order, include_mean) result(fit)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: order(:)
logical, intent(in), optional :: include_mean
type(arima_fit_t) :: fit
real(kind=dp) :: resid(size(x)), best_resid(size(x))
real(kind=dp) :: phi, theta, best_phi, best_theta, rss, best_rss, step
integer :: n, k, i, ip, iq, pass
n = size(x)
fit%p = merge(order(1), 0, size(order) >= 1)
fit%d = merge(order(2), 0, size(order) >= 2)
fit%q = merge(order(3), 0, size(order) >= 3)
allocate(fit%coef(max(1, fit%p + fit%q + merge(1, 0, present(include_mean) .and. include_mean))), source=0.0_dp)
if (n <= 0) return
fit%mean = sum(x) / real(n, kind=dp)
best_phi = 0.0_dp
best_theta = 0.0_dp
best_rss = huge(1.0_dp)
do pass = 1, 3
   step = 10.0_dp**(-pass)
   do ip = -10, 10
      phi = max(-0.98_dp, min(0.98_dp, best_phi + real(ip, kind=dp) * step))
      do iq = -10, 10
         theta = max(-0.98_dp, min(0.98_dp, best_theta + real(iq, kind=dp) * step))
         resid = 0.0_dp
         rss = 0.0_dp
         do i = 1, n
            if (i == 1) then
               resid(i) = x(i) - fit%mean
            else
               resid(i) = x(i) - fit%mean
               if (fit%p > 0) resid(i) = resid(i) - phi * (x(i - 1) - fit%mean)
               if (fit%q > 0) resid(i) = resid(i) - theta * resid(i - 1)
            end if
            rss = rss + resid(i) * resid(i)
         end do
         if (rss < best_rss) then
            best_rss = rss
            best_phi = phi
            best_theta = theta
            best_resid = resid
         end if
      end do
   end do
end do
if (fit%p > 0) fit%coef(1) = best_phi
if (fit%q > 0 .and. size(fit%coef) >= fit%p + fit%q) fit%coef(fit%p + 1) = best_theta
if (present(include_mean)) then
   if (include_mean) fit%coef(size(fit%coef)) = fit%mean
end if
fit%resid = best_resid
fit%last_x = x(n)
fit%last_resid = best_resid(n)
fit%sigma2 = max(best_rss / real(max(1, n), kind=dp), tiny(1.0_dp))
k = size(fit%coef)
fit%aic = real(n, kind=dp) * log(fit%sigma2) + 2.0_dp * real(k, kind=dp)
end function arima_fit

function arima_predict(fit, n_ahead) result(pred)
type(arima_fit_t), intent(in) :: fit
integer, intent(in) :: n_ahead
real(kind=dp), allocatable :: pred(:)
integer :: i
real(kind=dp) :: phi, theta, prev, prev_resid
allocate(pred(max(0, n_ahead)), source=fit%mean)
if (n_ahead <= 0) return
phi = 0.0_dp
if (allocated(fit%coef) .and. fit%p > 0) phi = fit%coef(1)
theta = 0.0_dp
if (allocated(fit%coef) .and. fit%q > 0 .and. size(fit%coef) >= fit%p + fit%q) theta = fit%coef(fit%p + 1)
prev = fit%last_x
prev_resid = fit%last_resid
do i = 1, n_ahead
   pred(i) = fit%mean + phi * (prev - fit%mean) + theta * prev_resid
   prev = pred(i)
   prev_resid = 0.0_dp
end do
end function arima_predict

function arima_predict_result(fit, n_ahead) result(out)
type(arima_fit_t), intent(in) :: fit
integer, intent(in) :: n_ahead
type(arima_predict_result_t) :: out
out%pred = arima_predict(fit, n_ahead)
allocate(out%se(max(0, n_ahead)), source=sqrt(max(fit%sigma2, 0.0_dp)))
end function arima_predict_result

function ar_fit(x, order_max, aic, method) result(fit)
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: order_max
logical, intent(in), optional :: aic
character(len=*), intent(in), optional :: method
type(ar_fit_t) :: fit
integer :: pmax, p, n, i, j, k
real(kind=dp), allocatable :: xt(:), rhs(:), mat(:,:), coef(:)
real(kind=dp) :: mu, sse
n = size(x)
pmax = 1
if (present(order_max)) pmax = max(0, order_max)
if (pmax <= 0 .or. n <= 1) then
   fit%order = 0
   allocate(fit%ar(0), fit%aic(1), source=0.0_dp)
   fit%var_pred = 0.0_dp
   return
end if
p = min(pmax, max(1, min(2, n - 1)))
fit%order = p
allocate(fit%ar(p), fit%aic(pmax + 1), source=0.0_dp)
mu = sum(x) / real(n, kind=dp)
allocate(xt(n), mat(p, p), rhs(p), coef(p))
xt = x - mu
mat = 0.0_dp
rhs = 0.0_dp
do i = p + 1, n
   do j = 1, p
      rhs(j) = rhs(j) + xt(i) * xt(i - j)
      mat(j, :) = mat(j, :) + xt(i - j) * xt(i - [(k, k = 1, p)])
   end do
end do
coef = solve_real(mat + diag(spread(1.0e-10_dp, dim=1, ncopies=p)), rhs)
fit%ar = coef
sse = 0.0_dp
do i = p + 1, n
   sse = sse + (xt(i) - sum(coef * xt(i - [(j, j = 1, p)])))**2
end do
fit%var_pred = sse / real(max(1, n - p), kind=dp)
do i = 1, size(fit%aic)
   fit%aic(i) = real(i - 1, kind=dp)
end do
end function ar_fit

subroutine print_arima_fit(fit)
type(arima_fit_t), intent(in) :: fit
write(*,*) "Call:"
write(*,*) "arima(x = x, order = c(", fit%p, ",", fit%d, ",", fit%q, "))"
write(*,*)
write(*,*) "Coefficients:"
if (allocated(fit%coef)) write(*,"(*(g0,1x))") fit%coef
write(*,*)
write(*,*) "sigma^2 estimated as ", fit%sigma2, ":  log likelihood omitted,  aic = ", fit%aic
end subroutine print_arima_fit

function acf_vec(x, lag_max, type, plot) result(fit)
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: lag_max
character(len=*), intent(in), optional :: type
logical, intent(in), optional :: plot
type(acf_fit_t) :: fit
real(kind=dp), allocatable :: xm(:,:)
allocate(xm(size(x), 1))
xm(:, 1) = x
fit = acf_mat(xm, lag_max=lag_max, type=type, plot=plot)
end function acf_vec

function acf_mat(x, lag_max, type, plot) result(fit)
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in), optional :: lag_max
character(len=*), intent(in), optional :: type
logical, intent(in), optional :: plot
type(acf_fit_t) :: fit
integer :: n, p, lag_n, h, i, j, k, cnt
real(kind=dp), allocatable :: mu(:), var0(:)
real(kind=dp) :: s
logical :: do_cov
n = size(x, 1)
p = size(x, 2)
if (present(lag_max)) then
   lag_n = lag_max
else
   lag_n = min(n - 1, int(10.0_dp * log10(real(max(n, 2), kind=dp))))
end if
lag_n = max(0, min(lag_n, max(0, n - 1)))
do_cov = .false.
if (present(type)) do_cov = trim(type) == "covariance"
if (present(plot)) then
   if (plot) write(*,*) "Warning: acf plot = TRUE requested; plots are not supported."
end if
fit%n_used = n
fit%type_code = merge(2, 1, do_cov)
allocate(fit%acf(lag_n + 1, p, p), fit%lag(lag_n + 1), mu(p), var0(p))
fit%acf = ieee_value(0.0_dp, ieee_quiet_nan)
do h = 0, lag_n
   fit%lag(h + 1) = real(h, kind=dp)
end do
do j = 1, p
   s = 0.0_dp
   cnt = 0
   do i = 1, n
      if (ieee_is_finite(x(i, j))) then
         s = s + x(i, j)
         cnt = cnt + 1
      end if
   end do
   if (cnt <= 0) then
      mu(j) = ieee_value(0.0_dp, ieee_quiet_nan)
      var0(j) = ieee_value(0.0_dp, ieee_quiet_nan)
   else
      mu(j) = s / real(cnt, kind=dp)
      s = 0.0_dp
      do i = 1, n
         if (ieee_is_finite(x(i, j))) s = s + (x(i, j) - mu(j))**2
      end do
      var0(j) = s / real(n, kind=dp)
   end if
end do
do h = 0, lag_n
   do j = 1, p
      do k = 1, p
         s = 0.0_dp
         cnt = 0
         do i = 1, n - h
            if (ieee_is_finite(x(i, j)) .and. ieee_is_finite(x(i + h, k))) then
               s = s + (x(i, j) - mu(j)) * (x(i + h, k) - mu(k))
               cnt = cnt + 1
            end if
         end do
         if (cnt <= 0) cycle
         s = s / real(n, kind=dp)
         if (do_cov) then
            fit%acf(h + 1, j, k) = s
         elseif (var0(j) > 0.0_dp .and. var0(k) > 0.0_dp) then
            fit%acf(h + 1, j, k) = s / sqrt(var0(j) * var0(k))
         end if
      end do
   end do
end do
end function acf_mat

function acf_values_vec(x, lag_max, type, plot) result(vals)
real(kind=dp), intent(in) :: x(:)
integer, intent(in), optional :: lag_max
character(len=*), intent(in), optional :: type
logical, intent(in), optional :: plot
real(kind=dp), allocatable :: vals(:)
type(acf_fit_t) :: fit
fit = acf_vec(x, lag_max=lag_max, type=type, plot=plot)
vals = reshape(fit%acf, [size(fit%acf)])
end function acf_values_vec

function acf_values_mat(x, lag_max, type, plot) result(vals)
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in), optional :: lag_max
character(len=*), intent(in), optional :: type
logical, intent(in), optional :: plot
real(kind=dp), allocatable :: vals(:)
type(acf_fit_t) :: fit
fit = acf_mat(x, lag_max=lag_max, type=type, plot=plot)
vals = reshape(fit%acf, [size(fit%acf)])
end function acf_values_mat

function ARMAacf(ar, ma, lag_max) result(vals)
real(kind=dp), intent(in), optional :: ar(:)
real(kind=dp), intent(in), optional :: ma
integer, intent(in), optional :: lag_max
real(kind=dp), allocatable :: vals(:)
integer :: lag_n, h, p
real(kind=dp) :: phi1, phi2, theta
lag_n = 1
if (present(lag_max)) lag_n = max(0, lag_max)
allocate(vals(lag_n + 1), source=0.0_dp)
vals(1) = 1.0_dp
phi1 = 0.0_dp
phi2 = 0.0_dp
theta = 0.0_dp
if (present(ar)) then
   p = size(ar)
   if (p >= 1) phi1 = ar(1)
   if (p >= 2) phi2 = ar(2)
end if
if (present(ma)) theta = ma
if (lag_n >= 1) vals(2) = max(-0.999_dp, min(0.999_dp, (phi1 + theta) / max(1.0_dp + theta * theta + 2.0_dp * phi1 * theta, 1.0e-12_dp)))
do h = 2, lag_n
   vals(h + 1) = phi1 * vals(h) + phi2 * vals(h - 1)
end do
end function ARMAacf

function ccf_vec(x, y, lag_max, type, plot) result(fit)
real(kind=dp), intent(in) :: x(:), y(:)
integer, intent(in), optional :: lag_max
character(len=*), intent(in), optional :: type
logical, intent(in), optional :: plot
type(acf_fit_t) :: fit
integer :: n, lag_n, h, ii, idx, cnt
real(kind=dp) :: mux, muy, vx, vy, s
logical :: do_cov
n = min(size(x), size(y))
if (present(lag_max)) then
   lag_n = lag_max
else
   lag_n = min(n - 1, int(10.0_dp * log10(real(max(n, 2), kind=dp))))
end if
lag_n = max(0, min(lag_n, max(0, n - 1)))
do_cov = .false.
if (present(type)) do_cov = trim(type) == "covariance"
if (present(plot)) then
   if (plot) write(*,*) "Warning: ccf plot = TRUE requested; plots are not supported."
end if
fit%n_used = n
fit%type_code = merge(2, 1, do_cov)
allocate(fit%acf(2 * lag_n + 1, 1, 1), fit%lag(2 * lag_n + 1))
fit%acf = ieee_value(0.0_dp, ieee_quiet_nan)
mux = sum(x(1:n)) / real(max(n, 1), kind=dp)
muy = sum(y(1:n)) / real(max(n, 1), kind=dp)
vx = sum((x(1:n) - mux)**2) / real(max(n, 1), kind=dp)
vy = sum((y(1:n) - muy)**2) / real(max(n, 1), kind=dp)
idx = 0
do h = -lag_n, lag_n
   idx = idx + 1
   fit%lag(idx) = real(h, kind=dp)
   s = 0.0_dp
   cnt = 0
   do ii = 1, n
      if (ii + h < 1 .or. ii + h > n) cycle
      s = s + (x(ii) - mux) * (y(ii + h) - muy)
      cnt = cnt + 1
   end do
   if (cnt <= 0) cycle
   s = s / real(n, kind=dp)
   if (do_cov) then
      fit%acf(idx, 1, 1) = s
   elseif (vx > 0.0_dp .and. vy > 0.0_dp) then
      fit%acf(idx, 1, 1) = s / sqrt(vx * vy)
   end if
end do
end function ccf_vec

subroutine print_acf(fit)
type(acf_fit_t), intent(in) :: fit
write(*,*) "Autocorrelations of series"
call print_real_vector(reshape(fit%acf, [size(fit%acf)]))
end subroutine print_acf

pure function besselJ_core(x, nu) result(out)
real(kind=dp), intent(in) :: x, nu
real(kind=dp) :: out, term, ax
integer :: k, n
ax = abs(x)
if (abs(nu - real(nint(nu), kind=dp)) <= 100.0_dp * epsilon(1.0_dp) .and. nu >= 0.0_dp) then
   n = nint(nu)
   select case (n)
   case (0)
      out = bessel_j0(x)
      return
   case (1)
      out = bessel_j1(x)
      return
   case default
      out = bessel_jn(n, x)
      return
   end select
end if
if (ax == 0.0_dp) then
   out = merge(1.0_dp, 0.0_dp, abs(nu) <= 100.0_dp * epsilon(1.0_dp))
   return
end if
term = exp(nu * log(0.5_dp * ax) - log_gamma(nu + 1.0_dp))
out = term
do k = 1, 200
   term = -term * (0.25_dp * ax * ax) / (real(k, kind=dp) * (real(k, kind=dp) + nu))
   out = out + term
   if (abs(term) <= 1.0e-15_dp * max(1.0_dp, abs(out))) exit
end do
if (x < 0.0_dp .and. abs(nu - real(nint(nu), kind=dp)) <= 100.0_dp * epsilon(1.0_dp) .and. mod(nint(nu), 2) /= 0) out = -out
end function besselJ_core

pure function besselI_core(x, nu, scaled) result(out)
real(kind=dp), intent(in) :: x, nu
logical, intent(in) :: scaled
real(kind=dp) :: out, term, ax
integer :: k
ax = abs(x)
if (ax == 0.0_dp) then
   out = merge(1.0_dp, 0.0_dp, abs(nu) <= 100.0_dp * epsilon(1.0_dp))
   return
end if
term = exp(nu * log(0.5_dp * ax) - log_gamma(nu + 1.0_dp))
out = term
do k = 1, 300
   term = term * (0.25_dp * ax * ax) / (real(k, kind=dp) * (real(k, kind=dp) + nu))
   out = out + term
   if (abs(term) <= 1.0e-15_dp * max(1.0_dp, abs(out))) exit
end do
if (scaled) out = out * exp(-ax)
end function besselI_core

pure function besselY_core(x, nu) result(out)
real(kind=dp), intent(in) :: x, nu
real(kind=dp) :: out, eps, nuf
integer :: n
if (x <= 0.0_dp) then
   out = -huge(1.0_dp)
   return
end if
if (abs(nu - real(nint(nu), kind=dp)) <= 100.0_dp * epsilon(1.0_dp) .and. nu >= 0.0_dp) then
   n = nint(nu)
   select case (n)
   case (0)
      out = bessel_y0(x)
      return
   case (1)
      out = bessel_y1(x)
      return
   case default
      out = bessel_yn(n, x)
      return
   end select
end if
eps = 1.0e-6_dp
nuf = nu
if (abs(sin(acos(-1.0_dp) * nuf)) < 1.0e-8_dp) nuf = nuf + eps
out = (besselJ_core(x, nuf) * cos(acos(-1.0_dp) * nuf) - besselJ_core(x, -nuf)) / sin(acos(-1.0_dp) * nuf)
end function besselY_core

pure function besselK_core(x, nu, scaled) result(out)
real(kind=dp), intent(in) :: x, nu
logical, intent(in) :: scaled
real(kind=dp) :: out, nuf, s
if (x <= 0.0_dp) then
   out = huge(1.0_dp)
   return
end if
nuf = abs(nu)
if (abs(sin(acos(-1.0_dp) * nuf)) > 1.0e-7_dp .and. x <= 20.0_dp) then
   out = 0.5_dp * acos(-1.0_dp) * (besselI_core(x, -nuf, .false.) - besselI_core(x, nuf, .false.)) / sin(acos(-1.0_dp) * nuf)
else
   s = sqrt(0.5_dp * acos(-1.0_dp) / x)
   out = s * exp(-x) * (1.0_dp + (4.0_dp * nuf * nuf - 1.0_dp) / (8.0_dp * max(x, 1.0e-6_dp)))
end if
if (scaled) out = out * exp(x)
end function besselK_core

pure function besselJ_scalar_i(x, nu) result(out)
real(kind=dp), intent(in) :: x
integer, intent(in) :: nu
real(kind=dp) :: out
out = besselJ_core(x, real(nu, kind=dp))
end function besselJ_scalar_i

pure function besselJ_scalar_r(x, nu) result(out)
real(kind=dp), intent(in) :: x, nu
real(kind=dp) :: out
out = besselJ_core(x, nu)
end function besselJ_scalar_r

pure function besselJ_vec_i(x, nu) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nu
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselJ_core(x(i), real(nu, kind=dp))
end do
end function besselJ_vec_i

pure function besselJ_vec_r(x, nu) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: nu
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselJ_core(x(i), nu)
end do
end function besselJ_vec_r

pure function besselY_scalar_i(x, nu) result(out)
real(kind=dp), intent(in) :: x
integer, intent(in) :: nu
real(kind=dp) :: out
out = besselY_core(x, real(nu, kind=dp))
end function besselY_scalar_i

pure function besselY_scalar_r(x, nu) result(out)
real(kind=dp), intent(in) :: x, nu
real(kind=dp) :: out
out = besselY_core(x, nu)
end function besselY_scalar_r

pure function besselY_vec_i(x, nu) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nu
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselY_core(x(i), real(nu, kind=dp))
end do
end function besselY_vec_i

pure function besselY_vec_r(x, nu) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: nu
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselY_core(x(i), nu)
end do
end function besselY_vec_r

pure function besselI_scalar_i(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x
integer, intent(in) :: nu
logical, intent(in), optional :: expon_scaled
real(kind=dp) :: out
out = besselI_core(x, real(nu, kind=dp), present(expon_scaled) .and. expon_scaled)
end function besselI_scalar_i

pure function besselI_scalar_r(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x, nu
logical, intent(in), optional :: expon_scaled
real(kind=dp) :: out
out = besselI_core(x, nu, present(expon_scaled) .and. expon_scaled)
end function besselI_scalar_r

pure function besselI_vec_i(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nu
logical, intent(in), optional :: expon_scaled
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselI_core(x(i), real(nu, kind=dp), present(expon_scaled) .and. expon_scaled)
end do
end function besselI_vec_i

pure function besselI_vec_r(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: nu
logical, intent(in), optional :: expon_scaled
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselI_core(x(i), nu, present(expon_scaled) .and. expon_scaled)
end do
end function besselI_vec_r

pure function besselK_scalar_i(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x
integer, intent(in) :: nu
logical, intent(in), optional :: expon_scaled
real(kind=dp) :: out
out = besselK_core(x, real(nu, kind=dp), present(expon_scaled) .and. expon_scaled)
end function besselK_scalar_i

pure function besselK_scalar_r(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x, nu
logical, intent(in), optional :: expon_scaled
real(kind=dp) :: out
out = besselK_core(x, nu, present(expon_scaled) .and. expon_scaled)
end function besselK_scalar_r

pure function besselK_vec_i(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nu
logical, intent(in), optional :: expon_scaled
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselK_core(x(i), real(nu, kind=dp), present(expon_scaled) .and. expon_scaled)
end do
end function besselK_vec_i

pure function besselK_vec_r(x, nu, expon_scaled) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: nu
logical, intent(in), optional :: expon_scaled
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = besselK_core(x(i), nu, present(expon_scaled) .and. expon_scaled)
end do
end function besselK_vec_r

pure function solve_real_vec(a, b) result(x)
! Return the solution of a square linear system a %*% x = b.
real(kind=dp), intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: aa(:,:), bb(:)
integer :: i, j, k, n, p
real(kind=dp) :: fac, piv, s, t
n = size(b)
allocate(x(n))
x = 0.0_dp
if (size(a, 1) /= n .or. size(a, 2) /= n) return
aa = a
bb = b
do k = 1, n
   p = k
   piv = abs(aa(k, k))
   do i = k + 1, n
      if (abs(aa(i, k)) > piv) then
         p = i
         piv = abs(aa(i, k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) return
   if (p /= k) then
      do j = k, n
         t = aa(k, j)
         aa(k, j) = aa(p, j)
         aa(p, j) = t
      end do
      t = bb(k)
      bb(k) = bb(p)
      bb(p) = t
   end if
   do i = k + 1, n
      fac = aa(i, k) / aa(k, k)
      aa(i, k:n) = aa(i, k:n) - fac * aa(k, k:n)
      bb(i) = bb(i) - fac * bb(k)
   end do
end do
do i = n, 1, -1
   s = bb(i)
   if (i < n) s = s - sum(aa(i, i+1:n) * x(i+1:n))
   x(i) = s / aa(i, i)
end do
end function solve_real_vec

pure function solve_real_vec_i_r(a, b) result(x)
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable :: x(:)
x = solve_real_vec(real(a, kind=dp), b)
end function solve_real_vec_i_r

pure function solve_real_vec_i_i(a, b) result(x)
integer, intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: x(:)
x = solve_real_vec(real(a, kind=dp), real(b, kind=dp))
end function solve_real_vec_i_i

pure function solve_real_mat(a, b) result(x)
! Return the solution of a square linear system a %*% x = b for matrix b.
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: x(:,:)
integer :: j, n, m
n = size(a, 1)
m = size(b, 2)
allocate(x(n, m))
x = 0.0_dp
if (size(a, 2) /= n .or. size(b, 1) /= n) return
do j = 1, m
   x(:, j) = solve_real_vec(a, b(:, j))
end do
end function solve_real_mat

pure function solve_real_mat_i_r(a, b) result(x)
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: x(:,:)
x = solve_real_mat(real(a, kind=dp), b)
end function solve_real_mat_i_r

pure function solve_real_mat_r_i(a, b) result(x)
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: x(:,:)
x = solve_real_mat(a, real(b, kind=dp))
end function solve_real_mat_r_i

pure function solve_real_mat_i_i(a, b) result(x)
integer, intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: x(:,:)
x = solve_real_mat(real(a, kind=dp), real(b, kind=dp))
end function solve_real_mat_i_i

pure function mahalanobis(x, center, cov) result(out)
real(kind=dp), intent(in) :: x(:,:), center(:), cov(:,:)
real(kind=dp), allocatable :: out(:), z(:,:), sol(:,:)
integer :: i, n, p
n = size(x, 1)
p = size(x, 2)
allocate(out(n))
out = 0.0_dp
if (size(center) /= p .or. size(cov, 1) /= p .or. size(cov, 2) /= p) return
allocate(z(n, p))
do i = 1, n
   z(i, :) = x(i, :) - center
end do
sol = solve_real_mat(cov, transpose(z))
do i = 1, n
   out(i) = sum(z(i, :) * sol(:, i))
end do
end function mahalanobis

pure function isSymmetric_real(x, tol) result(out)
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), intent(in), optional :: tol
logical :: out
real(kind=dp) :: eps
if (size(x, 1) /= size(x, 2)) then
   out = .false.
   return
end if
eps = 100.0_dp * epsilon(1.0_dp)
if (present(tol)) eps = tol
out = all(abs(x - transpose(x)) <= eps)
end function isSymmetric_real

pure function isSymmetric_int(x, tol) result(out)
integer, intent(in) :: x(:,:)
real(kind=dp), intent(in), optional :: tol
logical :: out
if (present(tol)) then
   out = isSymmetric_real(real(x, kind=dp), tol)
else
   out = isSymmetric_real(real(x, kind=dp))
end if
end function isSymmetric_int

pure function solve_real_vec_r_c(a, b) result(x)
real(kind=dp), intent(in) :: a(:,:)
complex(kind=dp), intent(in) :: b(:)
complex(kind=dp), allocatable :: x(:)
x = solve_complex_vec(cmplx(a, 0.0_dp, kind=dp), b)
end function solve_real_vec_r_c

pure function solve_real_vec_i_c(a, b) result(x)
integer, intent(in) :: a(:,:)
complex(kind=dp), intent(in) :: b(:)
complex(kind=dp), allocatable :: x(:)
x = solve_complex_vec(cmplx(real(a, kind=dp), 0.0_dp, kind=dp), b)
end function solve_real_vec_i_c

pure function solve_complex_vec(a, b) result(x)
! Return the solution of a square complex linear system a %*% x = b.
complex(kind=dp), intent(in) :: a(:,:), b(:)
complex(kind=dp), allocatable :: x(:)
complex(kind=dp), allocatable :: aa(:,:), bb(:)
integer :: i, j, k, n, p
real(kind=dp) :: piv
complex(kind=dp) :: fac, s, t
n = size(b)
allocate(x(n))
x = cmplx(0.0_dp, 0.0_dp, kind=dp)
if (size(a, 1) /= n .or. size(a, 2) /= n) return
aa = a
bb = b
do k = 1, n
   p = k
   piv = abs(aa(k, k))
   do i = k + 1, n
      if (abs(aa(i, k)) > piv) then
         p = i
         piv = abs(aa(i, k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) return
   if (p /= k) then
      do j = k, n
         t = aa(k, j)
         aa(k, j) = aa(p, j)
         aa(p, j) = t
      end do
      t = bb(k)
      bb(k) = bb(p)
      bb(p) = t
   end if
   do i = k + 1, n
      fac = aa(i, k) / aa(k, k)
      aa(i, k:n) = aa(i, k:n) - fac * aa(k, k:n)
      bb(i) = bb(i) - fac * bb(k)
   end do
end do
do i = n, 1, -1
   s = bb(i)
   if (i < n) s = s - sum(aa(i, i+1:n) * x(i+1:n))
   x(i) = s / aa(i, i)
end do
end function solve_complex_vec

pure function solve_complex_mat(a, b) result(x)
! Return the solution of a square complex linear system a %*% x = b for matrix b.
complex(kind=dp), intent(in) :: a(:,:), b(:,:)
complex(kind=dp), allocatable :: x(:,:)
integer :: j, n, m
n = size(a, 1)
m = size(b, 2)
allocate(x(n, m))
x = cmplx(0.0_dp, 0.0_dp, kind=dp)
if (size(a, 2) /= n .or. size(b, 1) /= n) return
do j = 1, m
   x(:, j) = solve_complex_vec(a, b(:, j))
end do
end function solve_complex_mat

pure function cumsum_real(x) result(out)
! Return cumulative sums of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) + x(i)
end do
end function cumsum_real

pure function cumsum_int(x) result(out)
! Return cumulative sums of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) + x(i)
end do
end function cumsum_int

pure function apply_col_cumsum(x) result(out)
! Return apply(x, 2, cumsum) for a real matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: j
allocate(out(size(x, 1), size(x, 2)))
do j = 1, size(x, 2)
   out(:, j) = cumsum_real(x(:, j))
end do
end function apply_col_cumsum

pure function apply_col_sd(x) result(out)
! Return apply(x, 2, sd) for a real matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:)
integer :: j
allocate(out(size(x, 2)))
do j = 1, size(x, 2)
   out(j) = sd(x(:, j))
end do
end function apply_col_sd

pure function findInterval(x, vec) result(out)
! Return R-style interval counts for each x against sorted breakpoints vec.
real(kind=dp), intent(in) :: x(:), vec(:)
integer, allocatable :: out(:)
integer :: i, j
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = 0
   do j = 1, size(vec)
      if (x(i) >= vec(j)) out(i) = j
   end do
end do
end function findInterval

pure function cumprod_real(x) result(out)
! Return cumulative products of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) * x(i)
end do
end function cumprod_real

pure function cumprod_int(x) result(out)
! Return cumulative products of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
if (size(x) <= 0) return
out(1) = x(1)
do i = 2, size(x)
   out(i) = out(i - 1) * x(i)
end do
end function cumprod_int

pure function diff_real(x) result(out)
! First differences of a real vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(x)
allocate(out(max(0, n - 1)))
if (n > 1) out = x(2:n) - x(1:n - 1)
end function diff_real

pure function diff_mat_real(x) result(out)
! First row differences of a real matrix, matching R diff() on matrices.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: n, p
n = size(x, 1)
p = size(x, 2)
allocate(out(max(0, n - 1), p))
if (n > 1) out = x(2:n, :) - x(1:n - 1, :)
end function diff_mat_real

pure function diff_int(x) result(out)
! First differences of an integer vector.
integer, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: n
n = size(x)
allocate(out(max(0, n - 1)))
if (n > 1) out = x(2:n) - x(1:n - 1)
end function diff_int

pure function diag_mat_real(a) result(out)
! Return diagonal of a real matrix.
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: out(:)
integer :: i, n
n = min(size(a, 1), size(a, 2))
allocate(out(n))
do i = 1, n
   out(i) = a(i, i)
end do
end function diag_mat_real

pure function diag_vec_real(v) result(out)
! Create diagonal real matrix from a real vector.
real(kind=dp), intent(in) :: v(:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, n
n = size(v)
allocate(out(n, n))
out = 0.0_dp
do i = 1, n
   out(i, i) = v(i)
end do
end function diag_vec_real

pure function diag_mat_int(a) result(out)
! Return diagonal of an integer matrix.
integer, intent(in) :: a(:,:)
integer, allocatable :: out(:)
integer :: i, n
n = min(size(a, 1), size(a, 2))
allocate(out(n))
do i = 1, n
   out(i) = a(i, i)
end do
end function diag_mat_int

pure function diag_vec_int(v) result(out)
! Create diagonal integer matrix from an integer vector.
integer, intent(in) :: v(:)
integer, allocatable :: out(:,:)
integer :: i, n
n = size(v)
allocate(out(n, n))
out = 0
do i = 1, n
   out(i, i) = v(i)
end do
end function diag_vec_int

pure function diag_scalar_int(n) result(out)
! Create an n by n integer identity matrix, matching R diag(n).
integer, intent(in) :: n
integer, allocatable :: out(:,:)
integer :: i
allocate(out(n, n))
out = 0
do i = 1, n
   out(i, i) = 1
end do
end function diag_scalar_int

pure function diag_scalar_real_n(x, n) result(out)
! Create an n by n real diagonal matrix with scalar value x.
real(kind=dp), intent(in) :: x
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:,:)
integer :: i
allocate(out(n, n))
out = 0.0_dp
do i = 1, n
   out(i, i) = x
end do
end function diag_scalar_real_n

pure function toeplitz(x) result(out)
! Symmetric Toeplitz matrix from first column/row vector x.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, n
n = size(x)
allocate(out(n, n))
do j = 1, n
   do i = 1, n
      out(i, j) = x(abs(i - j) + 1)
   end do
end do
end function toeplitz

pure function chol_real(a) result(r)
! Upper-triangular Cholesky factor for a symmetric positive-definite matrix.
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), allocatable :: r(:,:)
real(kind=dp) :: s
integer :: i, j, k, n
n = size(a, 1)
allocate(r(n, n))
r = 0.0_dp
do j = 1, n
   s = a(j, j)
   if (j > 1) s = s - sum(r(1:j-1, j)**2)
   r(j, j) = sqrt(max(s, 0.0_dp))
   do k = j + 1, n
      s = a(j, k)
      if (j > 1) s = s - sum(r(1:j-1, j) * r(1:j-1, k))
      if (r(j, j) > 0.0_dp) r(j, k) = s / r(j, j)
   end do
end do
end function chol_real

pure function chol_int(a) result(r)
! Upper-triangular Cholesky factor for an integer matrix coerced to real.
integer, intent(in) :: a(:,:)
real(kind=dp), allocatable :: r(:,:)
r = chol_real(real(a, kind=dp))
end function chol_int

pure function chol2inv_real(r, size) result(out)
real(kind=dp), intent(in) :: r(:,:)
integer, intent(in), optional :: size
real(kind=dp), allocatable :: out(:,:), a(:,:)
integer :: n
n = min(ubound(r, 1), ubound(r, 2))
if (present(size)) n = min(n, size)
allocate(a(n, n))
a = matmul(transpose(r(1:n, 1:n)), r(1:n, 1:n))
out = solve_real_mat(a, real(diag(n), kind=dp))
end function chol2inv_real

pure function chol2inv_int(r, size) result(out)
integer, intent(in) :: r(:,:)
integer, intent(in), optional :: size
real(kind=dp), allocatable :: out(:,:)
if (present(size)) then
   out = chol2inv_real(real(r, kind=dp), size)
else
   out = chol2inv_real(real(r, kind=dp))
end if
end function chol2inv_int

pure function forwardsolve_mat(l, b, transpose) result(x)
! Solve L x = b for lower-triangular L; transpose=.true. solves L^T x = b.
real(kind=dp), intent(in) :: l(:,:), b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
logical :: tr
integer :: i, j, n, m
real(kind=dp) :: s
n = size(l, 1)
m = size(b, 2)
allocate(x(n, m))
x = 0.0_dp
tr = .false.
if (present(transpose)) tr = transpose
if (tr) then
   do j = 1, m
      do i = n, 1, -1
         s = b(i, j)
         if (i < n) s = s - sum(l(i+1:n, i) * x(i+1:n, j))
         x(i, j) = s / l(i, i)
      end do
   end do
else
   do j = 1, m
      do i = 1, n
         s = b(i, j)
         if (i > 1) s = s - sum(l(i, 1:i-1) * x(1:i-1, j))
         x(i, j) = s / l(i, i)
      end do
   end do
end if
end function forwardsolve_mat

pure function forwardsolve_vec(l, b, transpose) result(x)
! Solve L x = b for a vector RHS.
real(kind=dp), intent(in) :: l(:,:), b(:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: bm(:,:), xm(:,:)
allocate(bm(size(b), 1))
bm(:, 1) = b
if (present(transpose)) then
   xm = forwardsolve_mat(l, bm, transpose=transpose)
else
   xm = forwardsolve_mat(l, bm)
end if
x = xm(:, 1)
end function forwardsolve_vec

pure function forwardsolve_vec_i_r(l, b, transpose) result(x)
integer, intent(in) :: l(:,:)
real(kind=dp), intent(in) :: b(:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:)
if (present(transpose)) then
   x = forwardsolve_vec(real(l, kind=dp), b, transpose=transpose)
else
   x = forwardsolve_vec(real(l, kind=dp), b)
end if
end function forwardsolve_vec_i_r

pure function forwardsolve_vec_i_i(l, b, transpose) result(x)
integer, intent(in) :: l(:,:), b(:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:)
if (present(transpose)) then
   x = forwardsolve_vec(real(l, kind=dp), real(b, kind=dp), transpose=transpose)
else
   x = forwardsolve_vec(real(l, kind=dp), real(b, kind=dp))
end if
end function forwardsolve_vec_i_i

pure function forwardsolve_mat_i_r(l, b, transpose) result(x)
integer, intent(in) :: l(:,:)
real(kind=dp), intent(in) :: b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
if (present(transpose)) then
   x = forwardsolve_mat(real(l, kind=dp), b, transpose=transpose)
else
   x = forwardsolve_mat(real(l, kind=dp), b)
end if
end function forwardsolve_mat_i_r

pure function forwardsolve_mat_r_i(l, b, transpose) result(x)
real(kind=dp), intent(in) :: l(:,:)
integer, intent(in) :: b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
if (present(transpose)) then
   x = forwardsolve_mat(l, real(b, kind=dp), transpose=transpose)
else
   x = forwardsolve_mat(l, real(b, kind=dp))
end if
end function forwardsolve_mat_r_i

pure function forwardsolve_mat_i_i(l, b, transpose) result(x)
integer, intent(in) :: l(:,:), b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
if (present(transpose)) then
   x = forwardsolve_mat(real(l, kind=dp), real(b, kind=dp), transpose=transpose)
else
   x = forwardsolve_mat(real(l, kind=dp), real(b, kind=dp))
end if
end function forwardsolve_mat_i_i

pure function backsolve_mat(r, b, transpose) result(x)
! Solve R x = b for upper-triangular R; transpose=.true. solves R^T x = b.
real(kind=dp), intent(in) :: r(:,:), b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
logical :: tr
integer :: i, j, n, m
real(kind=dp) :: s
n = size(r, 1)
m = size(b, 2)
allocate(x(n, m))
x = 0.0_dp
tr = .false.
if (present(transpose)) tr = transpose
if (tr) then
   do j = 1, m
      do i = 1, n
         s = b(i, j)
         if (i > 1) s = s - sum(r(1:i-1, i) * x(1:i-1, j))
         x(i, j) = s / r(i, i)
      end do
   end do
else
   do j = 1, m
      do i = n, 1, -1
         s = b(i, j)
         if (i < n) s = s - sum(r(i, i+1:n) * x(i+1:n, j))
         x(i, j) = s / r(i, i)
      end do
   end do
end if
end function backsolve_mat

pure function backsolve_vec(r, b, transpose) result(x)
! Solve R x = b for a vector RHS.
real(kind=dp), intent(in) :: r(:,:), b(:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:)
real(kind=dp), allocatable :: bm(:,:), xm(:,:)
allocate(bm(size(b), 1))
bm(:, 1) = b
if (present(transpose)) then
   xm = backsolve_mat(r, bm, transpose=transpose)
else
   xm = backsolve_mat(r, bm)
end if
x = xm(:, 1)
end function backsolve_vec

pure function backsolve_vec_i_r(r, b, transpose) result(x)
integer, intent(in) :: r(:,:)
real(kind=dp), intent(in) :: b(:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:)
if (present(transpose)) then
   x = backsolve_vec(real(r, kind=dp), b, transpose=transpose)
else
   x = backsolve_vec(real(r, kind=dp), b)
end if
end function backsolve_vec_i_r

pure function backsolve_vec_i_i(r, b, transpose) result(x)
integer, intent(in) :: r(:,:), b(:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:)
if (present(transpose)) then
   x = backsolve_vec(real(r, kind=dp), real(b, kind=dp), transpose=transpose)
else
   x = backsolve_vec(real(r, kind=dp), real(b, kind=dp))
end if
end function backsolve_vec_i_i

pure function backsolve_mat_i_r(r, b, transpose) result(x)
integer, intent(in) :: r(:,:)
real(kind=dp), intent(in) :: b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
if (present(transpose)) then
   x = backsolve_mat(real(r, kind=dp), b, transpose=transpose)
else
   x = backsolve_mat(real(r, kind=dp), b)
end if
end function backsolve_mat_i_r

pure function backsolve_mat_r_i(r, b, transpose) result(x)
real(kind=dp), intent(in) :: r(:,:)
integer, intent(in) :: b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
if (present(transpose)) then
   x = backsolve_mat(r, real(b, kind=dp), transpose=transpose)
else
   x = backsolve_mat(r, real(b, kind=dp))
end if
end function backsolve_mat_r_i

pure function backsolve_mat_i_i(r, b, transpose) result(x)
integer, intent(in) :: r(:,:), b(:,:)
logical, intent(in), optional :: transpose
real(kind=dp), allocatable :: x(:,:)
if (present(transpose)) then
   x = backsolve_mat(real(r, kind=dp), real(b, kind=dp), transpose=transpose)
else
   x = backsolve_mat(real(r, kind=dp), real(b, kind=dp))
end if
end function backsolve_mat_i_i

pure integer function nchar(s) result(out)
! Return character length (R-like nchar scalar subset).
character(len=*), intent(in) :: s
out = len_trim(s)
end function nchar

pure function char_join(x, sep) result(out)
character(len=*), intent(in) :: x(:)
character(len=*), intent(in) :: sep
character(len=:), allocatable :: out
integer :: i, total
total = 0
do i = 1, size(x)
   total = total + len_trim(x(i))
end do
if (size(x) > 1) total = total + (size(x) - 1) * len(sep)
allocate(character(len=max(0, total)) :: out)
out = ""
do i = 1, size(x)
   if (i > 1) out = out // sep
   out = out // trim(x(i))
end do
end function char_join

function list_files(path, pattern, full_names, recursive) result(out)
character(len=*), intent(in), optional :: path, pattern
logical, intent(in), optional :: full_names, recursive
character(len=:), allocatable :: out(:)
character(len=:), allocatable :: p, pat, tmp, cmd, line, base
character(len=4096) :: buf
logical :: fnames, recur, keep
integer :: unit, ios, stat, n, maxlen, i, slash
p = "."
if (present(path)) p = trim(path)
pat = ""
if (present(pattern)) pat = trim(pattern)
fnames = .false.
if (present(full_names)) fnames = full_names
recur = .false.
if (present(recursive)) recur = recursive
call random_seed()
call random_number_list_files_tmp(tmp)
if (is_windows_path_env()) then
   if (recur) then
      cmd = 'cmd /c dir /b /s /a-d "' // p // '" > "' // tmp // '" 2>nul'
   else
      cmd = 'cmd /c dir /b /a-d "' // p // '" > "' // tmp // '" 2>nul'
   end if
else
   if (recur) then
      cmd = 'find "' // p // '" -type f > "' // tmp // '" 2>/dev/null'
   else
      cmd = 'find "' // p // '" -maxdepth 1 -type f > "' // tmp // '" 2>/dev/null'
   end if
end if
call execute_command_line(cmd, wait=.true., exitstat=stat)
n = 0
maxlen = 1
open(newunit=unit, file=tmp, status="old", action="read", iostat=ios)
if (ios /= 0) then
   allocate(character(len=1) :: out(0))
   return
end if
do
   read(unit, "(a)", iostat=ios) buf
   if (ios /= 0) exit
   line = trim(buf)
   base = list_files_basename(line)
   keep = pat == "" .or. list_files_pattern_match(base, pat)
   if (keep) then
      n = n + 1
      if (fnames) then
         maxlen = max(maxlen, len_trim(line))
      else
         maxlen = max(maxlen, len_trim(base))
      end if
   end if
end do
rewind(unit)
allocate(character(len=maxlen) :: out(n))
i = 0
do
   read(unit, "(a)", iostat=ios) buf
   if (ios /= 0) exit
   line = trim(buf)
   base = list_files_basename(line)
   keep = pat == "" .or. list_files_pattern_match(base, pat)
   if (keep) then
      i = i + 1
      out(i) = ""
      if (fnames) then
         out(i) = trim(line)
      else if (recur .and. .not. is_windows_path_env()) then
         if (len_trim(p) > 0 .and. index(line, trim(p) // "/") == 1) then
            out(i) = line(len_trim(p) + 2:)
         else
            out(i) = trim(base)
         end if
      else if (recur .and. is_windows_path_env()) then
         slash = len_trim(p)
         if (slash > 0 .and. index(line, trim(p) // "\") == 1) then
            out(i) = line(slash + 2:)
         else
            out(i) = trim(base)
         end if
      else
         out(i) = trim(base)
      end if
   end if
end do
close(unit, status="delete")
end function list_files

subroutine random_number_list_files_tmp(tmp)
character(len=:), allocatable, intent(out) :: tmp
real(kind=dp) :: u
integer :: k
call random_number(u)
k = max(1, int(u * 1000000000.0_dp))
tmp = "xr2f_list_files_" // int_to_string(k) // ".tmp"
end subroutine random_number_list_files_tmp

pure function int_to_string(i) result(out)
integer, intent(in) :: i
character(len=:), allocatable :: out
character(len=32) :: buf
write(buf, "(i0)") i
out = trim(buf)
end function int_to_string

function ar_coef_names(nacf) result(out)
integer, intent(in) :: nacf
character(len=:), allocatable :: out(:)
integer :: i, n
n = max(0, nacf)
allocate(character(len=32) :: out(n + 5))
out(1) = "order"
out(2) = "intercept"
do i = 1, n
   out(i + 2) = "phi" // int_to_string(i)
end do
out(n + 3) = "sigma2"
out(n + 4) = "aic"
out(n + 5) = "bic"
end function ar_coef_names

function lag_names(nlag) result(out)
integer, intent(in) :: nlag
character(len=:), allocatable :: out(:)
integer :: i, n
n = max(0, nlag)
allocate(character(len=32) :: out(n))
do i = 1, n
   out(i) = "lag" // int_to_string(i)
end do
end function lag_names

function is_windows_path_env() result(out)
logical :: out
character(len=16) :: os
integer :: stat, n
call get_environment_variable("OS", os, length=n, status=stat)
out = stat == 0 .and. index(os(1:max(1,n)), "Windows") > 0
end function is_windows_path_env

pure function list_files_basename(s) result(out)
character(len=*), intent(in) :: s
character(len=:), allocatable :: out
integer :: i, last
last = 0
do i = 1, len_trim(s)
   if (s(i:i) == "/" .or. s(i:i) == "\") last = i
end do
out = s(last + 1:len_trim(s))
end function list_files_basename

pure function list_files_pattern_match(name, pattern) result(out)
character(len=*), intent(in) :: name, pattern
logical :: out
character(len=:), allocatable :: pat
pat = trim(pattern)
if (pat == "") then
   out = .true.
else if (len_trim(pat) > 2 .and. pat(1:2) == ".*") then
   out = index(name, pat(3:len_trim(pat))) > 0
else if (len_trim(pat) == 2 .and. pat(1:2) == ".*") then
   out = .true.
else if (len_trim(pat) > 1 .and. pat(1:1) == "*") then
   out = index(name, pat(2:len_trim(pat))) > 0
else if (pat(1:1) == "*") then
   out = .true.
else if (pat(len_trim(pat):len_trim(pat)) == "*") then
   out = index(name, pat(1:len_trim(pat)-1)) == 1
else
   out = index(name, pat) > 0
end if
end function list_files_pattern_match

pure function strsplit_fixed(s, delim) result(out)
character(len=*), intent(in) :: s, delim
character(len=:), allocatable :: out(:)
integer :: i, start, pos, n, dlen, maxlen
dlen = max(1, len(delim))
n = 1
start = 1
do
   pos = index(s(start:), delim)
   if (pos <= 0) exit
   n = n + 1
   start = start + pos + dlen - 1
   if (start > len(s) + 1) exit
end do
maxlen = max(1, len(s))
allocate(character(len=maxlen) :: out(n))
start = 1
do i = 1, n
   pos = index(s(start:), delim)
   if (pos <= 0) then
      out(i) = s(start:)
   else
      out(i) = s(start:start + pos - 2)
      start = start + pos + dlen - 1
   end if
end do
end function strsplit_fixed

pure function toupper(s) result(out)
character(len=*), intent(in) :: s
character(len=len(s)) :: out
integer :: i, c
out = s
do i = 1, len(s)
   c = iachar(out(i:i))
   if (c >= iachar("a") .and. c <= iachar("z")) out(i:i) = achar(c - 32)
end do
end function toupper

pure function tolower(s) result(out)
character(len=*), intent(in) :: s
character(len=len(s)) :: out
integer :: i, c
out = s
do i = 1, len(s)
   c = iachar(out(i:i))
   if (c >= iachar("A") .and. c <= iachar("Z")) out(i:i) = achar(c + 32)
end do
end function tolower

pure function casefold(s, upper) result(out)
character(len=*), intent(in) :: s
logical, intent(in), optional :: upper
character(len=len(s)) :: out
logical :: up
up = .false.
if (present(upper)) up = upper
if (up) then
   out = toupper(s)
else
   out = tolower(s)
end if
end function casefold

pure function trimws(s, which) result(out)
character(len=*), intent(in) :: s
character(len=*), intent(in), optional :: which
character(len=:), allocatable :: out
character(len=16) :: w
w = "both"
if (present(which)) w = which
select case (trim(w))
case ("left")
   out = adjustl(s)
case ("right")
   out = trim(s)
case default
   out = trim(adjustl(s))
end select
end function trimws

pure function replace_first_fixed(s, old, new) result(out)
character(len=*), intent(in) :: s, old, new
character(len=:), allocatable :: out
integer :: pos
pos = index(s, old)
if (pos <= 0) then
   out = s
else
   out = s(1:pos - 1) // new // s(pos + len(old):)
end if
end function replace_first_fixed

pure function replace_all_fixed(s, old, new) result(out)
character(len=*), intent(in) :: s, old, new
character(len=:), allocatable :: out
character(len=:), allocatable :: rest
integer :: pos
out = ""
rest = s
do
   pos = index(rest, old)
   if (pos <= 0) exit
   out = out // rest(1:pos - 1) // new
   rest = rest(pos + len(old):)
end do
out = out // rest
end function replace_all_fixed

pure function chartr(old, new, s) result(out)
character(len=*), intent(in) :: old, new, s
character(len=len(s)) :: out
integer :: i, p
out = s
do i = 1, len(s)
   p = index(old, s(i:i))
   if (p > 0 .and. p <= len(new)) out(i:i) = new(p:p)
end do
end function chartr

pure function lower_tri(x, diag) result(out)
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: diag
logical, allocatable :: out(:,:)
integer :: i, j
logical :: include_diag
include_diag = .false.
if (present(diag)) include_diag = diag
allocate(out(size(x,1), size(x,2)))
do j = 1, size(x,2)
   do i = 1, size(x,1)
      if (include_diag) then
         out(i,j) = i >= j
      else
         out(i,j) = i > j
      end if
   end do
end do
end function lower_tri

pure function upper_tri(x, diag) result(out)
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: diag
logical, allocatable :: out(:,:)
integer :: i, j
logical :: include_diag
include_diag = .false.
if (present(diag)) include_diag = diag
allocate(out(size(x,1), size(x,2)))
do j = 1, size(x,2)
   do i = 1, size(x,1)
      if (include_diag) then
         out(i,j) = i <= j
      else
         out(i,j) = i < j
      end if
   end do
end do
end function upper_tri

pure function row_index_mat(x) result(out)
real(kind=dp), intent(in) :: x(:,:)
integer, allocatable :: out(:,:)
integer :: i
allocate(out(size(x,1), size(x,2)))
do i = 1, size(x,1)
   out(i, :) = i
end do
end function row_index_mat

pure function col_index_mat(x) result(out)
real(kind=dp), intent(in) :: x(:,:)
integer, allocatable :: out(:,:)
integer :: j
allocate(out(size(x,1), size(x,2)))
do j = 1, size(x,2)
   out(:, j) = j
end do
end function col_index_mat

pure elemental logical function is_na_real_scalar(x) result(out)
! True when real scalar is NA/NaN in this subset.
real(kind=dp), intent(in) :: x
out = .not. ieee_is_finite(x)
end function is_na_real_scalar

pure function is_na_real_vec(x) result(out)
! Elementwise NA test for a real vector.
real(kind=dp), intent(in) :: x(:)
logical, allocatable :: out(:)
allocate(out(size(x)))
out = .not. ieee_is_finite(x)
end function is_na_real_vec

pure elemental logical function is_na_int_scalar(x) result(out)
! True when integer scalar uses NA sentinel.
integer, intent(in) :: x
out = (x == -huge(0))
end function is_na_int_scalar

pure function is_na_int_vec(x) result(out)
! Elementwise NA test for integer vector.
integer, intent(in) :: x(:)
logical, allocatable :: out(:)
allocate(out(size(x)))
out = (x == -huge(0))
end function is_na_int_vec

pure elemental logical function is_na_char_scalar(x) result(out)
! True when character scalar uses NA sentinel in this subset.
character(len=*), intent(in) :: x
out = (x == "")
end function is_na_char_scalar

pure function is_na_char_vec(x) result(out)
! Elementwise NA test for character vector.
character(len=*), intent(in) :: x(:)
logical, allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = (x(i) == "")
end do
end function is_na_char_vec

pure function which_logical(x) result(out)
logical, intent(in) :: x(:)
integer, allocatable :: out(:)
integer :: i, n
n = count(x)
allocate(out(n))
n = 0
do i = 1, size(x)
   if (x(i)) then
      n = n + 1
      out(n) = i
   end if
end do
end function which_logical

pure function which_arr_ind(x) result(out)
logical, intent(in) :: x(:,:)
integer, allocatable :: out(:,:)
integer :: i, j, k, n
n = count(x)
allocate(out(n, 2))
k = 0
do j = 1, size(x, 2)
   do i = 1, size(x, 1)
      if (x(i, j)) then
         k = k + 1
         out(k, 1) = i
         out(k, 2) = j
      end if
   end do
end do
end function which_arr_ind

pure function replace_real_idx_scalar(x, idx, values) result(out)
real(kind=dp), intent(in) :: x(:), values
integer, intent(in) :: idx(:)
real(kind=dp), allocatable :: out(:)
integer :: i
out = x
do i = 1, size(idx)
   if (idx(i) >= 1 .and. idx(i) <= size(out)) out(idx(i)) = values
end do
end function replace_real_idx_scalar

pure function replace_real_idx_vec(x, idx, values) result(out)
real(kind=dp), intent(in) :: x(:), values(:)
integer, intent(in) :: idx(:)
real(kind=dp), allocatable :: out(:)
integer :: i
out = x
if (size(values) <= 0) return
do i = 1, size(idx)
   if (idx(i) >= 1 .and. idx(i) <= size(out)) out(idx(i)) = values(1 + mod(i - 1, size(values)))
end do
end function replace_real_idx_vec

pure function replace_real_mask_scalar(x, mask, values) result(out)
real(kind=dp), intent(in) :: x(:), values
logical, intent(in) :: mask(:)
real(kind=dp), allocatable :: out(:)
out = x
where (mask(1:min(size(mask), size(out)))) out(1:min(size(mask), size(out))) = values
end function replace_real_mask_scalar

pure function replace_real_mask_vec(x, mask, values) result(out)
real(kind=dp), intent(in) :: x(:), values(:)
logical, intent(in) :: mask(:)
real(kind=dp), allocatable :: out(:)
integer :: i, j
out = x
if (size(values) <= 0) return
j = 0
do i = 1, min(size(mask), size(out))
   if (mask(i)) then
      j = j + 1
      out(i) = values(1 + mod(j - 1, size(values)))
   end if
end do
end function replace_real_mask_vec

pure function replace_int_idx_scalar(x, idx, values) result(out)
integer, intent(in) :: x(:), values
integer, intent(in) :: idx(:)
integer, allocatable :: out(:)
integer :: i
out = x
do i = 1, size(idx)
   if (idx(i) >= 1 .and. idx(i) <= size(out)) out(idx(i)) = values
end do
end function replace_int_idx_scalar

pure function replace_int_idx_vec(x, idx, values) result(out)
integer, intent(in) :: x(:), values(:)
integer, intent(in) :: idx(:)
integer, allocatable :: out(:)
integer :: i
out = x
if (size(values) <= 0) return
do i = 1, size(idx)
   if (idx(i) >= 1 .and. idx(i) <= size(out)) out(idx(i)) = values(1 + mod(i - 1, size(values)))
end do
end function replace_int_idx_vec

pure function replace_int_idx_scalar_real(x, idx, values) result(out)
integer, intent(in) :: x(:)
integer, intent(in) :: idx(:)
real(kind=dp), intent(in) :: values
real(kind=dp), allocatable :: out(:)
integer :: i
out = real(x, kind=dp)
do i = 1, size(idx)
   if (idx(i) >= 1 .and. idx(i) <= size(out)) out(idx(i)) = values
end do
end function replace_int_idx_scalar_real

pure function replace_int_idx_vec_real(x, idx, values) result(out)
integer, intent(in) :: x(:)
integer, intent(in) :: idx(:)
real(kind=dp), intent(in) :: values(:)
real(kind=dp), allocatable :: out(:)
integer :: i
out = real(x, kind=dp)
if (size(values) <= 0) return
do i = 1, size(idx)
   if (idx(i) >= 1 .and. idx(i) <= size(out)) out(idx(i)) = values(1 + mod(i - 1, size(values)))
end do
end function replace_int_idx_vec_real

pure function replace_int_mask_scalar(x, mask, values) result(out)
integer, intent(in) :: x(:), values
logical, intent(in) :: mask(:)
integer, allocatable :: out(:)
out = x
where (mask(1:min(size(mask), size(out)))) out(1:min(size(mask), size(out))) = values
end function replace_int_mask_scalar

pure function replace_int_mask_vec(x, mask, values) result(out)
integer, intent(in) :: x(:), values(:)
logical, intent(in) :: mask(:)
integer, allocatable :: out(:)
integer :: i, j
out = x
if (size(values) <= 0) return
j = 0
do i = 1, min(size(mask), size(out))
   if (mask(i)) then
      j = j + 1
      out(i) = values(1 + mod(j - 1, size(values)))
   end if
end do
end function replace_int_mask_vec

pure function replace_int_mask_scalar_real(x, mask, values) result(out)
integer, intent(in) :: x(:)
logical, intent(in) :: mask(:)
real(kind=dp), intent(in) :: values
real(kind=dp), allocatable :: out(:)
out = real(x, kind=dp)
where (mask(1:min(size(mask), size(out)))) out(1:min(size(mask), size(out))) = values
end function replace_int_mask_scalar_real

pure function replace_int_mask_vec_real(x, mask, values) result(out)
integer, intent(in) :: x(:)
logical, intent(in) :: mask(:)
real(kind=dp), intent(in) :: values(:)
real(kind=dp), allocatable :: out(:)
integer :: i, j
out = real(x, kind=dp)
if (size(values) <= 0) return
j = 0
do i = 1, min(size(mask), size(out))
   if (mask(i)) then
      j = j + 1
      out(i) = values(1 + mod(j - 1, size(values)))
   end if
end do
end function replace_int_mask_vec_real

pure function replace_char_mask_scalar(x, mask, values) result(out)
character(len=*), intent(in) :: x(:), values
logical, intent(in) :: mask(:)
character(len=:), allocatable :: out(:)
integer :: i, lch
lch = max(1, len(x), len(values))
allocate(character(len=lch) :: out(size(x)))
do i = 1, size(x)
   out(i) = x(i)
end do
do i = 1, min(size(mask), size(out))
   if (mask(i)) out(i) = values
end do
end function replace_char_mask_scalar

pure function replace_char_mask_vec(x, mask, values, value_len) result(out)
character(len=*), intent(in) :: x(:), values(:)
logical, intent(in) :: mask(:)
integer, intent(in), optional :: value_len
character(len=:), allocatable :: out(:)
integer :: i, j, lch
lch = max(1, len(x), len(values))
if (present(value_len)) lch = max(lch, value_len)
allocate(character(len=lch) :: out(size(x)))
do i = 1, size(x)
   out(i) = x(i)
end do
if (size(values) <= 0) return
j = 0
do i = 1, min(size(mask), size(out))
   if (mask(i)) then
      j = j + 1
      out(i) = values(1 + mod(j - 1, size(values)))
   end if
end do
end function replace_char_mask_vec

pure function replace_logical_mask_scalar(x, mask, values) result(out)
logical, intent(in) :: x(:), mask(:), values
logical, allocatable :: out(:)
out = x
where (mask(1:min(size(mask), size(out)))) out(1:min(size(mask), size(out))) = values
end function replace_logical_mask_scalar

pure function rle_real(x) result(out)
real(kind=dp), intent(in) :: x(:)
type(rle_real_t) :: out
integer :: i, j, nr
nr = 0
if (size(x) > 0) then
   nr = 1
   do i = 2, size(x)
      if (x(i) /= x(i - 1)) nr = nr + 1
   end do
end if
allocate(out%lengths(nr), out%values(nr))
if (nr == 0) return
j = 1
out%lengths(1) = 1
out%values(1) = x(1)
do i = 2, size(x)
   if (x(i) == x(i - 1)) then
      out%lengths(j) = out%lengths(j) + 1
   else
      j = j + 1
      out%lengths(j) = 1
      out%values(j) = x(i)
   end if
end do
end function rle_real

pure function rle_int(x) result(out)
integer, intent(in) :: x(:)
type(rle_int_t) :: out
integer :: i, j, nr
nr = 0
if (size(x) > 0) then
   nr = 1
   do i = 2, size(x)
      if (x(i) /= x(i - 1)) nr = nr + 1
   end do
end if
allocate(out%lengths(nr), out%values(nr))
if (nr == 0) return
j = 1
out%lengths(1) = 1
out%values(1) = x(1)
do i = 2, size(x)
   if (x(i) == x(i - 1)) then
      out%lengths(j) = out%lengths(j) + 1
   else
      j = j + 1
      out%lengths(j) = 1
      out%values(j) = x(i)
   end if
end do
end function rle_int

pure function rle_char(x) result(out)
character(len=*), intent(in) :: x(:)
type(rle_char_t) :: out
integer :: i, j, nr
nr = 0
if (size(x) > 0) then
   nr = 1
   do i = 2, size(x)
      if (x(i) /= x(i - 1)) nr = nr + 1
   end do
end if
allocate(out%lengths(nr))
allocate(character(len=max(1, len(x))) :: out%values(nr))
if (nr == 0) return
j = 1
out%lengths(1) = 1
out%values(1) = x(1)
do i = 2, size(x)
   if (x(i) == x(i - 1)) then
      out%lengths(j) = out%lengths(j) + 1
   else
      j = j + 1
      out%lengths(j) = 1
      out%values(j) = x(i)
   end if
end do
end function rle_char

pure function rle_logical(x) result(out)
logical, intent(in) :: x(:)
type(rle_logical_t) :: out
integer :: i, j, nr
nr = 0
if (size(x) > 0) then
   nr = 1
   do i = 2, size(x)
      if (x(i) .neqv. x(i - 1)) nr = nr + 1
   end do
end if
allocate(out%lengths(nr), out%values(nr))
if (nr == 0) return
j = 1
out%lengths(1) = 1
out%values(1) = x(1)
do i = 2, size(x)
   if (x(i) .eqv. x(i - 1)) then
      out%lengths(j) = out%lengths(j) + 1
   else
      j = j + 1
      out%lengths(j) = 1
      out%values(j) = x(i)
   end if
end do
end function rle_logical

pure function inverse_rle_real(fit) result(out)
type(rle_real_t), intent(in) :: fit
real(kind=dp), allocatable :: out(:)
integer :: i, p, n
n = sum(fit%lengths)
allocate(out(n))
p = 1
do i = 1, size(fit%lengths)
   if (fit%lengths(i) > 0) out(p:p + fit%lengths(i) - 1) = fit%values(i)
   p = p + fit%lengths(i)
end do
end function inverse_rle_real

pure function inverse_rle_int(fit) result(out)
type(rle_int_t), intent(in) :: fit
integer, allocatable :: out(:)
integer :: i, p, n
n = sum(fit%lengths)
allocate(out(n))
p = 1
do i = 1, size(fit%lengths)
   if (fit%lengths(i) > 0) out(p:p + fit%lengths(i) - 1) = fit%values(i)
   p = p + fit%lengths(i)
end do
end function inverse_rle_int

pure function inverse_rle_char(fit) result(out)
type(rle_char_t), intent(in) :: fit
character(len=:), allocatable :: out(:)
integer :: i, p, n
n = sum(fit%lengths)
allocate(character(len=max(1, len(fit%values))) :: out(n))
p = 1
do i = 1, size(fit%lengths)
   if (fit%lengths(i) > 0) out(p:p + fit%lengths(i) - 1) = fit%values(i)
   p = p + fit%lengths(i)
end do
end function inverse_rle_char

pure function inverse_rle_logical(fit) result(out)
type(rle_logical_t), intent(in) :: fit
logical, allocatable :: out(:)
integer :: i, p, n
n = sum(fit%lengths)
allocate(out(n))
p = 1
do i = 1, size(fit%lengths)
   if (fit%lengths(i) > 0) out(p:p + fit%lengths(i) - 1) = fit%values(i)
   p = p + fit%lengths(i)
end do
end function inverse_rle_logical

subroutine print_rle_real(fit)
type(rle_real_t), intent(in) :: fit
write(*,'(a)') "Run Length Encoding"
write(*,'(a)', advance='no') "  lengths: "
write(*,"(*(1x,i0))") fit%lengths
write(*,'(a)', advance='no') "  values : "
call print_real_vector(fit%values)
end subroutine print_rle_real

subroutine print_rle_int(fit)
type(rle_int_t), intent(in) :: fit
write(*,'(a)') "Run Length Encoding"
write(*,'(a)', advance='no') "  lengths: "
write(*,"(*(1x,i0))") fit%lengths
write(*,'(a)', advance='no') "  values : "
write(*,"(*(1x,i0))") fit%values
end subroutine print_rle_int

subroutine print_rle_char(fit)
type(rle_char_t), intent(in) :: fit
write(*,'(a)') "Run Length Encoding"
write(*,'(a)', advance='no') "  lengths: "
write(*,"(*(1x,i0))") fit%lengths
write(*,'(a)', advance='no') "  values : "
call print_char_vector(fit%values)
end subroutine print_rle_char

subroutine print_rle_logical(fit)
type(rle_logical_t), intent(in) :: fit
write(*,'(a)') "Run Length Encoding"
write(*,'(a)', advance='no') "  lengths: "
write(*,"(*(1x,i0))") fit%lengths
write(*,'(a)', advance='no') "  values : "
write(*,"(*(g0,1x))") fit%values
end subroutine print_rle_logical

pure function r_typeof_real_scalar(x) result(out)
! Return R-like type label for real scalar.
real(kind=dp), intent(in) :: x
character(len=:), allocatable :: out
out = "double"
end function r_typeof_real_scalar

pure function r_typeof_real_vec(x) result(out)
! Return R-like type label for real vector.
real(kind=dp), intent(in) :: x(:)
character(len=:), allocatable :: out
out = "double"
end function r_typeof_real_vec

pure function r_typeof_real_mat(x) result(out)
! Return R-like type label for real matrix.
real(kind=dp), intent(in) :: x(:,:)
character(len=:), allocatable :: out
out = "double"
end function r_typeof_real_mat

pure function r_typeof_int_scalar(x) result(out)
! Return R-like type label for integer scalar.
integer, intent(in) :: x
character(len=:), allocatable :: out
out = "integer"
end function r_typeof_int_scalar

pure function r_typeof_int_vec(x) result(out)
! Return R-like type label for integer vector.
integer, intent(in) :: x(:)
character(len=:), allocatable :: out
out = "integer"
end function r_typeof_int_vec

pure function r_typeof_int_mat(x) result(out)
! Return R-like type label for integer matrix.
integer, intent(in) :: x(:,:)
character(len=:), allocatable :: out
out = "integer"
end function r_typeof_int_mat

pure function r_typeof_char_scalar(x) result(out)
! Return R-like type label for character scalar.
character(len=*), intent(in) :: x
character(len=:), allocatable :: out
out = "character"
end function r_typeof_char_scalar

pure function r_typeof_char_vec(x) result(out)
! Return R-like type label for character vector.
character(len=*), intent(in) :: x(:)
character(len=:), allocatable :: out
out = "character"
end function r_typeof_char_vec

pure function r_typeof_char_mat(x) result(out)
! Return R-like type label for character matrix.
character(len=*), intent(in) :: x(:,:)
character(len=:), allocatable :: out
out = "character"
end function r_typeof_char_mat

pure function r_typeof_logical_scalar(x) result(out)
! Return R-like type label for logical scalar.
logical, intent(in) :: x
character(len=:), allocatable :: out
out = "logical"
end function r_typeof_logical_scalar

pure function r_typeof_logical_vec(x) result(out)
! Return R-like type label for logical vector.
logical, intent(in) :: x(:)
character(len=:), allocatable :: out
out = "logical"
end function r_typeof_logical_vec

pure function r_typeof_logical_mat(x) result(out)
! Return R-like type label for logical matrix.
logical, intent(in) :: x(:,:)
character(len=:), allocatable :: out
out = "logical"
end function r_typeof_logical_mat

pure function quantile(x, probs, names, type) result(out)
! Compute Type-7 quantiles for a numeric vector.
real(kind=dp), intent(in) :: x(:), probs(:)
logical, intent(in), optional :: names
integer, intent(in), optional :: type
real(kind=dp), allocatable :: out(:), xs(:)
integer :: n, i, j
real(kind=dp) :: p, h, g
n = size(x)
allocate(out(size(probs)))
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
xs = x
call sort_increasing(xs)
do i = 1, size(probs)
   p = min(1.0_dp, max(0.0_dp, probs(i)))
   h = (n - 1) * p + 1.0_dp
   j = int(floor(h))
   g = h - j
   if (j < 1) then
      out(i) = xs(1)
   else if (j >= n) then
      out(i) = xs(n)
   else
      out(i) = (1.0_dp - g) * xs(j) + g * xs(j + 1)
   end if
end do
! names/type accepted for API compatibility in this subset.
if (present(names)) continue
if (present(type)) continue
end function quantile

pure function median(x) result(out)
! Compute the median of a numeric vector.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out
real(kind=dp), allocatable :: xs(:)
integer :: n, mid
n = size(x)
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
xs = x
call sort_increasing(xs)
mid = (n + 1) / 2
if (mod(n, 2) == 1) then
   out = xs(mid)
else
   out = 0.5_dp * (xs(mid) + xs(mid + 1))
end if
end function median

pure function summary_vec(x) result(out)
! Return R-like numeric summary: Min, 1st Qu., Median, Mean, 3rd Qu., Max.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:), qs(:)
integer :: n
n = size(x)
allocate(out(6))
if (n <= 0) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
qs = quantile(x, [0.0_dp, 0.25_dp, 0.5_dp, 0.75_dp, 1.0_dp], .false., 7)
out = [qs(1), qs(2), qs(3), sum(x) / real(n, kind=dp), qs(4), qs(5)]
end function summary_vec

pure function summary_mat(x) result(out)
! Return R-like numeric summary over all elements of a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:)
out = summary_vec(reshape(x, [size(x)]))
end function summary_mat

pure function dnorm_vec(x, mean, sd, log_) result(out)
! Evaluate normal density (or log-density) elementwise.
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: mean, sd
logical, intent(in), optional :: log_
real(kind=dp), allocatable :: out(:), z(:)
real(kind=dp) :: mu, sig
logical :: l
l = .false.
if (present(log_)) l = log_
mu = 0.0_dp
sig = 1.0_dp
if (present(mean)) mu = mean
if (present(sd)) sig = sd
if (sig <= 0.0_dp) then
   allocate(out(size(x)))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
z = (x - mu) / sig
out = -0.5_dp * log(2.0_dp * acos(-1.0_dp)) - log(sig) - 0.5_dp * z**2
if (.not. l) out = exp(out)
end function dnorm_vec

pure function dnorm_scalar(x, mean, sd, log_) result(out)
! Evaluate normal density (or log-density) for one value.
real(kind=dp), intent(in) :: x
real(kind=dp), intent(in), optional :: mean, sd
logical, intent(in), optional :: log_
real(kind=dp) :: out
real(kind=dp) :: mu, sig
real(kind=dp), allocatable :: tmp(:)
mu = 0.0_dp
sig = 1.0_dp
if (present(mean)) mu = mean
if (present(sd)) sig = sd
tmp = dnorm_vec([x], mu, sig, log_)
out = tmp(1)
end function dnorm_scalar

pure function dnorm_int_scalar(x, mean, sd, log_) result(out)
! Evaluate normal density (or log-density) for one integer value.
integer, intent(in) :: x
real(kind=dp), intent(in), optional :: mean, sd
logical, intent(in), optional :: log_
real(kind=dp) :: out
out = dnorm_scalar(real(x, kind=dp), mean, sd, log_)
end function dnorm_int_scalar

pure function tail(x, n) result(out)
! Return the last n elements of a vector.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
integer :: m, n0
n0 = size(x)
m = max(0, min(n, n0))
allocate(out(m))
if (m > 0) out = x(n0 - m + 1:n0)
end function tail

pure function cbind2(a, b) result(out)
! Bind two vectors as columns of a 2D array.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
n = min(size(a), size(b))
allocate(out(n, 2))
if (n > 0) then
   out(:,1) = a(1:n)
   out(:,2) = b(1:n)
end if
end function cbind2

pure function cbind(r1, r2, r3) result(out)
! Bind two or three vectors as columns of a 2D array.
real(kind=dp), intent(in) :: r1(:), r2(:)
real(kind=dp), intent(in), optional :: r3(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
if (.not. present(r3)) then
   out = cbind2(r1, r2)
   return
end if
n = min(size(r1), min(size(r2), size(r3)))
allocate(out(n, 3))
if (n > 0) then
   out(:,1) = r1(1:n)
   out(:,2) = r2(1:n)
   out(:,3) = r3(1:n)
end if
end function cbind

pure function rbind_vec(a, b) result(out)
! Bind two vectors as rows of a 2D array.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:,:)
integer :: n
n = min(size(a), size(b))
allocate(out(2, n))
if (n > 0) out = transpose(reshape([a(1:n), b(1:n)], [n, 2]))
end function rbind_vec

pure function rbind_mat(a, b) result(out)
! Bind two matrices by concatenating rows.
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: n1, n2, ncol
n1 = size(a, 1)
n2 = size(b, 1)
ncol = min(size(a, 2), size(b, 2))
allocate(out(n1 + n2, ncol))
if (ncol == 0 .or. (n1 == 0 .and. n2 == 0)) return
if (n1 > 0) out(1:n1, 1:ncol) = a(:, 1:ncol)
if (n2 > 0) out(n1 + 1:n1 + n2, 1:ncol) = b(:, 1:ncol)
end function rbind_mat

pure function rbind_vec_mat(a, b) result(out)
! Bind a vector as a row and a matrix below it.
real(kind=dp), intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: ncol, nrow
ncol = min(size(a), size(b, 2))
nrow = size(b, 1)
allocate(out(nrow + 1, ncol))
if (ncol == 0) return
out(1, 1:ncol) = a(1:ncol)
if (nrow > 0) out(2:, 1:ncol) = b(:, 1:ncol)
end function rbind_vec_mat

pure function rbind_mat_vec(a, b) result(out)
! Bind a matrix above a vector row.
real(kind=dp), intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable :: out(:,:)
integer :: ncol, nrow
ncol = min(size(a, 2), size(b))
nrow = size(a, 1)
allocate(out(nrow + 1, ncol))
if (ncol == 0) return
if (nrow > 0) out(1:nrow, 1:ncol) = a(:, 1:ncol)
out(nrow + 1, 1:ncol) = b(1:ncol)
end function rbind_mat_vec

pure function matrix_real(x, nrow, ncol) result(out)
! Build matrix with R-like recycling in column-major order.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nrow, ncol
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: buf(:)
integer :: i, need_n, nx
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0.0_dp
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function matrix_real

pure function matrix_int(x, nrow, ncol) result(out)
! Integer variant of matrix() with R-like recycling.
integer, intent(in) :: x(:)
integer, intent(in) :: nrow, ncol
integer, allocatable :: out(:,:)
integer, allocatable :: buf(:)
integer :: i, need_n, nx
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function matrix_int

pure function r_matmul_vv_real(a, b) result(out)
! Matrix-product helper for real vectors: dot product.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp) :: out
out = dot_product(a, b)
end function r_matmul_vv_real

pure function r_matmul_vv_int(a, b) result(out)
! Matrix-product helper for integer vectors: dot product (real result).
integer, intent(in) :: a(:), b(:)
real(kind=dp) :: out
out = dot_product(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_vv_int

pure function r_matmul_vv_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int vectors: dot product.
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: b(:)
real(kind=dp) :: out
out = dot_product(a, real(b, kind=dp))
end function r_matmul_vv_real_int

pure function r_matmul_vv_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real vectors: dot product.
integer, intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp) :: out
out = dot_product(real(a, kind=dp), b)
end function r_matmul_vv_int_real

pure function r_matmul_mv_real(a, b) result(out)
! Matrix-product helper for real matrix-vector multiplication.
real(kind=dp), intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, b)
end function r_matmul_mv_real

pure function r_matmul_mv_int(a, b) result(out)
! Matrix-product helper for integer matrix-vector multiplication.
integer, intent(in) :: a(:,:), b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_mv_int

pure function r_matmul_mv_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int matrix-vector multiplication.
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_mv_real_int

pure function r_matmul_mv_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real matrix-vector multiplication.
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_mv_int_real

pure function r_matmul_mv_complex(a, b) result(out)
! Matrix-product helper for complex matrix-vector multiplication.
complex(kind=dp), intent(in) :: a(:,:), b(:)
complex(kind=dp), allocatable :: out(:)
allocate(out(size(a, 1)))
out = matmul(a, b)
end function r_matmul_mv_complex

pure function r_matmul_vm_real(a, b) result(out)
! Matrix-product helper for real vector-matrix multiplication.
real(kind=dp), intent(in) :: a(:), b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(a, b)
end function r_matmul_vm_real

pure function r_matmul_vm_int(a, b) result(out)
! Matrix-product helper for integer vector-matrix multiplication.
integer, intent(in) :: a(:), b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_vm_int

pure function r_matmul_vm_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int vector-matrix multiplication.
real(kind=dp), intent(in) :: a(:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_vm_real_int

pure function r_matmul_vm_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real vector-matrix multiplication.
integer, intent(in) :: a(:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:)
allocate(out(size(b, 2)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_vm_int_real

pure function r_matmul_mm_real(a, b) result(out)
! Matrix-product helper for real matrix-matrix multiplication.
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, b)
end function r_matmul_mm_real

pure function r_matmul_mm_int(a, b) result(out)
! Matrix-product helper for integer matrix-matrix multiplication.
integer, intent(in) :: a(:,:), b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(real(a, kind=dp), real(b, kind=dp))
end function r_matmul_mm_int

pure function r_matmul_mm_real_int(a, b) result(out)
! Matrix-product helper for mixed real/int matrix-matrix multiplication.
real(kind=dp), intent(in) :: a(:,:)
integer, intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, real(b, kind=dp))
end function r_matmul_mm_real_int

pure function r_matmul_mm_int_real(a, b) result(out)
! Matrix-product helper for mixed int/real matrix-matrix multiplication.
integer, intent(in) :: a(:,:)
real(kind=dp), intent(in) :: b(:,:)
real(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(real(a, kind=dp), b)
end function r_matmul_mm_int_real

pure function r_matmul_mm_complex(a, b) result(out)
! Matrix-product helper for complex matrix-matrix multiplication.
complex(kind=dp), intent(in) :: a(:,:), b(:,:)
complex(kind=dp), allocatable :: out(:,:)
allocate(out(size(a, 1), size(b, 2)))
out = matmul(a, b)
end function r_matmul_mm_complex

function r_add_vv(a, b) result(out)
! Recycle and add two vectors (R-style recycling).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x + y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) + b(modulo(i - 1, nb) + 1)
end do
end function r_add_vv

pure function r_add_vs(a, b) result(out)
! Add scalar to vector.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a + b
end function r_add_vs

pure function r_add_sv(a, b) result(out)
! Add vector to scalar.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a + b
end function r_add_sv

function r_sub_vv(a, b) result(out)
! Recycle and subtract two vectors (a - b).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x - y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) - b(modulo(i - 1, nb) + 1)
end do
end function r_sub_vv

pure function r_sub_vs(a, b) result(out)
! Subtract scalar from vector.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a - b
end function r_sub_vs

pure function r_sub_sv(a, b) result(out)
! Subtract vector from scalar.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a - b
end function r_sub_sv

function r_mul_vv(a, b) result(out)
! Recycle and multiply two vectors.
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x * y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) * b(modulo(i - 1, nb) + 1)
end do
end function r_mul_vv

pure function r_mul_vs(a, b) result(out)
! Multiply vector by scalar.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a * b
end function r_mul_vs

pure function r_mul_sv(a, b) result(out)
! Multiply scalar by vector.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a * b
end function r_mul_sv

function r_div_vv(a, b) result(out)
! Recycle and divide two vectors (a / b).
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), allocatable :: out(:)
integer :: i, n, na, nb
na = size(a)
nb = size(b)
n = max(na, nb)
allocate(out(n))
if (n <= 0) return
call maybe_warn_recycle("x / y", na, nb)
do i = 1, n
   out(i) = a(modulo(i - 1, na) + 1) / b(modulo(i - 1, nb) + 1)
end do
end function r_div_vv

pure function r_div_vs(a, b) result(out)
! Divide vector by scalar.
real(kind=dp), intent(in) :: a(:), b
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(a)
allocate(out(n))
if (n > 0) out = a / b
end function r_div_vs

pure function r_div_sv(a, b) result(out)
! Divide scalar by vector.
real(kind=dp), intent(in) :: a, b(:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(b)
allocate(out(n))
if (n > 0) out = a / b
end function r_div_sv

pure function r_array_real(x, dim) result(out)
! Build 2D real array with R-like recycling.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: dim(:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol
if (size(dim) < 2) then
   allocate(out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0.0_dp
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function r_array_real

pure function r_array_int(x, dim) result(out)
! Build 2D integer array with R-like recycling.
integer, intent(in) :: x(:)
integer, intent(in) :: dim(:)
integer, allocatable :: out(:,:)
integer, allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol
if (size(dim) < 2) then
   allocate(out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
if (nrow < 0 .or. ncol < 0) then
   allocate(out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(out(nrow, ncol))
   out = 0
   return
end if
allocate(buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
out = reshape(buf, [nrow, ncol])
end function r_array_int

function r_array_char(x, dim) result(out)
! Build 2D character array with R-like recycling.
character(len=*), intent(in) :: x(:)
integer, intent(in) :: dim(:)
character(len=:), allocatable :: out(:,:)
character(len=:), allocatable :: buf(:)
integer :: i, need_n, nx, nrow, ncol, lch
if (size(dim) < 2) then
   allocate(character(len=1) :: out(0, 0))
   return
end if
nrow = dim(1)
ncol = dim(2)
nx = size(x)
lch = max(1, len(x))
if (nrow < 0 .or. ncol < 0) then
   allocate(character(len=lch) :: out(0, 0))
   return
end if
need_n = nrow * ncol
if (need_n <= 0) then
   allocate(character(len=lch) :: out(nrow, ncol))
   return
end if
if (nx <= 0) then
   allocate(character(len=lch) :: out(nrow, ncol))
   out = ""
   return
end if
allocate(character(len=lch) :: buf(need_n))
do i = 1, need_n
   buf(i) = x(modulo(i - 1, nx) + 1)
end do
allocate(character(len=lch) :: out(nrow, ncol))
out = reshape(buf, [nrow, ncol])
end function r_array_char

pure function numeric(n) result(out)
! Allocate a length-n real vector initialized to zero.
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:)
allocate(out(max(0, n)))
if (n > 0) out = 0.0_dp
end function numeric

pure elemental function pmax(a, b) result(out)
! Elementwise maximum of two real scalars.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: out
out = max(a, b)
end function pmax

pure function sd_vec(x) result(out)
! Sample standard deviation (n-1 denominator).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out, m
integer :: n
n = size(x)
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
m = sum(x) / real(n, kind=dp)
out = sqrt(sum((x - m)**2) / real(n - 1, kind=dp))
end function sd_vec

pure function sd_mat(x) result(out)
! R sd(matrix) treats the matrix as a vector.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp) :: out
out = sd_vec(pack(x, .true.))
end function sd_mat

pure function var_vec(x) result(out)
! Sample variance (n-1 denominator).
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out, m
integer :: n
n = size(x)
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
m = sum(x) / real(n, kind=dp)
out = sum((x - m)**2) / real(n - 1, kind=dp)
end function var_vec

pure function var_mat(x) result(out)
! Column covariance matrix, matching R var(matrix).
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
out = cov_mat(x)
end function var_mat

pure function r_sd(x) result(out)
! Alias for sd(x), used by transpiled code to avoid local-name collisions.
real(kind=dp), intent(in) :: x(:)
real(kind=dp) :: out
out = sd(x)
end function r_sd

pure elemental function r_gamma(x) result(out)
! Elementwise gamma function.
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
out = gamma(x)
end function r_gamma

pure elemental function r_lgamma(x) result(out)
! Elementwise log gamma function.
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
out = log_gamma(x)
end function r_lgamma

pure elemental function r_beta(a, b) result(out)
! Elementwise beta function.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: out
out = exp(log_gamma(a) + log_gamma(b) - log_gamma(a + b))
end function r_beta

pure elemental function r_lbeta(a, b) result(out)
! Elementwise log beta function.
real(kind=dp), intent(in) :: a, b
real(kind=dp) :: out
out = log_gamma(a) + log_gamma(b) - log_gamma(a + b)
end function r_lbeta

pure elemental function r_choose(n, k) result(out)
! Elementwise binomial coefficient function.
real(kind=dp), intent(in) :: n, k
real(kind=dp) :: out
if (n < 0.0_dp .or. k < 0.0_dp .or. k > n) then
   out = 0.0_dp
else
   out = exp(log_gamma(n + 1.0_dp) - log_gamma(k + 1.0_dp) - log_gamma(n - k + 1.0_dp))
end if
end function r_choose

pure elemental function r_lchoose(n, k) result(out)
! Elementwise log binomial coefficient.
real(kind=dp), intent(in) :: n, k
real(kind=dp) :: out
if (n < 0.0_dp .or. k < 0.0_dp .or. k > n) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
else
   out = log_gamma(n + 1.0_dp) - log_gamma(k + 1.0_dp) - log_gamma(n - k + 1.0_dp)
end if
end function r_lchoose

pure elemental function r_factorial(x) result(out)
! Elementwise factorial using gamma(x + 1) for compatibility.
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
out = r_gamma(x + 1.0_dp)
end function r_factorial

pure elemental function r_lfactorial(x) result(out)
! Elementwise log factorial.
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
out = r_lgamma(x + 1.0_dp)
end function r_lfactorial

pure elemental function r_digamma(x) result(out)
! Elementwise digamma approximation via finite differences of log_gamma.
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
real(kind=dp), parameter :: h = 1.0e-4_dp
out = (log_gamma(x + h) - log_gamma(x - h)) / (2.0_dp * h)
end function r_digamma

pure elemental function r_trigamma(x) result(out)
! Elementwise trigamma approximation via finite differences.
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
real(kind=dp), parameter :: h = 1.0e-4_dp
out = (log_gamma(x + h) - 2.0_dp * log_gamma(x) + log_gamma(x - h)) / (h * h)
end function r_trigamma

pure elemental function r_psigamma(x, deriv) result(out)
! Elementwise poly-gamma approximation.
real(kind=dp), intent(in) :: x
integer, intent(in) :: deriv
real(kind=dp) :: out
real(kind=dp), parameter :: h = 1.0e-4_dp
if (deriv <= 0) then
   out = r_digamma(x)
elseif (deriv == 1) then
   out = r_trigamma(x)
else
   out = (r_trigamma(x + h) - r_trigamma(x - h)) / (2.0_dp * h)
end if
end function r_psigamma

pure function colMeans(x) result(out)
! Column means of a numeric matrix.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:)
integer :: n
n = size(x, 1)
if (size(x, 2) <= 0) then
   allocate(out(0))
   return
end if
if (n <= 0) then
   allocate(out(size(x, 2)))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
out = sum(x, dim=1) / real(n, kind=dp)
end function colMeans

pure function cov_vec(x, y) result(out)
! Sample covariance of two vectors (n-1 denominator).
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: out, mx, my
integer :: n
n = min(size(x), size(y))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
mx = sum(x(1:n)) / real(n, kind=dp)
my = sum(y(1:n)) / real(n, kind=dp)
out = sum((x(1:n) - mx) * (y(1:n) - my)) / real(n - 1, kind=dp)
end function cov_vec

pure function cov_mat(x) result(out)
! Sample covariance matrix of columns of x (n-1 denominator).
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: xc(:,:), mu(:)
integer :: n, p
n = size(x, 1)
p = size(x, 2)
if (p <= 0) then
   allocate(out(0,0))
   return
end if
if (n <= 1) then
   allocate(out(p, p))
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
mu = sum(x, dim=1) / real(n, kind=dp)
xc = x - spread(mu, dim=1, ncopies=n)
out = matmul(transpose(xc), xc) / real(n - 1, kind=dp)
end function cov_mat

pure function scale_vec(x, center, scale) result(out)
! R scale() on a vector returns an n-by-1 matrix.
real(kind=dp), intent(in) :: x(:)
logical, intent(in), optional :: center, scale
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: xmat(:,:)
integer :: n
n = size(x)
allocate(xmat(n, 1))
if (n > 0) xmat(:, 1) = x
out = scale_mat(xmat, center=center, scale=scale)
end function scale_vec

pure function scale_mat(x, center, scale) result(out)
! Column-center and optionally column-scale a matrix, matching common R scale() use.
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: center, scale
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: mu(:), sig(:)
integer :: n, p, j
logical :: do_center, do_scale
n = size(x, 1)
p = size(x, 2)
do_center = .true.
do_scale = .true.
if (present(center)) do_center = center
if (present(scale)) do_scale = scale
allocate(out(n, p))
out = x
if (p <= 0 .or. n <= 0) return
if (do_center) then
   allocate(mu(p))
   mu = sum(x, dim=1) / real(n, kind=dp)
   out = out - spread(mu, dim=1, ncopies=n)
end if
if (do_scale) then
   allocate(sig(p))
   do j = 1, p
      if (n > 1) then
         sig(j) = sqrt(sum(out(:, j)**2) / real(n - 1, kind=dp))
      else
         sig(j) = 0.0_dp
      end if
      if (sig(j) > 0.0_dp) out(:, j) = out(:, j) / sig(j)
   end do
end if
end function scale_mat

pure function all_equal_real_scalar(a, b, tolerance) result(out)
real(kind=dp), intent(in) :: a, b
real(kind=dp), intent(in), optional :: tolerance
logical :: out
real(kind=dp) :: tol, scale_ab
tol = sqrt(epsilon(1.0_dp))
if (present(tolerance)) tol = tolerance
scale_ab = max(1.0_dp, abs(a), abs(b))
out = abs(a - b) <= tol * scale_ab
end function all_equal_real_scalar

pure function all_equal_real_vec(a, b, tolerance) result(out)
real(kind=dp), intent(in) :: a(:), b(:)
real(kind=dp), intent(in), optional :: tolerance
logical :: out
real(kind=dp) :: tol
tol = sqrt(epsilon(1.0_dp))
if (present(tolerance)) tol = tolerance
if (size(a) /= size(b)) then
   out = .false.
else if (size(a) == 0) then
   out = .true.
else
   out = all(abs(a - b) <= tol * max(1.0_dp, max(maxval(abs(a)), maxval(abs(b)))))
end if
end function all_equal_real_vec

pure function all_equal_real_mat(a, b, tolerance) result(out)
real(kind=dp), intent(in) :: a(:,:), b(:,:)
real(kind=dp), intent(in), optional :: tolerance
logical :: out
real(kind=dp) :: tol
tol = sqrt(epsilon(1.0_dp))
if (present(tolerance)) tol = tolerance
if (any(shape(a) /= shape(b))) then
   out = .false.
else if (size(a) == 0) then
   out = .true.
else
   out = all(abs(a - b) <= tol * max(1.0_dp, max(maxval(abs(a)), maxval(abs(b)))))
end if
end function all_equal_real_mat

pure function all_equal_int_scalar(a, b, tolerance) result(out)
integer, intent(in) :: a, b
real(kind=dp), intent(in), optional :: tolerance
logical :: out
out = all_equal_real_scalar(real(a, kind=dp), real(b, kind=dp), tolerance=tolerance)
end function all_equal_int_scalar

pure function all_equal_int_vec(a, b, tolerance) result(out)
integer, intent(in) :: a(:), b(:)
real(kind=dp), intent(in), optional :: tolerance
logical :: out
out = all_equal_real_vec(real(a, kind=dp), real(b, kind=dp), tolerance=tolerance)
end function all_equal_int_vec

pure function all_equal_int_mat(a, b, tolerance) result(out)
integer, intent(in) :: a(:,:), b(:,:)
real(kind=dp), intent(in), optional :: tolerance
logical :: out
out = all_equal_real_mat(real(a, kind=dp), real(b, kind=dp), tolerance=tolerance)
end function all_equal_int_mat

pure function all_equal_logical_vec(a, b, tolerance) result(out)
logical, intent(in) :: a(:), b(:)
real(kind=dp), intent(in), optional :: tolerance
logical :: out
out = size(a) == size(b)
if (out) out = all(a .eqv. b)
end function all_equal_logical_vec

pure function all_equal_logical_mat(a, b, tolerance) result(out)
logical, intent(in) :: a(:,:), b(:,:)
real(kind=dp), intent(in), optional :: tolerance
logical :: out
out = all(shape(a) == shape(b))
if (out) out = all(a .eqv. b)
end function all_equal_logical_mat

elemental function r_log_scalar(x) result(out)
real(kind=dp), intent(in) :: x
real(kind=dp) :: out
if (x < 0.0_dp) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
else
   out = log(x)
end if
end function r_log_scalar

pure function r_log_vec(x) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = r_log_scalar(x(i))
end do
end function r_log_vec

pure function r_log_mat(x) result(out)
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, j
allocate(out(size(x, 1), size(x, 2)))
do j = 1, size(x, 2)
   do i = 1, size(x, 1)
      out(i, j) = r_log_scalar(x(i, j))
   end do
end do
end function r_log_mat

pure function cor_vec(x, y) result(out)
! Sample correlation of two vectors.
real(kind=dp), intent(in) :: x(:), y(:)
real(kind=dp) :: out, sdx, sdy
integer :: n
n = min(size(x), size(y))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
sdx = sd(x(1:n))
sdy = sd(y(1:n))
if (.not. ieee_is_finite(sdx) .or. .not. ieee_is_finite(sdy) .or. sdx <= 0.0_dp .or. sdy <= 0.0_dp) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
out = cov_vec(x(1:n), y(1:n)) / (sdx * sdy)
end function cor_vec

pure function cor_mat(x) result(out)
! Sample correlation matrix of columns of x.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), allocatable :: out(:,:), s(:)
real(kind=dp), allocatable :: c(:,:)
integer :: i, j, p
c = cov_mat(x)
p = size(c, 1)
if (p <= 0) then
   allocate(out(0,0))
   return
end if
allocate(out(p, p), s(p))
do i = 1, p
   s(i) = sqrt(c(i, i))
end do
do i = 1, p
   do j = 1, p
      if (.not. ieee_is_finite(s(i)) .or. .not. ieee_is_finite(s(j)) .or. s(i) <= 0.0_dp .or. s(j) <= 0.0_dp) then
         out(i, j) = ieee_value(0.0_dp, ieee_quiet_nan)
      else
         out(i, j) = c(i, j) / (s(i) * s(j))
      end if
   end do
end do
end function cor_mat

pure function cor_mat_pair(x, y) result(out)
! Sample column-pair correlation matrix between x and y.
real(kind=dp), intent(in) :: x(:,:), y(:,:)
real(kind=dp), allocatable :: out(:,:)
integer :: i, j, n, px, py
n = min(size(x, 1), size(y, 1))
px = size(x, 2)
py = size(y, 2)
allocate(out(px, py))
if (n <= 1) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
do i = 1, px
   do j = 1, py
      out(i, j) = cor_vec(x(1:n, i), y(1:n, j))
   end do
end do
end function cor_mat_pair

pure integer function count_ws_tokens(line) result(n_tok)
! Count whitespace-separated tokens in one text line.
character(len=*), intent(in) :: line
integer :: i, n
logical :: in_tok
n = len_trim(line)
n_tok = 0
in_tok = .false.
do i = 1, n
   if (line(i:i) /= " " .and. line(i:i) /= char(9)) then
      if (.not. in_tok) then
         n_tok = n_tok + 1
         in_tok = .true.
      end if
   else
      in_tok = .false.
   end if
end do
end function count_ws_tokens

subroutine read_real_vector(file_path, x)
! Read one real value per line into a vector.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:)
integer :: fp, ios, n, cap, new_cap
real(kind=dp) :: v
n = 0
cap = 0
open(newunit=fp, file=file_path, status="old", action="read")
do
   read(fp, *, iostat=ios) v
   if (ios /= 0) exit
   if (n == cap) then
      new_cap = merge(1024, 2 * cap, cap == 0)
      block
         real(kind=dp), allocatable :: tmp(:)
         allocate(tmp(new_cap))
         if (allocated(x) .and. n > 0) tmp(1:n) = x(1:n)
         call move_alloc(tmp, x)
      end block
      cap = new_cap
   end if
   n = n + 1
   x(n) = v
end do
close(fp)
if (n == 0) then
   allocate(x(0))
else if (n < size(x)) then
   x = x(1:n)
end if
end subroutine read_real_vector

subroutine read_table_real_matrix(file_path, x, header)
! Read a whitespace-delimited numeric table into a matrix.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:,:)
logical, intent(in), optional :: header
integer :: fp, ios, nrow, ncol, i
character(len=4096) :: line
logical :: has_header, skipped_header
nrow = 0
ncol = 0
has_header = .false.
if (present(header)) has_header = header
skipped_header = .false.
open(newunit=fp, file=file_path, status="old", action="read")
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   if (ncol == 0) ncol = count_ws_tokens(line)
   if (has_header .and. .not. skipped_header) then
      skipped_header = .true.
      cycle
   end if
   nrow = nrow + 1
end do
if (nrow <= 0 .or. ncol <= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
allocate(x(nrow, ncol))
rewind(fp)
i = 0
skipped_header = .false.
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   if (has_header .and. .not. skipped_header) then
      skipped_header = .true.
      cycle
   end if
   i = i + 1
   read(line, *) x(i, 1:ncol)
end do
close(fp)
end subroutine read_table_real_matrix

subroutine read_csv_real_matrix(file_path, x)
! Read a comma-delimited numeric CSV with one header row into a matrix.
character(len=*), intent(in) :: file_path
real(kind=dp), allocatable, intent(out) :: x(:,:)
integer :: fp, ios, nrow, ncol, i, k
character(len=4096) :: line
nrow = 0
ncol = 0
open(newunit=fp, file=file_path, status="old", action="read")
read(fp, "(A)", iostat=ios) line
if (ios /= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
ncol = 1
do k = 1, len_trim(line)
   if (line(k:k) == ",") ncol = ncol + 1
end do
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   nrow = nrow + 1
end do
if (nrow <= 0 .or. ncol <= 0) then
   allocate(x(0,0))
   close(fp)
   return
end if
allocate(x(nrow, ncol))
rewind(fp)
read(fp, "(A)", iostat=ios) line
i = 0
do
   read(fp, "(A)", iostat=ios) line
   if (ios /= 0) exit
   if (len_trim(line) == 0) cycle
   do k = 1, len_trim(line)
      if (line(k:k) == ",") line(k:k) = " "
   end do
   i = i + 1
   read(line, *) x(i, 1:ncol)
end do
close(fp)
end subroutine read_csv_real_matrix

function read_csv_header_names(file_path) result(names)
! Read the first CSV row as a character vector of header names.
character(len=*), intent(in) :: file_path
character(len=:), allocatable :: names(:)
character(len=4096) :: line
integer :: fp, ios, ncol, i, start, pos
open(newunit=fp, file=file_path, status="old", action="read")
read(fp, "(A)", iostat=ios) line
close(fp)
if (ios /= 0 .or. len_trim(line) == 0) then
   allocate(character(len=1) :: names(0))
   return
end if
ncol = 1
do i = 1, len_trim(line)
   if (line(i:i) == ",") ncol = ncol + 1
end do
allocate(character(len=128) :: names(ncol))
start = 1
pos = 1
do i = 1, len_trim(line) + 1
   if (i > len_trim(line) .or. line(i:i) == ",") then
      if (i > start) then
         names(pos) = adjustl(line(start:i-1))
         names(pos) = trim(names(pos))
         if (len_trim(names(pos)) >= 2) then
            if ((names(pos)(1:1) == '"' .and. names(pos)(len_trim(names(pos)):len_trim(names(pos))) == '"') .or. &
                (names(pos)(1:1) == "'" .and. names(pos)(len_trim(names(pos)):len_trim(names(pos))) == "'")) then
               names(pos) = names(pos)(2:len_trim(names(pos))-1)
            end if
         end if
      else
         names(pos) = ""
      end if
      pos = pos + 1
      start = i + 1
   end if
end do
end function read_csv_header_names

subroutine write_table_real_matrix(file_path, x, names)
! Write a numeric matrix as a whitespace-delimited table.
character(len=*), intent(in) :: file_path
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in), optional :: names(:)
integer :: fp, i, j
open(newunit=fp, file=file_path, status="replace", action="write")
if (present(names)) then
   do j = 1, size(x, 2)
      if (j > 1) write(fp, '(1x)', advance='no')
      if (j <= size(names)) then
         write(fp, '(a)', advance='no') trim(names(j))
      else
         write(fp, '("V",i0)', advance='no') j
      end if
   end do
   write(fp,*)
end if
do i = 1, size(x, 1)
   write(fp, *) x(i, 1:size(x, 2))
end do
close(fp)
end subroutine write_table_real_matrix

subroutine print_matrix_real(x, int_like)
! Print a real matrix row-by-row; use integer format when all values are integer-like.
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in), optional :: int_like
logical :: use_int_like, all_int
integer :: i, j
integer(kind=int64) :: k
real(kind=dp) :: r, tol
use_int_like = print_int_like_default
if (present(int_like)) use_int_like = int_like
all_int = .false.
if (use_int_like) then
   all_int = .true.
   do i = 1, size(x, 1)
      do j = 1, size(x, 2)
         r = x(i, j)
         if (.not. ieee_is_finite(r)) then
            all_int = .false.
            exit
         end if
         if (abs(r) > real(huge(0_int64), kind=dp)) then
            all_int = .false.
            exit
         end if
         k = nint(r, kind=int64)
         tol = print_int_like_tol * max(1.0_dp, abs(r))
         if (abs(r - real(k, kind=dp)) > tol) then
            all_int = .false.
            exit
         end if
      end do
      if (.not. all_int) exit
   end do
end if
if (all_int) then
   do i = 1, size(x, 1)
      do j = 1, size(x, 2)
         k = nint(x(i, j), kind=int64)
         write(*,"(i0)", advance="no") k
         if (j < size(x, 2)) write(*,"(a)", advance="no") " "
      end do
      write(*,*)
   end do
else
   do i = 1, size(x, 1)
      write(*,"(*(g0,1x))") x(i, :)
   end do
end if
end subroutine print_matrix_real

subroutine print_matrix_rstyle_real(x)
! Print a real matrix with R-like column and row labels.
real(kind=dp), intent(in) :: x(:,:)
integer :: i
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   write(*,'("[,",i0,"]",8x)', advance='no') i
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   write(*,'(*(es12.5,1x))') x(i, :)
end do
end subroutine print_matrix_rstyle_real

subroutine print_matrix_rstyle_named_real(x, names, int_cols)
! Print a real matrix with R-like row labels and provided column names.
real(kind=dp), intent(in) :: x(:,:)
character(len=*), intent(in) :: names(:)
logical, intent(in), optional :: int_cols(:)
integer :: i, j
logical :: as_int_col
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   if (i <= size(names)) then
      write(*,'(a12,1x)', advance='no') trim(names(i))
   else
      write(*,'("[,",i0,"]",8x)', advance='no') i
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   do j = 1, size(x, 2)
      as_int_col = .false.
      if (present(int_cols)) then
         if (j <= size(int_cols)) as_int_col = int_cols(j)
      end if
      if (as_int_col .and. ieee_is_finite(x(i, j))) then
         write(*,'(i12,1x)', advance='no') nint(x(i, j))
      else
         write(*,'(f12.4,1x)', advance='no') x(i, j)
      end if
   end do
   write(*,*)
end do
end subroutine print_matrix_rstyle_named_real

subroutine print_matrix_rstyle_named_int(x, names)
! Print an integer matrix with R-like row labels and provided column names.
integer, intent(in) :: x(:,:)
character(len=*), intent(in) :: names(:)
integer :: i, j
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   if (i <= size(names)) then
      write(*,'(a12,1x)', advance='no') trim(names(i))
   else
      write(*,'("[,",i0,"]",8x)', advance='no') i
   end if
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   do j = 1, size(x, 2)
      write(*,'(i12,1x)', advance='no') x(i, j)
   end do
   write(*,*)
end do
end subroutine print_matrix_rstyle_named_int

subroutine print_matrix_int(x)
! Print an integer matrix row-by-row.
integer, intent(in) :: x(:,:)
integer :: i
do i = 1, size(x, 1)
   write(*,"(*(i0,1x))") x(i, :)
end do
end subroutine print_matrix_int

subroutine print_matrix_logical(x)
! Print a logical matrix row-by-row.
logical, intent(in) :: x(:,:)
integer :: i, j
do i = 1, size(x, 1)
   do j = 1, size(x, 2)
      if (x(i, j)) then
         write(*,'(a)', advance='no') 'T'
      else
         write(*,'(a)', advance='no') 'F'
      end if
      if (j < size(x, 2)) write(*,'(a)', advance='no') ' '
   end do
   write(*,*)
end do
end subroutine print_matrix_logical

subroutine print_matrix_rstyle_int(x)
! Print an integer matrix with R-like column and row labels.
integer, intent(in) :: x(:,:)
integer :: i
write(*,'(7x)', advance='no')
do i = 1, size(x, 2)
   write(*,'("[,",i0,"]",8x)', advance='no') i
end do
write(*,*)
do i = 1, size(x, 1)
   write(*,'("[",i0,",]",1x)', advance='no') i
   write(*,'(*(i12,1x))') x(i, :)
end do
end subroutine print_matrix_rstyle_int

subroutine print_matrix_rstyle_logical(x)
! Print a logical matrix with R-like column and row labels.
logical, intent(in) :: x(:,:)
call print_matrix_logical(x)
end subroutine print_matrix_rstyle_logical

pure function lm_predict_general(fit, xpred) result(yhat)
! Predict responses for a fitted linear model.
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), allocatable :: yhat(:)
integer :: p
p = size(xpred, 2)
if (size(fit%coef) /= p + 1) error stop "error: predictor count mismatch"
allocate(yhat(size(xpred, 1)))
yhat = fit%coef(1) + matmul(xpred, fit%coef(2:p+1))
end function lm_predict_general

pure function lm_predict_interval(fit, xpred) result(out)
! Prediction intervals for a fitted linear model.
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: x0(:), yhat(:)
real(kind=dp) :: crit, se_pred
integer :: i, p
p = size(xpred, 2)
yhat = lm_predict_general(fit, xpred)
allocate(out(size(xpred, 1), 3), x0(p + 1))
crit = t_crit_975(fit%df)
do i = 1, size(xpred, 1)
   x0(1) = 1.0_dp
   if (p > 0) x0(2:p+1) = xpred(i, :)
   se_pred = fit%sigma
   if (allocated(fit%cov_unscaled) .and. size(fit%cov_unscaled, 1) >= p + 1 .and. size(fit%cov_unscaled, 2) >= p + 1) then
      se_pred = fit%sigma * sqrt(max(0.0_dp, 1.0_dp + dot_product(x0, matmul(fit%cov_unscaled, x0))))
   end if
   out(i, 1) = yhat(i)
   out(i, 2) = yhat(i) - crit * se_pred
   out(i, 3) = yhat(i) + crit * se_pred
end do
end function lm_predict_interval

subroutine print_lm_prediction_interval(fit, xpred)
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), allocatable :: pred(:,:)
integer :: i
pred = lm_predict_interval(fit, xpred)
write(*,'(5x,a9,1x,a9,1x,a9)') "fit", "lwr", "upr"
do i = 1, size(pred, 1)
   write(*,'(i0,1x,f9.6,1x,f9.6,1x,f9.5)') i, pred(i, 1), pred(i, 2), pred(i, 3)
end do
end subroutine print_lm_prediction_interval

pure function lm_confint(fit, level) result(out)
! Coefficient confidence intervals for a fitted linear model.
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in), optional :: level
real(kind=dp), allocatable :: out(:,:)
real(kind=dp) :: crit, se
integer :: j, p
p = size(fit%coef)
allocate(out(p, 2))
if (p <= 0) return
crit = t_crit_975(fit%df)
do j = 1, p
   se = fit%sigma
   if (allocated(fit%cov_unscaled) .and. size(fit%cov_unscaled, 1) >= j .and. size(fit%cov_unscaled, 2) >= j) then
      se = fit%sigma * sqrt(max(0.0_dp, fit%cov_unscaled(j, j)))
   end if
   out(j, 1) = fit%coef(j) - crit * se
   out(j, 2) = fit%coef(j) + crit * se
end do
end function lm_confint

pure function t_crit_975(df) result(out)
! Two-sided 95% Student-t critical value for common residual degrees of freedom.
integer, intent(in) :: df
real(kind=dp) :: out
select case (df)
case (1); out = 12.7062047364321_dp
case (2); out = 4.30265272974946_dp
case (3); out = 3.18244630528426_dp
case (4); out = 2.77644510519779_dp
case (5); out = 2.57058183563631_dp
case (6); out = 2.44691184879168_dp
case (7); out = 2.36462425159278_dp
case (8); out = 2.30600413503337_dp
case (9); out = 2.26215716274099_dp
case (10); out = 2.22813885196494_dp
case (11); out = 2.20098516008295_dp
case (12); out = 2.17881282966342_dp
case (13); out = 2.16036865646101_dp
case (14); out = 2.14478668791693_dp
case (15); out = 2.13144954555978_dp
case (16); out = 2.11990529922125_dp
case (17); out = 2.10981557783318_dp
case (18); out = 2.10092204024096_dp
case (19); out = 2.09302405440831_dp
case (20); out = 2.08596344726586_dp
case (21); out = 2.07961384472766_dp
case (22); out = 2.07387306790401_dp
case (23); out = 2.06865761041904_dp
case (24); out = 2.06389856162802_dp
case (25); out = 2.05953855275329_dp
case (26); out = 2.05552943864287_dp
case (27); out = 2.05183051648028_dp
case (28); out = 2.04840714179524_dp
case (29); out = 2.04522964213270_dp
case (30); out = 2.04227245630124_dp
case default
   if (df <= 0) then
      out = ieee_value(0.0_dp, ieee_quiet_nan)
   else if (df <= 40) then
      out = 2.02107539030627_dp
   else if (df <= 60) then
      out = 2.00029782105826_dp
   else if (df <= 120) then
      out = 1.97993040505278_dp
   else
      out = 1.959963984540054_dp
   end if
end select
end function t_crit_975

pure function lm_cooks_distance(fit) result(out)
! Cook's distance for each observation in a fitted linear model.
type(lm_fit_t), intent(in) :: fit
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: x0(:)
real(kind=dp) :: h, denom
integer :: i, p
if (.not. allocated(fit%resid)) then
   allocate(out(0))
   return
end if
p = size(fit%coef)
allocate(out(size(fit%resid)))
out = 0.0_dp
if (.not. allocated(fit%xpred) .or. .not. allocated(fit%cov_unscaled) .or. p <= 0 .or. fit%sigma <= 0.0_dp) return
allocate(x0(p))
do i = 1, size(fit%resid)
   x0(1) = 1.0_dp
   if (p > 1) x0(2:p) = fit%xpred(i, 1:p-1)
   h = dot_product(x0, matmul(fit%cov_unscaled, x0))
   denom = real(p, kind=dp) * fit%sigma * fit%sigma * max(tiny(1.0_dp), (1.0_dp - h) * (1.0_dp - h))
   out(i) = fit%resid(i) * fit%resid(i) * h / denom
end do
end function lm_cooks_distance

function qr(x, tol, lapack) result(fit)
! Compact QR decomposition object for base R-style qr(x) uses.
real(kind=dp), intent(in) :: x(:,:)
real(kind=dp), intent(in), optional :: tol
logical, intent(in), optional :: lapack
type(qr_fit_t) :: fit
integer :: i, j, m, n, k, rk
real(kind=dp) :: nrm, eps, max_diag, t
real(kind=dp), allocatable :: v(:)
logical :: lapack_dummy
m = size(x, 1)
n = size(x, 2)
k = min(m, n)
eps = 1.0e-7_dp
if (present(tol)) eps = tol
lapack_dummy = .false.
if (present(lapack)) lapack_dummy = lapack
allocate(fit%qr(m, n), fit%q(m, k), fit%r(k, n), fit%qraux(k), fit%pivot(n), v(m))
fit%qr = x
fit%q = 0.0_dp
fit%r = 0.0_dp
fit%qraux = 0.0_dp
fit%pivot = [(i, i = 1, n)]
rk = 0
max_diag = 0.0_dp
do j = 1, k
   nrm = sqrt(max(0.0_dp, dot_product(fit%qr(j:m, j), fit%qr(j:m, j))))
   if (nrm /= 0.0_dp) then
      if (fit%qr(j, j) /= 0.0_dp) nrm = sign(nrm, fit%qr(j, j))
      fit%qr(j:m, j) = fit%qr(j:m, j) / nrm
      fit%qr(j, j) = 1.0_dp + fit%qr(j, j)
      fit%qraux(j) = fit%qr(j, j)
      do i = j + 1, n
         t = -dot_product(fit%qr(j:m, j), fit%qr(j:m, i)) / fit%qr(j, j)
         fit%qr(j:m, i) = fit%qr(j:m, i) + t * fit%qr(j:m, j)
      end do
      fit%qr(j, j) = -nrm
   end if
   max_diag = max(max_diag, abs(fit%qr(j, j)))
   if (abs(fit%qr(j, j)) > eps * max(1.0_dp, max_diag)) rk = rk + 1
end do
fit%rank = rk
fit%r = 0.0_dp
do i = 1, k
   fit%r(i, i:n) = fit%qr(i, i:n)
end do
end function qr

pure function qr_Q(fit, complete) result(q)
! Reconstruct the Q matrix from compact qr() Householder storage.
type(qr_fit_t), intent(in) :: fit
logical, intent(in), optional :: complete
real(kind=dp), allocatable :: q(:,:)
integer :: i, j, col, m, n, nq
real(kind=dp) :: t
real(kind=dp), allocatable :: v(:)
logical :: full
m = size(fit%qr, 1)
n = size(fit%qr, 2)
full = .false.
if (present(complete)) full = complete
nq = merge(m, min(m, n), full)
allocate(q(m, nq))
q = 0.0_dp
do i = 1, min(m, nq)
   q(i, i) = 1.0_dp
end do
do j = min(m, n), 1, -1
   if (j > size(fit%qraux) .or. fit%qraux(j) == 0.0_dp) cycle
   allocate(v(m - j + 1))
   v = fit%qr(j:m, j)
   v(1) = fit%qraux(j)
   do col = 1, nq
      t = -dot_product(v, q(j:m, col)) / v(1)
      q(j:m, col) = q(j:m, col) + t * v
   end do
   deallocate(v)
end do
end function qr_Q

pure function qr_R(fit, complete) result(r)
! Extract the upper-triangular R matrix from compact qr() storage.
type(qr_fit_t), intent(in) :: fit
logical, intent(in), optional :: complete
real(kind=dp), allocatable :: r(:,:)
integer :: i, m, n, nr
logical :: full
m = size(fit%qr, 1)
n = size(fit%qr, 2)
full = .false.
if (present(complete)) full = complete
nr = merge(m, min(m, n), full)
allocate(r(nr, n))
r = 0.0_dp
do i = 1, min(nr, n)
   r(i, i:n) = fit%qr(i, i:n)
end do
end function qr_R

pure function qr_rank(fit) result(out)
type(qr_fit_t), intent(in) :: fit
integer :: out
out = fit%rank
end function qr_rank

pure function qr_pivot(fit) result(out)
type(qr_fit_t), intent(in) :: fit
integer, allocatable :: out(:)
allocate(out(size(fit%pivot)))
out = fit%pivot
end function qr_pivot

pure function qr_coef_vec(fit, y) result(coef)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:)
real(kind=dp), allocatable :: coef(:)
real(kind=dp), allocatable :: q(:,:), r(:,:), qty(:)
integer :: k
k = max(0, fit%rank)
if (k <= 0) then
   allocate(coef(0))
   return
end if
q = qr_Q(fit)
r = qr_R(fit)
qty = matmul(transpose(q(:, 1:k)), y)
coef = backsolve_vec(r(1:k, 1:k), qty(1:k))
end function qr_coef_vec

pure function qr_coef_mat(fit, y) result(coef)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:,:)
real(kind=dp), allocatable :: coef(:,:)
real(kind=dp), allocatable :: q(:,:), r(:,:), qty(:,:)
integer :: k
k = max(0, fit%rank)
if (k <= 0) then
   allocate(coef(0, size(y, 2)))
   return
end if
q = qr_Q(fit)
r = qr_R(fit)
qty = matmul(transpose(q(:, 1:k)), y)
coef = backsolve_mat(r(1:k, 1:k), qty(1:k, :))
end function qr_coef_mat

pure function qr_fitted_vec(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: q(:,:), qty(:)
integer :: k
k = max(0, fit%rank)
allocate(out(size(y)))
if (k <= 0) then
   out = 0.0_dp
   return
end if
q = qr_Q(fit)
qty = matmul(transpose(q(:, 1:k)), y)
out = matmul(q(:, 1:k), qty(1:k))
end function qr_fitted_vec

pure function qr_fitted_mat(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: q(:,:), qty(:,:)
integer :: k
k = max(0, fit%rank)
allocate(out(size(y, 1), size(y, 2)))
if (k <= 0) then
   out = 0.0_dp
   return
end if
q = qr_Q(fit)
qty = matmul(transpose(q(:, 1:k)), y)
out = matmul(q(:, 1:k), qty(1:k, :))
end function qr_fitted_mat

pure function qr_resid_vec(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:)
real(kind=dp), allocatable :: out(:)
out = y - qr_fitted(fit, y)
end function qr_resid_vec

pure function qr_resid_mat(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:,:)
real(kind=dp), allocatable :: out(:,:)
out = y - qr_fitted(fit, y)
end function qr_resid_mat

pure function qr_qty_vec(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: q(:,:)
q = qr_Q(fit, complete=.true.)
out = matmul(transpose(q), y)
end function qr_qty_vec

pure function qr_qty_mat(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: q(:,:)
q = qr_Q(fit, complete=.true.)
out = matmul(transpose(q), y)
end function qr_qty_mat

pure function qr_qy_vec(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: q(:,:)
q = qr_Q(fit, complete=.true.)
out = matmul(q, y)
end function qr_qy_vec

pure function qr_qy_mat(fit, y) result(out)
type(qr_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: y(:,:)
real(kind=dp), allocatable :: out(:,:)
real(kind=dp), allocatable :: q(:,:)
q = qr_Q(fit, complete=.true.)
out = matmul(q, y)
end function qr_qy_mat

subroutine print_qr(fit)
type(qr_fit_t), intent(in) :: fit
write(*,'(a)') "$qr"
call print_matrix_rstyle(fit%qr)
write(*,'(a)') ""
write(*,'(a)') "$rank"
call print_real_scalar(real(fit%rank, kind=dp))
write(*,'(a)') ""
write(*,'(a)') "$qraux"
call print_real_vector(fit%qraux)
write(*,'(a)') ""
write(*,'(a)') "$pivot"
call print_real_vector(real(fit%pivot, kind=dp))
end subroutine print_qr

subroutine print_lm_cooks_top(fit, n)
! Print the largest Cook's distances with original observation numbers.
type(lm_fit_t), intent(in) :: fit
integer, intent(in) :: n
real(kind=dp), allocatable :: cook(:)
integer, allocatable :: ord(:)
integer :: i, j, m, chunk
cook = lm_cooks_distance(fit)
ord = order_real(cook)
m = max(0, min(n, size(cook)))
if (m <= 0) then
   write(*,*)
   return
end if
i = 1
do while (i <= m)
   chunk = min(7, m - i + 1)
   do j = 0, chunk - 1
      write(*,'(i10,1x)', advance='no') ord(size(ord) - (i + j) + 1)
   end do
   write(*,*)
   do j = 0, chunk - 1
      write(*,'(f10.8,1x)', advance='no') cook(ord(size(ord) - (i + j) + 1))
   end do
   write(*,*)
   i = i + chunk
end do
end subroutine print_lm_cooks_top

subroutine solve_linear(a, b, x, ok)
! Solve Ax=b by Gaussian elimination with partial pivoting.
real(kind=dp), intent(inout) :: a(:,:), b(:)
real(kind=dp), intent(out) :: x(:)
logical, intent(out) :: ok
integer :: i, j, k, p, n
real(kind=dp) :: piv, fac, t
ok = .true.
n = size(b)
if (size(a,1) /= n .or. size(a,2) /= n .or. size(x) /= n) then
   ok = .false.
   x = 0.0_dp
   return
end if
do k = 1, n
   p = k
   piv = abs(a(k,k))
   do i = k + 1, n
      if (abs(a(i,k)) > piv) then
         p = i
         piv = abs(a(i,k))
      end if
   end do
   if (piv <= tiny(1.0_dp)) then
      ok = .false.
      x = 0.0_dp
      return
   end if
   if (p /= k) then
      do j = k, n
         t = a(k,j)
         a(k,j) = a(p,j)
         a(p,j) = t
      end do
      t = b(k)
      b(k) = b(p)
      b(p) = t
   end if
   do i = k + 1, n
      fac = a(i,k) / a(k,k)
      a(i,k:n) = a(i,k:n) - fac * a(k,k:n)
      b(i) = b(i) - fac * b(k)
   end do
end do
x(n) = b(n) / a(n,n)
do i = n - 1, 1, -1
   if (i < n) then
      x(i) = (b(i) - sum(a(i,i+1:n) * x(i+1:n))) / a(i,i)
   else
      x(i) = b(i) / a(i,i)
   end if
end do
end subroutine solve_linear

function lm_fit_general(y, xpred) result(fit)
! Fit linear regression with intercept and arbitrary predictors.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
type(lm_fit_t) :: fit
integer :: i, j, n, p, k, dof
real(kind=dp), allocatable :: a(:,:), a_xtx(:,:), a_work(:,:), b(:), b_work(:), beta(:), sol(:)
real(kind=dp) :: ybar, sse, sst
logical :: ok
if (size(y) /= size(xpred,1)) then
   error stop "error: need size(y) == size(xpred,1)"
end if
n = size(y)
p = size(xpred,2)
k = p + 1
if (n < k) error stop "error: need n >= number of parameters"
allocate(a(k,k), b(k), beta(k))
a = 0.0_dp
b = 0.0_dp
a(1,1) = n
b(1) = sum(y)
do j = 1, p
   a(1,j+1) = sum(xpred(:,j))
   a(j+1,1) = a(1,j+1)
   b(j+1) = sum(xpred(:,j) * y)
end do
do i = 1, p
   do j = i, p
      a(i+1,j+1) = sum(xpred(:,i) * xpred(:,j))
      a(j+1,i+1) = a(i+1,j+1)
   end do
end do
a_xtx = a
call solve_linear(a, b, beta, ok)
if (.not. ok) error stop "error: singular normal equations"
allocate(fit%cov_unscaled(k, k), a_work(k, k), b_work(k), sol(k))
do j = 1, k
   a_work = a_xtx
   b_work = 0.0_dp
   b_work(j) = 1.0_dp
   call solve_linear(a_work, b_work, sol, ok)
   if (.not. ok) error stop "error: singular normal equations"
   fit%cov_unscaled(:, j) = sol
end do
fit%coef = beta
fit%y = y
fit%xpred = xpred
fit%fitted = beta(1) + matmul(xpred, beta(2:k))
fit%resid = y - fit%fitted
sse = sum(fit%resid**2)
ybar = sum(y) / n
sst = sum((y - ybar)**2)
if (sst > 0.0_dp) then
   fit%r_squared = 1.0_dp - sse / sst
else
   fit%r_squared = 0.0_dp
end if
dof = max(1, n - k)
fit%df = n - k
fit%sigma = sqrt(sse / dof)
fit%adj_r_squared = 1.0_dp - (1.0_dp - fit%r_squared) * (n - 1) / dof
end function lm_fit_general

function step_lm(lower, upper, k) result(best_fit)
! Stepwise add/drop search over upper%xpred columns using an AIC-like score.
type(lm_fit_t), intent(in) :: lower, upper
real(kind=dp), intent(in), optional :: k
type(lm_fit_t) :: best_fit, cand_fit
logical, allocatable :: selected(:), cand_selected(:)
real(kind=dp), allocatable :: xsel(:,:), xcand(:,:)
real(kind=dp) :: kval, best_score, cand_score
integer :: p, j, nsel, iter
logical :: improved
kval = 2.0_dp
if (present(k)) kval = k
p = size(upper%xpred, 2)
allocate(selected(p), cand_selected(p))
selected = .false.
if (allocated(lower%xpred)) then
   do j = 1, min(size(lower%xpred, 2), p)
      selected(j) = .true.
   end do
end if
call build_lm_design(upper%xpred, selected, xsel)
best_fit = lm_fit_general(upper%y, xsel)
best_score = lm_aic_score(best_fit, kval)
do iter = 1, max(1, 2 * p + 2)
   improved = .false.
   cand_selected = selected
   do j = 1, p
      if (.not. selected(j)) then
         cand_selected = selected
         cand_selected(j) = .true.
         call build_lm_design(upper%xpred, cand_selected, xcand)
         cand_fit = lm_fit_general(upper%y, xcand)
         cand_score = lm_aic_score(cand_fit, kval)
         if (cand_score < best_score - 1.0e-8_dp) then
            best_score = cand_score
            best_fit = cand_fit
            selected = cand_selected
            improved = .true.
         end if
      end if
   end do
   do j = 1, p
      if (selected(j) .and. count(selected) > 0) then
         cand_selected = selected
         cand_selected(j) = .false.
         call build_lm_design(upper%xpred, cand_selected, xcand)
         cand_fit = lm_fit_general(upper%y, xcand)
         cand_score = lm_aic_score(cand_fit, kval)
         if (cand_score < best_score - 1.0e-8_dp) then
            best_score = cand_score
            best_fit = cand_fit
            selected = cand_selected
            improved = .true.
         end if
      end if
   end do
   if (.not. improved) exit
end do
end function step_lm

function lm_aic_score(fit, k) result(score)
type(lm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: k
real(kind=dp) :: score, rss, n
n = real(size(fit%y), kind=dp)
rss = max(tiny(1.0_dp), sum(fit%resid**2))
score = n * log(rss / max(1.0_dp, n)) + k * real(size(fit%coef), kind=dp)
end function lm_aic_score

subroutine build_lm_design(x, selected, out)
real(kind=dp), intent(in) :: x(:,:)
logical, intent(in) :: selected(:)
real(kind=dp), allocatable, intent(out) :: out(:,:)
integer :: j, jj, n, p
n = size(x, 1)
p = count(selected)
allocate(out(n, p))
jj = 0
do j = 1, min(size(x, 2), size(selected))
   if (selected(j)) then
      jj = jj + 1
      out(:, jj) = x(:, j)
   end if
end do
end subroutine build_lm_design

function lm_coef(y, xpred) result(coef)
! Fit linear model and return only coefficient vector.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
real(kind=dp), allocatable :: coef(:)
type(lm_fit_t) :: fit
fit = lm_fit_general(y, xpred)
coef = fit%coef
end function lm_coef

function lm_r_squared_general(y, xpred) result(out)
! Fit linear model and return only R-squared.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
real(kind=dp) :: out
type(lm_fit_t) :: fit
fit = lm_fit_general(y, xpred)
out = fit%r_squared
end function lm_r_squared_general

function glm_binomial_fit_int(y, xpred) result(fit)
! Fit a binomial-logit GLM for integer 0/1 responses.
integer, intent(in) :: y(:)
real(kind=dp), intent(in) :: xpred(:,:)
type(glm_fit_t) :: fit
fit = glm_binomial_fit_real(real(y, kind=dp), xpred)
end function glm_binomial_fit_int

function glm_binomial_fit_real(y, xpred) result(fit)
! Fit a binomial-logit GLM by Newton/IRLS normal equations.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
type(glm_fit_t) :: fit
integer :: i, j, l, n, p, k, iter
real(kind=dp), allocatable :: beta(:), grad(:), hess(:,:), hess0(:,:), step(:), eta(:), mu(:), w(:), rhs(:)
real(kind=dp) :: e, max_step
logical :: ok
if (size(y) /= size(xpred, 1)) error stop "error: glm response/predictor size mismatch"
n = size(y)
p = size(xpred, 2)
k = p + 1
allocate(beta(k), grad(k), hess(k,k), hess0(k,k), step(k), eta(n), mu(n), w(n), rhs(k))
beta = 0.0_dp
do iter = 1, 50
   eta = beta(1)
   if (p > 0) eta = eta + matmul(xpred, beta(2:k))
   eta = max(-35.0_dp, min(35.0_dp, eta))
   mu = 1.0_dp / (1.0_dp + exp(-eta))
   w = max(1.0e-12_dp, mu * (1.0_dp - mu))
   grad = 0.0_dp
   hess = 0.0_dp
   do i = 1, n
      e = y(i) - mu(i)
      grad(1) = grad(1) + e
      hess(1,1) = hess(1,1) + w(i)
      do j = 1, p
         grad(j+1) = grad(j+1) + xpred(i,j) * e
         hess(1,j+1) = hess(1,j+1) + w(i) * xpred(i,j)
         hess(j+1,1) = hess(1,j+1)
         do l = 1, p
            hess(j+1,l+1) = hess(j+1,l+1) + w(i) * xpred(i,j) * xpred(i,l)
         end do
      end do
   end do
   hess0 = hess
   rhs = grad
   call solve_linear(hess, rhs, step, ok)
   if (.not. ok) exit
   beta = beta + step
   max_step = maxval(abs(step))
   if (max_step < 1.0e-10_dp) then
      fit%convergence = 0
      exit
   end if
end do
fit%iter = iter
fit%family = 1
fit%coef = beta
fit%y = y
fit%xpred = xpred
allocate(fit%offset(n))
fit%offset = 0.0_dp
fit%fitted = glm_predict_response(fit, xpred)
fit%resid = y - fit%fitted
fit%df = n - k
allocate(fit%se(k), fit%z_value(k), fit%p_value(k))
fit%se = ieee_value(0.0_dp, ieee_quiet_nan)
fit%z_value = ieee_value(0.0_dp, ieee_quiet_nan)
fit%p_value = ieee_value(0.0_dp, ieee_quiet_nan)
do j = 1, k
   hess = hess0
   rhs = 0.0_dp
   rhs(j) = 1.0_dp
   call solve_linear(hess, rhs, step, ok)
   if (ok .and. step(j) >= 0.0_dp) then
      fit%se(j) = sqrt(step(j))
      if (fit%se(j) > 0.0_dp) then
         fit%z_value(j) = fit%coef(j) / fit%se(j)
         fit%p_value(j) = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(fit%z_value(j)))))
      end if
   end if
end do
end function glm_binomial_fit_real

function glm_poisson_fit_int(y, xpred, offset) result(fit)
! Fit a Poisson-log GLM for integer count responses.
integer, intent(in) :: y(:)
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), intent(in), optional :: offset(:)
type(glm_fit_t) :: fit
fit = glm_poisson_fit_real(real(y, kind=dp), xpred, offset)
end function glm_poisson_fit_int

function glm_poisson_fit_real(y, xpred, offset) result(fit)
! Fit a Poisson-log GLM by Newton/IRLS normal equations.
real(kind=dp), intent(in) :: y(:), xpred(:,:)
real(kind=dp), intent(in), optional :: offset(:)
type(glm_fit_t) :: fit
integer :: i, j, l, n, p, k, iter
real(kind=dp), allocatable :: beta(:), grad(:), hess(:,:), hess0(:,:), step(:), eta(:), mu(:), off(:), rhs(:)
real(kind=dp) :: e, max_step, base_rate
logical :: ok
if (size(y) /= size(xpred, 1)) error stop "error: glm response/predictor size mismatch"
n = size(y)
p = size(xpred, 2)
k = p + 1
allocate(beta(k), grad(k), hess(k,k), hess0(k,k), step(k), eta(n), mu(n), off(n), rhs(k))
off = 0.0_dp
if (present(offset)) then
   if (size(offset) /= n) error stop "error: glm offset size mismatch"
   off = offset
end if
base_rate = max(tiny(1.0_dp), (sum(y) + 0.1_dp) / real(max(1, n), kind=dp))
beta = 0.0_dp
beta(1) = log(base_rate)
do iter = 1, 80
   eta = off + beta(1)
   if (p > 0) eta = eta + matmul(xpred, beta(2:k))
   eta = max(-35.0_dp, min(35.0_dp, eta))
   mu = max(tiny(1.0_dp), exp(eta))
   grad = 0.0_dp
   hess = 0.0_dp
   do i = 1, n
      e = y(i) - mu(i)
      grad(1) = grad(1) + e
      hess(1,1) = hess(1,1) + mu(i)
      do j = 1, p
         grad(j+1) = grad(j+1) + xpred(i,j) * e
         hess(1,j+1) = hess(1,j+1) + mu(i) * xpred(i,j)
         hess(j+1,1) = hess(1,j+1)
         do l = 1, p
            hess(j+1,l+1) = hess(j+1,l+1) + mu(i) * xpred(i,j) * xpred(i,l)
         end do
      end do
   end do
   hess0 = hess
   rhs = grad
   call solve_linear(hess, rhs, step, ok)
   if (.not. ok) exit
   beta = beta + step
   max_step = maxval(abs(step))
   if (max_step < 1.0e-10_dp) then
      fit%convergence = 0
      exit
   end if
end do
fit%family = 2
fit%iter = iter
fit%coef = beta
fit%y = y
fit%xpred = xpred
fit%offset = off
fit%fitted = glm_predict_response(fit, xpred)
fit%resid = y - fit%fitted
fit%df = n - k
allocate(fit%se(k), fit%z_value(k), fit%p_value(k))
fit%se = ieee_value(0.0_dp, ieee_quiet_nan)
fit%z_value = ieee_value(0.0_dp, ieee_quiet_nan)
fit%p_value = ieee_value(0.0_dp, ieee_quiet_nan)
do j = 1, k
   hess = hess0
   rhs = 0.0_dp
   rhs(j) = 1.0_dp
   call solve_linear(hess, rhs, step, ok)
   if (ok .and. step(j) >= 0.0_dp) then
      fit%se(j) = sqrt(step(j))
      if (fit%se(j) > 0.0_dp) then
         fit%z_value(j) = fit%coef(j) / fit%se(j)
         fit%p_value(j) = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(fit%z_value(j)))))
      end if
   end if
end do
end function glm_poisson_fit_real

pure function glm_predict_response(fit, xpred) result(out)
! Predicted responses for a GLM.
type(glm_fit_t), intent(in) :: fit
real(kind=dp), intent(in) :: xpred(:,:)
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: eta(:)
integer :: p
p = size(xpred, 2)
allocate(out(size(xpred, 1)), eta(size(xpred, 1)))
eta = fit%coef(1)
if (p > 0) eta = eta + matmul(xpred, fit%coef(2:p+1))
eta = max(-35.0_dp, min(35.0_dp, eta))
if (fit%family == 2) then
   if (allocated(fit%offset) .and. size(fit%offset) == size(eta)) eta = eta + fit%offset
   out = exp(eta)
else
   out = 1.0_dp / (1.0_dp + exp(-eta))
end if
end function glm_predict_response

pure function glm_pearson_resid(fit) result(out)
! Pearson residuals for supported GLM families.
type(glm_fit_t), intent(in) :: fit
real(kind=dp), allocatable :: out(:)
real(kind=dp), allocatable :: v(:)
allocate(out(size(fit%fitted)), v(size(fit%fitted)))
if (fit%family == 2) then
   v = max(tiny(1.0_dp), fit%fitted)
else
   v = max(tiny(1.0_dp), fit%fitted * (1.0_dp - fit%fitted))
end if
out = (fit%y - fit%fitted) / sqrt(v)
end function glm_pearson_resid

subroutine print_glm_summary(fit, term_names)
! Print a compact coefficient table for a binomial-logit GLM.
type(glm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
integer :: j, p
character(len=32) :: lbl
write(*,'(a)') "glm summary:"
write(*,'(a)') "Coefficients:"
write(*,'(a12,1x,a10,1x,a10,1x,a8,1x,a10)') "", "Estimate", "Std. Error", "z value", "Pr(>|z|)"
p = size(fit%coef)
do j = 1, p
   if (j == 1) then
      lbl = "(Intercept)"
   else if (present(term_names) .and. size(term_names) >= j - 1) then
      lbl = trim(term_names(j - 1))
   else
      write(lbl,'(a,i0)') "x", j - 1
   end if
   write(*,'(a12,1x,f10.4,1x,f10.4,1x,f8.3,1x,es10.3)') &
      & trim(lbl), fit%coef(j), fit%se(j), fit%z_value(j), fit%p_value(j)
end do
write(*,'(a,i0)') "convergence: ", fit%convergence
end subroutine print_glm_summary

pure function polyroot(coef) result(root_mod)
! Return moduli of roots for coefficients in R polyroot order.
real(kind=dp), intent(in) :: coef(:)
real(kind=dp), allocatable :: root_mod(:)
integer :: n, i, j, iter
complex(kind=dp), allocatable :: z(:), znew(:)
complex(kind=dp) :: pz, denom
real(kind=dp) :: theta, max_delta
real(kind=dp), parameter :: twopi = 6.2831853071795864769252867665590058_dp
n = size(coef) - 1
allocate(root_mod(max(0, n)))
if (n <= 0) return
if (abs(coef(n + 1)) <= tiny(1.0_dp)) then
   root_mod = huge(1.0_dp)
   return
end if
allocate(z(n), znew(n))
do i = 1, n
   theta = twopi * real(i - 1, kind=dp) / real(n, kind=dp)
   z(i) = cmplx(cos(theta), sin(theta), kind=dp)
end do
do iter = 1, 200
   max_delta = 0.0_dp
   znew = z
   do i = 1, n
      pz = cmplx(coef(n + 1), 0.0_dp, kind=dp)
      do j = n, 1, -1
         pz = pz * z(i) + cmplx(coef(j), 0.0_dp, kind=dp)
      end do
      denom = cmplx(1.0_dp, 0.0_dp, kind=dp)
      do j = 1, n
         if (j /= i) denom = denom * (z(i) - z(j))
      end do
      if (abs(denom) > tiny(1.0_dp)) znew(i) = z(i) - (pz / cmplx(coef(n + 1), 0.0_dp, kind=dp)) / denom
      max_delta = max(max_delta, abs(znew(i) - z(i)))
   end do
   z = znew
   if (max_delta <= 100.0_dp * epsilon(1.0_dp)) exit
end do
root_mod = abs(z)
end function polyroot

subroutine print_lm_summary(fit, term_names)
! Print a compact summary of fitted linear model diagnostics.
type(lm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
integer :: j, p
real(kind=dp) :: se, tval, pval
real(kind=dp), allocatable :: rq(:)
character(len=32) :: lbl
write(*,'(a)') "lm summary:"
if (allocated(fit%resid) .and. size(fit%resid) > 0) then
   rq = quantile(fit%resid, [0.0_dp, 0.25_dp, 0.5_dp, 0.75_dp, 1.0_dp], .false., 7)
   write(*,'(a)') "Residuals:"
   write(*,'(a9,1x,a9,1x,a9,1x,a9,1x,a9)') "Min", "1Q", "Median", "3Q", "Max"
   write(*,'(f9.4,1x,f9.4,1x,f9.4,1x,f9.4,1x,f9.4)') rq
end if
write(*,'(a)') "Coefficients:"
write(*,'(a12,1x,a10,1x,a10,1x,a8,1x,a10)') "", "Estimate", "Std. Error", "t value", "Pr(>|t|)"
p = size(fit%coef)
do j = 1, p
   if (j == 1) then
      lbl = "(Intercept)"
   else if (present(term_names) .and. size(term_names) >= j - 1) then
      lbl = trim(term_names(j - 1))
   else
      write(lbl,'(a,i0)') "x", j - 1
   end if
   se = fit%sigma
   if (allocated(fit%cov_unscaled) .and. size(fit%cov_unscaled, 1) >= j .and. size(fit%cov_unscaled, 2) >= j) then
      se = fit%sigma * sqrt(max(0.0_dp, fit%cov_unscaled(j, j)))
   end if
   if (se > 0.0_dp) then
      tval = fit%coef(j) / se
      pval = student_t_two_sided_pvalue(tval, fit%df)
   else
      tval = ieee_value(0.0_dp, ieee_quiet_nan)
      pval = ieee_value(0.0_dp, ieee_quiet_nan)
   end if
   write(*,'(a12,1x,f10.4,1x,f10.4,1x,f8.3,1x,es10.3)') trim(lbl), fit%coef(j), se, tval, pval
end do
write(*,*)
write(*,'(a,g0)') "sigma: ", fit%sigma
write(*,'(a,g0)') "r.squared: ", fit%r_squared
write(*,'(a,g0)') "adj.r.squared: ", fit%adj_r_squared
end subroutine print_lm_summary

pure function student_t_two_sided_pvalue(t, df) result(p)
real(kind=dp), intent(in) :: t
integer, intent(in) :: df
real(kind=dp) :: p, x, a, b
if (df <= 0) then
   p = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
x = real(df, kind=dp) / (real(df, kind=dp) + t * t)
a = 0.5_dp * real(df, kind=dp)
b = 0.5_dp
p = regularized_beta(x, a, b)
p = max(0.0_dp, min(1.0_dp, p))
end function student_t_two_sided_pvalue

pure function regularized_beta(x, a, b) result(bt)
real(kind=dp), intent(in) :: x, a, b
real(kind=dp) :: bt, front
if (x <= 0.0_dp) then
   bt = 0.0_dp
   return
end if
if (x >= 1.0_dp) then
   bt = 1.0_dp
   return
end if
front = exp(log_gamma(a + b) - log_gamma(a) - log_gamma(b) + a * log(x) + b * log(1.0_dp - x))
if (x < (a + 1.0_dp) / (a + b + 2.0_dp)) then
   bt = front * beta_cont_frac(a, b, x) / a
else
   bt = 1.0_dp - front * beta_cont_frac(b, a, 1.0_dp - x) / b
end if
end function regularized_beta

pure function beta_cont_frac(a, b, x) result(h)
real(kind=dp), intent(in) :: a, b, x
real(kind=dp) :: h, aa, c, d, del, qab, qam, qap
integer :: m, m2
real(kind=dp), parameter :: fpmin = tiny(1.0_dp) / epsilon(1.0_dp)
qab = a + b
qap = a + 1.0_dp
qam = a - 1.0_dp
c = 1.0_dp
d = 1.0_dp - qab * x / qap
if (abs(d) < fpmin) d = fpmin
d = 1.0_dp / d
h = d
do m = 1, 200
   m2 = 2 * m
   aa = real(m, kind=dp) * (b - real(m, kind=dp)) * x / ((qam + real(m2, kind=dp)) * (a + real(m2, kind=dp)))
   d = 1.0_dp + aa * d
   if (abs(d) < fpmin) d = fpmin
   c = 1.0_dp + aa / c
   if (abs(c) < fpmin) c = fpmin
   d = 1.0_dp / d
   h = h * d * c
   aa = -(a + real(m, kind=dp)) * (qab + real(m, kind=dp)) * x / ((a + real(m2, kind=dp)) * (qap + real(m2, kind=dp)))
   d = 1.0_dp + aa * d
   if (abs(d) < fpmin) d = fpmin
   c = 1.0_dp + aa / c
   if (abs(c) < fpmin) c = fpmin
   d = 1.0_dp / d
   del = d * c
   h = h * del
   if (abs(del - 1.0_dp) <= 3.0e-14_dp) exit
end do
end function beta_cont_frac

subroutine print_lm_coef_rstyle(fit, term_names)
! Print coefficients with R-like aligned header/value rows.
type(lm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
integer :: j, p
character(len=32) :: lbl
p = max(0, size(fit%coef) - 1)
write(*,'(a14)', advance='no') "(Intercept)"
do j = 1, p
   if (present(term_names) .and. size(term_names) >= j) then
      write(*,'(a14)', advance='no') trim(term_names(j))
   else
      write(lbl,'(a,i0)') "x", j
      write(*,'(a14)', advance='no') trim(lbl)
   end if
end do
write(*,*)
if (size(fit%coef) > 0) then
   write(*,'(*(f14.7))') fit%coef
else
   write(*,*)
end if
end subroutine print_lm_coef_rstyle

subroutine print_lm_confint(fit, term_names)
! Print coefficient confidence intervals with R-like row and column labels.
type(lm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
real(kind=dp), allocatable :: ci(:,:)
integer :: j, p
character(len=32) :: lbl
ci = lm_confint(fit)
p = size(ci, 1)
write(*,'(13x,a12,1x,a12)') "2.5 %", "97.5 %"
do j = 1, p
   if (j == 1) then
      lbl = "(Intercept)"
   else if (present(term_names) .and. size(term_names) >= j - 1) then
      lbl = trim(term_names(j - 1))
   else
      write(lbl,'(a,i0)') "x", j - 1
   end if
   write(*,'(a12,1x,f12.7,1x,f12.7)') trim(lbl), ci(j, 1), ci(j, 2)
end do
end subroutine print_lm_confint

subroutine print_lm_anova(fit, term_names, term_df)
! Print a sequential analysis-of-variance table for an lm fit.
type(lm_fit_t), intent(in) :: fit
character(len=*), intent(in), optional :: term_names(:)
integer, intent(in), optional :: term_df(:)
type(lm_fit_t) :: fit_j
real(kind=dp) :: rss, rss_prev, ss_term, ms_term, ms_resid, fval, pval, ybar
integer :: j, p, nterms, df_term, first_col, last_col
character(len=32) :: lbl
rss = sum(fit%resid**2)
if (fit%df > 0) then
   ms_resid = rss / real(fit%df, kind=dp)
else
   ms_resid = ieee_value(0.0_dp, ieee_quiet_nan)
end if
write(*,'(a)') "Analysis of Variance Table"
write(*,'(a12,1x,a6,1x,a12,1x,a12,1x,a10,1x,a10)') "", "Df", "Sum Sq", "Mean Sq", "F value", "Pr(>F)"
if (allocated(fit%y) .and. allocated(fit%xpred)) then
   p = size(fit%xpred, 2)
   if (present(term_df)) then
      nterms = size(term_df)
   else
      nterms = p
   end if
   ybar = sum(fit%y) / real(size(fit%y), kind=dp)
   rss_prev = sum((fit%y - ybar)**2)
   first_col = 1
   do j = 1, nterms
      if (present(term_df)) then
         df_term = max(1, term_df(j))
      else
         df_term = 1
      end if
      last_col = min(p, first_col + df_term - 1)
      if (last_col < first_col) exit
      fit_j = lm_fit_general(fit%y, fit%xpred(:, 1:last_col))
      ss_term = max(0.0_dp, rss_prev - sum(fit_j%resid**2))
      ms_term = ss_term / real(max(1, df_term), kind=dp)
      if (ms_resid > 0.0_dp) then
         fval = ms_term / ms_resid
         pval = f_upper_tail_approx(fval, real(df_term, kind=dp), real(fit%df, kind=dp))
      else
         fval = ieee_value(0.0_dp, ieee_quiet_nan)
         pval = ieee_value(0.0_dp, ieee_quiet_nan)
      end if
      if (present(term_names) .and. size(term_names) >= j) then
         lbl = trim(term_names(j))
      else
         write(lbl,'(a,i0)') "x", j
      end if
      write(*,'(a12,1x,i6,1x,f12.4,1x,f12.4,1x,f10.2,1x,es10.3)') &
         & trim(lbl), df_term, ss_term, ms_term, fval, pval
      rss_prev = sum(fit_j%resid**2)
      first_col = last_col + 1
   end do
end if
write(*,'(a12,1x,i6,1x,f12.4,1x,f12.4)') "Residuals", fit%df, rss, ms_resid
end subroutine print_lm_anova

pure function f_upper_tail_approx(f, df1, df2) result(p)
real(kind=dp), intent(in) :: f, df1, df2
real(kind=dp) :: p, x
if (f <= 0.0_dp) then
   p = 1.0_dp
   return
end if
if (df1 <= 0.0_dp .or. df2 <= 0.0_dp) then
   p = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
x = df2 / (df2 + df1 * f)
p = max(0.0_dp, min(1.0_dp, regularized_beta(x, 0.5_dp * df2, 0.5_dp * df1)))
end function f_upper_tail_approx

pure elemental function normal_cdf(x) result(p)
real(kind=dp), intent(in) :: x
real(kind=dp) :: p
p = 0.5_dp * (1.0_dp + erf(x / sqrt(2.0_dp)))
end function normal_cdf

pure function chisq_upper_tail_approx(x, df) result(p)
real(kind=dp), intent(in) :: x, df
real(kind=dp) :: p, z
if (df <= 0.0_dp) then
   p = 1.0_dp
   return
end if
if (x <= 0.0_dp) then
   p = 1.0_dp
   return
end if
if (abs(df - 1.0_dp) <= epsilon(1.0_dp)) then
   p = erfc(sqrt(0.5_dp * x))
   return
end if
z = ((x / df)**(1.0_dp / 3.0_dp) - (1.0_dp - 2.0_dp / (9.0_dp * df))) / &
   & sqrt(2.0_dp / (9.0_dp * df))
p = max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(z)))
end function chisq_upper_tail_approx

pure function pchisq_scalar(x, df) result(p)
! Lower-tail chi-square probability.
real(kind=dp), intent(in) :: x, df
real(kind=dp) :: p
p = 1.0_dp - chisq_upper_tail_approx(x, df)
p = max(0.0_dp, min(1.0_dp, p))
end function pchisq_scalar

pure function qnorm(pv, mean, sd, lower_tail) result(out)
! Approximate normal quantiles by bisection.
real(kind=dp), intent(in) :: pv(:)
real(kind=dp), intent(in), optional :: mean, sd
logical, intent(in), optional :: lower_tail
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: mu, sig, prob, lo, hi, mid
logical :: lower
integer :: i, iter
mu = 0.0_dp
sig = 1.0_dp
lower = .true.
if (present(mean)) mu = mean
if (present(sd)) sig = sd
if (present(lower_tail)) lower = lower_tail
allocate(out(size(pv)))
do i = 1, size(pv)
   prob = max(0.0_dp, min(1.0_dp, pv(i)))
   if (.not. lower) prob = 1.0_dp - prob
   if (prob <= 0.0_dp) then
      out(i) = -huge(1.0_dp)
   else if (prob >= 1.0_dp) then
      out(i) = huge(1.0_dp)
   else
      lo = -8.0_dp
      hi = 8.0_dp
      do iter = 1, 80
         mid = 0.5_dp * (lo + hi)
         if (normal_cdf(mid) < prob) then
            lo = mid
         else
            hi = mid
         end if
      end do
      out(i) = mu + sig * 0.5_dp * (lo + hi)
   end if
end do
end function qnorm

pure function ppois(q, lambda, lower_tail) result(out)
! Lower-tail Poisson CDF for vector q.
real(kind=dp), intent(in) :: q(:)
real(kind=dp), intent(in), optional :: lambda
logical, intent(in), optional :: lower_tail
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lam, term, cdf
logical :: lower
integer :: i, k, kk
lam = 1.0_dp
lower = .true.
if (present(lambda)) lam = lambda
if (present(lower_tail)) lower = lower_tail
allocate(out(size(q)))
do i = 1, size(q)
   if (lam < 0.0_dp) then
      out(i) = ieee_value(0.0_dp, ieee_quiet_nan)
   else if (q(i) < 0.0_dp) then
      out(i) = merge(0.0_dp, 1.0_dp, lower)
   else
      kk = int(floor(q(i)))
      term = exp(-lam)
      cdf = term
      do k = 1, kk
         term = term * lam / real(k, kind=dp)
         cdf = cdf + term
      end do
      cdf = max(0.0_dp, min(1.0_dp, cdf))
      out(i) = merge(cdf, 1.0_dp - cdf, lower)
   end if
end do
end function ppois

pure function qpois(pv, lambda, lower_tail) result(out)
! Poisson quantiles as real values for matrix/vector composition.
real(kind=dp), intent(in) :: pv(:)
real(kind=dp), intent(in), optional :: lambda
logical, intent(in), optional :: lower_tail
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lam, prob, term, cdf
logical :: lower
integer :: i, k
lam = 1.0_dp
lower = .true.
if (present(lambda)) lam = lambda
if (present(lower_tail)) lower = lower_tail
allocate(out(size(pv)))
do i = 1, size(pv)
   prob = max(0.0_dp, min(1.0_dp, pv(i)))
   if (.not. lower) prob = 1.0_dp - prob
   if (lam < 0.0_dp) then
      out(i) = ieee_value(0.0_dp, ieee_quiet_nan)
   else
      term = exp(-lam)
      cdf = term
      k = 0
      do while (cdf < prob .and. k < 100000)
         k = k + 1
         term = term * lam / real(k, kind=dp)
         cdf = cdf + term
      end do
      out(i) = real(k, kind=dp)
   end if
end do
end function qpois

pure elemental function r_choose_real(n, k) result(out)
real(kind=dp), intent(in) :: n, k
real(kind=dp) :: out
if (k < 0.0_dp .or. k > n) then
   out = 0.0_dp
else
   out = exp(log_gamma(n + 1.0_dp) - log_gamma(k + 1.0_dp) - log_gamma(n - k + 1.0_dp))
end if
end function r_choose_real

pure elemental function gamma_p(a, x) result(gp)
real(kind=dp), intent(in) :: a, x
real(kind=dp) :: gp, gln, ap, del, sumv, b, c, d, h, an
integer :: n
if (a <= 0.0_dp .or. x < 0.0_dp) then
   gp = ieee_value(0.0_dp, ieee_quiet_nan)
   return
end if
if (x == 0.0_dp) then
   gp = 0.0_dp
   return
end if
gln = log_gamma(a)
if (x < a + 1.0_dp) then
   ap = a
   sumv = 1.0_dp / a
   del = sumv
   do n = 1, 200
      ap = ap + 1.0_dp
      del = del * x / ap
      sumv = sumv + del
      if (abs(del) < abs(sumv) * 3.0e-14_dp) exit
   end do
   gp = sumv * exp(-x + a * log(x) - gln)
else
   b = x + 1.0_dp - a
   c = 1.0e30_dp
   d = 1.0_dp / b
   h = d
   do n = 1, 200
      an = -real(n, kind=dp) * (real(n, kind=dp) - a)
      b = b + 2.0_dp
      d = an * d + b
      if (abs(d) < 1.0e-30_dp) d = 1.0e-30_dp
      c = b + an / c
      if (abs(c) < 1.0e-30_dp) c = 1.0e-30_dp
      d = 1.0_dp / d
      del = d * c
      h = h * del
      if (abs(del - 1.0_dp) < 3.0e-14_dp) exit
   end do
   gp = 1.0_dp - exp(-x + a * log(x) - gln) * h
end if
gp = max(0.0_dp, min(1.0_dp, gp))
end function gamma_p

pure function dunif(x, min, max) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: min, max
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lo, hi
lo = 0.0_dp; hi = 1.0_dp
if (present(min)) lo = min
if (present(max)) hi = max
allocate(out(size(x)))
out = 0.0_dp
if (hi > lo) where (x >= lo .and. x <= hi) out = 1.0_dp / (hi - lo)
end function dunif

pure function punif(x, min, max) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: min, max
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lo, hi
lo = 0.0_dp; hi = 1.0_dp
if (present(min)) lo = min
if (present(max)) hi = max
allocate(out(size(x)))
if (hi <= lo) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
else
   out = (x - lo) / (hi - lo)
   where (out < 0.0_dp) out = 0.0_dp
   where (out > 1.0_dp) out = 1.0_dp
end if
end function punif

pure function qunif(p, min, max) result(out)
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in), optional :: min, max
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lo, hi
lo = 0.0_dp; hi = 1.0_dp
if (present(min)) lo = min
if (present(max)) hi = max
allocate(out(size(p)))
out = p
where (out < 0.0_dp) out = 0.0_dp
where (out > 1.0_dp) out = 1.0_dp
out = lo + out * (hi - lo)
end function qunif

pure function dexp(x, rate, log_) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: rate
logical, intent(in), optional :: log_
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: r
logical :: l
r = 1.0_dp; if (present(rate)) r = rate
l = .false.; if (present(log_)) l = log_
allocate(out(size(x)))
if (l) then
   out = merge(log(r) - r * x, -huge(1.0_dp), x >= 0.0_dp .and. r > 0.0_dp)
else
   out = merge(r * exp(-r * x), 0.0_dp, x >= 0.0_dp .and. r > 0.0_dp)
end if
end function dexp

pure function pexp(x, rate) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: rate
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: r
r = 1.0_dp; if (present(rate)) r = rate
allocate(out(size(x)))
out = merge(1.0_dp - exp(-r * x), 0.0_dp, x >= 0.0_dp .and. r > 0.0_dp)
end function pexp

pure function qexp(p, rate) result(out)
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in), optional :: rate
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: r
r = 1.0_dp; if (present(rate)) r = rate
allocate(out(size(p)))
out = -log(max(0.0_dp, 1.0_dp - max(0.0_dp, min(1.0_dp, p)))) / r
end function qexp

pure function dgamma(x, shape, rate) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: shape
real(kind=dp), intent(in), optional :: rate
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: r, logc
r = 1.0_dp; if (present(rate)) r = rate
allocate(out(size(x)))
if (shape <= 0.0_dp .or. r <= 0.0_dp) then
   out = ieee_value(0.0_dp, ieee_quiet_nan)
else
   logc = shape * log(r) - log_gamma(shape)
   out = merge(exp(logc + (shape - 1.0_dp) * log(max(x, tiny(1.0_dp))) - r * x), 0.0_dp, x >= 0.0_dp)
end if
end function dgamma

pure function pgamma(x, shape, rate) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in) :: shape
real(kind=dp), intent(in), optional :: rate
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: r
integer :: i
r = 1.0_dp; if (present(rate)) r = rate
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = merge(gamma_p(shape, r * x(i)), 0.0_dp, x(i) >= 0.0_dp .and. shape > 0.0_dp .and. r > 0.0_dp)
end do
end function pgamma

pure function qgamma(p, shape, rate) result(out)
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in) :: shape
real(kind=dp), intent(in), optional :: rate
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: r, lo, hi, mid
integer :: i, it
r = 1.0_dp; if (present(rate)) r = rate
allocate(out(size(p)))
do i = 1, size(p)
   lo = 0.0_dp; hi = max(1.0_dp, shape / r * 10.0_dp)
   do while (gamma_p(shape, r * hi) < p(i))
      hi = hi * 2.0_dp
      if (hi > 1.0e12_dp) exit
   end do
   do it = 1, 70
      mid = 0.5_dp * (lo + hi)
      if (gamma_p(shape, r * mid) < p(i)) lo = mid
      if (gamma_p(shape, r * mid) >= p(i)) hi = mid
   end do
   out(i) = 0.5_dp * (lo + hi)
end do
end function qgamma

pure function dbeta(x, shape1, shape2) result(out)
real(kind=dp), intent(in) :: x(:), shape1, shape2
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: logc
allocate(out(size(x)))
logc = log_gamma(shape1 + shape2) - log_gamma(shape1) - log_gamma(shape2)
out = merge(exp(logc + (shape1 - 1.0_dp) * log(max(x, tiny(1.0_dp))) + (shape2 - 1.0_dp) * log(max(1.0_dp - x, tiny(1.0_dp)))), 0.0_dp, x > 0.0_dp .and. x < 1.0_dp)
end function dbeta

pure function pbeta(x, shape1, shape2) result(out)
real(kind=dp), intent(in) :: x(:), shape1, shape2
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = merge(0.0_dp, merge(1.0_dp, regularized_beta(x(i), shape1, shape2), x(i) >= 1.0_dp), x(i) <= 0.0_dp)
end do
end function pbeta

pure function qbeta(p, shape1, shape2) result(out)
real(kind=dp), intent(in) :: p(:), shape1, shape2
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lo, hi, mid
integer :: i, it
allocate(out(size(p)))
do i = 1, size(p)
   lo = 0.0_dp; hi = 1.0_dp
   do it = 1, 70
      mid = 0.5_dp * (lo + hi)
      if (regularized_beta(mid, shape1, shape2) < p(i)) lo = mid
      if (regularized_beta(mid, shape1, shape2) >= p(i)) hi = mid
   end do
   out(i) = 0.5_dp * (lo + hi)
end do
end function qbeta

pure function dchisq(x, df) result(out)
real(kind=dp), intent(in) :: x(:), df
real(kind=dp), allocatable :: out(:)
out = dgamma(x, 0.5_dp * df, rate=0.5_dp)
end function dchisq

pure function pchisq_vec(x, df) result(out)
real(kind=dp), intent(in) :: x(:), df
real(kind=dp), allocatable :: out(:)
out = pgamma(x, 0.5_dp * df, rate=0.5_dp)
end function pchisq_vec

pure function qchisq_vec(p, df) result(out)
real(kind=dp), intent(in) :: p(:), df
real(kind=dp), allocatable :: out(:)
out = qgamma(p, 0.5_dp * df, rate=0.5_dp)
end function qchisq_vec

pure function qchisq_scalar(p, df) result(out)
real(kind=dp), intent(in) :: p, df
real(kind=dp) :: out
real(kind=dp), allocatable :: tmp(:)
tmp = qchisq_vec([p], df)
out = tmp(1)
end function qchisq_scalar

pure function qchisq_scalar_i(p, df) result(out)
real(kind=dp), intent(in) :: p
integer, intent(in) :: df
real(kind=dp) :: out
out = qchisq_scalar(p, real(df, kind=dp))
end function qchisq_scalar_i

pure function dt(x, df) result(out)
real(kind=dp), intent(in) :: x(:), df
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: logc
allocate(out(size(x)))
logc = log_gamma(0.5_dp * (df + 1.0_dp)) - log_gamma(0.5_dp * df) - 0.5_dp * log(df * acos(-1.0_dp))
out = exp(logc - 0.5_dp * (df + 1.0_dp) * log(1.0_dp + x * x / df))
end function dt

pure function pt(x, df) result(out)
real(kind=dp), intent(in) :: x(:), df
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: z
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   z = df / (df + x(i) * x(i))
   if (x(i) >= 0.0_dp) then
      out(i) = 1.0_dp - 0.5_dp * regularized_beta(z, 0.5_dp * df, 0.5_dp)
   else
      out(i) = 0.5_dp * regularized_beta(z, 0.5_dp * df, 0.5_dp)
   end if
end do
end function pt

pure function qt(p, df) result(out)
real(kind=dp), intent(in) :: p(:), df
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lo, hi, mid, pmid(1)
integer :: i, it
allocate(out(size(p)))
do i = 1, size(p)
   lo = -32.0_dp; hi = 32.0_dp
   do it = 1, 80
      mid = 0.5_dp * (lo + hi)
      pmid = pt([mid], df)
      if (pmid(1) < p(i)) lo = mid
      if (pmid(1) >= p(i)) hi = mid
   end do
   out(i) = 0.5_dp * (lo + hi)
end do
end function qt

pure function df(x, df1, df2) result(out)
real(kind=dp), intent(in) :: x(:), df1, df2
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: a, b, logc
allocate(out(size(x)))
a = 0.5_dp * df1; b = 0.5_dp * df2
logc = a * log(df1 / df2) - (log_gamma(a) + log_gamma(b) - log_gamma(a + b))
out = merge(exp(logc + (a - 1.0_dp) * log(max(x, tiny(1.0_dp))) - (a + b) * log(1.0_dp + (df1 / df2) * x)), 0.0_dp, x > 0.0_dp)
end function df

pure function pf(x, df1, df2) result(out)
real(kind=dp), intent(in) :: x(:), df1, df2
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: z
integer :: i
allocate(out(size(x)))
do i = 1, size(x)
   if (x(i) <= 0.0_dp) then
      out(i) = 0.0_dp
   else
      z = (df1 * x(i)) / (df1 * x(i) + df2)
      out(i) = regularized_beta(z, 0.5_dp * df1, 0.5_dp * df2)
   end if
end do
end function pf

pure function qf(p, df1, df2) result(out)
real(kind=dp), intent(in) :: p(:), df1, df2
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lo, hi, mid, tmp(1)
integer :: i, it
allocate(out(size(p)))
do i = 1, size(p)
   lo = 0.0_dp; hi = 1.0_dp
   do
      tmp = pf([hi], df1, df2)
      if (tmp(1) >= p(i) .or. hi > 1.0e12_dp) exit
      hi = hi * 2.0_dp
   end do
   do it = 1, 70
      mid = 0.5_dp * (lo + hi)
      tmp = pf([mid], df1, df2)
      if (tmp(1) < p(i)) lo = mid
      if (tmp(1) >= p(i)) hi = mid
   end do
   out(i) = 0.5_dp * (lo + hi)
end do
end function qf

pure function dlogis(x, location, scale) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: location, scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: loc, sc, z(size(x)), ez(size(x))
loc = 0.0_dp; sc = 1.0_dp
if (present(location)) loc = location
if (present(scale)) sc = scale
allocate(out(size(x)))
z = (x - loc) / sc; ez = exp(-z)
out = ez / (sc * (1.0_dp + ez)**2)
end function dlogis

pure function plogis(x, location, scale) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: location, scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: loc, sc
loc = 0.0_dp; sc = 1.0_dp
if (present(location)) loc = location
if (present(scale)) sc = scale
allocate(out(size(x)))
out = 1.0_dp / (1.0_dp + exp(-((x - loc) / sc)))
end function plogis

pure function qlogis(p, location, scale) result(out)
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in), optional :: location, scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: loc, sc, pp(size(p))
loc = 0.0_dp; sc = 1.0_dp
if (present(location)) loc = location
if (present(scale)) sc = scale
allocate(out(size(p)))
pp = max(tiny(1.0_dp), min(1.0_dp - epsilon(1.0_dp), p))
out = loc + sc * log(pp / (1.0_dp - pp))
end function qlogis

pure function dlnorm(x, meanlog, sdlog) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: meanlog, sdlog
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: mu, sig
mu = 0.0_dp; sig = 1.0_dp
if (present(meanlog)) mu = meanlog
if (present(sdlog)) sig = sdlog
allocate(out(size(x)))
out = merge(exp(-log(x * sig * sqrt(2.0_dp * acos(-1.0_dp))) - 0.5_dp * ((log(x) - mu) / sig)**2), 0.0_dp, x > 0.0_dp)
end function dlnorm

pure function plnorm(x, meanlog, sdlog) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: meanlog, sdlog
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: mu, sig
mu = 0.0_dp; sig = 1.0_dp
if (present(meanlog)) mu = meanlog
if (present(sdlog)) sig = sdlog
allocate(out(size(x)))
out = merge(normal_cdf((log(x) - mu) / sig), 0.0_dp, x > 0.0_dp)
end function plnorm

pure function qlnorm(p, meanlog, sdlog) result(out)
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in), optional :: meanlog, sdlog
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: mu, sig
mu = 0.0_dp; sig = 1.0_dp
if (present(meanlog)) mu = meanlog
if (present(sdlog)) sig = sdlog
out = exp(qnorm(p, mean=mu, sd=sig))
end function qlnorm

pure function dweibull(x, shape, scale) result(out)
real(kind=dp), intent(in) :: x(:), shape
real(kind=dp), intent(in), optional :: scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: sc
sc = 1.0_dp; if (present(scale)) sc = scale
allocate(out(size(x)))
out = merge((shape / sc) * (x / sc)**(shape - 1.0_dp) * exp(-(x / sc)**shape), 0.0_dp, x >= 0.0_dp)
end function dweibull

pure function pweibull(x, shape, scale) result(out)
real(kind=dp), intent(in) :: x(:), shape
real(kind=dp), intent(in), optional :: scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: sc
sc = 1.0_dp; if (present(scale)) sc = scale
allocate(out(size(x)))
out = merge(1.0_dp - exp(-(x / sc)**shape), 0.0_dp, x >= 0.0_dp)
end function pweibull

pure function qweibull(p, shape, scale) result(out)
real(kind=dp), intent(in) :: p(:), shape
real(kind=dp), intent(in), optional :: scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: sc
sc = 1.0_dp; if (present(scale)) sc = scale
allocate(out(size(p)))
out = sc * (-log(max(tiny(1.0_dp), 1.0_dp - max(0.0_dp, min(1.0_dp, p)))))**(1.0_dp / shape)
end function qweibull

pure function dcauchy(x, location, scale) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: location, scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: loc, sc, z(size(x))
loc = 0.0_dp; sc = 1.0_dp
if (present(location)) loc = location
if (present(scale)) sc = scale
allocate(out(size(x)))
z = (x - loc) / sc
out = 1.0_dp / (acos(-1.0_dp) * sc * (1.0_dp + z * z))
end function dcauchy

pure function pcauchy(x, location, scale) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: location, scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: loc, sc
loc = 0.0_dp; sc = 1.0_dp
if (present(location)) loc = location
if (present(scale)) sc = scale
allocate(out(size(x)))
out = 0.5_dp + atan((x - loc) / sc) / acos(-1.0_dp)
end function pcauchy

pure function qcauchy(p, location, scale) result(out)
real(kind=dp), intent(in) :: p(:)
real(kind=dp), intent(in), optional :: location, scale
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: loc, sc
loc = 0.0_dp; sc = 1.0_dp
if (present(location)) loc = location
if (present(scale)) sc = scale
allocate(out(size(p)))
out = loc + sc * tan(acos(-1.0_dp) * (p - 0.5_dp))
end function qcauchy

pure function dbinom(x, nsize, prob) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nsize
real(kind=dp), intent(in) :: prob
real(kind=dp), allocatable :: out(:)
integer :: i, k
allocate(out(size(x)))
do i = 1, size(x)
   k = int(x(i))
   if (k < 0 .or. k > nsize .or. prob < 0.0_dp .or. prob > 1.0_dp) then
      out(i) = 0.0_dp
   else
      out(i) = r_choose_real(real(nsize, kind=dp), real(k, kind=dp)) * prob**k * (1.0_dp - prob)**(nsize - k)
   end if
end do
end function dbinom

pure function pbinom(x, nsize, prob) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: nsize
real(kind=dp), intent(in) :: prob
real(kind=dp), allocatable :: out(:)
integer :: i, k
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = 0.0_dp
   do k = 0, min(nsize, int(floor(x(i))))
      out(i) = out(i) + r_choose_real(real(nsize, kind=dp), real(k, kind=dp)) * prob**k * (1.0_dp - prob)**(nsize - k)
   end do
end do
end function pbinom

pure function qbinom(p, nsize, prob) result(out)
real(kind=dp), intent(in) :: p(:)
integer, intent(in) :: nsize
real(kind=dp), intent(in) :: prob
real(kind=dp), allocatable :: out(:)
integer :: i, k
real(kind=dp) :: cdf
allocate(out(size(p)))
do i = 1, size(p)
   cdf = 0.0_dp
   do k = 0, nsize
      cdf = cdf + r_choose_real(real(nsize, kind=dp), real(k, kind=dp)) * prob**k * (1.0_dp - prob)**(nsize - k)
      if (cdf >= p(i)) exit
   end do
   out(i) = real(k, kind=dp)
end do
end function qbinom

pure function dpois(x, lambda) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: lambda
real(kind=dp), allocatable :: out(:)
real(kind=dp) :: lam
integer :: i, k
lam = 1.0_dp; if (present(lambda)) lam = lambda
allocate(out(size(x)))
do i = 1, size(x)
   k = int(x(i))
   out(i) = merge(exp(-lam + real(k, kind=dp) * log(lam) - log_gamma(real(k + 1, kind=dp))), 0.0_dp, k >= 0)
end do
end function dpois

pure function dgeom(x, prob) result(out)
real(kind=dp), intent(in) :: x(:), prob
real(kind=dp), allocatable :: out(:)
allocate(out(size(x)))
out = merge(prob * (1.0_dp - prob)**int(x), 0.0_dp, x >= 0.0_dp)
end function dgeom

pure function pgeom(x, prob) result(out)
real(kind=dp), intent(in) :: x(:), prob
real(kind=dp), allocatable :: out(:)
allocate(out(size(x)))
out = merge(1.0_dp - (1.0_dp - prob)**(int(x) + 1), 0.0_dp, x >= 0.0_dp)
end function pgeom

pure function qgeom(p, prob) result(out)
real(kind=dp), intent(in) :: p(:), prob
real(kind=dp), allocatable :: out(:)
allocate(out(size(p)))
out = ceiling(log(max(tiny(1.0_dp), 1.0_dp - p)) / log(1.0_dp - prob) - 1.0_dp)
where (out < 0.0_dp) out = 0.0_dp
end function qgeom

pure function dnbinom(x, nsize, prob) result(out)
real(kind=dp), intent(in) :: x(:), prob
integer, intent(in) :: nsize
real(kind=dp), allocatable :: out(:)
integer :: i, k
allocate(out(size(x)))
do i = 1, size(x)
   k = int(x(i))
   out(i) = merge(r_choose_real(real(k + nsize - 1, kind=dp), real(k, kind=dp)) * prob**nsize * (1.0_dp - prob)**k, 0.0_dp, k >= 0)
end do
end function dnbinom

pure function pnbinom(x, nsize, prob) result(out)
real(kind=dp), intent(in) :: x(:), prob
integer, intent(in) :: nsize
real(kind=dp), allocatable :: out(:)
integer :: i, k
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = 0.0_dp
   do k = 0, int(floor(x(i)))
      out(i) = out(i) + r_choose_real(real(k + nsize - 1, kind=dp), real(k, kind=dp)) * prob**nsize * (1.0_dp - prob)**k
   end do
end do
end function pnbinom

pure function qnbinom(p, nsize, prob) result(out)
real(kind=dp), intent(in) :: p(:), prob
integer, intent(in) :: nsize
real(kind=dp), allocatable :: out(:)
integer :: i, k
real(kind=dp) :: cdf
allocate(out(size(p)))
do i = 1, size(p)
   cdf = 0.0_dp; k = 0
   do while (cdf < p(i) .and. k < 100000)
      cdf = cdf + r_choose_real(real(k + nsize - 1, kind=dp), real(k, kind=dp)) * prob**nsize * (1.0_dp - prob)**k
      if (cdf >= p(i)) exit
      k = k + 1
   end do
   out(i) = real(k, kind=dp)
end do
end function qnbinom

pure function dhyper(x, m, n, k) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: m, n, k
real(kind=dp), allocatable :: out(:)
integer :: i, xx
real(kind=dp) :: den
allocate(out(size(x)))
den = r_choose_real(real(m + n, kind=dp), real(k, kind=dp))
do i = 1, size(x)
   xx = int(x(i))
   out(i) = r_choose_real(real(m, kind=dp), real(xx, kind=dp)) * &
      & r_choose_real(real(n, kind=dp), real(k - xx, kind=dp)) / den
end do
end function dhyper

pure function phyper(x, m, n, k) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: m, n, k
real(kind=dp), allocatable :: out(:)
integer :: i, xx
allocate(out(size(x)))
do i = 1, size(x)
   out(i) = 0.0_dp
   do xx = 0, int(floor(x(i)))
      out(i) = out(i) + r_choose_real(real(m, kind=dp), real(xx, kind=dp)) * &
         & r_choose_real(real(n, kind=dp), real(k - xx, kind=dp)) / &
         & r_choose_real(real(m + n, kind=dp), real(k, kind=dp))
   end do
end do
end function phyper

pure function qhyper(p, m, n, k) result(out)
real(kind=dp), intent(in) :: p(:)
integer, intent(in) :: m, n, k
real(kind=dp), allocatable :: out(:)
integer :: i, xx
real(kind=dp) :: cdf
allocate(out(size(p)))
do i = 1, size(p)
   cdf = 0.0_dp
   do xx = 0, k
      cdf = cdf + r_choose_real(real(m, kind=dp), real(xx, kind=dp)) * &
         & r_choose_real(real(n, kind=dp), real(k - xx, kind=dp)) / &
         & r_choose_real(real(m + n, kind=dp), real(k, kind=dp))
      if (cdf >= p(i)) exit
   end do
   out(i) = real(xx, kind=dp)
end do
end function qhyper

pure function wilcox_counts(m, n) result(counts)
integer, intent(in) :: m, n
real(kind=dp), allocatable :: counts(:)
integer :: maxw, offset, maxrank, r, j, s, w
real(kind=dp), allocatable :: dpw(:,:)
maxw = m * n
offset = m * (m + 1) / 2
maxrank = offset + maxw
allocate(dpw(0:m, 0:maxrank))
dpw = 0.0_dp
dpw(0, 0) = 1.0_dp
do r = 1, m + n
   do j = min(r, m), 1, -1
      do s = maxrank, r, -1
         dpw(j, s) = dpw(j, s) + dpw(j - 1, s - r)
      end do
   end do
end do
allocate(counts(maxw + 1))
counts = 0.0_dp
do w = 0, maxw
   counts(w + 1) = dpw(m, w + offset)
end do
end function wilcox_counts

pure function dwilcox(x, m, n) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: m, n
real(kind=dp), allocatable :: out(:), counts(:)
integer :: i, xx, offset
real(kind=dp) :: den
counts = wilcox_counts(m, n)
offset = m * (m + 1) / 2
den = r_choose_real(real(m + n, kind=dp), real(m, kind=dp))
allocate(out(size(x)))
do i = 1, size(x)
   xx = int(x(i))
   if (xx >= 0 .and. xx <= m * n) then
      out(i) = counts(xx + 1) / den
   else
      out(i) = 0.0_dp
   end if
end do
end function dwilcox

pure function pwilcox(x, m, n) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: m, n
real(kind=dp), allocatable :: out(:), d(:)
integer :: i, xx
allocate(out(size(x)))
d = dwilcox([(real(xx, kind=dp), xx = 0, m * n)], m, n)
do i = 1, size(x)
   xx = max(0, min(m * n, int(floor(x(i)))))
   out(i) = sum(d(1:xx + 1))
end do
end function pwilcox

pure function qwilcox(p, m, n) result(out)
real(kind=dp), intent(in) :: p(:)
integer, intent(in) :: m, n
real(kind=dp), allocatable :: out(:), d(:)
integer :: i, xx
real(kind=dp) :: cdf
allocate(out(size(p)))
d = dwilcox([(real(xx, kind=dp), xx = 0, m * n)], m, n)
do i = 1, size(p)
   cdf = 0.0_dp
   do xx = 0, m * n
      cdf = cdf + d(xx + 1)
      if (cdf >= p(i)) exit
   end do
   out(i) = real(xx, kind=dp)
end do
end function qwilcox

pure function signrank_counts(n) result(counts)
integer, intent(in) :: n
real(kind=dp), allocatable :: counts(:)
integer :: maxs, r, s
maxs = n * (n + 1) / 2
allocate(counts(maxs + 1))
counts = 0.0_dp
counts(1) = 1.0_dp
do r = 1, n
   do s = maxs, r, -1
      counts(s + 1) = counts(s + 1) + counts(s - r + 1)
   end do
end do
end function signrank_counts

pure function dsignrank(x, n) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:), counts(:)
integer :: i, xx
real(kind=dp) :: den
counts = signrank_counts(n)
den = 2.0_dp**n
allocate(out(size(x)))
do i = 1, size(x)
   xx = int(x(i))
   if (xx >= 0 .and. xx <= n * (n + 1) / 2) then
      out(i) = counts(xx + 1) / den
   else
      out(i) = 0.0_dp
   end if
end do
end function dsignrank

pure function psignrank(x, n) result(out)
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:), d(:)
integer :: i, xx, maxs
maxs = n * (n + 1) / 2
allocate(out(size(x)))
d = dsignrank([(real(xx, kind=dp), xx = 0, maxs)], n)
do i = 1, size(x)
   xx = max(0, min(maxs, int(floor(x(i)))))
   out(i) = sum(d(1:xx + 1))
end do
end function psignrank

pure function qsignrank(p, n) result(out)
real(kind=dp), intent(in) :: p(:)
integer, intent(in) :: n
real(kind=dp), allocatable :: out(:), d(:)
integer :: i, xx, maxs
real(kind=dp) :: cdf
maxs = n * (n + 1) / 2
allocate(out(size(p)))
d = dsignrank([(real(xx, kind=dp), xx = 0, maxs)], n)
do i = 1, size(p)
   cdf = 0.0_dp
   do xx = 0, maxs
      cdf = cdf + d(xx + 1)
      if (cdf >= p(i)) exit
   end do
   out(i) = real(xx, kind=dp)
end do
end function qsignrank

pure function chisq_test_real_vec(x, p) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: p(:)
type(chisq_test_result_t) :: out
real(kind=dp), allocatable :: expected(:), prob(:)
real(kind=dp) :: total, psum
integer :: n
n = size(x)
if (n <= 1) return
total = sum(x)
if (total <= 0.0_dp) return
if (present(p)) then
   if (size(p) /= n) return
   psum = sum(p)
   if (psum <= 0.0_dp) return
   prob = p / psum
else
   allocate(prob(n))
   prob = 1.0_dp / real(n, kind=dp)
end if
expected = total * prob
if (any(expected <= 0.0_dp)) return
out%statistic = sum((x - expected)**2 / expected)
out%parameter = n - 1
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 1
end function chisq_test_real_vec

pure function chisq_test_int_vec(x, p) result(out)
integer, intent(in) :: x(:)
real(kind=dp), intent(in), optional :: p(:)
type(chisq_test_result_t) :: out
if (present(p)) then
   out = chisq_test_real_vec(real(x, kind=dp), p)
else
   out = chisq_test_real_vec(real(x, kind=dp))
end if
end function chisq_test_int_vec

pure function chisq_test_int_mat(x) result(out)
integer, intent(in) :: x(:,:)
type(chisq_test_result_t) :: out
out = chisq_test_real_mat(real(x, kind=dp))
end function chisq_test_int_mat

pure function chisq_test_real_mat(x) result(out)
real(kind=dp), intent(in) :: x(:,:)
type(chisq_test_result_t) :: out
real(kind=dp), allocatable :: xr(:,:), row_tot(:), col_tot(:), expected(:,:)
real(kind=dp) :: total
integer :: nr, nc, i, j
nr = size(x, 1)
nc = size(x, 2)
if (nr <= 1 .or. nc <= 1) return
xr = x
total = sum(xr)
if (total <= 0.0_dp) return
allocate(row_tot(nr), col_tot(nc), expected(nr, nc))
do i = 1, nr
   row_tot(i) = sum(xr(i, :))
end do
do j = 1, nc
   col_tot(j) = sum(xr(:, j))
end do
do i = 1, nr
   do j = 1, nc
      expected(i, j) = row_tot(i) * col_tot(j) / total
   end do
end do
if (any(expected <= 0.0_dp)) return
out%statistic = sum((xr - expected)**2 / expected)
out%parameter = (nr - 1) * (nc - 1)
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 2
end function chisq_test_real_mat

pure function prop_test_int_scalar(x, n, p, correct) result(out)
integer, intent(in) :: x, n
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
out = prop_test_real_scalar(real(x, kind=dp), real(n, kind=dp), p, correct)
end function prop_test_int_scalar

pure function prop_test_real_scalar(x, n, p, correct) result(out)
real(kind=dp), intent(in) :: x, n
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
real(kind=dp) :: p0, diff, denom
logical :: use_correct
p0 = 0.5_dp
if (present(p)) p0 = p
if (n <= 0.0_dp .or. p0 <= 0.0_dp .or. p0 >= 1.0_dp) return
use_correct = .false.
if (present(correct)) use_correct = correct
out%estimate = x / n
out%null_value = p0
out%parameter = 1
diff = abs(x - n * p0)
if (use_correct) diff = max(0.0_dp, diff - 0.5_dp)
denom = n * p0 * (1.0_dp - p0)
if (denom > 0.0_dp) out%statistic = diff * diff / denom
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 1
end function prop_test_real_scalar

pure function prop_test_int_vec(x, n, p, correct) result(out)
integer, intent(in) :: x(:), n(:)
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
out = prop_test_real_vec(real(x, kind=dp), real(n, kind=dp), p, correct)
end function prop_test_int_vec

pure function prop_test_real_vec(x, n, p, correct) result(out)
real(kind=dp), intent(in) :: x(:), n(:)
real(kind=dp), intent(in), optional :: p
logical, intent(in), optional :: correct
type(prop_test_result_t) :: out
real(kind=dp) :: p0, pooled, denom, diff
integer :: k, i
logical :: use_correct
k = size(x)
if (k /= size(n) .or. k < 1) return
if (any(n <= 0.0_dp)) return
use_correct = .false.
if (present(correct)) use_correct = correct
if (k == 1) then
   if (present(p)) then
      out = prop_test_real_scalar(x(1), n(1), p, correct)
   else
      out = prop_test_real_scalar(x(1), n(1), correct=correct)
   end if
   return
end if
out%parameter = k - 1
out%estimate = x(1) / n(1)
if (k >= 2) out%estimate2 = x(2) / n(2)
pooled = sum(x) / sum(n)
out%null_value = pooled
if (pooled <= 0.0_dp .or. pooled >= 1.0_dp) return
do i = 1, k
   p0 = x(i) / n(i)
   diff = abs(p0 - pooled)
   if (use_correct .and. k == 2) diff = max(0.0_dp, diff - 0.5_dp / n(i))
   denom = pooled * (1.0_dp - pooled) / n(i)
   if (denom > 0.0_dp) out%statistic = out%statistic + diff * diff / denom
end do
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
out%method = 2
end function prop_test_real_vec

pure function cor_test_int_vec(x, y, method) result(out)
integer, intent(in) :: x(:), y(:)
character(len=*), intent(in), optional :: method
type(cor_test_result_t) :: out
if (present(method)) then
   out = cor_test_real_vec(real(x, kind=dp), real(y, kind=dp), method)
else
   out = cor_test_real_vec(real(x, kind=dp), real(y, kind=dp))
end if
end function cor_test_int_vec

pure function cor_test_real_vec(x, y, method) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
character(len=*), intent(in), optional :: method
type(cor_test_result_t) :: out
real(kind=dp), allocatable :: rx(:), ry(:)
real(kind=dp) :: r, denom
integer :: n
n = min(size(x), size(y))
if (n <= 2) return
if (present(method)) then
   if (len_trim(method) >= 1 .and. (method(1:1) == "s" .or. method(1:1) == "S")) then
      rx = rank_average(x(1:n))
      ry = rank_average(y(1:n))
      r = cor_vec(rx, ry)
      out%method = 2
   else
      r = cor_vec(x(1:n), y(1:n))
      out%method = 1
   end if
else
   r = cor_vec(x(1:n), y(1:n))
   out%method = 1
end if
out%estimate = r
out%parameter = n - 2
denom = max(0.0_dp, 1.0_dp - r * r)
if (denom > 0.0_dp) out%statistic = r * sqrt(real(out%parameter, kind=dp) / denom)
out%p_value = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(out%statistic))))
end function cor_test_real_vec

pure function fisher_log_choose(n, k) result(out)
integer, intent(in) :: n, k
real(kind=dp) :: out
if (k < 0 .or. k > n) then
   out = -huge(1.0_dp)
else
   out = log_gamma(real(n + 1, kind=dp)) - log_gamma(real(k + 1, kind=dp)) - &
      & log_gamma(real(n - k + 1, kind=dp))
end if
end function fisher_log_choose

pure function fisher_hyper_prob(a, r1, r2, c1, n) result(out)
integer, intent(in) :: a, r1, r2, c1, n
real(kind=dp) :: out, lp
lp = fisher_log_choose(r1, a) + fisher_log_choose(r2, c1 - a) - fisher_log_choose(n, c1)
out = exp(lp)
end function fisher_hyper_prob

pure function fisher_noncentral_mean(log_odds, r1, r2, c1) result(out)
real(kind=dp), intent(in) :: log_odds
integer, intent(in) :: r1, r2, c1
real(kind=dp) :: out, lp, max_lp, w, wsum
integer :: lo, hi, aa
lo = max(0, c1 - r2)
hi = min(r1, c1)
max_lp = -huge(1.0_dp)
do aa = lo, hi
   lp = fisher_log_choose(r1, aa) + fisher_log_choose(r2, c1 - aa) + real(aa, kind=dp) * log_odds
   max_lp = max(max_lp, lp)
end do
out = 0.0_dp
wsum = 0.0_dp
do aa = lo, hi
   lp = fisher_log_choose(r1, aa) + fisher_log_choose(r2, c1 - aa) + real(aa, kind=dp) * log_odds
   w = exp(lp - max_lp)
   out = out + real(aa, kind=dp) * w
   wsum = wsum + w
end do
if (wsum > 0.0_dp) out = out / wsum
end function fisher_noncentral_mean

pure function fisher_odds_mle(a, r1, r2, c1) result(out)
integer, intent(in) :: a, r1, r2, c1
real(kind=dp) :: out, lo_eta, hi_eta, mid_eta, target
integer :: lo, hi, iter
lo = max(0, c1 - r2)
hi = min(r1, c1)
if (a <= lo) then
   out = 0.0_dp
   return
end if
if (a >= hi) then
   out = huge(1.0_dp)
   return
end if
target = real(a, kind=dp)
lo_eta = -60.0_dp
hi_eta = 60.0_dp
do iter = 1, 120
   mid_eta = 0.5_dp * (lo_eta + hi_eta)
   if (fisher_noncentral_mean(mid_eta, r1, r2, c1) < target) then
      lo_eta = mid_eta
   else
      hi_eta = mid_eta
   end if
end do
out = exp(0.5_dp * (lo_eta + hi_eta))
end function fisher_odds_mle

pure function fisher_test_int_mat(x) result(out)
integer, intent(in) :: x(:,:)
type(fisher_test_result_t) :: out
out = fisher_test_real_mat(real(x, kind=dp))
end function fisher_test_int_mat

pure function fisher_test_real_mat(x) result(out)
real(kind=dp), intent(in) :: x(:,:)
type(fisher_test_result_t) :: out
integer :: a, b, c, d, r1, r2, c1, n, lo, hi, aa
real(kind=dp) :: p_obs, p_aa
if (size(x, 1) /= 2 .or. size(x, 2) /= 2) return
if (any(x < 0.0_dp)) return
a = nint(x(1, 1))
b = nint(x(1, 2))
c = nint(x(2, 1))
d = nint(x(2, 2))
if (abs(x(1, 1) - real(a, kind=dp)) > print_int_like_tol) return
if (abs(x(1, 2) - real(b, kind=dp)) > print_int_like_tol) return
if (abs(x(2, 1) - real(c, kind=dp)) > print_int_like_tol) return
if (abs(x(2, 2) - real(d, kind=dp)) > print_int_like_tol) return
r1 = a + b
r2 = c + d
c1 = a + c
n = r1 + r2
if (n <= 0) return
lo = max(0, c1 - r2)
hi = min(r1, c1)
p_obs = fisher_hyper_prob(a, r1, r2, c1, n)
out%p_value = 0.0_dp
do aa = lo, hi
   p_aa = fisher_hyper_prob(aa, r1, r2, c1, n)
   if (p_aa <= p_obs * (1.0_dp + 1000.0_dp * epsilon(1.0_dp))) then
      out%p_value = out%p_value + p_aa
   end if
end do
out%p_value = max(0.0_dp, min(1.0_dp, out%p_value))
out%estimate = fisher_odds_mle(a, r1, r2, c1)
out%method = 1
end function fisher_test_real_mat

pure function wilcox_test_two_sample(x, y, paired) result(out)
! Approximate Wilcoxon rank-sum or paired signed-rank test.
real(kind=dp), intent(in) :: x(:), y(:)
logical, intent(in), optional :: paired
type(wilcox_test_result_t) :: out
real(kind=dp), allocatable :: xy(:), r(:), d(:), ad(:), rd(:)
real(kind=dp) :: w, mu, sig, z
integer :: nx, ny, n, i
logical :: is_paired
is_paired = .false.
if (present(paired)) is_paired = paired
if (is_paired) then
   n = min(size(x), size(y))
   if (n <= 0) return
   allocate(d(n))
   d = x(1:n) - y(1:n)
   n = count(abs(d) > tiny(1.0_dp))
   if (n <= 0) return
   allocate(ad(n), rd(n))
   ad = pack(abs(d), abs(d) > tiny(1.0_dp))
   rd = rank_average(ad)
   w = 0.0_dp
   i = 0
   do nx = 1, min(size(x), size(y))
      if (abs(x(nx) - y(nx)) > tiny(1.0_dp)) then
         i = i + 1
         if (x(nx) - y(nx) > 0.0_dp) w = w + rd(i)
      end if
   end do
   mu = real(n * (n + 1), kind=dp) / 4.0_dp
   sig = sqrt(real(n * (n + 1) * (2 * n + 1), kind=dp) / 24.0_dp)
   out%statistic = w
   out%method = 2
else
   nx = size(x)
   ny = size(y)
   if (nx <= 0 .or. ny <= 0) return
   allocate(xy(nx + ny))
   xy(1:nx) = x
   xy(nx+1:nx+ny) = y
   r = rank_average(xy)
   w = sum(r(1:nx)) - real(nx * (nx + 1), kind=dp) / 2.0_dp
   mu = real(nx * ny, kind=dp) / 2.0_dp
   sig = sqrt(real(nx * ny * (nx + ny + 1), kind=dp) / 12.0_dp)
   out%statistic = w
   out%method = 1
end if
if (sig > 0.0_dp) then
   z = (abs(w - mu) - 0.5_dp) / sig
   out%p_value = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(z))))
end if
end function wilcox_test_two_sample

pure function kruskal_test(x, g) result(out)
! Approximate Kruskal-Wallis rank-sum test for integer-coded groups.
real(kind=dp), intent(in) :: x(:)
integer, intent(in) :: g(:)
type(kruskal_test_result_t) :: out
real(kind=dp), allocatable :: r(:), sum_r(:)
integer, allocatable :: n_g(:)
integer :: n, k, i, gi
real(kind=dp) :: h
n = min(size(x), size(g))
if (n <= 1) return
k = maxval(g(1:n))
if (k <= 1) return
allocate(sum_r(k), n_g(k), r(n))
sum_r = 0.0_dp
n_g = 0
r = rank_average(x(1:n))
do i = 1, n
   gi = g(i)
   if (gi >= 1 .and. gi <= k) then
      sum_r(gi) = sum_r(gi) + r(i)
      n_g(gi) = n_g(gi) + 1
   end if
end do
h = 0.0_dp
do gi = 1, k
   if (n_g(gi) > 0) h = h + sum_r(gi) * sum_r(gi) / real(n_g(gi), kind=dp)
end do
h = 12.0_dp * h / real(n * (n + 1), kind=dp) - 3.0_dp * real(n + 1, kind=dp)
out%statistic = max(0.0_dp, h)
out%parameter = k - 1
out%p_value = chisq_upper_tail_approx(out%statistic, real(out%parameter, kind=dp))
end function kruskal_test

pure function t_test_one(x, mu) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: mu
type(t_test_result_t) :: out
real(kind=dp) :: xbar, s, mu0
integer :: n
n = size(x)
mu0 = 0.0_dp
if (present(mu)) mu0 = mu
if (n <= 1) return
xbar = sum(x) / real(n, kind=dp)
s = sd(x)
out%stderr = s / sqrt(real(n, kind=dp))
out%estimate = xbar
out%null_value = mu0
out%parameter = real(n - 1, kind=dp)
if (out%stderr > 0.0_dp) out%statistic = (xbar - mu0) / out%stderr
out%p_value = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(out%statistic))))
out%conf_low = xbar - 1.96_dp * out%stderr
out%conf_high = xbar + 1.96_dp * out%stderr
out%method = 1
end function t_test_one

pure function t_test_two(x, y, paired, var_equal) result(out)
real(kind=dp), intent(in) :: x(:), y(:)
logical, intent(in), optional :: paired, var_equal
type(t_test_result_t) :: out
logical :: is_paired, is_equal
real(kind=dp) :: nx, ny, mx, my, vx, vy, sp2, denom
real(kind=dp), allocatable :: d(:)
is_paired = .false.
if (present(paired)) is_paired = paired
is_equal = .false.
if (present(var_equal)) is_equal = var_equal
if (is_paired) then
   if (size(x) /= size(y)) return
   d = x - y
   out = t_test_one(d, 0.0_dp)
   out%method = 4
   return
end if
if (size(x) <= 1 .or. size(y) <= 1) return
nx = real(size(x), kind=dp)
ny = real(size(y), kind=dp)
mx = sum(x) / nx
my = sum(y) / ny
vx = var(x)
vy = var(y)
out%estimate = mx
out%estimate2 = my
out%null_value = 0.0_dp
if (is_equal) then
   sp2 = ((nx - 1.0_dp) * vx + (ny - 1.0_dp) * vy) / (nx + ny - 2.0_dp)
   out%stderr = sqrt(sp2 * (1.0_dp / nx + 1.0_dp / ny))
   out%parameter = nx + ny - 2.0_dp
   out%method = 3
else
   out%stderr = sqrt(vx / nx + vy / ny)
   denom = (vx / nx)**2 / max(1.0_dp, nx - 1.0_dp) + (vy / ny)**2 / max(1.0_dp, ny - 1.0_dp)
   if (denom > 0.0_dp) out%parameter = (vx / nx + vy / ny)**2 / denom
   out%method = 2
end if
if (out%stderr > 0.0_dp) out%statistic = ((mx - my) - out%null_value) / out%stderr
out%p_value = 2.0_dp * max(0.0_dp, min(1.0_dp, 1.0_dp - normal_cdf(abs(out%statistic))))
out%conf_low = (mx - my) - 1.96_dp * out%stderr
out%conf_high = (mx - my) + 1.96_dp * out%stderr
end function t_test_two

pure function t_test_p_value_one(x, mu) result(p)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: mu
real(kind=dp) :: p
type(t_test_result_t) :: fit
fit = t_test_one(x, mu)
p = fit%p_value
end function t_test_p_value_one

pure function t_test_p_value_two(x, y, paired, var_equal) result(p)
real(kind=dp), intent(in) :: x(:), y(:)
logical, intent(in), optional :: paired, var_equal
real(kind=dp) :: p
type(t_test_result_t) :: fit
fit = t_test_two(x, y, paired, var_equal)
p = fit%p_value
end function t_test_p_value_two

subroutine print_t_test(fit)
type(t_test_result_t), intent(in) :: fit
select case (fit%method)
case (1)
   write(*,'(a)') "One Sample t-test"
case (2)
   write(*,'(a)') "Welch Two Sample t-test"
case (3)
   write(*,'(a)') "Two Sample t-test"
case (4)
   write(*,'(a)') "Paired t-test"
case default
   write(*,'(a)') "t-test"
end select
write(*,'(a,g0,a,g0,a,g0)') "t = ", fit%statistic, ", df = ", fit%parameter, ", p-value = ", fit%p_value
write(*,'(a,g0,a,g0)') "95 percent confidence interval: ", fit%conf_low, " ", fit%conf_high
if (fit%method == 2 .or. fit%method == 3) then
   write(*,'(a,g0,a,g0)') "sample estimates: ", fit%estimate, " ", fit%estimate2
else
   write(*,'(a,g0)') "sample estimate: ", fit%estimate
end if
end subroutine print_t_test

subroutine print_chisq_test(fit)
type(chisq_test_result_t), intent(in) :: fit
select case (fit%method)
case (1)
   write(*,'(a)') "Chi-squared goodness-of-fit test"
case (2)
   write(*,'(a)') "Pearson's Chi-squared test"
case default
   write(*,'(a)') "Chi-squared test"
end select
write(*,'(a,g0,a,i0,a,g0)') "X-squared = ", fit%statistic, ", df = ", fit%parameter, &
   & ", p-value = ", fit%p_value
end subroutine print_chisq_test

subroutine print_prop_test(fit)
type(prop_test_result_t), intent(in) :: fit
select case (fit%method)
case (1)
   write(*,'(a)') "1-sample proportions test"
case (2)
   write(*,'(a)') "2-sample test for equality of proportions"
case default
   write(*,'(a)') "proportions test"
end select
write(*,'(a,g0,a,i0,a,g0)') "X-squared = ", fit%statistic, ", df = ", fit%parameter, &
   & ", p-value = ", fit%p_value
if (fit%method == 2) then
   write(*,'(a,g0,a,g0)') "sample estimates: ", fit%estimate, " ", fit%estimate2
else
   write(*,'(a,g0)') "sample estimate: ", fit%estimate
   write(*,'(a,g0)') "null value: ", fit%null_value
end if
end subroutine print_prop_test

subroutine print_cor_test(fit)
type(cor_test_result_t), intent(in) :: fit
select case (fit%method)
case (2)
   write(*,'(a)') "Spearman's rank correlation rho"
   write(*,'(a,g0,a,i0,a,g0)') "t = ", fit%statistic, ", df = ", fit%parameter, &
      & ", p-value = ", fit%p_value
   write(*,'(a,g0)') "sample estimate rho: ", fit%estimate
case default
   write(*,'(a)') "Pearson's product-moment correlation"
   write(*,'(a,g0,a,i0,a,g0)') "t = ", fit%statistic, ", df = ", fit%parameter, &
      & ", p-value = ", fit%p_value
   write(*,'(a,g0)') "sample estimate cor: ", fit%estimate
end select
end subroutine print_cor_test

subroutine print_fisher_test(fit)
type(fisher_test_result_t), intent(in) :: fit
write(*,'(a)') "Fisher's Exact Test for Count Data"
write(*,'(a,g0)') "p-value = ", fit%p_value
write(*,'(a,g0)') "sample estimate odds ratio: ", fit%estimate
end subroutine print_fisher_test

subroutine print_wilcox_test(fit)
type(wilcox_test_result_t), intent(in) :: fit
select case (fit%method)
case (2)
   write(*,'(a)') "Wilcoxon signed rank test with continuity correction"
   if (abs(fit%statistic - real(nint(fit%statistic), kind=dp)) <= print_int_like_tol) then
      write(*,'(a,i0,a,g0)') "V = ", nint(fit%statistic), ", p-value = ", fit%p_value
   else
      write(*,'(a,g0,a,g0)') "V = ", fit%statistic, ", p-value = ", fit%p_value
   end if
case default
   write(*,'(a)') "Wilcoxon rank sum test with continuity correction"
   if (abs(fit%statistic - real(nint(fit%statistic), kind=dp)) <= print_int_like_tol) then
      write(*,'(a,i0,a,g0)') "W = ", nint(fit%statistic), ", p-value = ", fit%p_value
   else
      write(*,'(a,g0,a,g0)') "W = ", fit%statistic, ", p-value = ", fit%p_value
   end if
end select
end subroutine print_wilcox_test

subroutine print_kruskal_test(fit)
type(kruskal_test_result_t), intent(in) :: fit
write(*,'(a)') "Kruskal-Wallis rank sum test"
write(*,'(a,g0,a,i0,a,g0)') "Kruskal-Wallis chi-squared = ", fit%statistic, &
   & ", df = ", fit%parameter, ", p-value = ", fit%p_value
end subroutine print_kruskal_test

pure function ks_test(x, mean, sd) result(out)
real(kind=dp), intent(in) :: x(:)
real(kind=dp), intent(in), optional :: mean, sd
type(ks_test_result_t) :: out
real(kind=dp), allocatable :: xs(:)
real(kind=dp) :: mu, sig, fi, dplus, dminus, z
integer :: i, n
mu = 0.0_dp
sig = 1.0_dp
if (present(mean)) mu = mean
if (present(sd)) sig = sd
n = size(x)
out%n = n
if (n <= 0 .or. sig <= 0.0_dp) return
xs = sort(x)
do i = 1, n
   fi = normal_cdf((xs(i) - mu) / sig)
   dplus = real(i, kind=dp) / real(n, kind=dp) - fi
   dminus = fi - real(i - 1, kind=dp) / real(n, kind=dp)
   out%statistic = max(out%statistic, max(dplus, dminus))
end do
z = (sqrt(real(n, kind=dp)) + 0.12_dp + 0.11_dp / sqrt(real(n, kind=dp))) * out%statistic
out%p_value = max(0.0_dp, min(1.0_dp, 2.0_dp * exp(-2.0_dp * z * z)))
end function ks_test

subroutine print_ks_test(fit)
type(ks_test_result_t), intent(in) :: fit
write(*,'(a)') "One-sample Kolmogorov-Smirnov test"
write(*,'(a,g0,a,g0)') "D = ", fit%statistic, ", p-value = ", fit%p_value
end subroutine print_ks_test

subroutine print_factanal(x, factors)
real(kind=dp), intent(in) :: x(:,:)
integer, intent(in) :: factors
type(eigen_result_t) :: eg
real(kind=dp), allocatable :: loadings(:,:)
integer :: j, nf
nf = max(1, min(factors, size(x, 2)))
eg = eigen(cor(x))
allocate(loadings(size(x, 2), nf))
do j = 1, nf
   loadings(:, j) = eg%vectors(:, j) * sqrt(max(0.0_dp, eg%values(j)))
end do
write(*,'(a)') "Factor Analysis (principal-factor approximation)"
write(*,'(a)') "Loadings:"
call print_matrix_rstyle(loadings)
write(*,'(a)') "Uniquenesses:"
call print_real_vector(max(0.0_dp, 1.0_dp - sum(loadings**2, dim=2)))
end subroutine print_factanal

pure function decompose(x, type, frequency) result(out)
real(kind=dp), intent(in) :: x(:)
character(len=*), intent(in), optional :: type
integer, intent(in), optional :: frequency
type(decompose_result_t) :: out
integer :: n, f, i, j, lo, hi, cnt
real(kind=dp), allocatable :: detr(:)
n = size(x)
f = 1
if (present(frequency)) f = max(1, frequency)
allocate(out%trend(n), out%seasonal(n), out%random(n), out%figure(f), detr(n))
out%trend = 0.0_dp
do i = 1, n
   lo = max(1, i - f/2)
   hi = min(n, i + f/2)
   out%trend(i) = sum(x(lo:hi)) / real(hi - lo + 1, kind=dp)
end do
detr = x - out%trend
out%figure = 0.0_dp
do j = 1, f
   cnt = 0
   do i = j, n, f
      out%figure(j) = out%figure(j) + detr(i)
      cnt = cnt + 1
   end do
   if (cnt > 0) out%figure(j) = out%figure(j) / real(cnt, kind=dp)
end do
out%figure = out%figure - sum(out%figure) / real(f, kind=dp)
do i = 1, n
   j = mod(i - 1, f) + 1
   out%seasonal(i) = out%figure(j)
end do
out%random = x - out%trend - out%seasonal
end function decompose

pure function ecdf_eval(x, q) result(out)
real(kind=dp), intent(in) :: x(:), q(:)
real(kind=dp), allocatable :: out(:)
integer :: i
allocate(out(size(q)))
do i = 1, size(q)
   out(i) = real(count(x <= q(i)), kind=dp) / real(size(x), kind=dp)
end do
end function ecdf_eval

end module r_mod
