! Optional helper runtime for LLM/manual R-to-Fortran translations.
!
! This module is intentionally separate from r.f90.  It provides small,
! general-purpose containers that are useful when hand-repairing or extending
! generated Fortran: named numeric matrices, printable numeric tables, compact
! model summaries, and linear-model result records.

module r2f_llm_runtime_mod
   use, intrinsic :: iso_fortran_env, only: dp => real64
   use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan, ieee_positive_inf
   implicit none
   private

   public :: dp
   integer, parameter, public :: name_len = 64
   integer, parameter, public :: title_len = 160

   type, public :: named_real_matrix_t
      real(dp), allocatable :: x(:,:)
      character(len=name_len), allocatable :: row_names(:)
      character(len=name_len), allocatable :: col_names(:)
   end type named_real_matrix_t

   type, public :: real_table_t
      character(len=title_len) :: title = ""
      real(dp), allocatable :: x(:,:)
      character(len=name_len), allocatable :: row_names(:)
      character(len=name_len), allocatable :: col_names(:)
      integer :: digits = 4
   end type real_table_t

   type, public :: model_summary_t
      logical :: valid = .true.
      logical :: converged = .false.
      integer :: nobs = 0
      integer :: npar = 0
      integer :: iter = 0
      integer :: convergence_code = 0
      real(dp) :: loglik = 0.0_dp
      real(dp) :: aic = 0.0_dp
      real(dp) :: bic = 0.0_dp
      character(len=title_len) :: message = ""
   end type model_summary_t

   type, public :: lm_result_t
      type(model_summary_t) :: summary
      real(dp), allocatable :: coef(:)
      real(dp), allocatable :: se(:)
      real(dp), allocatable :: t_stat(:)
      real(dp), allocatable :: fitted(:)
      real(dp), allocatable :: residuals(:)
      character(len=name_len), allocatable :: term_names(:)
      real(dp) :: r2 = 0.0_dp
      real(dp) :: sigma = 0.0_dp
   end type lm_result_t

   public :: init_matrix, init_table
   public :: set_row_names, set_col_names
   public :: row_index, col_index
   public :: print_matrix, print_table
   public :: print_model_summary, print_lm_result
   public :: NA_dp, Inf_dp, is_na, is_finite
   public :: nanmean, nansd, nanskew, nankurt, na_omit
   public :: which_true, first_finite
   public :: split_tokens, parse_int_list, parse_real_list
   public :: itoa, ftoa
   public :: cumsum_dp, cumprod_dp, cummax_dp

   interface init_matrix
      module procedure init_named_real_matrix
   end interface

   interface init_table
      module procedure init_real_table
   end interface

   interface set_row_names
      module procedure set_matrix_row_names
      module procedure set_table_row_names
   end interface

   interface set_col_names
      module procedure set_matrix_col_names
      module procedure set_table_col_names
   end interface

   interface row_index
      module procedure matrix_row_index
      module procedure table_row_index
   end interface

   interface col_index
      module procedure matrix_col_index
      module procedure table_col_index
   end interface

   interface print_matrix
      module procedure print_named_real_matrix
   end interface

   interface print_table
      module procedure print_real_table
   end interface

contains

   real(dp) function NA_dp() result(x)
      x = ieee_value(0.0_dp, ieee_quiet_nan)
   end function NA_dp

   real(dp) function Inf_dp() result(x)
      x = ieee_value(0.0_dp, ieee_positive_inf)
   end function Inf_dp

   elemental logical function is_finite(x) result(ok)
      real(dp), intent(in) :: x
      ok = ieee_is_finite(x)
   end function is_finite

   elemental logical function is_na(x) result(ok)
      real(dp), intent(in) :: x
      ok = .not. ieee_is_finite(x)
   end function is_na

   real(dp) function nanmean(x) result(y)
      real(dp), intent(in) :: x(:)
      logical :: ok(size(x))
      integer :: n

      ok = is_finite(x)
      n = count(ok)
      if (n > 0) then
         y = sum(pack(x, ok)) / real(n, dp)
      else
         y = NA_dp()
      end if
   end function nanmean

   real(dp) function nansd(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: z(:)
      real(dp) :: m
      integer :: n

      z = na_omit(x)
      n = size(z)
      if (n > 1) then
         m = sum(z) / real(n, dp)
         y = sqrt(sum((z - m)**2) / real(n - 1, dp))
      else
         y = NA_dp()
      end if
   end function nansd

   real(dp) function nanskew(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: z(:)
      real(dp) :: m, s
      integer :: n

      z = na_omit(x)
      n = size(z)
      if (n > 2) then
         m = sum(z) / real(n, dp)
         s = sqrt(sum((z - m)**2) / real(n - 1, dp))
         if (s > 0.0_dp) then
            y = sum(((z - m) / s)**3) / real(n, dp)
         else
            y = NA_dp()
         end if
      else
         y = NA_dp()
      end if
   end function nanskew

   real(dp) function nankurt(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: z(:)
      real(dp) :: m, s
      integer :: n

      z = na_omit(x)
      n = size(z)
      if (n > 3) then
         m = sum(z) / real(n, dp)
         s = sqrt(sum((z - m)**2) / real(n - 1, dp))
         if (s > 0.0_dp) then
            y = sum(((z - m) / s)**4) / real(n, dp) - 3.0_dp
         else
            y = NA_dp()
         end if
      else
         y = NA_dp()
      end if
   end function nankurt

   function na_omit(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: y(:)
      y = pack(x, is_finite(x))
   end function na_omit

   function which_true(mask) result(idx)
      logical, intent(in) :: mask(:)
      integer, allocatable :: idx(:)
      integer :: i
      idx = pack([(i, i = 1, size(mask))], mask)
   end function which_true

   integer function first_finite(x) result(idx)
      real(dp), intent(in) :: x(:)
      integer :: i
      idx = 0
      do i = 1, size(x)
         if (is_finite(x(i))) then
            idx = i
            return
         end if
      end do
   end function first_finite

   subroutine split_tokens(str, tokens, n)
      character(len=*), intent(in) :: str
      character(len=*), intent(out) :: tokens(:)
      integer, intent(out) :: n
      integer :: i, start, len_s

      tokens = ""
      n = 0
      len_s = len_trim(str)
      i = 1
      do while (i <= len_s)
         do while (i <= len_s .and. str(i:i) == " ")
            i = i + 1
         end do
         if (i > len_s) exit
         start = i
         do while (i <= len_s .and. str(i:i) /= " ")
            i = i + 1
         end do
         if (n < size(tokens)) then
            n = n + 1
            tokens(n) = str(start:i-1)
         end if
      end do
   end subroutine split_tokens

   subroutine parse_int_list(str, vals, n)
      character(len=*), intent(in) :: str
      integer, intent(out) :: vals(:)
      integer, intent(out) :: n
      character(len=name_len) :: toks(size(vals))
      integer :: i, ios, ntok

      vals = 0
      call split_tokens(str, toks, ntok)
      n = 0
      do i = 1, ntok
         if (n >= size(vals)) exit
         read(toks(i), *, iostat=ios) vals(n + 1)
         if (ios == 0) n = n + 1
      end do
   end subroutine parse_int_list

   subroutine parse_real_list(str, vals, n)
      character(len=*), intent(in) :: str
      real(dp), intent(out) :: vals(:)
      integer, intent(out) :: n
      character(len=name_len) :: toks(size(vals))
      integer :: i, ios, ntok

      vals = 0.0_dp
      call split_tokens(str, toks, ntok)
      n = 0
      do i = 1, ntok
         if (n >= size(vals)) exit
         read(toks(i), *, iostat=ios) vals(n + 1)
         if (ios == 0) n = n + 1
      end do
   end subroutine parse_real_list

   function itoa(i) result(s)
      integer, intent(in) :: i
      character(len=:), allocatable :: s
      character(len=32) :: buf

      write(buf, '(i0)') i
      s = trim(buf)
   end function itoa

   function ftoa(x, digits) result(s)
      real(dp), intent(in) :: x
      integer, intent(in), optional :: digits
      character(len=:), allocatable :: s
      character(len=64) :: fmt, buf
      integer :: d

      d = 6
      if (present(digits)) d = max(0, min(12, digits))
      write(fmt, '("(f0.",i0,")")') d
      write(buf, fmt) x
      s = trim(adjustl(buf))
   end function ftoa

   function cumsum_dp(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: y(:)
      integer :: i

      allocate(y(size(x)))
      if (size(x) == 0) return
      y(1) = x(1)
      do i = 2, size(x)
         y(i) = y(i - 1) + x(i)
      end do
   end function cumsum_dp

   function cumprod_dp(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: y(:)
      integer :: i

      allocate(y(size(x)))
      if (size(x) == 0) return
      y(1) = x(1)
      do i = 2, size(x)
         y(i) = y(i - 1) * x(i)
      end do
   end function cumprod_dp

   function cummax_dp(x) result(y)
      real(dp), intent(in) :: x(:)
      real(dp), allocatable :: y(:)
      integer :: i

      allocate(y(size(x)))
      if (size(x) == 0) return
      y(1) = x(1)
      do i = 2, size(x)
         y(i) = max(y(i - 1), x(i))
      end do
   end function cummax_dp

   subroutine init_named_real_matrix(mat, x, row_names, col_names)
      type(named_real_matrix_t), intent(out) :: mat
      real(dp), intent(in) :: x(:,:)
      character(len=*), intent(in), optional :: row_names(:), col_names(:)

      allocate(mat%x(size(x, 1), size(x, 2)))
      mat%x = x
      call default_names(mat%row_names, "r", size(x, 1))
      call default_names(mat%col_names, "c", size(x, 2))
      if (present(row_names)) call assign_names(mat%row_names, row_names, size(x, 1))
      if (present(col_names)) call assign_names(mat%col_names, col_names, size(x, 2))
   end subroutine init_named_real_matrix

   subroutine init_real_table(tab, x, row_names, col_names, title, digits)
      type(real_table_t), intent(out) :: tab
      real(dp), intent(in) :: x(:,:)
      character(len=*), intent(in), optional :: row_names(:), col_names(:), title
      integer, intent(in), optional :: digits

      allocate(tab%x(size(x, 1), size(x, 2)))
      tab%x = x
      call default_names(tab%row_names, "", size(x, 1))
      call default_names(tab%col_names, "V", size(x, 2))
      if (present(row_names)) call assign_names(tab%row_names, row_names, size(x, 1))
      if (present(col_names)) call assign_names(tab%col_names, col_names, size(x, 2))
      if (present(title)) tab%title = title
      if (present(digits)) tab%digits = max(0, min(12, digits))
   end subroutine init_real_table

   subroutine set_matrix_row_names(mat, names)
      type(named_real_matrix_t), intent(inout) :: mat
      character(len=*), intent(in) :: names(:)
      call assign_names(mat%row_names, names, size(mat%x, 1))
   end subroutine set_matrix_row_names

   subroutine set_matrix_col_names(mat, names)
      type(named_real_matrix_t), intent(inout) :: mat
      character(len=*), intent(in) :: names(:)
      call assign_names(mat%col_names, names, size(mat%x, 2))
   end subroutine set_matrix_col_names

   subroutine set_table_row_names(tab, names)
      type(real_table_t), intent(inout) :: tab
      character(len=*), intent(in) :: names(:)
      call assign_names(tab%row_names, names, size(tab%x, 1))
   end subroutine set_table_row_names

   subroutine set_table_col_names(tab, names)
      type(real_table_t), intent(inout) :: tab
      character(len=*), intent(in) :: names(:)
      call assign_names(tab%col_names, names, size(tab%x, 2))
   end subroutine set_table_col_names

   integer function matrix_row_index(mat, name) result(idx)
      type(named_real_matrix_t), intent(in) :: mat
      character(len=*), intent(in) :: name
      idx = find_name(mat%row_names, name)
   end function matrix_row_index

   integer function matrix_col_index(mat, name) result(idx)
      type(named_real_matrix_t), intent(in) :: mat
      character(len=*), intent(in) :: name
      idx = find_name(mat%col_names, name)
   end function matrix_col_index

   integer function table_row_index(tab, name) result(idx)
      type(real_table_t), intent(in) :: tab
      character(len=*), intent(in) :: name
      idx = find_name(tab%row_names, name)
   end function table_row_index

   integer function table_col_index(tab, name) result(idx)
      type(real_table_t), intent(in) :: tab
      character(len=*), intent(in) :: name
      idx = find_name(tab%col_names, name)
   end function table_col_index

   subroutine print_named_real_matrix(mat, title, digits)
      type(named_real_matrix_t), intent(in) :: mat
      character(len=*), intent(in), optional :: title
      integer, intent(in), optional :: digits
      type(real_table_t) :: tab

      call init_table(tab, mat%x, mat%row_names, mat%col_names, digits=merge(digits, 4, present(digits)))
      if (present(title)) tab%title = title
      call print_real_table(tab)
   end subroutine print_named_real_matrix

   subroutine print_real_table(tab)
      type(real_table_t), intent(in) :: tab
      integer :: i, j, w_name, w_num, d

      if (allocated(tab%x)) then
         d = max(0, min(12, tab%digits))
      else
         return
      end if
      w_name = max(1, max_name_width(tab%row_names))
      w_num = max(8, d + 7)
      w_num = max(w_num, max_name_width(tab%col_names))

      if (len_trim(tab%title) > 0) then
         write(*,'(/,a)') trim(tab%title)
         write(*,'(a)') repeat("-", len_trim(tab%title))
      end if

      write(*,'(a)', advance='no') repeat(" ", w_name + 1)
      do j = 1, size(tab%x, 2)
         write(*,'(a)', advance='no') pad_left(tab%col_names(j), w_num + 1)
      end do
      write(*,*)

      do i = 1, size(tab%x, 1)
         write(*,'(a)', advance='no') pad_left(tab%row_names(i), w_name)
         do j = 1, size(tab%x, 2)
            write(*,'(a)', advance='no') format_real(tab%x(i, j), w_num + 1, d)
         end do
         write(*,*)
      end do
   end subroutine print_real_table

   subroutine print_model_summary(summary, title)
      type(model_summary_t), intent(in) :: summary
      character(len=*), intent(in), optional :: title

      if (present(title)) then
         write(*,'(/,a)') trim(title)
         write(*,'(a)') repeat("-", len_trim(title))
      end if
      write(*,'(a,l1)') "valid:       ", summary%valid
      write(*,'(a,l1)') "converged:   ", summary%converged
      write(*,'(a,i0)') "nobs:        ", summary%nobs
      write(*,'(a,i0)') "npar:        ", summary%npar
      write(*,'(a,i0)') "iter:        ", summary%iter
      write(*,'(a,i0)') "conv_code:   ", summary%convergence_code
      write(*,'(a,g0)') "loglik:      ", summary%loglik
      write(*,'(a,g0)') "aic:         ", summary%aic
      write(*,'(a,g0)') "bic:         ", summary%bic
      if (len_trim(summary%message) > 0) write(*,'(a,a)') "message:     ", trim(summary%message)
   end subroutine print_model_summary

   subroutine print_lm_result(fit, title, digits)
      type(lm_result_t), intent(in) :: fit
      character(len=*), intent(in), optional :: title
      integer, intent(in), optional :: digits
      real(dp), allocatable :: coef_tab(:,:)
      character(len=name_len), allocatable :: cols(:), terms(:)
      integer :: n, d

      if (present(title)) then
         call print_model_summary(fit%summary, title)
      else
         call print_model_summary(fit%summary)
      end if
      write(*,'(a,g0)') "r2:          ", fit%r2
      write(*,'(a,g0)') "sigma:       ", fit%sigma

      if (.not. allocated(fit%coef)) return
      n = size(fit%coef)
      allocate(coef_tab(n, 3), cols(3), terms(n))
      coef_tab(:, 1) = fit%coef
      coef_tab(:, 2) = optional_vector(fit%se, n)
      coef_tab(:, 3) = optional_vector(fit%t_stat, n)
      cols = [character(len=name_len) :: "estimate", "std_err", "t_stat"]
      if (allocated(fit%term_names)) then
         terms = fit%term_names
      else
         call default_names(terms, "x", n)
      end if
      d = merge(digits, 4, present(digits))
      call print_real_table(real_table_t("coefficients", coef_tab, terms, cols, d))
   end subroutine print_lm_result

   subroutine default_names(names, prefix, n)
      character(len=name_len), allocatable, intent(out) :: names(:)
      character(len=*), intent(in) :: prefix
      integer, intent(in) :: n
      integer :: i

      allocate(names(n))
      do i = 1, n
         if (len_trim(prefix) > 0) then
            write(names(i), '(a,i0)') trim(prefix), i
         else
            names(i) = ""
         end if
      end do
   end subroutine default_names

   subroutine assign_names(dest, src, expected_n)
      character(len=name_len), allocatable, intent(inout) :: dest(:)
      character(len=*), intent(in) :: src(:)
      integer, intent(in) :: expected_n
      integer :: i, n

      if (allocated(dest)) deallocate(dest)
      allocate(dest(expected_n))
      dest = ""
      n = min(expected_n, size(src))
      do i = 1, n
         dest(i) = src(i)
      end do
   end subroutine assign_names

   integer function find_name(names, name) result(idx)
      character(len=*), intent(in) :: names(:)
      character(len=*), intent(in) :: name
      integer :: i

      idx = 0
      do i = 1, size(names)
         if (trim(names(i)) == trim(name)) then
            idx = i
            return
         end if
      end do
   end function find_name

   integer function max_name_width(names) result(width)
      character(len=*), intent(in) :: names(:)
      integer :: i

      width = 0
      do i = 1, size(names)
         width = max(width, len_trim(names(i)))
      end do
   end function max_name_width

   function pad_left(s, width) result(out)
      character(len=*), intent(in) :: s
      integer, intent(in) :: width
      character(len=:), allocatable :: out
      integer :: n

      n = len_trim(s)
      allocate(character(len=max(width, n)) :: out)
      if (len(out) > n) then
         out = repeat(" ", len(out) - n) // trim(s)
      else
         out = trim(s)
      end if
   end function pad_left

   function format_real(x, width, digits) result(out)
      real(dp), intent(in) :: x
      integer, intent(in) :: width, digits
      character(len=:), allocatable :: out
      character(len=64) :: fmt, buf

      write(fmt, '("(f",i0,".",i0,")")') max(width, digits + 7), digits
      write(buf, fmt) x
      out = buf(1:min(len(buf), max(width, digits + 7)))
   end function format_real

   function optional_vector(x, n) result(y)
      real(dp), allocatable, intent(in) :: x(:)
      integer, intent(in) :: n
      real(dp) :: y(n)

      if (allocated(x) .and. size(x) == n) then
         y = x
      else
         y = 0.0_dp
      end if
   end function optional_vector

end module r2f_llm_runtime_mod
