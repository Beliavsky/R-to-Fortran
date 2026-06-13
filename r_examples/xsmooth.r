# test_r_smoothing_functions.R
# Test R smoothing functions corresponding to procedures translated in rsmooth.f90.
#
# Functions tested:
#   smooth()
#   runmed()
#   filter()
#   ksmooth()
#   loess()
#   lowess()
#   smooth.spline()
#
# No plotting. Uses base R / stats only.

set.seed(123)

cat("\nR smoothing function test script\n")

print_header <- function(title) {
  cat("\n", paste(rep("=", nchar(title)), collapse = ""), "\n", sep = "")
  cat(title, "\n")
  cat(paste(rep("=", nchar(title)), collapse = ""), "\n", sep = "")
}

print_vec <- function(name, x, digits = 6) {
  cat("\n", name, ":\n", sep = "")
  print(round(as.vector(x), digits))
}

print_mat <- function(name, x, digits = 6) {
  cat("\n", name, ":\n", sep = "")
  print(round(x, digits))
}

rmse <- function(a, b) {
  sqrt(mean((as.vector(a) - as.vector(b))^2, na.rm = TRUE))
}


# ----------------------------------------------------------------------
# Data
# ----------------------------------------------------------------------

print_header("1. Test data")

n <- 41
x <- seq(0, 10, length.out = n)

y_true <- sin(x) + 0.15 * x
y <- y_true + rnorm(n, sd = 0.25)

# Add a few outliers to make robust methods interesting.
y_out <- y
y_out[c(8, 21, 34)] <- y_out[c(8, 21, 34)] + c(2.0, -2.5, 1.8)

print_vec("x", x)
print_vec("y_true", y_true)
print_vec("y", y)
print_vec("y_out", y_out)


# ----------------------------------------------------------------------
# smooth()
# ----------------------------------------------------------------------

print_header("2. smooth(): Tukey-style smoothing")

# smooth() works on an ordered sequence.
# kind = "3RS3R" is a common Tukey smoother.
s1 <- smooth(y_out)
s2 <- smooth(y_out, kind = "3")
s3 <- smooth(y_out, kind = "3RS3R")

print_vec("smooth(y_out), default", s1)
print_vec("smooth(y_out, kind = '3')", s2)
print_vec("smooth(y_out, kind = '3RS3R')", s3)

cat("\nRMSE against y_true:\n")
print(c(
  raw = rmse(y_out, y_true),
  smooth_default = rmse(s1, y_true),
  smooth_3 = rmse(s2, y_true),
  smooth_3RS3R = rmse(s3, y_true)
))


# ----------------------------------------------------------------------
# runmed()
# ----------------------------------------------------------------------

print_header("3. runmed(): running median")

r3 <- runmed(y_out, k = 3)
r5 <- runmed(y_out, k = 5)
r7 <- runmed(y_out, k = 7)

print_vec("runmed(y_out, k = 3)", r3)
print_vec("runmed(y_out, k = 5)", r5)
print_vec("runmed(y_out, k = 7)", r7)

cat("\nRMSE against y_true:\n")
print(c(
  raw = rmse(y_out, y_true),
  runmed_3 = rmse(r3, y_true),
  runmed_5 = rmse(r5, y_true),
  runmed_7 = rmse(r7, y_true)
))


# ----------------------------------------------------------------------
# filter()
# ----------------------------------------------------------------------

print_header("4. filter(): moving averages / linear filters")

# Centered moving average: sides = 2.
f3_centered <- filter(y, filter = rep(1 / 3, 3), sides = 2)
f5_centered <- filter(y, filter = rep(1 / 5, 5), sides = 2)

# One-sided moving average: sides = 1.
f3_one_sided <- filter(y, filter = rep(1 / 3, 3), sides = 1)

# A custom linear filter.
custom_weights <- c(0.25, 0.50, 0.25)
f_custom <- filter(y, filter = custom_weights, sides = 2)

print_vec("filter(y, rep(1/3, 3), sides = 2)", f3_centered)
print_vec("filter(y, rep(1/5, 5), sides = 2)", f5_centered)
print_vec("filter(y, rep(1/3, 3), sides = 1)", f3_one_sided)
print_vec("filter(y, c(0.25, 0.50, 0.25), sides = 2)", f_custom)

cat("\nRMSE against y_true, ignoring NA endpoints:\n")
print(c(
  raw = rmse(y, y_true),
  filt_3_centered = rmse(f3_centered, y_true),
  filt_5_centered = rmse(f5_centered, y_true),
  filt_3_one_sided = rmse(f3_one_sided, y_true),
  filt_custom = rmse(f_custom, y_true)
))


# ----------------------------------------------------------------------
# ksmooth()
# ----------------------------------------------------------------------

print_header("5. ksmooth(): kernel smoothing")

xout <- x

k_norm_05 <- ksmooth(x, y, kernel = "normal", bandwidth = 0.5, x.points = xout)
k_norm_10 <- ksmooth(x, y, kernel = "normal", bandwidth = 1.0, x.points = xout)
k_box_10 <- ksmooth(x, y, kernel = "box", bandwidth = 1.0, x.points = xout)

print_mat("ksmooth normal bandwidth 0.5: cbind(x, y)", cbind(k_norm_05$x, k_norm_05$y))
print_mat("ksmooth normal bandwidth 1.0: cbind(x, y)", cbind(k_norm_10$x, k_norm_10$y))
print_mat("ksmooth box bandwidth 1.0: cbind(x, y)", cbind(k_box_10$x, k_box_10$y))

cat("\nRMSE against y_true:\n")
print(c(
  raw = rmse(y, y_true),
  ksmooth_normal_05 = rmse(k_norm_05$y, y_true),
  ksmooth_normal_10 = rmse(k_norm_10$y, y_true),
  ksmooth_box_10 = rmse(k_box_10$y, y_true)
))


# ----------------------------------------------------------------------
# loess()
# ----------------------------------------------------------------------

print_header("6. loess(): local polynomial regression")

fit_loess_025_d1 <- loess(y ~ x, span = 0.25, degree = 1)
fit_loess_050_d1 <- loess(y ~ x, span = 0.50, degree = 1)
fit_loess_050_d2 <- loess(y ~ x, span = 0.50, degree = 2)

loess_025_d1 <- predict(fit_loess_025_d1, newdata = data.frame(x = xout))
loess_050_d1 <- predict(fit_loess_050_d1, newdata = data.frame(x = xout))
loess_050_d2 <- predict(fit_loess_050_d2, newdata = data.frame(x = xout))

print_vec("loess span 0.25 degree 1", loess_025_d1)
print_vec("loess span 0.50 degree 1", loess_050_d1)
print_vec("loess span 0.50 degree 2", loess_050_d2)

cat("\nRMSE against y_true:\n")
print(c(
  raw = rmse(y, y_true),
  loess_025_d1 = rmse(loess_025_d1, y_true),
  loess_050_d1 = rmse(loess_050_d1, y_true),
  loess_050_d2 = rmse(loess_050_d2, y_true)
))

cat("\nloess fitted object names:\n")
print(names(fit_loess_050_d1))


# ----------------------------------------------------------------------
# lowess()
# ----------------------------------------------------------------------

print_header("7. lowess(): robust local scatterplot smoothing")

# lowess() returns a list with x and y.
# f controls the smoothing fraction.
# iter controls robustness iterations.
low_f_025_i0 <- lowess(x, y_out, f = 0.25, iter = 0)
low_f_025_i3 <- lowess(x, y_out, f = 0.25, iter = 3)
low_f_050_i3 <- lowess(x, y_out, f = 0.50, iter = 3)

print_mat("lowess f = 0.25, iter = 0: cbind(x, y)", cbind(low_f_025_i0$x, low_f_025_i0$y))
print_mat("lowess f = 0.25, iter = 3: cbind(x, y)", cbind(low_f_025_i3$x, low_f_025_i3$y))
print_mat("lowess f = 0.50, iter = 3: cbind(x, y)", cbind(low_f_050_i3$x, low_f_050_i3$y))

cat("\nRMSE against y_true:\n")
print(c(
  raw_outlier = rmse(y_out, y_true),
  lowess_025_i0 = rmse(low_f_025_i0$y, y_true),
  lowess_025_i3 = rmse(low_f_025_i3$y, y_true),
  lowess_050_i3 = rmse(low_f_050_i3$y, y_true)
))


# ----------------------------------------------------------------------
# smooth.spline()
# ----------------------------------------------------------------------

print_header("8. smooth.spline(): smoothing spline")

ss_default <- smooth.spline(x, y)
ss_df_8 <- smooth.spline(x, y, df = 8)
ss_df_16 <- smooth.spline(x, y, df = 16)
ss_spar_04 <- smooth.spline(x, y, spar = 0.4)
ss_spar_10 <- smooth.spline(x, y, spar = 1.0)

pred_default <- predict(ss_default, xout)
pred_df_8 <- predict(ss_df_8, xout)
pred_df_16 <- predict(ss_df_16, xout)
pred_spar_04 <- predict(ss_spar_04, xout)
pred_spar_10 <- predict(ss_spar_10, xout)

print_mat("smooth.spline default: cbind(x, y)", cbind(pred_default$x, pred_default$y))
print_mat("smooth.spline df = 8: cbind(x, y)", cbind(pred_df_8$x, pred_df_8$y))
print_mat("smooth.spline df = 16: cbind(x, y)", cbind(pred_df_16$x, pred_df_16$y))
print_mat("smooth.spline spar = 0.4: cbind(x, y)", cbind(pred_spar_04$x, pred_spar_04$y))
print_mat("smooth.spline spar = 1.0: cbind(x, y)", cbind(pred_spar_10$x, pred_spar_10$y))

cat("\nEffective degrees of freedom:\n")
print(c(
  default = ss_default$df,
  df_8 = ss_df_8$df,
  df_16 = ss_df_16$df,
  spar_04 = ss_spar_04$df,
  spar_10 = ss_spar_10$df
))

cat("\nRMSE against y_true:\n")
print(c(
  raw = rmse(y, y_true),
  smooth_spline_default = rmse(pred_default$y, y_true),
  smooth_spline_df_8 = rmse(pred_df_8$y, y_true),
  smooth_spline_df_16 = rmse(pred_df_16$y, y_true),
  smooth_spline_spar_04 = rmse(pred_spar_04$y, y_true),
  smooth_spline_spar_10 = rmse(pred_spar_10$y, y_true)
))


# ----------------------------------------------------------------------
# Combined comparison
# ----------------------------------------------------------------------

print_header("9. Combined comparison matrix")

# Put selected smoothers side by side.
# This is useful for comparing with Fortran output from rsmooth.f90.

combined <- cbind(
  x = x,
  y_true = y_true,
  y = y,
  y_out = y_out,
  runmed5 = as.vector(r5),
  filter5 = as.vector(f5_centered),
  ksmooth_norm_10 = as.vector(k_norm_10$y),
  loess_050_d1 = as.vector(loess_050_d1),
  lowess_050_i3 = as.vector(low_f_050_i3$y),
  spline_df_8 = as.vector(pred_df_8$y)
)

print_mat("combined", combined)

cat("\nRMSE summary:\n")
rmse_summary <- c(
  raw_y = rmse(y, y_true),
  raw_y_out = rmse(y_out, y_true),
  smooth_default = rmse(s1, y_true),
  runmed5 = rmse(r5, y_true),
  filter5 = rmse(f5_centered, y_true),
  ksmooth_norm_10 = rmse(k_norm_10$y, y_true),
  loess_050_d1 = rmse(loess_050_d1, y_true),
  lowess_050_i3 = rmse(low_f_050_i3$y, y_true),
  spline_df_8 = rmse(pred_df_8$y, y_true)
)

print(round(rmse_summary, 6))


# ----------------------------------------------------------------------
# Export numeric output for comparison with Fortran, if desired
# ----------------------------------------------------------------------

print_header("10. Optional CSV output")

outfile <- "r_smoothing_reference.csv"

write.table(
  combined,
  file = outfile,
  sep = ",",
  row.names = FALSE,
  col.names = TRUE
)

cat("Wrote reference results to:", outfile, "\n")

cat("\nDone.\n")
