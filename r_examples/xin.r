xi = c(4L, 9L)
yi = 1:5
tf = xi %in% yi
print(tf)
xr = c(4, 9)
yr = as.double(1:5)
tf = xr %in% yr
print(tf)
xc = c("one", "two", "three")
yc = c("two", "four")
tf = xc %in% yc
print(tf)
xb = c(TRUE, FALSE)
yb = c(TRUE)
tf = xb %in% yb
print(tf)

