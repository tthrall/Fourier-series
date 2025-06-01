#####
###
#     u_grid
#
#       generate equispaced points within either
#       the open unit interval (wrap = FALSE) or
#       the circle of unit circumference (wrap = TRUE)
###
#####

##
#  u_grid()
#  
#    0 < u < 1
##
u_grid <- function(
  n    = 1024L, # <int> number of grid-points
  wrap = TRUE   # <lgl> identify endpoints of unit interval?
) {
  if (wrap) {
    u_vec <- seq.int(1, (2*n)-1, 2)/(2*n)
  } else {
    u_vec <- (1:n)/(n+1)
  }

  return(u_vec)
}

##
#  x_grid()
#  
#    x_0 < x < x_1
##
x_grid <- function(
    n    = 1024L, # <int> number of grid-points
    wrap = TRUE,  # <lgl> identify endpoints of interval?
    x_0 = -1,     # <dbl> initial bound on grid values
    x_1 =  1      # <dbl> final bound on grid values
) {
  assertthat::assert_that(! dplyr::near(x_0, x_1))
  u <- u_grid(n, wrap)
  x <- x_0 + ((x_1 - x_0) * u)
  return(x)
}

##
#  z_grid()
#  
#    z = cos(x) + i sin(x), 0 <= x < 2 pi
##
z_grid <- function(
    n    = 1024L # <int> number of grid-points
) {
  u <- u_grid(n - 1L, wrap = FALSE)
  x <- (2 * pi) * c(0, u)
  z <- exp( (0+1i) * x)
  return(z)
}


##
#  EOF
##
