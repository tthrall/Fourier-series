#####
###
#     unif_convolution
#
#       Density and cum probability of W:
#
#       W = t*Y + (1-t)*X
#
#       where X, Y are iid Uniform(0, 1) variates
###
#####

##
#  unif_convolve()
#    density and cum probability of W
#
#    Requires
#      assertthat::
#      dplyr::
#      tibble::
#      u_grid.R
##
unif_convolve <- function(
  n_pts = 1024L, # <int> number of input grid-points
  mix   = 1/2,   # <dbl> mixing coefficient (denoted t above)
  wrap  = TRUE   # identify {0, 1} as an arc midpoint
) {
  assert_that(0 < mix, mix < 1)
  mix_0 <- min(mix, 1 - mix)

  uc_tbl <- tibble(
    u   = u_grid(n = n_pts, wrap = wrap),
    d_0 = case_when(
      u <= mix_0       ~ u,
      u <= (1 - mix_0) ~ mix_0,
      TRUE             ~ (1 - u)),
    p_0 = case_when(
      u <= mix_0       ~ u^2/2,
      u <= (1 - mix_0) ~ mix_0 * (2*u - mix_0)/2,
      TRUE             ~ (mix_0 * (1 - mix_0)) - (1 - u)^2/2),
    dw  = d_0/(mix_0 * (1 - mix_0)),
    pw  = p_0/(mix_0 * (1 - mix_0))
  )

  return(
    uc_tbl %>% select(u, dw, pw)
  )
}


##
#  EOF
##
