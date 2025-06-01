##### 
### 
#     pf_factory.R
#     
#       create and manage periodic functions
### 
##### 

### 
#   mod_0()
#     
#     t <- x mod (-p, p]
#     
#     Note: x %% (2p) in [0, 2p)
### 
mod_0 <- function(
  x,                   # <dbl> input vector
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  assertthat::assert_that(p > 0)
  
  if ( max( abs(x) ) < p ) {
    # avoid unnecessary computation
	t <- x
  } else {
    if (include_upper) {
	  # t in (-p, p]
	  t <- p - ((-x - p) %% (2 * p))
	} else {
	  # t in [-p, p)
	  t <- ((x + p) %% (2 * p)) - p
	}
  }
  return(t)
}

### 
#   f_to_pf()
#     Return a periodic version pf() of given function f()
#     
#     t <- x mod (-p, p]
#     
#     pf( x ) = f( t )
### 
f_to_pf <- function(
  f,                   # input function
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  assertthat::assert_that(p > 0)
  
  pf <- function(
    x,  # <dbl> input vector
    ... # additional arguments to f()
  ) {
    t <- mod_0(x, p, include_upper)
	return( f(t, ...) )
  }
  return(pf)
}

### 
#   pf_tbl()
#     
#     given vector x and periodic function pf(), 
#     return tibble (x, t, pf(t))
#     
#     where:
#     
#     t <- x mod (-p, p]
#     
### 
pf_tbl <- function(
  x,                   # <dbl> input vector
  pf,                  # periodic function
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  # t <- x mod (-p, p]
  t <- mod_0 (x, p, include_upper)
  
  # evaluate function
  f <- pf(t, p, include_upper)
  
  return(tibble::tibble(
    x = x, t = t, f = f
  ))
}

### 
#   pf_cs_vecs()
#     
#     Given: functions that generate (cos, sin) coefficients of pf()
#     
#     Return: tibble( k, c_vec, s_vec ) of (cos, sin) coefficients
#     
#     Note: k in 0:k_max, typically
#     
### 
pf_cs_vecs <- function(
  k_vec = 0:2, # <int> freq indices of returned coefficients
  c_fn,        # <dbl> function that generates pf() cosine coefficients
  s_fn         # <dbl> function that generates pf() sine coefficients
) {
  coeff_tbl <- tibble::tibble(
    k     = k_vec, 
	c_vec = c_fn(k_vec), 
	s_vec = s_fn(k_vec)
  )
  return(coeff_tbl)
}

### 
# EOF 
### 
