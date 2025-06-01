##### 
### 
#     xmpl_fns.R
#     
#       a few 2-pi periodic, piecewise linear functions
### 
##### 

### 
#   saw_pf()
#     Sawtooth periodic function
### 
saw_pf <- function(
  x,                   # <dbl> input vector
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  assertthat::assert_that(p > 0)
  t <- mod_0(x, p, include_upper)
  return(t)
}

### 
#   sqr_pf()
#     Square wave periodic function
### 
sqr_pf <- function(
  x,                   # <dbl> input vector
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  assertthat::assert_that(p > 0)
  t <- mod_0(x, p, include_upper)
  return( sign(t) )
}

### 
#   tri_pf()
#     Triangular wave periodic function
### 
tri_pf <- function(
  x,                   # <dbl> input vector
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  assertthat::assert_that(p > 0)
  t <- mod_0(x, p, include_upper)
  return( ( p/2 ) - abs(t) )
}

### 
#   xmpl_pf_vecs()
#     
#     Given x, return tibble (fn, x, t, value)
#     where fn <chr> ranges over the identifiers of 
#     the example periodic functions in this file.
#     
### 
xmpl_pf_vecs <- function(
  x,                   # <dbl> input vector
  p = pi,              # <dbl> half-period
  include_upper = TRUE # <lgl> mod (-p, p] rather than [-p, p)
) {
  # x_reg_tbl = tibble (fn, pf, cs_fns)
  x_reg_tbl <- xmpl_registry()
  
  # initialize return tibble
  x_pf_long <- tibble::tibble()
  
  # t <- x mod (-p, p]
  t <- mod_0(x = x, p = p, include_upper = include_upper)
  
  # tibble for each example function: (fn, x, t, value)
  for (idx in 1:nrow(x_reg_tbl)) {
    x_fn <- x_reg_tbl [[idx, "fn"]]
    x_pf <- x_reg_tbl [[idx, "pf"]] [[1]]
	
	tmp_tbl <- tibble::tibble(
	  fn    = x_fn, 
	  x     = x, 
	  t     = t, 
	  value = x_pf (t)
	)
	# append tmp_tbl
	x_pf_long <- x_pf_long |> 
	  dplyr::bind_rows(tmp_tbl)
  }
  return(x_pf_long)
}

### 
#   saw_cs_fns()
#     
#     Sawtooth
#     
#     Return: functions that generate (cos, sin) coefficients
#     
#     Note: k in 0:k_max, typically
#     
### 
saw_cs_fns <- function() {
  
  c_fn <- function(k_vec) 0 * k_vec
  
  s_fn <- function(
    k_vec # <int> freq indices of returned coefficient vector
  ) {
    # initialize return vector
    s_vec = 0 * k_vec
    
    # non-zero coefficients
    k_nz <- k_vec [ k_vec != 0L ]
    s_vec [ k_vec != 0L ] <- 
      (-1L)^k_nz / k_nz
    
    # correction
    s_vec <- (-2) * s_vec
    
    return(s_vec)
  }
  return(list(
    c_fn = c_fn, 
    s_fn = s_fn
  ))
}

### 
#   sqr_cs_fns()
#     
#     Square wave
#     
#     Return: functions that generate (cos, sin) coefficients
#     
#     Note: k in 0:k_max, typically
#     
### 
sqr_cs_fns <- function() {
  
  c_fn <- function(k_vec) 0 * k_vec
  
  s_fn <- function(
    k_vec # <int> freq indices of returned coeff[k]
  ) {
    # initialize coefficients
    s_vec = 0 * k_vec
    
    # non-zero coefficients
    k_nz <- k_vec [ k_vec %% 2L != 0L ]
    s_vec [ k_vec %% 2L != 0L ] <- 
      1 / k_nz
    
    # correction
    s_vec <- (4 / pi) * s_vec
    
    return(s_vec)
  }
  return(list(
    c_fn = c_fn, 
    s_fn = s_fn
  ))
}

### 
#   tri_cs_fns()
#     
#     Triangular wave
#     
#     Return: functions that generate (cos, sin) coefficients
#     
#     Note: k in 0:k_max
#     
### 
tri_cs_fns <- function() {
  
  s_fn <- function(k_vec) 0 * k_vec
  
  c_fn <- function(
    k_vec # <int> freq indices of returned coeff[k]
  ) {
    # initialize coefficients
    c_vec = 0 * k_vec
    
    # non-zero coefficients
    k_nz <- k_vec [ k_vec %% 2L != 0L ]
    c_vec [ k_vec %% 2L != 0L ] <- 
      1 / (k_nz^2L)
    
    # correction
    c_vec <- (4 / pi) * c_vec
    
    return(c_vec)
  }
  return(list(
    c_fn = c_fn, 
    s_fn = s_fn
  ))
}

### 
#   xmpl_registry()
#     
#     Example periodic functions
#     
#     Return: tibble( fn, pf, cs_fns)
#     
#     Note: pf and cs_fns are columns each of 
#           whose elements are lists of length 1
#     
### 
xmpl_registry <- function() {
  x_reg_tbl <- tibble::tribble(
    ~fn, ~pf, ~cs_fns, 
	"saw", saw_pf, saw_cs_fns, 
	"sqr", sqr_pf, sqr_cs_fns, 
	"tri", tri_pf, tri_cs_fns
  )
  return(x_reg_tbl)
}

### 
#   xmpl_cs_vecs()
#     
#     Piecewise periodic functions: Fourier coefficients
#     
#     Return: tibble( fn, k, c_vec, s_vec ) of (cos, sin) coefficients
#     
#     Note: k in 0:k_max, typically
#     
### 
xmpl_cs_vecs <- function(
  k_vec = 0:15 # <int> freq indices k of returned coeff [k]
) {
  # x_reg_tbl = tibble (fn, pf, cs_fns)
  x_reg_tbl <- xmpl_registry()
  
  # initialize return tibble
  coeff_tbl <- tibble::tibble()
  
  # tibble for each example function: (fn, k, c_vec, s_vec)
  for (idx in 1:nrow(x_reg_tbl)) {
    fn_tag <- x_reg_tbl [[idx, "fn"]]
    cs_lst <- x_reg_tbl [[idx, "cs_fns"]] [[1]] ()
	
	tmp_tbl <- tibble::tibble(
	  fn    = fn_tag, 
	  k     = k_vec, 
	  c_vec = cs_lst$ c_fn (k), 
	  s_vec = cs_lst$ s_fn (k)
	)
	
	# append tmp_tbl
	coeff_tbl <- coeff_tbl |> 
	  dplyr::bind_rows(tmp_tbl)
  }
  return(coeff_tbl)
}

### 
# EOF 
### 
