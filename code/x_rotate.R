##### 
### 
#     x_rotate.R
#     
#     circular rotation of vector x[1:n]
#     via circular rotation of indices [1:n]
### 
##### 

### 
#   i_center()
#     
#     Given n, return vector i_ctr of n successive 
#     integers centered at zero: (-h, -h+1, ..., k) 
#     where |h - k| <= 1 and h + k = n - 1.
### 
i_center <- function(
    n     = 1L,  # <int> positive integer
    upper = TRUE # <lgl> ensure k >= h
) {
  assertthat::assert_that(
    n >= 1, 
    is.integer(n))
  
  if (n == 0L) return(0)
  if (n == 2L) {
    if (upper) {
      return(0:1) 
    } else {
      return(-1:0)
    }
  }
  
  h_min <- floor(   (n-1)/2 ) |> as.integer()
  h_max <- ceiling( (n-1)/2 ) |> as.integer()
  
  if (upper) {
    i_ctr <- c(-(h_min:0), 1:h_max)
  } else {
    i_ctr <- c(-(h_max:0), 1:h_min)
  }
  return(i_ctr)
}
# > tibble::tibble(n = 3L, lwr = i_center(3L, FALSE), upr = i_center(3L))
# # A tibble: 3 × 3
#       n   lwr   upr
#   <int> <int> <int>
# 1     3    -1    -1
# 2     3     0     0
# 3     3     1     1
# 
# > tibble::tibble(n = 4L, lwr = i_center(4L, FALSE), upr = i_center(4L))
# # A tibble: 4 × 3
#       n   lwr   upr
#   <int> <int> <int>
# 1     4    -2    -1
# 2     4    -1     0
# 3     4     0     1
# 4     4     1     2

### 
#   i_rotate()
#     
#     Given n, return integer vector idx, where 
#     idx = ((1:n + i_add - 1) mod n) + 1.
### 
i_rotate <- function(
    n     = 1L, # <int> positive integer
    i_add = 1L  # <int> degree and direction of rotation
) {
  assertthat::assert_that(
    n >= 1, 
    is.integer(n), 
    is.integer(i_add))
  
  if (n <= 1) return (1L)
  
  idx <- ((1:n + i_add - 1L) %% n) + 1L
  
  return(idx)
}

### 
#   x_rotate()
#     
#     Given vector x[1:n], return y = x[idx], where 
#     idx = ((1:n + i_add - 1) mod n) + 1.
### 
x_rotate <- function(
    x,          # input vector
    i_add = 1L  # <int> degree and direction of rotation
) {
  idx <- i_rotate(n = length(x), i_add = i_add)
  return(x [idx])
}
# > numbers::Primes(20)
# [1]  2  3  5  7 11 13 17 19
# > Primes(20) |> x_rotate(i_add = 1L)
# [1]  3  5  7 11 13 17 19  2
# > Primes(20) |> x_rotate(i_add = -1L)
# [1] 19  2  3  5  7 11 13 17

### 
# EOF 
### 
