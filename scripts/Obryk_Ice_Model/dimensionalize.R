dimensionalize <- function(theta, N, params, H0, ablated, invert, zgrid) {
  # Transform non-dimensional quantity theta(x, ndtime) to dimensional quantity T(z, t)
  
  h <- theta[length(theta)] / params$e - ablated
  theta <- theta[-length(theta)]  # drop h row
  
  T_ <- invert(theta)
  
  u <- T_[1:(N + 1)]
  v <- T_[(N + 2):length(T_)]
  
  # Create Chebyshev differentiation matrix and grid
  cheb_result <- cheb(N)
  D <- cheb_result[[1]]
  x <- cheb_result[[2]]
  
  if (missing(invert)) {
    # must reverse T on each subdomain
    v <- v[-length(v)]  # drop last temperature row (y_N == x_0)
    
    T <- c(rev(u), rev(v))  # reverse u on x and v on y
    
    # similarly: reverse x to get z increasing [drop last row of y as above]
    one <- rep(1, N + 1)
    z_ <- c(
      (h / 2) * (rev(x) + one),
      ((params$L_ - h) / 2) * (rev(x[-length(x)]) + one[-length(one)]) + h * one[-length(one)]
    )
    z <- H0 * z_
    temp <- T
  } else {
    # partition z grid
    H_idx <- which(zgrid > h * H0)[1]
    zx <- zgrid[1:(H_idx - 1)]
    zy <- zgrid[H_idx:length(zgrid)]
    
    xx <- 2 / (h * H0) * zx - rep(1, length(zx))
    yy <- 2 / (params$L_ - h) * (zy / H0 - rep(1, length(zy)) * h) - rep(1, length(zy))
    
    # interpolate u on x
    Tzx <- approx(x, u, xx)$y  # pchip interpolation in MATLAB equivalent
    
    # interpolate y on v
    Tzy <- approx(x, v, yy)$y  # pchip interpolation in MATLAB equivalent
    
    z <- zgrid
    temp <- c(Tzx, Tzy)
  }
  
  return(list(temp = temp, z = z))
}
