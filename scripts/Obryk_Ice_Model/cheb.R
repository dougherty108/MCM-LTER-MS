cheb <- function(N) {
  # CHEB computes D = differentiation matrix and x = Chebyshev grid
  
  if (N == 0) {
    D <- 0
    x <- 1
    return(list(D = D, x = x))
  }
  
  x <- cos(pi * (0:N) / N)  # compute the grid
  
  c <- c(2, rep(1, N - 1), 2) * (-1)^(0:N)  # coefficients
  X <- matrix(rep(x, each = N + 1), nrow = N + 1)  # create a matrix of x
  dX <- X - t(X)  # compute the difference matrix
  
  # Off-diagonal entries
  D <- (c %*% t(1 / c)) / (dX + diag(1, N + 1))
  
  # Diagonal entries
  D <- D - diag(rowSums(D))
  
  return(list(D = D, x = x))
}
