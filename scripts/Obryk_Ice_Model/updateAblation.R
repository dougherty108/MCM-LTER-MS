updateAblation <- function(Sprev, dt, flux, flux_prev, Tscale, pdeParams) {
  # Use an optimization function to solve the equation
  Sb <- newtonSolve(TrapStep, Sprev)
  beta <- Sb / pdeParams$eL
  bdot <- Sdot(flux) / pdeParams$eL
  
  return(list(Sb = Sb, beta = beta, bdot = bdot))
}

TrapStep <- function(S) {
  # Backward-Euler time-stepping
  dS <- S - Sprev
  F <- dS / dt - 0.5 * (Sdot(flux) + Sdot(flux_prev))
  return(F)
}

Sdot <- function(Q) {
  F <- pdeParams$eps * (Q) / (Tscale * pdeParams$htc_ice)   
  return(F)
}

# Example of a Newton solver function
newtonSolve <- function(func, x0, tol = 1e-8, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    f_val <- func(x)
    # Implement a simple numerical derivative
    derivative <- (func(x + tol) - f_val) / tol
    x_new <- x - f_val / derivative
    if (abs(x_new - x) < tol) {
      return(x_new)
    }
    x <- x_new
  }
  stop("Newton's method did not converge.")
}
