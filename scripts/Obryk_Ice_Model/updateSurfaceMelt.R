updateSurfaceMelt <- function(Sprev, dt, Fi, melt_flux, Tscale, pdeParams) {
  
  opt <- list(lmeth = 2, etamax = 0.5)
  
  if (Sprev > 0) {
    Sguess <- Sprev
  } else {
    Sguess <- 1e-3 * pdeParams$e
  }
  
  Sg_result <- newtonSolve(TrapStep, Sguess, opt)
  Sg <- Sg_result$solution
  gamma <- Sg / pdeParams$e
  gdot <- Sdot(Sg) / pdeParams$e
  
  return(list(Sg = Sg, gamma = gamma, gdot = gdot))
}

TrapStep <- function(S, Sprev, dt) {
  # Trapezoidal time-stepping
  dS <- S - Sprev
  F <- dS / dt - 0.5 * (Sdot(S) + Sdot(Sprev))
  return(F)
}

Sdot <- function(S, Fi, melt_flux, pdeParams, Tscale) {
  gamma <- S / pdeParams$e
  Fw <- melt_flux(gamma)
  F <- pdeParams$eps * (-Fi + Fw) / (Tscale * pdeParams$htc_ice)
  return(F)
}

# Example of a Newton solver function
newtonSolve <- function(func, x0, opt, tol = 1e-8, max_iter = 100) {
  x <- x0
  for (i in seq_len(max_iter)) {
    f_val <- func(x)
    derivative <- (func(x + tol) - f_val) / tol
    x_new <- x - f_val / derivative
    if (abs(x_new - x) < tol) {
      return(list(solution = x_new))
    }
    x <- x_new
  }
  stop("Newton's method did not converge.")
}
