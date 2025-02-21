updateIceCover <- function(N, bc, bc_prev, Uprev, I0, ablated, dt, params, opt) {
  # Unpack necessary parameters
  stationary <- FALSE
  if (!missing(opt) && !is.null(opt$stationary)) {
    stationary <- opt$stationary
  }
  
  eps <- params$eps
  Tscale <- params$tempND(0)[2]
  htc_ice <- params$htc_ice
  htc_water <- params$htc_water
  r <- params$r
  c <- params$c
  kappa_ <- params$kappa_
  L_ <- params$L_
  e <- params$e
  
  # Setup radiation term
  absorb_indicator <- function(v) v >= 0
  source <- function(z, htc) kappa_ * (I0 / (htc * Tscale)) * exp(-kappa_ * z)
  
  # Setup the grid
  grid <- cheb(N)
  D <- grid$D
  x <- grid$x
  y <- x
  D2 <- D %*% D
  D2 <- D2[2:N, 2:N]
  
  # Solve F(U_) = 0
  dirichlet_result <- makeDirichlet(Uprev)
  Uprev <- dirichlet_result$Uprev
  u0 <- dirichlet_result$u0
  Du0 <- dirichlet_result$Du0
  v0 <- dirichlet_result$v0
  Dv0 <- dirichlet_result$Dv0
  
  if (!stationary) {
    U_ <- newtonSolve(TrapStep, Uprev, opt)
  } else {
    U_ <- newtonSolve(Udot, Uprev, opt)
  }
  
  absorbed <- dt * eps * Udot(U_)
  U <- applyBCs(U_)
  udot <- Udot(U_)
  hdot <- eps * udot[length(udot)] / e
  
  return(list(U = U, hdot = hdot, absorbed = absorbed))
}

BEStep <- function(U_, Uprev, dt, eps) {
  dU <- U_ - Uprev
  dU0 <- c(Du0[2:N], Dv0[2:N], 0)
  F <- (dU - dU0) - dt * eps * Udot(U_)
  return(F)
}

TrapStep <- function(U_, Uprev, dt, eps) {
  dU <- U_ - Uprev
  dU0 <- c(Du0[2:N], Dv0[2:N], 0)
  F <- (dU - dU0) / dt - 0.5 * eps * (Udot(U_) + Udot(Uprev))
  return(F)
}

Udot <- function(U_) {
  F_ <- numeric(length(U_))
  one <- rep(1, N - 1)
  
  Utmp <- applyBCs(U_)
  dudx <- D[1, ] %*% Utmp[1:(N + 1)]  # boundary flux at x = 1
  dvdy <- D[N + 1, ] %*% Utmp[(N + 2):(length(Utmp) - 1)]  # boundary flux at y = -1
  
  h <- U_[length(U_)] / e - ablated  # new interface position
  Ju <- 2 / h
  Jv <- 2 / (L_ - h)
  
  # Heat equation in ice
  z <- (x[2:N] + one) / Ju
  iu <- seq_len(N - 1)
  S <- source(z, htc_ice)
  absorbed <- S * absorb_indicator((U_[iu] + u0[2:N]) * Tscale)
  udot <- (Ju^2) * (D2 %*% U_[iu]) + (S - absorbed)
  F_[iu] <- udot
  
  # Heat equation in water
  z <- (y[2:N] + one) / Jv + h * one
  iv <- seq(N, 2 * N - 2)
  vdot <- (Jv^2) * (r / c) * (D2 %*% U_[iv]) + (r / c) * source(z, htc_water)
  F_[iv] <- vdot
  
  # Last row in Dirichlet matrix from interface balance
  Sdot <- (Ju * dudx - r * Jv * dvdy)
  F_[length(F_)] <- Sdot
  
  return(list(F_ = F_, absorbed = absorbed))
}

# Utility functions
makeDirichlet <- function(U) {
  one <- rep(1, length(x))
  u0 <- (bc$Theta_s / 2) * (one - x) + (bc$Theta_m / 2) * (one + x)
  v0 <- (bc$Theta_m / 2) * (one - y) + (bc$Theta_0 / 2) * (one + y)
  
  Du0 <- 0.5 * (bc$Theta_s - bc_prev$Theta_s) * (one - x) +
    0.5 * (bc$Theta_m - bc_prev$Theta_m) * (one + x)
  Dv0 <- 0.5 * (bc$Theta_m - bc_prev$Theta_m) * (one - y) +
    0.5 * (bc$Theta_0 - bc_prev$Theta_0) * (one + y)
  
  u_ <- U[1:(N + 1)] - u0
  v_ <- U[(N + 2):(length(U) - 1)] - v0
  S <- U[length(U)]
  
  U_ <- c(u_[2:N], v_[2:N], S)
  return(list(U_ = U_, u0 = u0, Du0 = Du0, v0 = v0, Dv0 = Dv0))
}

applyBCs <- function(U_) {
  u <- U_[1:(N - 1)] + u0[2:N]
  v <- U_[N:(length(U_) - 1)] + v0[2:N]
  U <- c(bc$Theta_m, u, bc$Theta_s, bc$Theta_0, v, bc$Theta_m, U_[length(U_)])
  return(U)
}

# Example of a Newton solver function
newtonSolve <- function(func, x0, opt, tol = 1e-8, max_iter = 100) {
  x <- x0
  for (i in seq_len(max_iter)) {
    f_val <- func(x)
    derivative <- (func(x + tol) - f_val) / tol
    x_new <- x - f_val / derivative
    if (all(abs(x_new - x) < tol)) {
      return(x_new)
    }
    x <- x_new
  }
  stop("Newton's method did not converge.")
}
