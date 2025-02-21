computeSurfaceTemp <- function(Tguess, U, N, surfaceProperties, surfaceVars, opt = NULL) {
  # Computes the surface temperature and surface fluxes
  
  T_melt <- 273.15  # Melting temperature in K
  
  # ----- Unpack properties -----
  ca <- surfaceProperties$ca
  Ch <- surfaceProperties$Ch
  Ce <- surfaceProperties$Ce
  s <- surfaceProperties$s
  ed <- surfaceProperties$ed
  albedo <- surfaceProperties$albedo
  chi <- surfaceProperties$chi
  air_press <- surfaceProperties$air_press
  htc_ice <- surfaceProperties$htc_ice
  
  RH <- surfaceVars$RH
  V <- surfaceVars$V
  T_air <- surfaceVars$t_air
  
  # Get differentiation matrix from cheb function
  D <- cheb(N)
  Ds <- D[N + 1, ]  # Derivative operator at surface
  h <- U[length(U)] / surfaceProperties$e
  
  # ----- Compute derived quantities -----
  Rg <- 8.31  # U.S. Std Atmosphere gas constant [J mol-1 K-1]
  molar_mass <- 28.9645e-3  # kg/mol
  R_air <- Rg / molar_mass  # Specific gas constant [J g-1 K-1]
  pa <- (air_press * 1e3) / (R_air * T_air)  # Convert pressure from kg -> g
  
  # Saturation vapor pressure [Pa]
  es <- 610.8 * exp((17.27 * (T_air - T_melt)) / (237.3 + (T_air - T_melt)))
  
  # Actual vapor pressure [Pa]
  ea <- es * RH / 100
  
  # Specific humidity of air (qa) and next to ice surface (qs)
  qa <- (0.622 * ea) / (air_press - (0.378 * ea))
  qs <- (0.622 * es) / (air_press - (0.378 * es))
  
  # ----- Unpack fluxes -----
  sw <- surfaceVars$sw
  lwdown <- surfaceVars$lwdown
  
  # -------------------------------------------------------
  # Solve F(Ts) = 0, where F(Ts,flux) is the total flux
  if (is.null(opt)) {
    F <- function(T) totalFlux(T)
  } else {
    F <- function(T) totalFluxInit(T)
  }
  
  result <- newtonSolve(F, Tguess)
  Ts <- result$Ts
  hist <- result$hist
  
  if (Ts >= 273.15) {
    Ts <- T_melt - 1
  }
  
  # Output surface fluxes for this timestep
  flux <- computeFluxes(Ts)
  flux$Qabl <- (1 - albedo) * flux$Qs + flux$Qd - flux$Qb - (flux$Qh + flux$Ql) - flux$Fi
  
  # -------------------------------------------------------
  
  # Nested functions
  totalFlux <- function(ts) {
    # Summation of heat fluxes at surface
    fluxes <- computeFluxes(ts)
    
    # Net incoming shortwave radiation
    SW_absorbed <- (1 - albedo) * fluxes$Q_s - fluxes$I0
    
    # Net incoming longwave radiation
    LW_absorbed <- fluxes$Q_d - fluxes$Q_b
    
    # ----- Net flux -----
    Fnet <- SW_absorbed + LW_absorbed - (fluxes$Q_h + fluxes$Q_l) - fluxes$Fi
    return(Fnet)
  }
  
  totalFluxInit <- function(ts) {
    # Summation of heat fluxes at surface
    fluxes <- computeFluxes(ts)
    
    # Net incoming shortwave radiation
    SW_absorbed <- (1 - albedo) * fluxes$Q_s - fluxes$I0
    
    # Net incoming longwave radiation
    LW_absorbed <- fluxes$Q_d - fluxes$Q_b
    
    # Use linear approximation at t=0
    dTdz_ <- Tscale * (T_melt - ts) / h
    Fi <- -htc_ice * dTdz_
    
    # ----- Net flux -----
    Fnet <- SW_absorbed + LW_absorbed - (fluxes$Q_h + fluxes$Q_l) - Fi
    return(Fnet)
  }
  
  computeFluxes <- function(ts) {
    # Shortwave radiation
    Q_s <- sw               # Downwelling
    I0 <- (1 - chi) * (1 - albedo) * Q_s  # Penetrating shortwave
    
    # Longwave radiation
    Q_b <- ed * s * ts^4   # Upwelling
    Q_d <- lwdown          # Downwelling
    
    # Sensible heat (outgoing flux)
    Q_h <- pa * ca * Ch * V * (ts - T_air)
    
    # Latent heat (outgoing flux)
    L <- 2.5e3  # Latent heat of vaporization [J/g]
    c <- 2375 / 1e3  # Specific heat derived based on empirical formulation [J/g]
    sub <- 335000 / 1e3  # For sublimation [J/g]
    
    if (ts >= T_melt) {
      Rl <- L - c * (ts - T_melt)
    } else {
      Rl <- L - c * (ts - T_melt) + sub
    }
    
    Q_l <- pa * Rl * Ce * V * (qs - qa)
    
    # Conductive heat flux
    Us <- surfaceProperties$tempND(ts)
    U[N + 1] <- Us
    dudx <- Ds %*% U[1:(N + 1)]  # Boundary flux at x = -1
    dTdz_ <- Tscale * (2 / h) * dudx
    Fi <- -htc_ice * dTdz_
    
    return(list(Qs = Q_s, I0 = I0, Qb = Q_b, Qd = Q_d, Ql = Q_l, Qh = Q_h, Fi = Fi))
  }
  
  return(list(Ts = Ts, flux = flux))
}
