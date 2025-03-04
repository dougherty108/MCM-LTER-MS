simulate_ice <- function(days, dt_min, lake_depth, bottom_temp, H0, N, data, opt = NULL) {
  # Constants
  melt_tol <- 0.5
  Tm <- 273.15  # Freezing temperature of fresh water (K)
  
  # Adjust temperature to Kelvin
  T0 <- bottom_temp + Tm
  tempND <- function(T) getTheta(T, Tm, T0)
  
  if (is.null(opt)) {
    solverOpt <- list(atol = 0.0001, rtol = 0.01, lmeth = 1)
  } else {
    solverOpt <- opt
  }
  
  # Space and time setup
  zgrid <- seq(0, lake_depth, length.out = 101)
  D <- cheb(N)  # Assume cheb is defined elsewhere
  
  days_in_year <- 365
  years <- ceiling(days / days_in_year)
  
  minutes_per_day <- 1440
  sec_per_min <- 60
  steps_per_day <- minutes_per_day / dt_min
  steps <- days * steps_per_day
  dt <- dt_min * sec_per_min  # Timestep in seconds
  
  # Model parameters
  c_ice <- 2.108  # Specific heat of ice (J/g/K)
  c_water <- 4.187  # Specific heat of water (J/g/K)
  k_ice <- 2.3  # Thermal conductivity of ice (W/m/K)
  k_water <- 0.58  # Thermal conductivity of water (W/m/K)
  kappa <- 0.5  # Extinction coefficient of ice [m-1]
  ice_dens <- 915 * 1e3  # Ice density (g/m³)
  latent_freezing <- 334  # Latent heat of freezing (J/g)
  latent_sublimation <- 2838  # Latent heat of sublimation
  albedo <- calculate_albedo(years, steps_per_day)  # Ice albedo [unitless %]
  Chw <- 1.5e-3  # Bulk transfer coefficient (N.D.)
  
  # Time scale
  k_ <- k_ice / (ice_dens * c_ice)  # Thermal diffusivity [m²/s]
  diff_scale <- (H0^2) / k_  # in sec
  t_scale <- 86400  # in sec
  dt <- dt / t_scale  # Non-dimensional timestep
  
  # Scaled parameters for bulk PDEs
  Tscale <- tempND(T0)[2]
  pdeParams <- list(
    eps = t_scale / diff_scale,
    kappa_ = kappa * H0,
    r = k_water / k_ice,
    htc_ice = k_ice / H0,
    htc_water = k_water / H0,
    c = c_water / c_ice,
    eL = latent_freezing / (c_ice * Tscale),
    eS = Chw * (c_water / c_ice - 1) * Tm / Tscale,
    eABL = latent_sublimation / (c_ice * Tscale),
    e = (latent_freezing + Chw * (c_water / c_ice - 1) * Tm) / (c_ice * Tscale),
    L_ = lake_depth / H0,
    alpha = albedo[1],
    tempND = tempND
  )
  
  # Surface properties
  surfaceProperties <- list(
    lat = -77.024722,
    ca = 1.004,
    Ch = 1.75e-3,
    Ce = 1.75e-3,
    s = 5.67e-8,
    ed = 0.97,
    albedo = albedo[1],
    chi = 0.82,
    air_press = 98555,
    tempND = tempND,
    htc_ice = pdeParams$htc_ice,
    e = pdeParams$e
  )
  
  # Preallocate matrices
  sw <- numeric(steps)
  lwup <- numeric(steps)
  lwdown <- numeric(steps)
  sens_heat <- numeric(steps)
  lat_heat <- numeric(steps)
  conductive_surf <- numeric(steps)
  
  t_surface <- numeric(steps)
  t_bulk <- matrix(0, nrow = length(zgrid), ncol = steps)
  H <- numeric(steps)
  
  # Initial conditions
  step <- 0
  pb <- txtProgressBar(min = 0, max = steps, style = 3)
  
  # Initialize ice thickness
  h0 <- 1
  S0 <- pdeParams$e * h0
  U <- c(rep(0, 2 * (N + 1) + 1), S0)
  
  # Surface temperature at "hour 0"
  surfaceVars <- determineDailySurfaceConditions(1, data)
  incident_flux <- determineDailyIncidentFlux(1, 1, dt_min, steps_per_day, surfaceProperties, surfaceVars, data)
  
  sw0 <- incident_flux$sw
  lwdown0 <- incident_flux$lwdown
  surfaceVars$sw <- sw0
  surfaceVars$lwdown <- lwdown0
  Ts_flux <- computeSurfaceTemp(surfaceVars$t_air, U, N, surfaceProperties, surfaceVars)
  
  if (Ts_flux > Tm) {
    stop('Ts_0 > Tm (must now fix initial condition)')
  }
  
  gamma <- 0
  Sb <- 0
  Qabl <- Ts_flux$Qabl
  
  # Initial boundary conditions
  bc <- list(
    Theta_s = tempND(Ts_flux),
    Theta_m = tempND(Tm),
    Theta_0 = tempND(T0)
  )
  
  # Load initial conditions
  ini_cond <- readRDS('init_cond.rds')  # Adjust as necessary
  in_cond <- ini_cond$initial_cond
  
  y <- D$x
  u0 <- in_cond[1:17]
  v0 <- in_cond[18:35]
  
  U <- c(bc$Theta_m, u0[-1], bc$Theta_s, bc$Theta_0, v0[-1], bc$Theta_m, S0)
  
  # Simulation loop
  for (day in 1:days) {
    surfaceVars <- determineDailySurfaceConditions(day, data)
    
    for (hour in 1:steps_per_day) {
      step <- step + 1
      setTxtProgressBar(pb, step)
      
      surfaceProperties$albedo <- albedo[step]
      
      # Determine ambient conditions
      incident_flux <- determineDailyIncidentFlux(day, hour, dt_min, steps_per_day, surfaceProperties, surfaceVars, data)
      surfaceVars$sw <- incident_flux$sw
      surfaceVars$lwdown <- incident_flux$lwdown
      
      # Compute surface temperature and heat fluxes
      Ts_flux <- computeSurfaceTemp(Ts_flux, U, N, surfaceProperties, surfaceVars)
      bc$Theta_s <- tempND(Ts_flux)
      
      # Compute ablation
      Sb <- updateAblation(Sb, dt, Ts_flux$Qabl, Qabl, Tscale, pdeParams)
      Qabl <- Ts_flux$Qabl
      ablated <- Sb$beta
      
      # Solve heat equation in the bulk and advance the interface
      U <- updateIceCover(N, bc, U, Ts_flux$I0, ablated, dt, pdeParams, solverOpt)
      
      # Update total ice thickness
      H[step] <- H0 * U[length(U)] / pdeParams$e - (H0 * ablated)
      
      if (H[step] < melt_tol) {
        stop(sprintf('Ice has melted to thickness of %f meters --- stopping simulation.', H[step]))
      }
      
      # Capture output variables
      [T, z] <- dimensionalize(U, N, pdeParams, H0, ablated, invert, zgrid)
      t_bulk[, step] <- T
      t_surface[step] <- Ts_flux
      
      # Update flux history
      sw[step] <- Ts_flux$Qs
      lwdown[step] <- Ts_flux$Qd
      lwup[step] <- Ts_flux$Qb
      sens_heat[step] <- Ts_flux$Qh
      lat_heat[step] <- Ts_flux$Ql
      conductive_surf[step] <- Ts_flux$Fi
      
      # Store all data in a list
      all_data <- list(
        t_surface = t_surface,
        t_bulk = t_bulk,
        H = H,
        sw = sw,
        lwdown = lwdown,
        lwup = lwup,
        sens_heat = sens_heat,
        lat_heat = lat_heat,
        conductive_surf = conductive_surf
      )
    }  # End of hour loop
  }  # End of day loop
  
  close(pb)
  return(all_data)
}

