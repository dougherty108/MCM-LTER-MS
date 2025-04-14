determineDailyIncidentFlux <- function(day, hour, dt, steps_per_day, surfaceProperties, surfaceVars, data = NULL) {
  
  if (!is.null(data) && !is.null(data$sw) && day <= nrow(data$sw)) {
    sw <- data$sw[day, hour]
    if (is.nan(sw)) {
      sw <- simulateSW()
    }
  } else {
    sw <- simulateSW()
  }
  
  if (!is.null(data) && !is.null(data$lwdown) && day <= nrow(data$lwdown)) {
    lwdown <- data$lwdown[day, hour]
    if (is.nan(lwdown)) {
      lwdown <- simulateLW()
    }
  } else {
    lwdown <- simulateLW()
  }
  
  # Nested function to simulate longwave radiation
  simulateLW <- function() {
    s <- surfaceProperties$s
    t_air <- surfaceVars$t_air
    cl <- surfaceVars$cl
    lw <- s * t_air^4 * (0.765 + 0.22 * (cl^3))
    return(lw)
  }
  
  # Nested function to simulate shortwave radiation
  simulateSW <- function() {
    rad <- 1 * (pi / 180)  # degrees to radians
    lat_r <- surfaceProperties$lat * rad  # latitude in radians
    fcirc <- 360 * rad  # 360 degrees in radians
    
    # Solar declination in rad
    decl <- 23.45 * rad * sin(fcirc * ((day + 284) / 365))
    
    # Solar angle in dt-min interval
    HA <- (hour - (steps_per_day / 2)) * ((15 * dt / 60) * rad)
    z <- acos(sin(decl) * sin(lat_r) + cos(lat_r) * cos(decl) * cos(HA))
    
    # Shortwave radiation
    cloud_cover <- surfaceVars$cl
    sw <- 1376 * (1 - 0.48 * cloud_cover) * cos(z) * (0.33 + 0.43 * cos(z))
    sw[sw < 0] <- 0  # simulates polar night/winter
    return(sw)
  }
  
  return(list(sw = sw, lwdown = lwdown))
}
