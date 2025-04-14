determineDailySurfaceConditions <- function(day, data) {
  # Determines daily surface conditions, either heuristically or from data
  
  # OUTPUT
  #   air_temp [K]
  #   wind_speed [m/s]
  #   rel_humid [%]  (relative humidity)
  #   cloud_cover
  
  if (!is.null(data) && "air_temp" %in% names(data) && day <= length(data$air_temp)) {
    air_temp <- data$air_temp[day]
    if (is.nan(air_temp)) {
      air_temp <- air_temp_model(day)
    }
  } else {
    air_temp <- air_temp_model(day)
  }
  
  if (!is.null(data) && "wind_speed" %in% names(data) && day <= length(data$wind_speed)) {
    wind_speed <- data$wind_speed[day]
    if (is.nan(wind_speed)) {
      wind_speed <- sample(2:20, 1)
    }
  } else {
    wind_speed <- sample(2:20, 1)
  }
  
  if (!is.null(data) && "RH" %in% names(data) && day <= length(data$RH)) {
    rel_humid <- data$RH[day]
    if (is.nan(rel_humid)) {
      rel_humid <- sample(30:80, 1)
    }
  } else {
    rel_humid <- sample(30:80, 1)
  }
  
  # Cloud cover randomizer
  cloud_cover <- runif(1)  # random number between 0 and 1
  
  # Wrap up
  SurfaceVars <- list(
    t_air = air_temp,
    RH = rel_humid,
    V = wind_speed,
    cl = cloud_cover
  )
  
  return(SurfaceVars)
}
