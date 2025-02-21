parseLTER <- function(filename, dt_min) {
  # Load the data from the specified file
  load(filename)  # Assuming 'All_LB_met' is loaded from the file
  
  min_per_day <- 1440
  steps_per_day <- min_per_day / dt_min
  
  # Process air temperature
  air_temp_matrix <- matrix(All_LB_met[, 1], nrow = 96, ncol = 365 * 18)
  air_temp <- rowMeans(air_temp_matrix) + 273.15  # Convert to Kelvin
  
  # Initialize result list
  data <- list(air_temp = air_temp)
  
  # Process shortwave radiation
  if (dt_min > 15) {
    sw_matrix <- matrix(All_LB_met[, 4], nrow = 96 / steps_per_day, ncol = 365 * 18 * steps_per_day)
    sw <- rowMeans(sw_matrix)
    sw <- matrix(sw, nrow = steps_per_day, ncol = 365 * 18)
    data$sw <- sw
  } else {
    sw_matrix <- matrix(All_LB_met[, 4], nrow = 96, ncol = 365 * 18)
    data$sw <- t(sw_matrix)  # Transpose to match MATLAB output
  }
  
  # Process relative humidity
  rh_matrix <- matrix(All_LB_met[, 5], nrow = 96, ncol = 365 * 18)
  data$RH <- rowMeans(rh_matrix)  # Average RH
  
  # Process wind speed
  wind_speed_matrix <- matrix(All_LB_met[, 6], nrow = 96, ncol = 365 * 18)
  data$wind_speed <- rowMeans(wind_speed_matrix)  # Average wind speed
  
  return(data)
}
