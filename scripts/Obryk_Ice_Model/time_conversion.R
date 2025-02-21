time_conversion <- function(n, y, dt, first_year) {
  # Initialize variables
  counter <- 0
  day_steps <- 24 * 60 / dt
  decimal_time <- numeric(n * y * day_steps)  # Preallocate for speed
  hourmin <- numeric(n * y * day_steps)  # Preallocate for hour:min
  
  # Loop over years
  for (k in 1:n) {
    # Converts n to an actual year
    year <- k + (first_year - 1)
    
    # Loop over days
    for (j in 1:y) {
      Julian_day <- j
      
      if (dt == 15) {
        # Generates hour:min for 15-minute increments
        for (i in 1:24) {
          hr <- 100 * (i - 1)
          
          for (h in 1:4) {
            counter <- counter + 1
            hourmin[counter] <- hr + (dt * (h - 1))
            
            # Generates decimal time
            decimal_time[counter] <- year + ((((Julian_day - 1) +
                                                (floor(hourmin[counter] / 100) + 
                                                   ((hourmin[counter] - (floor(hourmin[counter] / 100) * 100)) / 60)) / 24) / 365))
          }
        }
      } else {
        # Generates hour:min for other time steps
        for (i in 1:day_steps) {
          hr <- (100 * 24 / day_steps) * (i - 1)
          counter <- counter + 1
          hourmin[counter] <- hr
          
          # Generates decimal time
          decimal_time[counter] <- year + ((((Julian_day - 1) +
                                              (floor(hourmin[counter] / 100) + 
                                                 ((hourmin[counter] - (floor(hourmin[counter] / 100) * 100)) / 60)) / 24) / 365))
        }
      }
    }
  }
  
  # Return decimal_time as a vector
  return(decimal_time[1:counter])  # Trim to the actual number of entries
}
