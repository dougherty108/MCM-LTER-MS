dailyAverage <- function(y, dt = 96) {
  # Returns a time series in daily increments, computed from the full (dt-min
  # increments) time series by averaging over each day.
  
  # INPUT
  #   y       : an Nx1 vector of data at N time points
  #   dt      : (optional) number of time points per day (default: 96)
  
  ndays <- length(y) / dt
  
  # Reshape y into a matrix with dt rows and ndays columns
  tmp <- matrix(y, nrow = dt, ncol = ndays, byrow = TRUE)
  
  # Average over each row (i.e., each day)
  avg <- rowMeans(tmp)
  
  return(avg)
}
