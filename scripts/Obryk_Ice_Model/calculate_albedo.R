calculate_albedo <- function(years, steps_per_day) {
  # Generates daily ALBEDO data based on a spline function that follows
  # (eyeballs) empirical data
  
  # This needs to be generated at the beginning of the model before the main loop
  # and read at each day interval
  
  x <- 1:(12 * years)
  y <- c(0.76, 0.6, 0.6, 0.55, 0.5, 0.5, 0.5, 0.5, 0.54, 0.5, 0.5, 0.62)
  y <- rep(y, years)  # Repeat y for the number of years
  
  # Create a spline model
  cs <- splinefun(x, y)
  
  # Generate points for evaluation
  xx <- seq(1, max(x), length.out = 365 * years * steps_per_day)
  
  # Evaluate the spline at the generated points
  albedo <- cs(xx)
  
  # Uncomment to plot
  # plot(x, y, pch = 19, col = "blue", xlab = "Months", ylab = "Albedo")
  # lines(xx, cs(xx), col = "red")
  
  return(albedo)
}
