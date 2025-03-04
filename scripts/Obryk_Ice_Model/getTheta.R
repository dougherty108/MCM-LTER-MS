getTheta <- function(T, Tm, T0, flag = NULL) {
  # GETTHETA converts between real Temperature (in K) and "model temp" Theta
  # Normally, produces Theta from T
  # When 'invert' is passed, computes T from Theta
  
  Tscale <- T0 - Tm
  
  if (missing(flag)) {
    Theta <- (T - rep(Tm, length(T))) / Tscale
  } else if (identical(flag, "invert")) {
    Theta <- Tm + T * Tscale
  } else {
    stop("Wrong arguments")
  }
  
  return(list(Theta = Theta, Tscale = Tscale))
}
