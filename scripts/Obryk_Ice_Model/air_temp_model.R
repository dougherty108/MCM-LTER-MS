air_temp_model <- function(day) {
  # Simulates air temperature (in K) based on a fitted equation to
  # daily average temperature based on 19 years of data (obtained from
  # MCM-LTER (LB met station). Air temperature was modeled using
  # Fourier with 8 terms.
  
  # Declares Coefficients (with 95% confidence bounds):
  a0 <- 255.5  # (254.3, 256.7)
  a1 <- 13.44  # (12.14, 14.73)
  b1 <- 0.1542 # (-2.903, 3.212)
  a2 <- 4.855  # (3.641, 6.068)
  b2 <- -1.193 # (-3.155, 0.7697)
  a3 <- -0.2864 # (-0.6428, 0.06995)
  b3 <- 1.069  # (0.6421, 1.497)
  a4 <- -0.1925 # (-0.6821, 0.297)
  b4 <- 0.1334 # (-0.1034, 0.3702)
  a5 <- -0.6822 # (-1.066, -0.298)
  b5 <- -0.1479 # (-0.8366, 0.5409)
  a6 <- -0.1583 # (-0.4251, 0.1084)
  b6 <- -0.1098 # (-0.5099, 0.2902)
  a7 <- 0.2494  # (-0.7456, 1.244)
  b7 <- 0.5159  # (-0.1571, 1.189)
  a8 <- 0.1304  # (-0.8528, 1.114)
  b8 <- -0.4879 # (-1.019, 0.04342)
  w  <- 0.01727 # (0.01605, 0.01849)
  
  x <- day
  
  # Calculate modeled air temperature
  t_air_modeled <- a0 +
    a1 * cos(x * w) + b1 * sin(x * w) +
    a2 * cos(2 * x * w) + b2 * sin(2 * x * w) +
    a3 * cos(3 * x * w) + b3 * sin(3 * x * w) +
    a4 * cos(4 * x * w) + b4 * sin(4 * x * w) +
    a5 * cos(5 * x * w) + b5 * sin(5 * x * w) +
    a6 * cos(6 * x * w) + b6 * sin(6 * x * w) +
    a7 * cos(7 * x * w) + b7 * sin(7 * x * w) +
    a8 * cos(8 * x * w) + b8 * sin(8 * x * w)
  
  return(t_air_modeled)
}
