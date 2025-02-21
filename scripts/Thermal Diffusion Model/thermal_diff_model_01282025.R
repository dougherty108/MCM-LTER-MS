### ice thickness model that doesn't work ####

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load your time series data for air temperature and radiation
airt <- read_csv("data/thermal diffusion model data/mcmlter-clim-boym_airt-15min-20230605.csv") %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  filter(date_time > '2016-12-31 23:59:59')

radn <- read_csv("data/thermal diffusion model data/mcmlter-clim-boym_radn-15min-20230605.csv") %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  filter(date_time > '2016-12-31 23:59:59')

lw_rad <- read_csv("data/thermal diffusion model data/mcmlter-clim-boym_radn-15min-20230605.csv") %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  filter(date_time > '2016-12-31 23:59:59')


# Parameters
L_initial <- 3.5           # Initial ice thickness (m)
nx <- 30                   # Number of spatial steps
dx <- L_initial / nx       # Spatial step size (m)
dt <-  1/24                  # Time step for stability (in days)
nt <- (1/dt)*1*365                 # Number of time steps

k <- 2.2                   # Thermal conductivity of ice (W/m/K)
rho <- 917                 # Density of ice (kg/m^3)
c <- 2100                  # Specific heat capacity of ice (J/kg/K)
alpha <- k / (rho * c)     # Thermal diffusivity (m^2/s)
L_f <- 3.65e5             # Latent heat of fusion for ice (J/kg)

# Stability check: Ensure R < 0.5 for stability
r <- alpha * (dt * 86400) / dx^2  # dt is in days, so multiply by 86400 to convert to seconds
if (r > 0.5) stop("r > 0.5, solution may be unstable. Reduce dt or dx.")

# Define the start time based on the input data
start_time <- min(airt$date_time)

# Generate model time steps (POSIXct format)
time_model <- start_time + seq(0, by = dt * 86400, length.out = nt)  # Convert dt from days to seconds

# Interpolate air temperature to match the model time steps
airt_interp <- approx(
  x = as.numeric(airt$date_time),  # Convert date_time to numeric for interpolation
  y = airt$airt3m,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Interpolate shortwave radiation to match the model time steps
radn_interp <- approx(
  x = as.numeric(radn$date_time),  # Convert date_time to numeric for interpolation
  y = radn$swradin,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Interpolate longwave radiation to match the model time steps
LWR_in_interp <- approx(
  x = as.numeric(lw_rad$date_time),  # Convert date_time to numeric for interpolation
  y = lw_rad$lwradin2,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

LWR_out_interp <- approx(
  x = as.numeric(lw_rad$date_time),  # Convert date_time to numeric for interpolation
  y = lw_rad$lwradout2,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Check if lengths of interpolated data match the time model
if (length(airt_interp) != length(time_model) | 
    length(radn_interp) != length(time_model) | 
    length(LWR_in_interp) != length(time_model) |
    length(LWR_out_interp) != length(time_model)) {
  stop("Length of interpolated data does not match the model time steps!")
}

# Create the time series tibble for model time
time_series <- tibble(
  time = time_model,          # Model time steps
  T_air = airt_interp + 273.15,  # Interpolated air temperature in Kelvin
  SW_in = radn_interp,        # Interpolated shortwave radiation
  LWR_in = LWR_in_interp,     # Interpolated incoming longwave radiation
  LWR_out = LWR_out_interp,   # Interpolated outgoing longwave radiation
  albedo = rep(0.8, nt)       # Constant albedo (can be replaced with a time series if needed)
)

# Initialize results tibble
results <- tibble(
  time = as.POSIXct(character()),  # Initialize `time` as POSIXct
  depth = numeric(),               # Initialize `depth` as numeric
  temperature = numeric(),         # Initialize `temperature` as numeric
  thickness = numeric()            # Initialize `thickness` as numeric
)

# Initialize temperature profile and ice thickness
L <- L_initial  # Ice thickness
Temp <- seq(from = time_series$T_air[1], to = 273.15, length.out = nx)  # Linear initial gradient
depth <- seq(0, L, length.out = nx)  # Depth grid points

# Simulation loop
for (t_idx in 1:nrow(time_series)) {
  # Extract current air temperature, shortwave radiation, longwave radiation, and time step
  T_air <- time_series$T_air[t_idx]
  SW_in <- time_series$SW_in[t_idx]
  LWR_in <- time_series$LWR_in[t_idx]
  LWR_out <- time_series$LWR_out[t_idx]
  albedo <- time_series$albedo[t_idx]
  
  # Net longwave radiation
  Q_longwave <- LWR_in - LWR_out
  
  # Update temperature profile using the 1D heat diffusion equation
  T_new <- Temp  # Copy the current temperature profile
  for (i in 2:(nx - 1)) {
    T_new[i] <- Temp[i] + alpha * (dt * 86400) / dx^2 * (Temp[i + 1] - 2 * Temp[i] + Temp[i - 1])
    print(T_new)
  }
  
  
  # Apply boundary conditions
  T_new[1] <- T_air  # Surface temperature equals air temperature
  T_new[nx] <- 273.15  # Bottom temperature equals freezing point of water
  
  # Update T
  Temp <- T_new
  
  # Calculate absorbed shortwave radiation (with albedo)
  SW_abs <- SW_in * (1 - albedo)
  
  # Net longwave radiation (incoming - outgoing)
  LW_net <- LWR_in - LWR_out
  
  # Surface heat flux (absorbed shortwave, net longwave, and conductive heat flux)
  surface_flux <- SW_abs + LW_net - (k * (T[1] - T_air) / dx)  # Adjust conductive term
  # Calculate melting at the surface
  if (!is.na(surface_flux) && surface_flux > 0) {
    dL_surface <- surface_flux * (dt * 86400) / (rho * L_f)
    L <- L - dL_surface
  }
  
  # Calculate freezing/melting at the bottom
  if (!is.na(L) && L > 0) {
    Q_bottom <- -k * (T[nx - 1] - T[nx]) / dx
    dL_bottom <- -Q_bottom * (dt * 86400) / (rho * L_f)
    L <- L + dL_bottom
  }
  
  # Ensure ice thickness remains positive
  L <- max(0, L)
  
  # Adjust spatial resolution if thickness changes
  if (L > 0) {
    nx <- max(15, round(L / dx))  # Ensure at least 15 layers
    dx <- L / nx  # Recalculate spatial step size
    Temp <- approx(seq(0, L_initial, length.out = nx), T, seq(0, L, length.out = nx), rule = 2)$y  # Interpolate
    depth <- seq(0, L, length.out = nx)  # Update depth values
  } else {
    Temp <- rep(0, nx)  # Reset temperature profile if no ice
    depth <- NA  # No depth when no ice
  }
  
  # Store results for this time step
  results <- results %>% 
    add_row(
      time = time_series$time[t_idx], 
      depth = ifelse(L > 0, depth, NA), 
      temperature = Temp,
      #temperature = ifelse(L > 0, T, NA),
      thickness = L
    )
}

# Plot ice thickness over time
results %>%
  group_by(time) %>%
  summarize(thickness = max(thickness)) %>%
  ggplot(aes(x = time, y = thickness)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Time (days)", y = "Ice Thickness (m)",
       title = "Dynamic Ice Thickness Over Time") +
  theme_minimal()

# Plot temperature profiles at selected time steps
results %>%
  ggplot(aes(x = temperature, y = depth, color = factor(time), group = time)) +
  geom_line() +
  scale_y_reverse() +  # Reverse depth for proper orientation
  labs(
    x = "Temperature (°C)",
    y = "Depth in Ice (m)",
    color = "Time (days)")








