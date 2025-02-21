# AUTHORS: Charlie Dougherty, Hilary Dugan
# Troubleshooting provided using ChatGPT

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(progress)
library(suncalc)
library(ggpubr)

### set working directory

setwd("~/Documents/R-Repositories/MCM-LTER")

###################### Load your time series data for air temperature and radiation ######################
airt <- read_csv("data/thermal diffusion model data/mcmlter-clim-boym_airt-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time)) |> 
  filter(date_time > '2016-12-01 00:00:00')


# define solar constant for use in filling in NA values
S = 1376 # W m^-2
coords <- data.frame(lat = -77.709828, lon = 162.441604) # coordinates of rough center for East Lobe Lake Bonney
radn <- read_csv("data/thermal diffusion model data/mcmlter-clim-boym_radn-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time)) |> 
  filter(date_time > '2016-12-01 00:00:00')# |> 
  
#mutate(solar_position = getSunlightPosition(date = date_time, lat = coords$lat, lon = coords$lon), 
  #       Zenith = (90 - solar_position$azimuth)
  #       ) |> 
  #select(-c(solar_position)) |> 
  #mutate(swradin = ifelse(is.na(swradin), S*cos(Zenith),swradin)
 # )

# count remaining NAs as a check  
sum(is.na(radn$swradin))

# quick check for albedo of soil
ggplot(radn, aes(date_time, swradin)) + 
  geom_line()

lw_rad <- read_csv("data/thermal diffusion model data/mcmlter-clim-cohm_radn-15min-20230705.csv") |> 
  mutate(date_time = mdy_hm(date_time)) |>
  filter(date_time > '2016-12-01 00:00:00') |> 
  filter(lwradout2 >= 0) 

# ice thickness
ice_thickness <- read_csv("data/thermal diffusion model data/mcmlter-lake-ice_thickness-20230726.csv") |>
  mutate(date_time = mdy_hm(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(lake == "East Lake Bonney") |> 
  pivot_longer(cols = c(z_water_m, z_ice_m), names_to = "ice measurement", values_to = "thickness") |> 
  filter(date_time > "2015-12-01" & date_time < "2024-01-01")

ice_thickness_forplot <- read_csv("data/thermal diffusion model data/mcmlter-lake-ice_thickness-20230726.csv") |>
  mutate(date_time = mdy_hm(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(lake == "East Lake Bonney") |> 
 # pivot_longer(cols = c(z_water_m, z_ice_m), names_to = "ice measurement", values_to = "thickness") |> 
  filter(date_time > "2015-12-01" & date_time < "2022-01-01")


# sediment abundance values
# albedo using values from M. Obryk Model
#a <- c(0.750, 0.743, 0.6, 0.55, 0.5, 0.5, 0.5, 0.5, 0.5, 0.491, 0.572, 0.746)
#months <- c(1,2,3,4,5,6,7,8,9,10,11,12)
#yearly_data <- data.frame(Month = months, Albedo = a)

#start_year = 2016
#end_year = 2021

#years <- rep(start_year:end_year, each = 12)  # Create a vector of years repeated for each month
#monthly_data <- data.frame(year = years, month = rep(months, times = 6), Albedo = rep(a, times = 6))

#albedod <- monthly_data 
#albedod$date <- as.Date(paste(albedod$year, albedod$month, "01", sep = "-"))
#start_date <- min(albedod$date)
#end_date <- ceiling_date(max(albedod$date), "month") - minutes(15)  # End of the last month

#time_15min <- seq(from = as.POSIXct(start_date), 
#                                   to = as.POSIXct(end_date), 
#                                   by = "15 mins")

#albedo <- data.frame(time = time_15min) |> 
#    mutate(
#      year = year(time),
#      month = month(time)
#    ) |> 
#    left_join(albedod, by = c("year", "month"))

#### ALBEDO CORRECTION ####
# Read and prepare the data
albedo_orig <- read_csv("data/sediment abundance data/sediment_abundances_20250207.csv") |>  
  filter(lake == "eastlobe") |> 
  mutate(date = ymd(date), 
         month = month(date), 
         year = year(date)) |> 
  drop_na(sediment) |> 
  group_by(year, month) |> 
  summarize(albedo_mean = (1 - mean(sediment, na.rm = TRUE)))

# Set the date as the first day of each month
albedo_orig$date <- as.Date(paste(albedo_orig$year, albedo_orig$month, "01", sep = "-"))

# Generate 15-minute intervals across the full date range
start_date <- min(albedo_orig$date)
end_date <- ceiling_date(max(albedo_orig$date), "month") - minutes(15)  # End at last month's end
time_15min <- seq(from = as.POSIXct(start_date), 
                  to = as.POSIXct(end_date), 
                  by = "15 mins")

# Create a new data frame with 15-minute intervals
albedo1 <- data.frame(time = time_15min) |> 
  mutate(year = year(time),
         month = month(time))

# Left join with the monthly data to get the corresponding albedo mean for each month
albedo1 <- left_join(albedo1, albedo_orig, by = c("year", "month"))

# Sort and fill the albedo_mean using the most recent previous value
albedo1 <- albedo1 |> 
  arrange(time) |> 
  fill(albedo_mean, .direction = "down")

#plot to quick check
ggplot(albedo1, aes(time, albedo_mean)) + 
  geom_path()


#Pressure data
pressure <- read_csv("data/thermal diffusion model data/mcmlter-clim-hoem_pressta-15min-20230620.csv") %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  filter(date_time > '2016-12-01 00:00:00')

#Wind Data 
wind <- read_csv("data/thermal diffusion model data/mcmlter-clim-boym_wind-15min-20230605.csv") %>%
  mutate(date_time = mdy_hm(date_time)) %>%
  filter(date_time > '2016-12-01 00:00:00')


###################### Define Parameters ######################
L_initial <- 4.07           # Initial ice thickness (m) Ice thickness at 12/17/2015 ice to ice
# nx <- 30                  # Number of spatial steps
dx <- 0.10                  # Spatial step size (m)
nx = L_initial/dx          # Number of spatial steps
dt <-  1/24                 # Time step for stability (in days)
nt <- (1/dt)*5*365.        # Number of time steps
sigma = 6.67e-8            # stefan boltzman constant
R = 8.314462                # Universal gas constant kg⋅m^2⋅s^-2⋅K^-1⋅mol^-1
Ma = 28.97                 # Molecular Weight of Air kg/mol
Ca = 1.004                # Specific heat capacity of air kJ/kg*K
Ch = 1.75e-3             # bulk transfer coefficient as defined in 1979 Parkinson and Washington
Ce = 1.75e-3             # bulk transfer coefficient as defined in 1979 Parkinson and Washington
e = 0.97                # surface emissivity (for estimating LW if we ever get there)
#S = 1367                # solar constant W m^-2


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

###################### Interpolate Data to match model time steps ######################
#Interpolate air temperature to match the model time steps
airt_interp <- approx(
  x = as.numeric(airt$date_time),  # Convert date_time to numeric for interpolation
  y = airt$airt3m,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Interpolate shortwave radiation to match the model time steps
sw_interp <- approx(
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

#reshape albedo (use this for the GEE dataset)
albedo_interp <- approx(
  x = as.numeric(albedo1$time),                     # Original dates as numeric
  y = albedo1$albedo_mean,                          # Albedo means to interpolate
  xout = as.numeric(time_model),                   # Target times as numeric
  rule = 2                                         # Constant extrapolation for out-of-bound values
)$y

#reshape albedo (use this for static empiral dataset)
#albedo_interp <- approx(
#  x = as.numeric(albedo$time),                     # Original dates as numeric
#  y = albedo$Albedo,                               # Albedo means to interpolate
#  xout = as.numeric(time_model),                   # Target times as numeric
#  rule = 2                                         # Constant extrapolation for out-of-bound values
#)$y

#pressure interpolate
pressure_interp <- approx(
  x = as.numeric(pressure$date_time),  # Convert date_time to numeric for interpolation
  y = pressure$pressta,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

#wind interpolate
wind_interp <- approx(
  x = as.numeric(wind$date_time),  # Convert date_time to numeric for interpolation
  y = wind$wspd,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Check if lengths of interpolated data match the time model
if (length(airt_interp) != length(time_model) | 
    length(sw_interp) != length(time_model) | 
    length(LWR_in_interp) != length(time_model) |
    length(LWR_out_interp) != length(time_model) |
    length(albedo_interp) != length(time_model) |
    length(pressure_interp) != length(time_model) |
    length(wind_interp) != length(time_model)
    ) {
  stop("Length of interpolated data does not match the model time steps!")
}

#pull mean values for each parameter in order to plug in one at a time to troubleshoot
#the downward slope that keeps showing up in model output. 

###################### Create the time series tibble for model time ######################
time_series <- tibble(
  time = time_model,          # Model time steps
  T_air = airt_interp + 273.15,  # Interpolated air temperature in Kelvin
  SW_in = sw_interp,        # Interpolated shortwave radiation
  LWR_in = LWR_in_interp,     # Interpolated incoming longwave radiation
  LWR_out = LWR_out_interp,  # Interpolated outgoing longwave radiation
  albedo = 0.10 + ((albedo_interp)*0.90),
  #albedo = albedo_interp,      # Constant albedo (can be replaced with a time series if needed)
  pressure = pressure_interp,
  wind = wind_interp,
  delta_T = T_air - lag(T_air)
) |> 
  drop_na(delta_T)

# plot all input data together to do a visual check
series <- time_series |> 
  pivot_longer(cols = c(T_air, SW_in, LWR_in, LWR_out, pressure, albedo), names_to = "variable", values_to = "data")

# Plot of input ice data
ggplot(series, aes(time, data)) + 
  geom_line() + 
  xlab("Date") + ylab("Value") +
  facet_wrap(vars(variable), scales = "free") + 
  theme_minimal()

n_iterations <- nt

# Initialize results tibble
results <- tibble(
  time = rep(as.POSIXct(NA), n_iterations),  # Initialize `time` as NA POSIXct
  depth = numeric(n_iterations),             # Initialize `depth` as numeric
  temperature = numeric(n_iterations),       # Initialize `temperature` as numeric
  thickness = numeric(n_iterations),         # Initialize `thickness` as numeric
  Iteration = numeric(n_iterations)          # Initialize `Iteration` as numeric
)

# View the structure of the tibble
str(results)

###################### Initialize temperature profile and ice thickness ######################
L = L_initial
prevL <- L_initial  # Initial ice thickness
depth <- seq(0, L, by = dx)  # Depth grid points
prevT <- seq(from = time_series$T_air[1], to = 273.15, length.out = length(depth))  # Linear initial gradient
dL_bottom.vec = NA # store these values for troubleshooting
dL_surface.vec = NA # store these values for troubleshooting

# lastly, add a progress bar because this stuff takes forever
pb <- progress_bar$new(
  format = "[:bar] :percent :elapsed | ETA: :eta",
  total = nrow(time_series), # Total iterations
  clear = FALSE
)


###################### Simulation loop ######################
for (t_idx in 1:nrow(time_series)) {
  
  # Store results for this time step
  results <- results %>% 
    add_row(
      time = time_series$time[t_idx], 
      depth = depth, 
      temperature = prevT,
      #temperature = ifelse(L > 0, T, NA),
      thickness = prevL
    )
  
  #ice thickness
  newL = prevL # Copy current thickness
  newT <- prevT  # Copy the current temperature profile
  
  # Extract current air temperature, shortwave radiation, longwave radiation, and time step
  T_air <- time_series$T_air[t_idx]
  SW_in <- time_series$SW_in[t_idx]
  LWR_in <- time_series$LWR_in[t_idx]
  LWR_out <- time_series$LWR_out[t_idx]
  albedo <- (time_series$albedo[t_idx])
  press <- (time_series$pressure[t_idx])
  wind <- (time_series$wind[t_idx])
  delta_T <- (time_series$delta_T[t_idx])
  
  # Net longwave radiation
  Q_longwave <- (LWR_in - LWR_out)
  
  # Update temperature profile using the 1D heat diffusion equation
  for (i in 2:length(prevT)) {
    newT[i] <- prevT[i] + alpha * (dt * 86400) / dx^2 * (prevT[i + 1] - 2 * prevT[i] + prevT[i - 1])
    # print(newT)
  }
  
  # Apply boundary conditions
  newT[1] <- T_air  # Surface temperature equals air temperature
  newT[length(prevT)] <- 273.15  # Bottom temperature equals freezing point of water
  
  # Calculate absorbed shortwave radiation (with albedo)
  SW_abs <- SW_in * (1 - albedo)
  
  # Net longwave radiation (incoming - outgoing)
  LW_net <- (LWR_in - LWR_out)
  
  #calculate sensible heat flux
  rho_air = (press*Ma)*0.1 / (R*T_air)
  
  #random number generator for cloud cover
  #don't need this
  #cloud = runif(1, min = 0, max = 1)
  
  #sensible heat flux
  Qh = rho_air*Ca*Ch*(delta_T)*wind
  
  #latent heat flux
  #Don't know how to find delta_Q: relative humidity difference between air and ice surface
  esat = 6.112 * e^((22.46*T_air)/(T_air + 272.62))
  #if (newT[1] > 273.15) {
  #  Ql = rho_air*Rl*Ce*delta_Q*wind
  #}
  
  #if (newT[1] < 273.15) {
  #  Ql = rho_air*Rl*Ce*delta_Q*wind
  #}
  #Ql = rho_air*Rl*Ce*delta_Q*wind
  
  # Surface heat flux (absorbed shortwave, net longwave, conductive heat flux, and sensible heat flux)
  surface_flux <- SW_abs + (LW_net - (k * (prevT[1] - T_air) / dx)) + Qh  # Adjust conductive term
  # Calculate melting at the surface
  if (!is.na(surface_flux) && surface_flux > 0) {
    dL_surface <- surface_flux * (dt * 86400) / (rho * L_f)
    newL <- newL - dL_surface
  }
  
  # Calculate freezing/melting at the bottom
  if (!is.na(newL) && newL > 0) {
    Q_bottom <- -k * (newT[length(newT) - 1] - newT[length(newT)]) / dx
    dL_bottom <- Q_bottom * (dt * 86400) / (rho * L_f)
    newL <- newL + dL_bottom
  }
  
  dL_surface.vec[t_idx] = dL_surface
  dL_bottom.vec[t_idx] = dL_bottom
  
  # Ensure ice thickness remains positive
  newL <- max(0, newL)
  
  # Adjust spatial resolution if thickness changes
  if (newL > 0) {
    # nx <- 30  # Ensure at least 15 layers
    dx <- 0.1  # Recalculate spatial step size
    newdepth <- seq(0, newL, by = dx)  # Update depth values
    newT <- approx(seq(0, prevL, length.out = length(depth)), newT, seq(0, newL, length.out = length(newdepth)), rule = 2)$y  # Interpolate
  } else {
    newT <- rep(0, nx)  # Reset temperature profile if no ice
    depth <- NA  # No depth when no ice
  }
  
  # Update prevT
  prevT <- newT
  prevL = newL
  depth = newdepth
  
  pb$tick()
}

#troubleshooting plots, to find distance of change at top and bottom
plot(dL_bottom.vec)
plot(dL_surface.vec)


###################### plotting of results ######################
results %>%
  # slice(1:60000) |>
  group_by(time) %>%
  summarize(thickness = max(thickness)) %>%
  ggplot(aes(x = time, y = thickness)) +
  geom_line(color = "red", size = 1) +
  labs(x = "Time (days)", y = "Ice Thickness (m)",
       title = "East Lake Bonney Modeled Ice Thickness over Time", 
       subtitle = "raw output GEE data") +
  geom_point(data = ice_thickness_forplot, aes(x = date_time, y = z_water_m)) + 
  theme_bw()

ggplot(time_series, aes(time, T_air)) + 
  geom_point()

# plot time series data to see if there's anything that explains this odd dip in the modeled data
ggplot(series, aes(time, data)) + 
  geom_line() + 
  facet_wrap(vars(variable), scales = "free")

# Plot temperature profiles at selected time steps
results %>%
   ggplot(aes(x = temperature, y = depth, color = factor(time), group = time)) +
   geom_line() +
   scale_y_reverse() +  # Reverse depth for proper orientation
   labs(
     x = "Temperature (°C)",
     y = "Depth in Ice (m)",
     color = "Time (days)")
 
 


### Compare two outputs
plot2 <- GEE_results |> 
 group_by(time) |> 
   summarize(thickness = max(thickness)) |> 
   ggplot(aes(x = time, y = thickness)) +
   geom_line(color = "red", size = 1) +
   labs(x = "Time (days)", y = "Ice Thickness (m)",
        title = "                      ", 
        subtitle = "GEE Albedo Product") +
   geom_point(data = ice_thickness_forplot, aes(x = date_time, y = z_water_m)) + 
   theme_minimal() + 
   scale_y_continuous(limits = c(1.5,5))
 

plot1 <- static_results |> 
  group_by(time) |> 
  summarize(thickness = max(thickness)) |> 
  ggplot(aes(x = time, y = thickness)) +
  geom_line(color = "blue", size = 1) +
  labs(x = "Time (days)", y = "Ice Thickness (m)",
       title = "East Lake Bonney Modeled Ice Thickness over Time", 
       subtitle = "Static Albedo Input") +
  geom_point(data = ice_thickness_forplot, aes(x = date_time, y = z_water_m)) + 
  theme_minimal() + 
  scale_y_continuous(limits = c(1.5,5))

ggarrange(plot1, plot2)




