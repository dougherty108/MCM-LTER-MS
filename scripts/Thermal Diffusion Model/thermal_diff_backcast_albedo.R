###### attempt to backcast Albedo

###### thermal diffusion model ########

### Authors
# Charlie Dougherty

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(progress)
library(suncalc)

#set working directory
setwd("~charliedougherty")

###################### Load Time Series Data by Station######################
BOYM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-21 00:00:00')

HOEM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-21 00:00:00')

COHM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_cohm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-21 00:00:00')

TARM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_tarm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-21 00:00:00')

###################### Define Parameters ######################
L_initial <- 3.88       # Initial ice thickness (m) Ice thickness at 12/17/2015 ice to ice
dx <- 0.10              # Spatial step size (m)
nx = L_initial/dx       # Number of spatial steps
dt <-  1/24             # Time step for stability (in days)
nt <- (1/dt)*7.0*365.   # Number of time steps


sigma = 5.67e-8         # stefan boltzman constant
R = 8.314462            # Universal gas constant kg⋅m^2⋅s^-2⋅K^-1⋅mol^-1
Ma = 28.97              # Molecular Weight of Air kg/mol
Ca = 1.004              # Specific heat capacity of air J/g*K
Ch = 1.75e-3            # bulk transfer coefficient as defined in 1979 Parkinson and Washington
Ce = 1.75e-3            # bulk transfer coefficient as defined in 1979 Parkinson and Washington
epsilon = 0.97          # surface emissivity (for estimating LW if we ever get there)
S = 1367                # solar constant W m^-2
Tf = 273.16             # Temperature of water freezing (K)
xLv = 2.500e6           # Latent Heat of Evaporation (J/kg)
xLf = 3.34e5            # Latent Heat of Fusion (J/kg)
xLs = xLv + xLf         # Latent Heat of Sublimation


k <- 2.3                # Thermal conductivity of ice (W/m/K)
rho <- 917              # Density of ice (kg/m^3)
c <- 2100               # Specific heat capacity of ice (J/kg/K)
alpha <- k / (rho * c)  # Thermal diffusivity (m^2/s)
L_f <- 3.65e5           # Latent heat of fusion for ice (J/kg)

# Stability check: Ensure R < 0.5 for stability
r <- alpha * (dt * 86400) / dx^2  # dt is in days, so multiply by 86400 to convert to seconds
if (r > 0.5) stop("r > 0.5, solution may be unstable. Reduce dt or dx.")

############## Separate data out into input parameters #############
#preemptively set working directory back 
setwd("~/Documents/R-Repositories/MCM-LTER-MS")

# select air temperature data from Lake Bonney Met
orig_air_temperature <- BOYM |> 
  mutate(airtemp_3m_degc = ifelse(is.na(airtemp_3m_degc), HOEM$airtemp_3m_degc, airtemp_3m_degc)) |> 
  mutate(airtemp_3m_K = airtemp_3m_degc + 273.15) |> 
  select(metlocid, date_time, airtemp_3m_K) 


# Define the start time based on the input data
start_time <- min(orig_air_temperature$date_time)

# Generate model time steps (POSIXct format)
time_model <- start_time + seq(0, by = dt * 86400, length.out = nt)  # Convert dt from days to seconds

## alternative option for air temperature, air temperature at the blue box
air_temperature <- read_csv("data/thermal diffusion model data/ice surface temp/air_temp_ELBBB.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         airtemp_3m_K = surface_temp_C + 273.15)


# Define the full sequence of timestamps at 15-minute intervals
full_timestamps <- data.frame(date_time = seq(from = min(air_temperature$date_time), 
                                              to = max(air_temperature$date_time), 
                                              by = "15 min"))

# Merge with original data and fill missing values with NA
air_temp_gaps <- full_timestamps |> 
  left_join(air_temperature, by = "date_time")

air_temperature <- air_temp_gaps |> 
  mutate(airtemp_3m_K = ifelse(is.na(airtemp_3m_K), orig_air_temperature$airtemp_3m_K, airtemp_3m_K))


# select incoming shortwave radiation data from Lake Bonney Met and fill gaps
# this shortwave object has gaps in the data. Fill the gaps with computed values
shortwave_radiation_initial <- BOYM |> 
  select(metlocid, date_time, swradin_wm2) |> 
  mutate(swradin_wm2 = ifelse(is.na(swradin_wm2), TARM$swradin_wm2, swradin_wm2)) # replace empty shortwave data with TARM, nearest met station

# create an artificial shortwave object
# Coordinates of East Lobe Bonney Blue Box
lat <- -77.13449
lon <- 162.449716

artificial_shortwave <- tibble(
  date_time = time_model, 
  zenith = 90 - getSunlightPosition(time_model, lat, lon)$altitude, #convert to zenith by subtracting the altitude from 90 degrees. 
  sw = S*cos(zenith)*3.0)

shortwave_radiation <- shortwave_radiation_initial |> 
  left_join(artificial_shortwave, by = "date_time") |>    # Join on date_time
  mutate(swradin_wm2 = ifelse(is.na(swradin_wm2), sw, swradin_wm2)) |>   # Fill missing values
  select(-sw)  |> # Remove extra column
  filter(swradin_wm2 > 0)


############### OUTGOING (UPWELLING) LONGWAVE RADIATION
# select outgoing longwave radiation data from Commonwealth Glacier Met 
outgoing_longwave_radiation_initial <- COHM |> 
  select(metlocid, date_time, lwradout2_wm2)


# tried the ice surface temperature product, but the fit was way worse
artificial_longwave_out <- BOYM |> 
  mutate(airtemp_1m_degc = ifelse(is.na(airtemp_1m_degc), HOEM$airtemp_1m_degc, airtemp_1m_degc)) |> 
  mutate(surftemp_K = (airtemp_1m_degc + 273.15)) |> 
  select(date_time, surftemp_K) |> 
  #mutate(surftemp_K = if_else(date_time > "2023-01-05 01:45:00", surftemp_K * 1.03, surftemp_K)) |> 
  mutate(lwout = (epsilon*sigma*(surftemp_K^4)))

artificial_longwave_out <- BOYM |> 
  mutate(airtemp_1m_degc = ifelse(is.na(airtemp_1m_degc), HOEM$airtemp_1m_degc, airtemp_1m_degc)) |> # fill holes in 1m temp with HOEM data
  mutate(surftemp_K = (airtemp_1m_degc + 273.15)) |> 
  select(date_time, surftemp_K) |> 
  #mutate(surftemp_K = if_else(date_time > "2023-01-05 01:45:00", surftemp_K, surftemp_K), ) |> 
  mutate(lwout = (epsilon*sigma*(surftemp_K^4)))

#join datasets together to fill holes
outgoing_longwave_radiation <- outgoing_longwave_radiation_initial |> 
  left_join(artificial_longwave_out, by = "date_time") |>    # Join on date_time
  mutate(lwradout2_wm2 = ifelse(is.na(lwradout2_wm2), lwout, lwradout2_wm2)) |>   # Fill missing values
  select(-lwout)  

############# ################### INCOMING (DOWNWELLING) LONGWAVE RADIATION 
# select incoming longwave radiation data from Commonwealth Glacier Met
incoming_longwave_radiation_initial <- BOYM |> 
  select(metlocid, date_time, lwradin2_wm2)

# Determine the last timestamp
last_timestamp <- max(incoming_longwave_radiation_initial$date_time)

# Generate new timestamps up to 2025 at the same 15-minute interval
new_timestamps <- seq.POSIXt(from = last_timestamp + 15*60, 
                             to = as.POSIXct("2025-01-31 23:45:00"), 
                             by = "15 min")

# Create an empty dataframe with new timestamps and NA for other columns
new_df <- data.frame(date_time = new_timestamps)

# Bind the old and new dataframes
incoming_longwave_radiation_initial <- bind_rows(incoming_longwave_radiation_initial, new_df)

#using air temperature from Bonney Met (plug in gap filled Air Temperature)
artificial_lw_in <- air_temperature

# Generate daily timestamps from the min to max timestamp in your dataframe
daily_timestamps <- seq.Date(from = as.Date(min(artificial_lw_in$date_time)),
                             to = as.Date(max(artificial_lw_in$date_time)),
                             by = "day")

# Generate random cloud cover values (one per day)
daily_cloud_cover <- runif(length(daily_timestamps), min = 0.00, max = 0.75)

# Create a cloud cover dataframe
cloud_cover_df <- data.frame(date = daily_timestamps, cloud_cover = daily_cloud_cover)

# Add a date column to df_extended for joining
artificial_longwave_in <- artificial_lw_in |> 
  mutate(date = as.Date(date_time)) |> 
  left_join(cloud_cover_df, by = "date") |> 
  select(-date) |> # Remove the helper date column
  mutate(lwin = (0.765 + 0.22*cloud_cover^3)*sigma*(airtemp_3m_K)^4)

#join to fill gaps
incoming_longwave_radiation <- incoming_longwave_radiation_initial |> 
  left_join(artificial_longwave_in, by = "date_time") |>    # Join on date_time
  mutate(lwradin2_wm2 = ifelse(is.na(lwradin2_wm2), lwin, lwradin2_wm2)) |>   # Fill missing values
  select(-lwin)   # Remove extra column 

# select air pressure data from Lake Hoare Met
air_pressure = HOEM |> 
  mutate(bpress_Pa = bpress_mb*100) |>  # air pressure was initially in mbar, needs to be in Pascal. 
  select(metlocid, date_time, bpress_Pa)

# select wind speed data from Lake Bonney Met
wind_speed = BOYM |> 
  select(metlocid, date_time, wspd_ms) |>  # wind speed is in meters per second
  mutate(wspd_ms = ifelse(is.na(wspd_ms), TARM$wspd_ms, wspd_ms)) # fill in lost wind values from TARM, next nearest met station

# load ice thickness data and manipulate for easier plotting
ice_thickness <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |>
  mutate(date_time = mdy_hm(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(lake == "East Lake Bonney") |> 
  filter(date_time > "2016-12-01" & date_time < "2025-02-01")

############ ALBEDO CORRECTION ###########
# Read and prepare the data
albedo_orig <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250301.csv") |>  
  mutate(sediment = sediment_abundance) |> 
  filter(lake == "East Lake Bonney") |> 
  mutate(date = ymd(date), 
         month = month(date), 
         year = year(date)) |> 
  drop_na(sediment) #|> 
#group_by(year, month) |> 
#summarize(albedo_mean = (mean(sediment, na.rm = TRUE)))

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
  fill(ice_abundance, .direction = "down")


# load relative humidity data
relative_humidity <- BOYM |> 
  select(metlocid, date_time, rhh2o_3m_pct, rhice_3m_pct) |> 
  mutate(rhh2o_3m_pct = ifelse(is.na(rhh2o_3m_pct), TARM$rhh2o_3m_pct, rhh2o_3m_pct))

###################### Interpolate Data to match model time steps ######################
#time_model = start_time + seq(0, by = dt* 86400, length.out = nt)  # Convert dt from days to seconds

#Interpolate air temperature to match the model time steps
airt_interp <- approx(
  x = as.numeric(air_temperature$date_time),  # Convert date_time to numeric for interpolation
  y = air_temperature$airtemp_3m_K,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Interpolate shortwave radiation to match the model time steps
sw_interp <- approx(
  x = as.numeric(shortwave_radiation$date_time),  # Convert date_time to numeric for interpolation
  y = shortwave_radiation$swradin_wm2,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# Interpolate longwave radiation to match the model time steps
LWR_in_interp <- approx(
  x = as.numeric(incoming_longwave_radiation$date_time),  # Convert date_time to numeric for interpolation
  y = incoming_longwave_radiation$lwradin2_wm2,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

# longwave outgoign interpolate
LWR_out_interp <- approx(
  x = as.numeric(outgoing_longwave_radiation$date_time),  # Convert date_time to numeric for interpolation
  y = outgoing_longwave_radiation$lwradout2_wm2,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

#reshape albedo (use this for the GEE dataset)
albedo_interp <- approx(
  x = as.numeric(albedo1$time),                     # Original dates as numeric
  y = albedo1$ice_abundance,                          # Albedo means to interpolate
  xout = as.numeric(time_model),                   # Target times as numeric
  rule = 2                                         # Constant extrapolation for out-of-bound values
)$y

#pressure interpolate
pressure_interp <- approx(
  x = as.numeric(air_pressure$date_time),  # Convert date_time to numeric for interpolation
  y = air_pressure$bpress_Pa,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

#wind interpolate
wind_interp <- approx(
  x = as.numeric(wind_speed$date_time),  # Convert date_time to numeric for interpolation
  y = wind_speed$wspd_ms,
  xout = as.numeric(time_model),   # Interpolate at model time steps
  rule = 2                         # Use constant extrapolation for out-of-bound values
)$y

#relative humidity interpolate
relative_humidity_interp <- approx(
  x = as.numeric(relative_humidity$date_time),
  y = relative_humidity$rhh2o_3m_pct, 
  xout = as.numeric(time_model),
  rule = 2
)$y

# Check if lengths of interpolated data match the time model
if (length(airt_interp) != length(time_model) | 
    length(sw_interp) != length(time_model) | 
    length(LWR_in_interp) != length(time_model) |
    length(LWR_out_interp) != length(time_model) |
    length(albedo_interp) != length(time_model) |
    length(pressure_interp) != length(time_model) |
    length(wind_interp) != length(time_model) |
    length(relative_humidity_interp) != length(time_model)
) {
  stop("Length of interpolated data does not match the model time steps!")
}

###################### Create the time series tibble for model time ######################
time_series <- tibble(
  time = time_model,                        # Model time steps
  T_air = airt_interp,                      # Interpolated air temperature Kelvin
  SW_in = sw_interp,                        # Interpolated shortwave radiation w/m2
  LWR_in = LWR_in_interp,                   # Interpolated incoming longwave radiation w/m2
  LWR_out = LWR_out_interp,                 # Interpolated outgoing longwave radiation w/m2
  albedo = 0.1402 + ((albedo_interp)*0.7200),  # albedo, unitless (lower albedo value from measured BOYM data)
  #albedo = alb_altered,                    # Constant albedo (can be replaced with a time series if needed)
  #albedo = albedo_interp,
  pressure = pressure_interp,               # Interpolated air pressure, Pa
  wind = wind_interp,                       # interpolated wind speed, m/s
  delta_T = T_air - lag(T_air),             # difference in air temperature, for later flux calculation
  relative_humidity = relative_humidity_interp # relative humidity
) |> 
  drop_na(delta_T) # removes the first row where the difference in temperatures yields NA

# plot all input data together to do a visual check
series <- time_series |> 
  pivot_longer(cols = c(T_air, SW_in, LWR_in, LWR_out, pressure, albedo, relative_humidity, wind, delta_T), 
               names_to = "variable", values_to = "data")


############### MODEL BEGINS ###########

n_iterations <- nt

# Initialize results tibble
results <- tibble(
  time = rep(as.POSIXct(NA), n_iterations),  # Initialize `time` as NA POSIXct
  depth = numeric(n_iterations),             # Initialize `depth` as numeric
  temperature = numeric(n_iterations),       # Initialize `temperature` as numeric
  thickness = numeric(n_iterations),         # Initialize `thickness` as numeric
  Iteration = numeric(n_iterations)          # Initialize `Iteration` as numeric
)

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



# Function to simulate ice thickness given an albedo value
simulate_thickness <- function(albedo_guess, other_inputs) {
  # Extract relevant environmental inputs
  T_air <- other_inputs$T_air
  SW_in <- other_inputs$SW_in
  LWR_in <- other_inputs$LWR_in
  LWR_out <- other_inputs$LWR_out
  press <- other_inputs$pressure
  wind <- other_inputs$wind
  delta_T <- other_inputs$delta_T
  rh <- other_inputs$relative_humidity
  prevL <- other_inputs$prevL  # Previous ice thickness
  prevT <- other_inputs$prevT  # Previous temperature profile
  
  # Compute absorbed shortwave radiation
  SW_abs <- SW_in * (1 - albedo_guess)
  
  # Compute net longwave radiation
  LW_net <- (LWR_in - LWR_out)
  
  # Compute sensible and latent heat fluxes
  rho_air <- (press * Ma) * 0.1 / (R * T_air)
  Qh <- rho_air * Ca * Ch * delta_T * wind
  
  if (prevT[1] >= Tf) {
    A <- 6.1121
    B <- 17.502
    C <- 240.97
    xLatent <- xLv
  } else {
    A <- 6.1115
    B <- 22.452
    C <- 272.55
    xLatent <- xLs
  }
  
  ea <- ((rh/100) * A * exp((B * (T_air - Tf)) / (C + (T_air - Tf)))) / 100
  es0 <- (A * exp((B * (Tf - Tf)) / (C + (Tf - Tf)))) / 100
  Ql <- rho_air * xLatent * Ce * (0.622 / press) * (ea - es0) * wind
  
  # Compute surface flux
  surface_flux <- SW_abs + (LW_net - (k * (prevT[1] - T_air) / dx)) + Qh + Ql
  
  # Compute new ice thickness
  newL <- prevL
  if (!is.na(surface_flux) && surface_flux > 0) {
    dL_surface <- surface_flux * (dt * 86400) / (rho * L_f)
    newL <- newL - dL_surface
  }
  
  return(max(0, newL))  # Ensure non-negative ice thickness
}

# Objective function to minimize (error between modeled and observed thickness)
error_function <- function(albedo_guess, observed_thickness, other_inputs) {
  simulated_thickness <- simulate_thickness(albedo_guess, other_inputs)
  return(abs(simulated_thickness - observed_thickness))  # Minimize absolute error
}

# Function to find optimal albedo for a given ice thickness observation
backcast_albedo <- function(observed_thickness, other_inputs) {
  result <- optim(
    par = 0.5,  # Initial albedo guess
    fn = error_function,
    observed_thickness = observed_thickness,
    other_inputs = other_inputs,
    method = "Brent",
    lower = 0.1,  # Albedo typically ranges from 0.1 to 0.9
    upper = 0.9
  )
  return(result$par)  # Return estimated albedo
}

# Example usage:
observed_thickness <- 0.5  # Example observed ice thickness
other_inputs <- list(
  T_air = -10,
  SW_in = 200,
  LWR_in = 300,
  LWR_out = 250,
  pressure = 1013,
  wind = 3,
  delta_T = 5,
  relative_humidity = 80,
  prevL = 0.6,
  prevT = rep(273.15, 10)  # Example temperature profile
)

estimated_albedo <- backcast_albedo(observed_thickness, other_inputs)
print(paste("Estimated albedo:", estimated_albedo))
