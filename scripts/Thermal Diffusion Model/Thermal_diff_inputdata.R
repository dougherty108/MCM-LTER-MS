###### thermal diffusion model ########

### Authors
# Charlie Dougherty

# goal of this script is to create clean, gap filled data to be passed through the thermal diffusion model
# the data should be cleaned and without gaps for the period covering 2016-2025. This is the time 
# span where there is landsat images for the albedo measurements
# Also, there is finally met data through 2024 on the website, so need to update the workflow with 
# the new files, since the structure of these files also changes

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(progress)
library(suncalc)
library(ggpubr)

setwd("~/Documents/R-Repositories/MCM-LTER")

###################### Load Time Series Data by Station######################
BOYM <- read_csv("data/thermal diffusion model data/BOYM/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-01 00:00:00')
  
HOEM <- read_csv("data/thermal diffusion model data/HOEM/mcmlter-clim_hoem_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-01 00:00:00')

COHM <- read_csv("data/thermal diffusion model data/COHM/mcmlter-clim_cohm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-01 00:00:00')

TARM <- read_csv("data/thermal diffusion model data/TARM/mcmlter-clim_tarm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-12-01 00:00:00')

############## Create the time model for use in filling gaps in below data ############
# define length that model should run
dx <- 0.10                  # Spatial step size (m)
nx = L_initial/dx          # Number of spatial steps
dt <-  1/24                 # Time step for stability (in days)

# define other constants for filling in data
S = 1376                  # w/m2
epsilon = 0.97            # unitless
sigma = 5.67e-8            # stefan boltzman constant

# number of time steps. If the starting point is in 2016, we want it to run through 2025
nt <- (1/dt)*8*365.        # Number of time steps

############## Separate data out into input parameters #############

# select air temperature data from Lake Bonney Met
air_temperature <- BOYM |> 
  mutate(airtemp_3m_K = airtemp_3m_degc + 273.15) |> 
  select(metlocid, date_time, airtemp_3m_K)

# Define the start time based on the input data
start_time <- min(air_temperature$date_time)

# Generate model time steps (POSIXct format)
time_model_for_gaps <- start_time + seq(0, by = dt* 86400, length.out = nt)  # Convert dt from days to seconds

# select incoming shortwave radiation data from Lake Bonney Met and fill gaps

# this shortwave object has gaps in the data. Fill the gaps with computed values
shortwave_radiation_initial <- BOYM |> 
  select(metlocid, date_time, swradin_wm2)

# create an artificial shortwave object
# Coordinates of East Lobe Bonney Blue Box
lat <- -77.13449
lon <- 162.449716

artificial_shortwave <- tibble(
  date_time = time_model_for_gaps, 
  zenith = 90 - getSunlightPosition(time_model_for_gaps, lat, lon)$altitude, #convert to zenith by subtracting the altitude from 90 degrees. 
  sw = S*cos(zenith)*3.0)

shortwave_radiation <- shortwave_radiation_initial %>%
  left_join(artificial_shortwave, by = "date_time") %>%   # Join on date_time
  mutate(swradin_wm2 = ifelse(is.na(swradin_wm2), sw, swradin_wm2)) %>%  # Fill missing values
  select(-sw)  |> # Remove extra column
  filter(swradin_wm2 > 0)


# select incoming longwave radiation data from Commonwealth Glacier Met 
incoming_longwave_radiation_initial <- COHM |> 
  select(metlocid, date_time, lwradin2_wm2)

#plug Taylor Glacier ice surface temperature in here as placeholder
artificial_longwave_in <- TARM |> 
  mutate(surftemp_K = surftemp_degc + 273.15) |> 
  select(date_time, surftemp_K) |> 
  mutate(lw = epsilon*sigma*(surftemp_K^4))

# plot ice surface temperature
ggplot(artificial_longwave_in, aes(date_time, surftemp_K)) +
  geom_line()

incoming_longwave_radiation <- incoming_longwave_radiation_initial |> 
  left_join(artificial_longwave_in, by = "date_time") |>    # Join on date_time
  mutate(lwradin2_wm2 = ifelse(is.na(lwradin2_wm2), lw, lwradin2_wm2)) |>   # Fill missing values
  select(-lw)  |> # Remove extra column
  filter(lwradin2_wm2 > 0)

#seems to be a pretty poor fit for longwave in. 
ggplot(incoming_longwave_radiation, aes(date_time, lwradin2_wm2)) + 
  geom_path()

# select outgoing longwave radiation data from Commonwealth Glacier Met
outgoing_longwave_radiation_initial <- COHM |> 
  select(metlocid, date_time, lwradout2_wm2) |> 
  filter(lwradout2_wm2 >=0)

artificial_longwave_out <- BOYM |> 
  select(date_time, airtemp_3m_degc) |> 
  mutate(airtemp_3m_K = airtemp_3m_degc + 273.15, 
         cloud_cover = runif(1, min = 0, max = 1),
         lwout = (epsilon*sigma*cloud_cover*(airtemp_3m_K^4)*2))

# outgoing longwave looks pretty good
ggplot(artificial_longwave_out, aes(date_time, lwout)) + 
  geom_line()

#join to fill gaps
outgoing_longwave_radiation <- outgoing_longwave_radiation_initial |> 
  left_join(artificial_longwave_out, by = "date_time") |>    # Join on date_time
  mutate(lwradout2_wm2 = ifelse(is.na(lwradout2_wm2), lwout, lwradout2_wm2)) |>   # Fill missing values
  select(-lwout) # Remove extra column

#plot to check for gaps
ggplot(outgoing_longwave_radiation, aes(date_time, lwradout2_wm2)) + 
  geom_path()

# select air pressure data from Lake Hoare Met
air_pressure = HOEM |> 
  mutate(bpress_Pa = bpress_mb*100) |>  # air pressure was initially in mbar, needs to be in Pascal. 
  select(metlocid, date_time, bpress_Pa)

# select wind speed data from Lake Bonney Met
wind_speed = BOYM |> 
  select(metlocid, date_time, wspd_ms) # wind speed is in meters per second

# load ice thickness data and manipulate for easier plotting
ice_thickness <- read_csv("data/thermal diffusion model data/mcmlter-lake-ice_thickness-20230726.csv") |>
  mutate(date_time = mdy_hm(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(lake == "East Lake Bonney") |> 
  pivot_longer(cols = c(z_water_m, z_ice_m), names_to = "ice measurement", values_to = "thickness") |> 
  filter(date_time > "2015-12-01" & date_time < "2024-01-01")

############ ALBEDO CORRECTION ###########
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

###################### Interpolate Data to match model time steps ######################
time_model = start_time + seq(0, by = dt* 86400, length.out = nt)  # Convert dt from days to seconds

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

LWR_out_interp <- approx(
  x = as.numeric(outgoing_longwave_radiation$date_time),  # Convert date_time to numeric for interpolation
  y = outgoing_longwave_radiation$lwradout2_wm2,
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

###################### Create the time series tibble for model time ######################
time_series <- tibble(
  time = time_model,          # Model time steps
  T_air = airt_interp,  # Interpolated air temperature Kelvin
  SW_in = sw_interp,        # Interpolated shortwave radiation w/m2
  LWR_in = LWR_in_interp,     # Interpolated incoming longwave radiation w/m2
  LWR_out = LWR_out_interp,  # Interpolated outgoing longwave radiation w/m2
  albedo = 0.10 + ((albedo_interp)*0.90), # albedo, unitless
  #albedo = albedo_interp,      # Constant albedo (can be replaced with a time series if needed)
  pressure = pressure_interp, # Interpolated air pressure, Pa
  wind = wind_interp, # interpolated wind speed, m/s
  delta_T = T_air - lag(T_air) # difference in air temperature, for later flux calculation
) |> 
  drop_na(delta_T) # removes the first row where the difference in temperatures yields NA

# plot all input data together to do a visual check
series <- time_series |> 
  pivot_longer(cols = c(T_air, SW_in, LWR_in, LWR_out, pressure, albedo), names_to = "variable", values_to = "data")

# Plot of input ice data
ggplot(series, aes(time, data)) + 
  geom_line() + 
  xlab("Date") + ylab("Value") +
  facet_wrap(vars(variable), scales = "free") + 
  theme_minimal()

# from initial plot, it's obvious that there's holes in the longwave and shortwave datasets. 
# the hole in the longwave dataset is because the sensors never worked post datalogger upgrade in 2022, 
# the various shortwave holes are due to errors 
# longwave and shortwave radiation can be approximated by the following equations
# longwave = surface emissivity * stefan boltzman constant * surface ice temperature ^4
# shortwave = Solar Constant * cos(solar zenith)





