###### Kayla Lidar Helper Script ######### 
# load libraries 
library(terra) 
library(tidyverse) 
setwd("~/Google Drive/My Drive/MCMLTER_Met") 
# load files 
DEM <- rast("output_be.tif") 
plot(DEM, main = "Bonney + Hoare Basin") 
filled_dem <- focal(DEM, w = matrix(1, 3, 3), fun = mean, na.policy = "only") 
plot(filled_dem, main = "interpolated") 
slope <- terrain(DEM, v = "slope", unit = "degrees") 
aspect <- terrain(DEM, v = "aspect", unit = "degrees") 
plot(slope) plot(aspect) # Load Wind Data from Lake Bonney and Lake Hoare 

BOYM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) 
HOEM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> filter(date_time > '2016-12-21 00:00:00')

# Function to compute wind alignment
calc_wind_alignment <- function(wind_dir, aspect_raster) {
  wind_dir_rast <- rast(aspect_raster) # Convert wind direction to raster format
  values(wind_dir_rast) <- wind_dir # Apply wind direction value
  
  # Compute alignment: cos(wind_direction - aspect)
  alignment <- cos((wind_dir_rast - aspect_raster) * pi / 180)
  return(alignment)
}

# Example: Apply function for a given wind direction (e.g., 230°)
wind_direction <- 230
wind_alignment <- calc_wind_alignment(wind_direction, aspect)

# Plot wind alignment
plot(wind_alignment, main = "Wind Alignment with Terrain")


# Identify regions with sufficient wind speed (example: 12 m/s)
wind_speed <- 12  # Example from station data

# Create a raster where conditions are met
transport_potential <- ifel((wind_speed >= 10) & (wind_alignment > 0), 1, 0)

# Plot potential transport zones
plot(transport_potential, main = "Potential Aeolian Sediment Transport Areas")
