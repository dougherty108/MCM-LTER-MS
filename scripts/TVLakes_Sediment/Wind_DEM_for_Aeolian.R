###### Kayla Lidar Helper Script ######### 
# load libraries 
library(terra) 
library(tidyverse) 
setwd("~/Google Drive/My Drive/MCMLTER_Met") 
# load files 
DEM <- rast("output_be.tif") 
plot(DEM, main = "Bonney + Hoare Basin") 

#crop DEM to bonney basin
bonney_basin = ext(4900, 16000, 29000, 36000)

DEM = crop(DEM, bonney_basin)

plot(DEM)

slope <- terrain(DEM, v = "slope", unit = "degrees") 
aspect <- terrain(DEM, v = "aspect", unit = "degrees") 
plot(slope) 
plot(aspect) 

# Load Wind Data from Lake Bonney and Lake Hoare 

BOYM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         timestamp = as.POSIXct(date_time))

#filter wind data down to March 18, 2022 and April 22, 2020
wind_data <- BOYM %>%
  select(timestamp, wspd_ms, wspdmax_ms, wdir_deg) %>% 
  filter(
    (timestamp < as.POSIXct("2022-03-18 18:00:00") & timestamp > as.POSIXct("2022-03-18 06:00:00")) |
           (timestamp < as.POSIXct("2020-04-23 18:00:00") & timestamp > as.POSIXct("2020-04-22 18:00:00"))
    ) %>% 
  #pivot_longer(cols = c(wspd_ms, wspdmax_ms, wdir_deg), values_to = "wind", names_to = "measurement_type") %>% 
  mutate(month = month(timestamp))

# plot data
ggplot(wind_data, aes(timestamp, wind, color = measurement_type)) + 
  geom_path() + 
  facet_wrap(vars(month), scales = "free")

# Create the plot
ggplot(wind_data, aes(x = timestamp)) +
  geom_line(aes(y = wspd_ms, color = "Wind Speed"), size = 1) +
  geom_line(aes(y = wdir_deg / 36, color = "Wind Direction"), size = 1, linetype = "dashed") +
  scale_y_continuous(
    name = "Wind Speed (m/s)",  
    sec.axis = sec_axis(~ . * 36, name = "Wind Direction (°)")  # Rescale wind direction back to 0-360
  ) +
  labs(x = "Timestamp", color = "Variable") +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red")
  )  + 
  facet_wrap(vars(month), scales = "free")
  


library(terra)

# Define the lake point as a SpatVector (change x, y to real coordinates)
lake_point <- vect(data.frame(x = 10500, y = 31800), geom = c("x", "y"), crs = crs(aspect))

# Function to compute wind alignment
calc_wind_alignment <- function(wind_dir, aspect_raster) {
  wind_dir_rast <- rast(aspect_raster) # Convert wind direction to raster format
  values(wind_dir_rast) <- wind_dir # Apply wind direction value
  
  # Compute alignment: cos(wind_direction - aspect)
  alignment <- cos((wind_dir_rast - aspect_raster) * pi / 180)
  return(alignment)
}

# Function to plot wind alignment and add arrow
plot_wind_alignment <- function(wind_dir, aspect, lake_point, title) {
  wind_alignment_rast <- calc_wind_alignment(wind_dir, aspect)
  
  # Extract wind alignment value at the lake point
  lake_alignment_value <- terra::extract(wind_alignment_rast, lake_point)
  print(lake_alignment_value)
  
  # Convert SpatVector to numeric coordinates for plotting
  lake_coords <- as.data.frame(geom(lake_point))
  lake_x <- lake_coords$x
  lake_y <- lake_coords$y
  
  # Compute arrow direction (convert wind direction to radians)
  wind_rad <- (270 - wind_dir) * pi / 180  # Convert wind direction to mathematical coordinates
  
  # Define arrow length (adjust as needed)
  arrow_length <- 1000  # Adjust for visualization scale
  
  # Compute arrow end coordinates
  arrow_x <- lake_x + arrow_length * cos(wind_rad)
  arrow_y <- lake_y + arrow_length * sin(wind_rad)
  
  # Plot wind alignment raster
  plot(wind_alignment_rast, main = title)
  points(lake_x, lake_y, col = "red", pch = 19, cex = 1.5)
  
  # Add wind direction arrow
  arrows(lake_x, lake_y, arrow_x, arrow_y, col = "blue", lwd = 2, length = 0.15)
}

# Plot for 70-degree wind
plot_wind_alignment(70, aspect, lake_point, "70-degree wind")

# Plot for 230-degree wind
plot_wind_alignment(230, aspect, lake_point, "230-degree wind")

