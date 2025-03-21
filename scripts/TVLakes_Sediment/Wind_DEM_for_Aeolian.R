###### Kayla Lidar Helper Script ######### 
# load libraries 
library(terra) 
library(tidyverse) 
library(viridis)
setwd("~/Google Drive/My Drive/MCMLTER_Met") 
# load files 
DEM <- rast("output_be.tif") 
plot(DEM, col = viridis(1500), main = "Bonney + Hoare Basin") 

#crop DEM to bonney basin
bonney_basin = ext(4900, 16000, 29000, 36000)

DEM = crop(DEM, bonney_basin)

#plot(DEM)

slope <- terrain(DEM, v = "slope", unit = "degrees") 
aspect <- terrain(DEM, v = "aspect", unit = "degrees") 
#plot(slope) 
#plot(aspect) 

# Load Wind Data from Lake Bonney and Lake Hoare 

BOYM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         timestamp = as.POSIXct(date_time))

#filter wind data down to March 18, 2022 and April 22, 2020 and April 15, 2020
wind_data <- BOYM %>%
  select(timestamp, wspd_ms, wspdmax_ms, wdir_deg) %>% 
  mutate(wdir_deg = wdir_deg / 36) %>% 
  filter(
    (timestamp < as.POSIXct("2022-03-18 18:00:00", tz = "NZ") & timestamp > as.POSIXct("2022-03-18 06:00:00", tz = "NZ")) |
      (timestamp < as.POSIXct("2020-04-23 18:00:00", tz = "NZ") & timestamp > as.POSIXct("2020-04-22 18:00:00", tz = "NZ")) |
      (timestamp < as.POSIXct("2020-04-15 23:59:00", tz = "NZ") & timestamp > as.POSIXct("2020-04-15 00:00:00", tz = "NZ"))
    ) %>% 
  pivot_longer(cols = c(wspd_ms, wspdmax_ms, wdir_deg), values_to = "wind", names_to = "measurement_type") %>% 
  mutate(event_group = case_when(
    between(timestamp, as.POSIXct("2022-03-18 06:00:00", tz = "NZ"), as.POSIXct("2022-03-18 18:00:00", tz = "NZ")) ~ "Event 1 (Mar 18, 2022)",
    between(timestamp, as.POSIXct("2020-04-22 18:00:00", tz = "NZ"), as.POSIXct("2020-04-23 18:00:00", tz = "NZ")) ~ "Event 2 (Apr 22-23, 2020)",
    between(timestamp, as.POSIXct("2020-04-15 00:00:00", tz = "NZ"), as.POSIXct("2020-04-15 23:59:00", tz = "NZ")) ~ "Event 3 (Apr 15, 2020)"
  )) %>% 
  mutate(month = month(timestamp), 
         week = week(timestamp))

#ggplot(wind_data, aes(timestamp, wind, color = measurement_type)) + 
#  geom_path() + 
#  facet_wrap(vars(month, week), scales = "free")

c("#E41A1CFF", "#377EB8FF", "#4DAF4AFF", "#984EA3FF", "#FF7F00FF", "#FFFF33FF", "#A65628FF", "#F781BFFF", "#999999FF")

# Adjust the wind speed values by dividing by 10 to match the scale of wind direction
wind_data <- wind_data %>%
  mutate(wind = ifelse(measurement_type %in% c("wspd_ms", "wspdmax_ms"), wind, wind))  # Adjust wind speed by factor of 10

# Plot the data with wind direction on a different y-axis
ggplot(wind_data, aes(x = timestamp, y = wind, color = measurement_type)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(vars(event_group), scales = "free") +
  scale_y_continuous(
    name = "Wind Speed (m/s)",   # Left y-axis label for wind speed
    sec.axis = sec_axis(~ .*10, name = "Wind Direction (°)")  # Right y-axis for wind direction (0-360)
  ) +
  theme_linedraw() +
  theme(
    axis.title.y.left = element_text(color = "#377EB8FF"),
    axis.title.y.right = element_text(color = "#E41A1CFF")
  ) +
  scale_color_manual(
    values = c("wspd_ms" = "#377EB8FF", "wspdmax_ms" = "darkblue", "wdir_deg" = "#E41A1CFF")
  ) +
  labs(x = "Timestamp", color = "Measurement Type") 

setwd("~/Documents/R-Repositories/MCM-LTER-MS")
ggsave("plots/manuscript/chapter 1/wind_events_wdir_wspd.png", 
       width = 12, height = 8, dpi = 300)


library(terra)

# Define the lake point as a SpatVector (change x, y to real coordinates)
lake_point <- vect(data.frame(x = 10500, y = 31800), geom = c("x", "y"), crs = crs(aspect))

# Function to compute wind alignment with slope for aeolian entrainment likelihood
calc_wind_alignment_with_slope <- function(wind_dir, aspect_raster, slope_raster) {
  wind_dir_rast <- rast(aspect_raster)  # Convert aspect to raster format
  values(wind_dir_rast) <- wind_dir     # Apply wind direction value
  
  # Compute wind alignment: cos(wind_direction - aspect)
  alignment <- cos((wind_dir_rast - aspect_raster) * pi / 180)
  
  # Filter: Keep values where alignment > 0.5, set others to NA
  alignment[alignment <= 0.5] <- NA  
  
  # Weight alignment by slope: Multiply alignment by slope values
  entrainment_likelihood <- alignment * slope_raster
  
  return(entrainment_likelihood)
}

# Function to plot wind alignment and add arrow, incorporating aeolian entrainment likelihood
plot_wind_alignment_with_slope <- function(wind_dir, aspect, slope, lake_point, title) {
  entrainment_rast <- calc_wind_alignment_with_slope(wind_dir, aspect, slope)
  
  # Extract aeolian entrainment likelihood value at the lake point
  lake_entrainment_value <- terra::extract(entrainment_rast, lake_point)
  print(lake_entrainment_value)
  
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
  
  # Plot the entrainment likelihood raster
  plot(entrainment_rast, col = viridis(100), main = title)
  points(lake_x, lake_y, col = "red", pch = 19, cex = 1.5)
  
  # Add wind direction arrow
  arrows(lake_x, lake_y, arrow_x, arrow_y, col = "blue", lwd = 2, length = 0.15)
}



# Plot for 70-degree wind
plot_wind_alignment_with_slope(70, aspect, slope, lake_point, "70-degree wind")

# Plot for 80-degree wind
plot_wind_alignment_with_slope(80, aspect, slope, lake_point, "80-degree wind")


# Plot for 90-degree wind
plot_wind_alignment_with_slope(90, aspect, slope, lake_point, "90-degree wind")


# Plot for 100-degree wind
plot_wind_alignment_with_slope(100, aspect, slope, lake_point, "100-degree wind")

# Plot for 150-degree wind
plot_wind_alignment_with_slope(150, aspect, slope, lake_point, "150-degree wind")

# Plot for 175-degree wind
plot_wind_alignment_with_slope(175, aspect, slope, lake_point, "175-degree wind")




