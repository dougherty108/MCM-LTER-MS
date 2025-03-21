###### Kayla Lidar Helper Script ######### 
# load libraries 
library(terra) 

library(tidyverse) 
library(viridis)

# Load Wind Data from Lake Bonney and Lake Hoare 

BOYM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         timestamp = as.POSIXct(date_time, tz = "Antarctica/McMurdo"))

#filter wind data down to March 18, 2022 and April 22, 2020 and April 15, 2020
wind_data <- BOYM %>%
  select(timestamp, wspd_ms, wspdmax_ms, wdir_deg) %>% 
  mutate(wdir_deg = wdir_deg/10) %>% 
  filter(
    (timestamp < as.POSIXct("2022-03-18 18:00:00", tz = "Antarctica/McMurdo") & timestamp > as.POSIXct("2022-03-18 06:00:00", tz = "Antarctica/McMurdo")) |
      (timestamp < as.POSIXct("2020-04-23 18:00:00", tz = "Antarctica/McMurdo") & timestamp > as.POSIXct("2020-04-22 18:00:00", tz = "Antarctica/McMurdo")) |
      (timestamp < as.POSIXct("2020-04-15 23:59:00", tz = "Antarctica/McMurdo") & timestamp > as.POSIXct("2020-04-15 00:00:00", tz = "Antarctica/McMurdo"))
    ) %>% 
 # pivot_longer(cols = c(wspd_ms, wspdmax_ms, wdir_deg), values_to = "wind", names_to = "measurement_type") %>% 
  mutate(event_group = case_when(
    between(timestamp, as.POSIXct("2022-03-18 06:00:00", tz = "Antarctica/McMurdo"), as.POSIXct("2022-03-18 18:00:00", tz = "Antarctica/McMurdo")) ~ "Sediment (Mar 18, 2022)",
    between(timestamp, as.POSIXct("2020-04-15 00:00:00", tz = "Antarctica/McMurdo"), as.POSIXct("2020-04-15 23:59:00", tz = "Antarctica/McMurdo")) ~ "No Sediment (Apr 15, 2020)",
    between(timestamp, as.POSIXct("2020-04-22 18:00:00", tz = "Antarctica/McMurdo"), as.POSIXct("2020-04-23 18:00:00", tz = "Antarctica/McMurdo")) ~ "Sediment (Apr 22-23, 2020)"
  )) %>% 
  mutate(month = month(timestamp), 
         week = week(timestamp))

ggplot(wind_data, aes(x = timestamp)) + 
  geom_path(aes(y = wspd_ms, color = "Wind Speed (m/s)"), linewidth = 1.5) + 
  geom_path(aes(y = wspdmax_ms, color = "Max Wind Speed (m/s)"), linewidth = 1.5) + 
  geom_path(aes(y = wdir_deg, color = "Wind Direction (°)"), linewidth = 1.5) + 
  scale_y_continuous(
    name = "Wind Speed (m/s)",   
    sec.axis = sec_axis(~ .*10, name = "Wind Direction (°)")  # Right y-axis for wind direction (0-360)
  ) +
  facet_wrap(vars(event_group), scales = "free") +  
  xlab("Timestamp") +  
  theme_linedraw(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"  # Adjust legend placement
  ) +
  scale_color_manual(
    values = c(
      "Wind Speed (m/s)" = "#377EB8FF", 
      "Max Wind Speed (m/s)" = "#4DAF4AFF", 
      "Wind Direction (°)" = "#E41A1CFF"
    ),
    name = "Legend",  # Legend title
    guide = guide_legend(override.aes = list(linewidth = 3))  # Make legend lines thicker for visibility
  )


setwd("~/Documents/R-Repositories/MCM-LTER-MS")
ggsave("plots/manuscript/chapter 1/wind_events_wdir_wspd.png", 
       width = 16, height = 8, dpi = 300)

setwd("~/Google Drive/My Drive/MCMLTER_Met") 
# load files 
DEM <- rast("output_be.tif") 
plot(DEM, col = viridis(1500), main = "Bonney + Hoare Basin") 

#crop DEM to bonney basin
bonney_basin = ext(4900, 16000, 29000, 36000)

DEM = crop(DEM, bonney_basin)

slope <- terrain(DEM, v = "slope", unit = "degrees") 
aspect <- terrain(DEM, v = "aspect", unit = "degrees") 
plot(slope, col = viridis(100)) 
plot(aspect, col = viridis(100)) 

# Define the lake point as a SpatVector (change x, y to real coordinates)
lake_point <- vect(data.frame(x = 10500, y = 31800), geom = c("x", "y"), crs = crs(aspect))

# Function to compute wind alignment with slope for aeolian entrainment likelihood
calc_wind_alignment_with_slope <- function(wind_dir, aspect_raster, slope_raster) {
  wind_dir_rast <- rast(aspect_raster)  # Convert aspect to raster format
  values(wind_dir_rast) <- wind_dir     # Apply wind direction value
  
  # Compute wind alignment: cos(wind_direction - aspect)
  alignment <- cos((aspect_raster - wind_dir_rast) * pi / 180)
  #alignment <- aspect_raster-wind_dir_rast
  
  # Filter: Keep values where alignment > 0.5, set others to NA
  
  slope_corr = slope_raster / 90
  
  # Weight alignment by slope: Multiply alignment by slope values
  entrainment_likelihood <- alignment #* slope_corr
  
  entrainment_likelihood[entrainment_likelihood >= -0.25] <- NA  
  
  return(entrainment_likelihood)
}

# Function to plot wind alignment and add arrow, incorporating aeolian entrainment likelihood
plot_wind_alignment_with_slope <- function(wind_dir, aspect, slope, lake_point, title) {
  entrainment_rast <- calc_wind_alignment_with_slope(wind_dir, aspect, slope)
  
  # Extract aeolian entrainment likelihood value at the lake point
  #lake_entrainment_value <- terra::extract(entrainment_rast, lake_point)
  #print(lake_entrainment_value)
  
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
  points(lake_x, lake_y, col = "red", pch = 8, cex = 1.5)
  
  # Add wind direction arrow
  arrows(lake_x, lake_y, arrow_x, arrow_y, col = "blue", lwd = 2, length = 0.15)
}



# Plot for 70-degree wind
plot_wind_alignment_with_slope(50, aspect, slope, lake_point, "50-degree wind")

plot_wind_alignment_with_slope(60, aspect, slope, lake_point, "60-degree wind")

plot_wind_alignment_with_slope(70, aspect, slope, lake_point, "70-degree wind")

plot_wind_alignment_with_slope(80, aspect, slope, lake_point, "80-degree wind")

plot_wind_alignment_with_slope(90, aspect, slope, lake_point, "90-degree wind")

plot_wind_alignment_with_slope(100, aspect, slope, lake_point, "100-degree wind")

plot_wind_alignment_with_slope(110, aspect, slope, lake_point, "110-degree wind")

plot_wind_alignment_with_slope(120, aspect, slope, lake_point, "120-degree wind")

plot_wind_alignment_with_slope(130, aspect, slope, lake_point, "130-degree wind")

plot_wind_alignment_with_slope(140, aspect, slope, lake_point, "140-degree wind")

plot_wind_alignment_with_slope(150, aspect, slope, lake_point, "150-degree wind")

plot_wind_alignment_with_slope(170, aspect, slope, lake_point, "170-degree wind")

plot_wind_alignment_with_slope(180, aspect, slope, lake_point, "50-degree wind")

plot_wind_alignment_with_slope(190, aspect, slope, lake_point, "60-degree wind")

plot_wind_alignment_with_slope(200, aspect, slope, lake_point, "70-degree wind")

plot_wind_alignment_with_slope(210, aspect, slope, lake_point, "80-degree wind")

plot_wind_alignment_with_slope(220, aspect, slope, lake_point, "90-degree wind")

plot_wind_alignment_with_slope(230, aspect, slope, lake_point, "100-degree wind")

plot_wind_alignment_with_slope(240, aspect, slope, lake_point, "110-degree wind")

plot_wind_alignment_with_slope(250, aspect, slope, lake_point, "120-degree wind")

plot_wind_alignment_with_slope(260, aspect, slope, lake_point, "130-degree wind")

plot_wind_alignment_with_slope(270, aspect, slope, lake_point, "140-degree wind")

plot_wind_alignment_with_slope(280, aspect, slope, lake_point, "160-degree wind")

plot_wind_alignment_with_slope(290, aspect, slope, lake_point, "170-degree wind")




plot_wind_alignment_with_slope(136, aspect, slope, lake_point, "136-degree wind" )

plot_wind_alignment_with_slope(150, aspect, slope, lake_point, "150-degree wind")

plot_wind_alignment_with_slope(250, aspect, slope, lake_point, "250-degree wind")

# sanity checks
plot_wind_alignment_with_slope(360, aspect, slope, lake_point, "360 Degrees")

plot_wind_alignment_with_slope(180, aspect, slope, lake_point, "180 Degrees")


library(ggpubr)

ggarrange(seventy, two50)

# what directions of wind are dominant at ELB? 

summary(BOYM$wdir_deg)





