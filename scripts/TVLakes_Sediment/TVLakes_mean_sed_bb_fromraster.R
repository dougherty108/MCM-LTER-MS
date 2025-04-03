## libraries
library(raster)
library(sf)
library(tidyverse)

setwd("~charliedougherty")

files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/20250325", pattern = ".tif", full.names = TRUE)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")

# Predefine output tibble
output <- tibble(
  date = character(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

# Define point coordinates
points_df <- data.frame(
  name = c("Lake Fryxell", "Lake Hoare", "East Lake Bonney", "West Lake Bonney"),
  x = c(391748.223282, 396517.85394052055, 404047.2666197109, 407169.73944380396),
  y = c(-1293198.163127, -1289740.3825915689, -1277516.4884229063, -1275776.8988470172)
)

# Convert to sf object and buffer
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)  
# Buffer after ensuring the correct CRS
buffered_points_sf <- st_buffer(points_sf, dist = 150)

# Convert `sf` buffer object to `Spatial` before using extract()
buffered_points_sp <- as(buffered_points_sf, "Spatial")  

setwd("~charliedougherty")
# Loop through each raster file
for (i in seq_along(files)) {
  raster_file <- raster(files[i])

  # Extract mean values using the corrected object
  extracted_values <- raster::extract(raster_file, buffered_points_sp, fun = mean, na.rm = TRUE)
  
  # Extract date from filename
  date <- str_extract(files[i], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Append results to output tibble
  output <- bind_rows(output, tibble(
    date = date, 
    `Lake Fryxell` = extracted_values[1],
    `Lake Hoare` = extracted_values[2], 
    `East Lake Bonney` = extracted_values[3], 
    `West Lake Bonney` = extracted_values[4]
  ))
  
  print(i)  # Keep track of progress
}

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

# Transform and save output
output_to_save <- output |> 
  pivot_longer(cols = c(`East Lake Bonney`, `Lake Hoare`, `Lake Fryxell`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |>
  drop_na() |> 
  mutate(date = ymd(date), 
         ice_abundance = sediment, 
         sediment_abundance = 1-sediment) |> 
  drop_na()

write_csv(output_to_save, "data/sediment abundance data/LANDSAT_sediment_abundances_20250403.csv")

# Plot results
ggplot(output_to_save, aes(date, sediment_abundance)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat") + 
  theme_minimal()

ggplot(output_to_save, aes(date, ice_abundance)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat") + 
  theme_minimal()


### longer script to make different buffer distances

library(sf)
library(raster)
library(tibble)
library(stringr)
library(dplyr)

setwd("~charliedougherty")

files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/20250325", pattern = ".tif", full.names = TRUE)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")

# Predefine output tibble
output <- tibble(
  date = character(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

# Define point coordinates
points_df <- data.frame(
  name = c("Lake Fryxell", "Lake Hoare", "East Lake Bonney", "West Lake Bonney"),
  x = c(391748.223282, 396517.85394052055, 404047.2666197109, 407169.73944380396),
  y = c(-1293198.163127, -1289740.3825915689, -1277516.4884229063, -1275776.8988470172)
)

# Convert to sf object and buffer
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)  
# Buffer after ensuring the correct CRS
buffered_points_sf <- st_buffer(points_sf, dist = 150)

# Convert `sf` buffer object to `Spatial` before using extract()
buffered_points_sp <- as(buffered_points_sf, "Spatial")  

setwd("~charliedougherty")


setwd("~charliedougherty")

# Define buffer distances
buffer_distances <- c(50, 100, 150, 200, 300)  # Modify distances as needed

# Convert points to sf object
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)

output_buffers <- tibble(
  date = character(),
  buffer_distance = numeric(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

# Loop through each buffer distance
for (buffer_dist in buffer_distances) {
  
  # Create buffered points for the current distance
  buffered_points_sf <- st_buffer(points_sf, dist = buffer_dist)
  
  # Convert to Spatial object before extracting values
  buffered_points_sp <- as(buffered_points_sf, "Spatial")
  
  # Loop through each raster file
  for (i in seq_along(files)) {
    raster_file <- raster(files[i])
    
    # Extract mean values using the corrected object
    extracted_values <- raster::extract(raster_file, buffered_points_sp, fun = mean, na.rm = TRUE)
    
    # Extract date from filename
    date <- str_extract(files[i], "20\\d{2}-\\d{2}-\\d{2}")
    
    # Append results to output tibble
    output_buffers <- bind_rows(output, tibble(
      date = date,
      buffer_distance = buffer_dist,
      `Lake Fryxell` = extracted_values[1],
      `Lake Hoare` = extracted_values[2],
      `East Lake Bonney` = extracted_values[3],
      `West Lake Bonney` = extracted_values[4]
    ))
    
    print(paste("Processed file", i, "for buffer", buffer_dist))  # Keep track of progress
  }
}

buffer_plot <- output_buffers |> 
  pivot_longer(cols = c(`Lake Fryxell`, `Lake Hoare`, `East Lake Bonney`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |> 
  drop_na(sediment) |> 
  mutate(date = ymd(date), 
         buffer_distance = as.character(buffer_distance))

ggplot(buffer_plot, aes(date, sediment, color = buffer_distance)) + 
  geom_path() + 
  facet_wrap(vars(lake), scales = "free") + 
  theme_linedraw()




