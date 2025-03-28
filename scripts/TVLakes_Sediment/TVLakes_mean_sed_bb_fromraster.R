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
buffered_points_sf <- st_buffer(points_sf, dist = 300)

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

#output_corr <- output |> 
#  mutate(`Lake Fryxell` = 1-`Lake Fryxell`, 
#         `Lake Hoare` = 1-`Lake Hoare`)

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

# Transform and save output
output_to_save <- output |> 
  pivot_longer(cols = c(`East Lake Bonney`, `Lake Hoare`, `Lake Fryxell`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |>
  drop_na() |> 
  mutate(date = ymd(date), 
         ice_abundance = sediment, 
         sediment_abundance = 1-sediment) |> 
  drop_na()

write_csv(output_to_save, "data/sediment abundance data/LANDSAT_sediment_abundances_20250328.csv")

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

