###### lm sensitivity analysis #####
library(raster)
library(sf)
library(tidyverse)
library(lubridate)

setwd("~/Documents/R-Repositories/MCM-LTER")


# create lists

setwd("~/Documents/R-Repositories/MCM-LTER/data/ENVI Unmixing")
files <- list.files(pattern = ".tif")
year = integer()
mean_value <= integer()
output <- list()
range <- c(50, 100, 150, 200, 250, 300, 350, 400)

# loop
# I'm sort of lost on how to get this to work. 
for(i in 2:length(files)) {
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE)
  colnames(raster_df)[3] <- "value"
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  point <- st_sfc(st_point(c(391748.223282, -1293198.163127)))
  point_sf <- st_sf(geom = point)
  
  buffer_distance <- range
  buffered_point <- st_buffer(point_sf, dist = buffer_distance)
  print("complete")
  
  sf_cropped <- st_intersection(buffered_point, raster_sf)
  
  final_raster <- st_rasterize(sf_cropped |> dplyr::select(value, geom))
  df_buffered <- as.data.frame(final_raster, xy = TRUE)
  
  mean_value <- mean(df_buffered$value, na.rm = T)
  
  year <- str_extract(files[[i]], "20[:digit:][:digit:]")
  print("complete")
  
  output[[i]] <- c(
    mean_value = mean_value, 
    year = year,
    buffer_distance = range[[i]]
  )
}

output_df <- bind_rows(output)





