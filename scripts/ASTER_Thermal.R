# ASTER thermal band plotting

# libraries
library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)

setwd("~/Google Drive/My Drive/EarthEngine")

files_IR <- list.files(pattern = ".tif")
output = list()

# create a progress bar for the loop, better for large loops like this
pb = txtProgressBar(min = 0, max = length(files_IR), initial = 0) 

for (i in 1:length(files_IR)) {
  tif <- raster(files_IR[[i]])
  dataframe <- as.data.frame(tif, xy = TRUE)
  raster_sf <- st_as_sf(dataframe, coords = c("x", "y"))
  st_geometry(raster_sf) <- "geom"
  
  point <- st_sfc(st_point(c(391748.223282, -1293198.163127)))
  point_sf <- st_sf(geom = point)
  
  buffer_distance = 1000
  buffered_point <- st_buffer(point_sf, dist = buffer_distance)
  
  sf_cropped <- st_intersection(raster_sf, buffered_point)
  
  final_raster <- st_rasterize(raster_sf |> dplyr::select(Thermal_IR, geom))
  df_buffered <- as.data.frame(final_raster, xy = TRUE)

  mean_value <- mean(df_buffered$Thermal_IR, na.rm = T)
  year <- str_extract(files_IR[[i]], "20[:digit:][:digit:][:digit:][:digit:]")
  
  output <- list(
    date = year,
    mean_value = mean_value[i]
  )
  
  setTxtProgressBar(pb,i)
  
  close(pb)
}


ggplot() + 
  geom_raster(data = sf_cropped, aes(x, y, fill = mean_value)) + 
  scale_fill_gradient(low = "blue", high = "red") + 
  coord_fixed() + 
  labs(x = "Northing", y = "Easting") 

