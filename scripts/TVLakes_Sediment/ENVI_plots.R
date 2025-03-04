# plotting rasters of Fryxell

library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)

setwd("~/Documents/R-Repositories/MCM-LTER")

# create lists
output <- list()
mean_value <- list()
year <- list()

# LF BB coords -77.610275	163.146877
# EPSG:3031 -1293198.163127, 391748.223282

LFBB <- st_point(x = c(391748.223282, -1293198.163127))

setwd("~/Documents/R-Repositories/MCM-LTER/data/ENVI Unmixing")
files <- list.files(pattern = ".tif")
for(i in 1:length(files)) {
  setwd("~/Documents/R-Repositories/MCM-LTER/data/ENVI Unmixing")
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE)
  colnames(raster_df)[3] <- "value"

  
  final_raster <- st_rasterize(sf_cropped |> dplyr::select(value, geom))
  df_buffered <- as.data.frame(final_raster, xy = TRUE)
  
  year <- str_extract(files, "20[:digit:][:digit:][:digit:][:digit:][:digit:][:digit:]")
  year_date = ymd(as.numeric(year))
  
  print(
    ggplot() + 
    geom_raster(data = raster_df, aes(x, y, fill = value)) + 
    scale_fill_gradient(low = "yellow", high = "blue") + 
    coord_fixed() + 
    labs(title = paste(year_date[[i]]), x = "Northing", y = "Easting") 
  )
  
  setwd("~/Documents/R-Repositories/MCM-LTER/plots/ENVI")
  ggsave(filename = paste0("plot_", year_date[[i]], ".png"))
  print(paste0("you are on plot #",i))
 }







