### plot up RGB images to compare sediment outputs to

# libraries
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(ggpubr)

# load GEE files

setwd("/Users/charliedougherty")

files = dir(path = "Google Drive/My Drive/SMA_EarthEngine_20240907", pattern = ".tif")
date <- list()

target_dir = "~/Documents/R-Repositories/MCM-LTER/plots/GEE"

files = dir(path = "Google Drive/My Drive/EarthEngine", pattern = ".tif")
date <- list()

output <- list()
year = list()
mean = list()

for(i in 1:length(files)) {
  setwd("/Users/charliedougherty")
  raster_file <- raster(paste0("Google Drive/My Drive/EarthEngine/", files[[i]]))
  raster_df <- as.data.frame(raster_file, xy = TRUE) #|> 
    #drop_na()
  colnames(raster_df)[3] <- "value"
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  year <- str_extract(files[[i]], "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]")
  
  print(
    ggplot() + 
      geom_raster(data = raster_df, aes(x, y, fill = value)) + 
      geom_spatial_rgb(
        data = raster_df,
        mapping = aes(
          x = x,
          y = y,
          r = red,
          g = green,
          b = blue
        )
      )
      scale_fill_gradient(low = "blue", high = "yellow") + 
      coord_fixed() + 
      labs(title = paste(year), x = "Northing", y = "Easting") 
  )
  
  setwd("~/Documents/R-Repositories/MCM-LTER/plots/GEE/20240907")
  ggsave(filename = paste0("plot_", year, ".png"))
  print(paste0("you are on plot #",i))
}