###### TVLakes plotting of shapefiles #####

library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)
library(MetBrewer)

setwd("~charliedougherty")

raster_file = raster("Google Drive/My Drive/EarthEngine/landsat/20250218/LANDSAT_unmix_feb18_2016-11-02.tif")

### load polygons ##
lk_east_shp <- read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/East Lake Bonney.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  #select(-Description) |> 
  st_zm()

#lh polygon
lk_hoare_shp <- read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/Lake Hoare Shapefile.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  #select(-Description) |> 
  st_zm()

#lf polygon
lk_fryxell_shp <- read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/Lake Fryxell Shapefile.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  #select(-Description) |> 
  st_zm()

#wlb linestring
lk_west_shp <-read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/West Lake Bonney.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  #select(-Description) |> 
  st_zm()

setwd("~/Google Drive/My Drive/EarthEngine")
files <- list.files(pattern = ".tif")

#select color palette
met_palette <- MetBrewer::met.brewer("Ingres")


#for loop to create files
for(i in 1:length(files)) {
  setwd("~/Google Drive/My Drive/EarthEngine")
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  colnames(raster_df)[3] <- "value"
  #uncomment out if you need to filter out above 0 values. 
  #raster_df = raster_df |> 
    #mutate(value = (1-value)) |> 
    #filter(value < 1.05)
  
  year <- str_extract(files[[i]], "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]")
  
  
  ggplot() +
      geom_raster(data = raster_df, aes(x = x, y = y, fill = value)) +
      geom_sf(data = lk_east_shp, fill = NA, color = "red", size = 1) +  # Outline the polygon
      geom_sf(data = lk_west_shp, fill = NA, color = "red", size = 1) +  # Outline the polygon
      geom_sf(data = lk_hoare_shp, fill = NA, color = "red", size = 1) +  # Outline the polygon
      geom_sf(data = lk_fryxell_shp, fill = NA, color = "red", size = 1) +  # Outline the polygon
      coord_sf() +
      scale_fill_gradientn(colors = met_palette) +
      labs(title = paste0(year), x = "Northing", y = "Easting") +
      theme_minimal()
  
  setwd("~/Documents/R-Repositories/MCM-LTER/plots/GEE/SENTINEL/20250218")
  ggsave(filename = paste0("SENTINEL_plot_", year, ".png"))
  print(paste0("you are on plot #",i))
}


