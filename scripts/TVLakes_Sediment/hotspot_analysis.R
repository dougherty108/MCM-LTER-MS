####### TV Lakes HotSpot Analysis #####

# idea one, persistence. 
# create a raster stack of all images (start with Bonney)
# and make sure all cells align (I think this should be fine to start with)
# take a mean value of each cell, and plot that. The mean value gives you an i
# idea of the average sediment cover through time 
# library
library(terra)
library(tidyverse)
library(raster)
library(sf)

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250308"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_BON.*\\.tif$", full.names = TRUE)

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[2]]))  # Adjust `[[1]]` to desired band index

# load shapefile of East Lake Bonney
#eastlobe_outline <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml")

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE)

colnames(mean_df)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

ggplot() +
  geom_raster(data = mean_df, aes(x = x, y = y, fill = sediment_mean)) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Bonney Hotspots", x = "Easting", y = "Northing") +
  theme_linedraw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       dpi = 400)


