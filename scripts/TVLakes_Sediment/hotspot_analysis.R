####### TV Lakes HotSpot Analysis #####

# idea one, persistence. 
# create a raster stack of all images (start with Bonney)
# and make sure all cells align (I think this should be fine to start with)
# take a mean value of each cell, and plot that. The mean value gives you an i
# idea of the average sediment cover through time 
# library
library(terra)
library(tidyverse)
#library(raster)
library(sf)
library(ggpubr)
library(ggspatial)

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250325"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_BON.*\\.tif$", full.names = TRUE)

#tif_files = tif_files[tif_files != 'Google Drive/My Drive/EarthEngine/landsat/20250308/LANDSAT_BON_unmix_mar01_2016-12-13.tif']

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[1]]))  # Adjust `[[1]]` to desired band index

# load shapefile of East Lake Bonney
#eastlobe_outline <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml")

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE) |> 
  mutate(x = x*-1)

colnames(mean_df)[3] = "sediment_mean"

mean_df2 <- mean_df |> 
  mutate(sediment_filter = sediment_mean, 
         ice_mean = 1-sediment_mean) #|> 
  #filter(sediment_filter < 0.99)

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

bonney <- ggplot() +
  geom_raster(data = mean_df2, aes(x = x, y = y, fill = (ice_mean)*100)) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Bonney Hotspots", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  #scale_x_reverse() + 
  scale_y_reverse() + 
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none"
        )

dummy = ggplot() +
  geom_raster(data = mean_df2, aes(x = x, y = y, fill = (ice_mean)*100)) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Bonney Hotspots", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  #scale_x_reverse() + 
  scale_y_reverse() + 
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        #legend.position = "none"
  )

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       plot = bonney,
       dpi = 400)

###### HOARE 

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250325"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_HOA.*\\.tif$", full.names = TRUE)

#tif_files = tif_files[tif_files != 'Google Drive/My Drive/EarthEngine/landsat/20250325/LANDSAT_HOA_unmix_mar12_2020-01-02.tif']

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[2]]))  # Adjust `[[1]]` to desired band index

# load shapefile of East Lake Bonney
#eastlobe_outline <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml")

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE) |> 
  mutate(y = y*-1)

colnames(mean_df)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

hoare <- ggplot() +
  geom_raster(data = mean_df, aes(x = x, y = y, fill = (sediment_mean*100))) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Hoare Hotspots", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  scale_x_reverse() + 
  #scale_y_reverse() +  
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       plot = hoare,
       dpi = 400)


###### Fryxell

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250325"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_FRY.*\\.tif$", full.names = TRUE)

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[2]]))  # Adjust `[[1]]` to desired band index

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE) |> 
  mutate(y = y*-1)

colnames(mean_df)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

fryxell <- ggplot() +
  geom_raster(data = mean_df, aes(x = x, y = y, fill = (sediment_mean*100))) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Fryxell Hotspots", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  theme_linedraw(base_size = 15) +
  scale_x_reverse() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none"
        )

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

#ggsave("plots/hotspot/lk_bonney_hotspot.png", 
#       plot = fryxell, dpi = 400)


# final plot
bonney <- bonney +
  annotation_scale(location = "bl", pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"))

hoare <- hoare +
  annotation_scale(location = "bl", pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"))

fryxell <- fryxell +
  annotation_scale(location = "bl", pad_x = unit(0.5, "cm"), pad_y = unit(0.5, "cm"))

legend <- get_legend(dummy)

ggarrange(bonney, hoare, fryxell, legend,
          nrow = 1, widths = c(1, 1, 1))

setwd("~/Documents/R-Repositories/MCM-LTER-MS/plots/manuscript/chapter 1")
ggsave("hotpots_redone_foraxes.png", 
       dpi = 300, height = 7, width = 14)







