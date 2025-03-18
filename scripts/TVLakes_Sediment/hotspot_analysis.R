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

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250308"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_BON.*\\.tif$", full.names = TRUE)

tif_files = tif_files[tif_files != 'Google Drive/My Drive/EarthEngine/landsat/20250308/LANDSAT_BON_unmix_mar01_2016-12-13.tif']

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[1]]))  # Adjust `[[1]]` to desired band index

# load shapefile of East Lake Bonney
#eastlobe_outline <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml")

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE)

colnames(mean_df)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

bonney <- ggplot() +
  geom_raster(data = mean_df, aes(x = x, y = y, fill = (1-sediment_mean)*100)) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Bonney Hotspots", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       dpi = 400)

###### HOARE 

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250308"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_HOA.*\\.tif$", full.names = TRUE)

tif_files = tif_files[tif_files != 'Google Drive/My Drive/EarthEngine/landsat/20250308/LANDSAT_HOA_unmix_mar12_2020-01-02.tif']

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[1]]))  # Adjust `[[1]]` to desired band index

# load shapefile of East Lake Bonney
#eastlobe_outline <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml")

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE)

colnames(mean_df)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

hoare <- ggplot() +
  geom_raster(data = mean_df, aes(x = x, y = y, fill = (sediment_mean*100))) +
  coord_sf() +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Hoare Hotspots", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  theme_linedraw(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       dpi = 400)


###### Fryxell

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250308"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_FRY.*\\.tif$", full.names = TRUE)

#tif_files = tif_files[tif_files != 'Google Drive/My Drive/EarthEngine/landsat/20250308/LANDSAT_HOA_unmix_mar12_2020-01-02.tif']

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[1]]))  # Adjust `[[1]]` to desired band index

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE)

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
  theme(axis.text.x = element_text(angle = 45, hjust = 1)#, 
        #legend.position = "none"
        )

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       plot = fryxell, dpi = 400)


# final plot

ggarrange(bonney, hoare, fryxell, 
          nrow = 1)

##### HOTSPOT ANALYSIS USING Getis-Ord-Gi

### hotspot analysis using Getis-Ord Gi
library(sf)
library(sfdep)
library(tidyverse)

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250308"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_BON.*\\.tif$", full.names = TRUE)

# Function to extract date from filename (modify regex pattern as needed)
extract_date <- function(filename) {
  str_extract(basename(filename), "\\d{4}-\\d{2}-\\d{2}")  # Example: Extracts YYYY-MM-DD
}

# Load rasters and assign a date from filename
raster_list <- lapply(tif_files, function(file) {
  r <- rast(lapply(file, function(f) rast(f)[[2]]))  # Load raster
  names(r) <- "value"  # Ensure all bands are under a common name
  df <- as.data.frame(r, xy = TRUE)  # Convert raster to dataframe
  df$scene_date <- extract_date(file)  # Extract and add scene date
  return(df)
})


# Combine all rasters into one dataframe
df_all <- bind_rows(raster_list)


df_all |> 
  ggplot(aes(fill = value)) +
  geom_sf(color = "black", lwd = 0.15)


crimes_raw |> 
  ggplot(aes(fill = r)) +
  geom_sf(color = "black", lwd = 0.15)


crime_nbs <- crimes_raw |> 
  mutate(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    robbery_lag = st_lag(robbery, nb, wt)
  ) 

crime_nbs |> 
  ggplot(aes(fill = robbery_lag)) +
  geom_sf(color = "black", lwd = 0.15)

# the lag shows a lot of smoothing and the amount of 
# crime in the neighborhood being a real smooth gradident
# is there any global clustering? 

global_g_test(crime_nbs$robbery, crime_nbs$nb, crime_nbs$wt)

# suggests there is clustering. low values indicates 
# there might be cold clusters
# we can calciulate the Gi using local_g_perm()

crime_hot_spots <- crime_nbs |> 
  mutate(
    Gi = local_g_perm(robbery, nb, wt, nsim = 499)
  ) |> 
  unnest(Gi)

crime_hot_spots |> 
  ggplot((aes(fill = gi))) +
  geom_sf(color = "black", lwd = 0.15) +
  scale_fill_gradient2()

# this cursory visualization is informative!
# colors are backwards and we're seeing _all_ locationns
# not just significant ones.

# lets classify these in 7 different categories
# very hot (cold), cold (hot), somewhat hot (cold), insignificant

# very = p < 0.01
# cold/hot = p <= 0.05
# somewhat = p <= 0.1


gg <- crime_hot_spots |> 
  select(gi, p_folded_sim) |> 
  mutate(
    classification = case_when(
      gi > 0 & p_folded_sim <= 0.01 ~ "Very hot",
      gi > 0 & p_folded_sim <= 0.05 ~ "Hot",
      gi > 0 & p_folded_sim <= 0.1 ~ "Somewhat hot",
      gi < 0 & p_folded_sim <= 0.01 ~ "Very cold",
      gi < 0 & p_folded_sim <= 0.05 ~ "Cold",
      gi < 0 & p_folded_sim <= 0.1 ~ "Somewhat cold",
      TRUE ~ "Insignificant"
    ),
    # we now need to make it look better :) 
    # if we cast to a factor we can make diverging scales easier 
    classification = factor(
      classification,
      levels = c("Very hot", "Hot", "Somewhat hot",
                 "Insignificant",
                 "Somewhat cold", "Cold", "Very cold")
    )
  ) |> 
  ggplot(aes(fill = classification)) +
  geom_sf(color = "black", lwd = 0.1) +
  scale_fill_brewer(type = "div", palette = 5) +
  theme_void() +
  labs(
    fill = "Hot Spot Classification",
    title = "Robbery Hot Spots in Metro Atlanta"
  )













