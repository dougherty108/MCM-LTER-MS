# ENVI vs. GEE

# libraries
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(ggpubr)

# load ENVI plots that Mark generated

setwd("~/Documents/R-Repositories/MCM-LTER/data/ENVI Unmixing")

# Fryxell mean values
fryx_ENVI <- read_csv("FRYX_sed_cover.csv") |> 
  mutate(mean_value = as.numeric(mean_value), 
         year = as.numeric(year))


# load GEE files

setwd("/Users/charliedougherty")

files = dir(path = "Google Drive/My Drive/SMA_EarthEngine_20240907", pattern = ".tif")
date <- list()

target_dir = "~/Documents/R-Repositories/MCM-LTER/plots/GEE"

files = dir(path = "Google Drive/My Drive/SMA_EarthEngine", pattern = ".tif")
date <- list()

output <- list()
year = list()
mean = list()

for(i in 1:length(files)) {
  raster_file <- raster(paste0("Google Drive/My Drive/SMA_EarthEngine/", files[[i]]))
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  colnames(raster_df)[3] <- "value"
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  point <- st_sfc(st_point(c(391748.223282, -1293198.163127)))
  point_sf <- st_sf(geom = point)
  
  buffer_distance <- 300
  buffered_point <- st_buffer(point_sf, dist = buffer_distance)
  
  sf_cropped <- st_intersection(buffered_point, raster_sf)
  
  final_raster <- st_rasterize(sf_cropped |> dplyr::select(value, geom))
  df_buffered <- as.data.frame(final_raster, xy = TRUE)
  
  mean[[i]] = mean(df_buffered$value)
  
  year[[i]] <- str_extract(files[[i]], "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]")
  
  #print(
  #  ggplot() + 
  #    geom_raster(data = df_buffered, aes(x, y, fill = value)) + 
  #    scale_fill_gradient(low = "blue", high = "yellow") + 
  #    coord_fixed() + 
  #    labs(title = paste(year), x = "Northing", y = "Easting") 
  #)
  
  output = tibble(
    date = year, 
    value = mean
  )
  
  print(i)
}

output_df = data.frame(output) |> 
  mutate(date = ymd(date), 
         value = as.numeric(value), 
         converted_value = 1-value)

ggplot(output_df, aes(date, converted_value)) + 
  geom_point() + 
  xlab("Date") + ylab("Sediment Coverage") + 
  ggtitle("GEE Generated Estimates", 
          subtitle = "LFBB") + 
  ylim(0, 0.6)

january_gee = output_df |> 
  mutate(month = month(date), 
         day = day(date)) |> 
  filter(month == 1 &
           day == c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))

## plot up the output for Fryxell BB

GEE <- ggplot(january_gee, aes(date, converted_value)) + 
  geom_point() + 
  xlab("Date") + ylab("Sediment Coverage") + 
  ggtitle("GEE Generated Estimates", 
          subtitle = "LFBB") + 
  ylim(0, 0.6)

ENVI <- ggplot(fryx_ENVI, aes(year, mean_value)) + 
  geom_point() + 
  xlab("Date") + ylab("Sediment Coverage") + 
  ggtitle("ENVI Generated Estimates", 
          subtitle = "LFBB-January estimates") + 
  ylim(0, 0.6)

ggarrange(GEE, ENVI)


# setwd("/Users/charliedougherty")


# for loop to plot all the GEE plots and save the outputs to a local destination
# crop them to the same extent that is in the ENVI plots
setwd("~/Documents/R-Repositories/MCM-LTER/data/ENVI Unmixing")
ENVI_raster <- raster("LC08_L2SR_054116_20150122_20201016_02_T2_SR_stack_fryxell_refl_unmix-sed.tif")
bounding_box <- st_bbox(ENVI_raster)

for(i in 1:length(files)) {
  setwd("/Users/charliedougherty")
  raster_file <- raster(paste0("Google Drive/My Drive/SMA_EarthEngine_20240907/", files[[i]])) |> 
    crop(bounding_box)
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  colnames(raster_df)[3] <- "value"
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  
  year <- str_extract(files[[i]], "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]")
  
  print(
    ggplot() + 
      geom_raster(data = raster_df, aes(x, y, fill = value)) + 
      scale_fill_gradient(low = "blue", high = "yellow") + 
      coord_fixed() + 
      labs(title = paste(year), x = "Northing", y = "Easting") 
  )
  
  setwd("~/Documents/R-Repositories/MCM-LTER/plots/GEE/20240923")
  ggsave(filename = paste0("Fryxell_plot_", year, ".png"))
  print(paste0("you are on plot #",i))
}


#### load in ice thickness data and compare 

lakeice <- read_csv("Documents/R-Repositories/MCM-LTER/data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv")

