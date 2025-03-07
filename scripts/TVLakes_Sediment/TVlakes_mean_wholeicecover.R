# Load necessary libraries
library(sf)
library(raster)
library(dplyr)

# Load the list of raster files
setwd("/Users/charliedougherty")
files <- list.files(path = "Google Drive/My Drive/EarthEngine/landsat/20250301", pattern = ".tif", full.names = TRUE)

# Load and transform polygons
lk_east_shp <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = st_crs(raster(files[1]))) |> 
  #select(-Description) |> 
  st_zm()

lk_hoare_shp <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/Lake Hoare Shapefile.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = st_crs(raster(files[1]))) |> 
  #select(-Description) |> 
  st_zm()

lk_fryxell_shp <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/Lake Fryxell Shapefile.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = st_crs(raster(files[1]))) |> 
  #select(-Description) |> 
  st_zm()

lk_west_shp <- read_sf("Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/West Lake Bonney.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = st_crs(raster(files[1]))) |> 
  #select(-Description) |> 
  st_zm()

# Initialize output list
results <- list(
  `Lake Hoare` = as.numeric(), 
  `Lake Fryxell` = 
  mean_fry <- NA,
  mean_hor <- NA,
  mean_eas <- NA,
  mean_wes <- NA
)

# Loop through each raster file
for(i in 1:length(files)) {
  raster_file <- raster(files[i])
  
  date = str_extract(files[i], "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]")
  
  # Lake Fryxell
  raster_crop_fry <- crop(raster_file, extent(lk_fryxell_shp))
  if (!is.null(raster_crop_fry) && any(!is.na(values(raster_crop_fry)) & values(raster_crop_fry) > 0)) {
    raster_mask_fry <- mask(raster_crop_fry, lk_fryxell_shp)
    raster_df_fry <- as.data.frame(raster_mask_fry, xy = TRUE) #|> drop_na()
    mean_fry <- mean(raster_df_fry$B2, na.rm = TRUE)
    #median_fry <- median(raster_df_fry$band_0, na.rm = TRUE)
  }
  
  # Lake Hoare
  raster_crop_hor <- crop(raster_file, extent(lk_hoare_shp))
  if (!is.null(raster_crop_hor) && any(!is.na(values(raster_crop_hor)) & values(raster_crop_hor) > 0)) {
    raster_mask_hor <- mask(raster_crop_hor, lk_hoare_shp)
    raster_df_hor <- as.data.frame(raster_mask_hor, xy = TRUE) #|> drop_na()
    mean_hor <- mean(raster_df_hor$B2, na.rm = TRUE)
    #median_hor <- median(raster_df_hor$band_0, na.rm = TRUE)
  }
  
  # East Lake Bonney
  raster_crop_eas <- crop(raster_file, extent(lk_east_shp))
  if (!is.null(raster_crop_eas) && any(!is.na(values(raster_crop_eas)) & values(raster_crop_eas) > 0)) {
    raster_mask_eas <- mask(raster_crop_eas, lk_east_shp)
    raster_df_eas <- as.data.frame(raster_mask_eas, xy = TRUE) #|> drop_na()
    mean_eas <- mean(raster_df_eas$B2, na.rm = TRUE)
    #median_eas <- median(raster_df_eas$band_0, na.rm = TRUE)
  }
  
  # West Lake Bonney
  raster_crop_wes <- crop(raster_file, extent(lk_west_shp))
  if (!is.null(raster_crop_wes) && any(!is.na(values(raster_crop_wes)) & values(raster_crop_wes) > 0)) {
    raster_mask_wes <- mask(raster_crop_wes, lk_west_shp)
    raster_df_wes <- as.data.frame(raster_mask_wes, xy = TRUE) #|> drop_na()
    mean_wes <- mean(raster_df_wes$B2, na.rm = TRUE)
    #median_wes <- median(raster_df_wes$band_0, na.rm = TRUE)
  }
  
  # Store results for the current raster
  results[[i]] <- list(
    date = date,
    `Lake Fryxell` = mean_fry,
    `Lake Hoare` = mean_hor,
    `East Lake Bonney` = mean_eas,
    `West Lake Bonney` = mean_wes
  )
  
  # Print the results for the current iteration
  print(paste("File:", files[i]))
  print(results[[i]])
}

# bind everything into a dataframe
output_df = bind_rows(results) |> 
  pivot_longer(cols = c(`Lake Fryxell`, `Lake Hoare`, `East Lake Bonney`, `West Lake Bonney`), 
               names_to = "lake", values_to = 'mean_coverage') |> 
  mutate(mean_coverage = 1-mean_coverage, 
         date = ymd(date), 
         year = year(date), 
         month = month(date))

#save output
#write_csv(output_df, "Documents/R-Repositories/MCM-LTER/data/whole_lake_sediment_abundance.csv")

## test to fix the seasons issue
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Winter ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Winter ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Winter ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Spring ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Spring ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Summer ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Summer ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Fall ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Winter ", year))  # October is Fall
  }
}

# Apply the function and group by season
dates_grouped <- output_df |> 
  mutate(season = sapply(date, get_season))

#create a plot to visualize the output by lake over all years. 
ggplot(output_df, aes(date, mean_coverage)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  xlab("Date") + ylab("Estimate Sediment Coverage (%)") + 
  ggtitle("Whole Lake Sediment Concentration")

# create one that visualizes sediment concentration by year, with multiple facets for year and all lakes on the same plot
ggplot(output_df, aes(date, mean_coverage, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(year), scales = "free")

## create one that visualizes sediment concentration by month, with multiple facets for month and all lakes on the same plot
ggplot(dates_grouped, aes(date, mean_coverage, color = lake)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(vars((season)), scales = "free") + 
  theme_bw(base_size = 10) + 
  xlab("Date") + ylab("Whole Lake Sediment Percentage (%)") + 
  ggtitle("Whole Lake Sediment Concentration by season across TV Lakes")

######### write the datafile as an output
setwd("/Users/charliedougherty/Documents/R-Repositories/MCM-LTER")
write_csv(dates_grouped, "data/sediment abundance data/sediment_abundance_wholelake_20250211.csv")


####### updated version of script using extract function: 
## libraries
library(raster)
library(sf)
library(tidyverse)

setwd("~charliedougherty")

files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/20250301", pattern = ".tif", full.names = TRUE)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250301")

# Predefine output tibble
output <- tibble(
  date = character(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

# shapefiles
setwd("/Users/charliedougherty")

# Load required libraries
library(sf)
library(raster)
library(dplyr)
library(stringr)

files <- list.files(path = "Google Drive/My Drive/EarthEngine/landsat/20250301", pattern = ".tif", full.names = TRUE)

# Load and transform polygons
lake_shapefiles <- list(
  "Lake Fryxell" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/Lake Fryxell Shapefile.kml",
  "Lake Hoare" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/Lake Hoare Shapefile.kml",
  "East Lake Bonney" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml",
  "West Lake Bonney" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/West Lake Bonney.kml"
)

lakes_sf <- lapply(lake_shapefiles, function(shp) {
  read_sf(shp) |> 
    st_cast("POLYGON") |> 
    st_transform(crs = st_crs(raster(files[1]))) |> 
    st_zm()
})

setwd("~charliedougherty")
output <- tibble()

# Loop through each raster file
for (i in seq_along(files)) {
  raster_file <- raster(files[i])
  
  # Extract mean values within each lake polygon
  extracted_values <- sapply(lakes_sf, function(lake) {
    cropped_raster <- mask(crop(raster_file, lake), lake)
    cellStats(cropped_raster, stat = 'mean', na.rm = TRUE)
  })
  
  # Extract date from filename
  date <- str_extract(files[i], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Append results to output tibble
  output <- bind_rows(output, tibble(
    date = date, 
    `Lake Fryxell` = extracted_values["Lake Fryxell"],
    `Lake Hoare` = extracted_values["Lake Hoare"], 
    `East Lake Bonney` = extracted_values["East Lake Bonney"], 
    `West Lake Bonney` = extracted_values["West Lake Bonney"]
  ))
  
  print(i)  # Keep track of progress
}

output_for_save <- output |> 
  pivot_longer(cols = c(`East Lake Bonney`, `Lake Hoare`, `Lake Fryxell`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |>
  drop_na() |> 
  mutate(date = ymd(date), 
         ice_abundance = sediment, 
         sediment_abundance = 1-sediment) |> 
  drop_na()

write_csv(output_for_save, "Documents/R-Repositories/MCM-LTER-MS/data/sediment abundance data/LANDSAT_wholelake_mean_20250307.csv")

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
