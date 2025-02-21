##### TVLakes Whole Lake Sediment ########

#libraries
library(tidyverse)
library(lubridate)
library(broom)

#set working directory
setwd("Documents/R-Repositories/MCM-LTER")

#load file
alllakes <- read_csv("data/whole_lake_sediment_abundance.csv") |> 
  mutate(date = ymd(date), 
         mean_coverage = mean_coverage*100, 
         year = year(date), 
         month = month(date))

## test to fix the seasons issue
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Summer ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Summer ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Summer ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Fall ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Fall ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Winter ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Winter ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Spring ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Summer ", year))  # October is Fall
  }
}

# Apply the function and group by season
alllakes <- alllakes |> 
  mutate(season = sapply(date, get_season))

#create a plot to visualize the output by lake over all years. 
ggplot(alllakes, aes(date, mean_coverage)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  xlab("Date") + ylab("Estimate Sediment Coverage (%)") + 
  ggtitle("Whole Lake Sediment Concentration") +
  theme_bw(base_size = 10)

# create one that visualizes sediment concentration by year, with multiple facets for year and all lakes on the same plot
ggplot(alllakes, aes(date, mean_coverage, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(year), scales = "free")

## create one that visualizes sediment concentration by month, with multiple facets for month and all lakes on the same plot
ggplot(dates_grouped, aes(date, mean_coverage, fill= lake, color = lake)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(vars((season)), scales = "free") + 
  theme_bw(base_size = 10) + 
  xlab("Date") + ylab("Whole Lake Sediment Percentage (%)") + 
  ggtitle("Whole Lake Sediment Concentration by season across TV Lakes")


#### create lm for each lake in each season and report out slopes, r_square, p values
lm_models <- alllakes |> 
  group_by(season, lake) |> 
  do(model = lm(mean_coverage ~ date, data = alllakes)) |> 
  mutate(tidy_model = purrr::map(model, broom::tidy)) |> 
  unnest(tidy_model)

library(purrr)
results <- alllakes %>%
  group_by(season, lake) %>%
  nest() %>%
  mutate(model = map(alllakes, ~ lm(mean_coverage ~ date, data = alllakes))) %>%
  mutate(tidy_model = map(model, broom::tidy)) %>%
  unnest(tidy_model)

# plot to show the shapefiles to crop the lakes to
# load raster_file to do conversions to all polygons below.
setwd("/Users/charliedougherty")
raster_file <- raster("Google Drive/My Drive/SMA_EarthEngine_20240907/spectral_unmixing_sde_2016-11-04.tif")

#elb polygon
lk_east_shp <- read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/East Lake Bonney.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  select(-Description) |> 
  st_zm()

#lh polygon
lk_hoare_shp <- read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/Lake Hoare Shapefile.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  select(-Description) |> 
  st_zm()

#lf polygon
lk_fryxell_shp <- read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/Lake Fryxell Shapefile.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  select(-Description) |> 
  st_zm()

#wlb linestring
lk_west_shp <-read_sf("Documents/R-Repositories/MCM-LTER/data/shapefiles/West Lake Bonney.kml") |> 
  st_cast("POLYGON") |> 
  st_transform(crs = crs(raster_file)) |> 
  select(-Description) |> 
  st_zm()

## load example file and try cropping a lake to the kml above, to see if it works
raster_file <- raster("Google Drive/My Drive/SMA_EarthEngine_20240907/spectral_unmixing_sde_2016-11-13.tif")
raster_orig <- as.data.frame(raster_file, xy = TRUE) |> 
  drop_na()

raster_cropped <- crop(raster_file, extent(lk_east_shp))
raster_mask <- mask(raster_cropped, lk_east_shp)
raster_df <- as.data.frame(raster_mask, xy = TRUE) |> 
  drop_na()

## shapefile check, are the outlines doing a good job of summarizing the lake surface (i.e. ice instead of surrounding hillslope)
## output: seems like these ones do a pretty good job! 
ggplot() +
  geom_raster(data = raster_orig, aes(x = x, y = y, fill = band_0)) +
  geom_sf(data = lk_east_shp, fill = NA, color = "red", size = 10) +  # Outline the polygon
  geom_sf(data = lk_west_shp, fill = NA, color = "red", size = 4) +  # Outline the polygon
  geom_sf(data = lk_hoare_shp, fill = NA, color = "red", size = 4) +  # Outline the polygon
  geom_sf(data = lk_fryxell_shp, fill = NA, color = "red", size = 4) +  # Outline the polygon
  scale_fill_viridis(option = "D", name = "Raster Value") +
  coord_sf() +
  labs(title = "Raster with Polygon Overlay", x = "Northing", y = "Easting") +
  theme_minimal()




