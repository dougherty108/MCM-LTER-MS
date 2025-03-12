#### GEE vs. Ice Thickness for all BB sites #######

# load the necessary libraries
library(tidyverse)
library(lubridate)
library(raster)
library(sf)
library(stars)
library(ggpubr)

#set working directory
setwd("/Users/charliedougherty")


#### load in ice thickness data and compare 
lakeice <- read_csv("Documents/R-Repositories/MCM-LTER/data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv")

# load empty lists to be used in the for loop
date = list()
mean_LF = list()
mean_LH = list()
mean_EL = list()
mean_WL = list()

# load the files list in for the loop
files = dir(path = "Google Drive/My Drive/SMA_EarthEngine/20240907/", pattern = ".tif")

#for loop, this will produce mean values at a 300 buffer distance to be compared to ice thicknesses
#this is using the coordinates for each blue box pulled out the Master GPS List 2022-23 in the Doran
for(i in 1:length(files)) {
  raster_file <- raster(paste0("Google Drive/My Drive/SMA_EarthEngine/20240907/", files[[i]]))
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  colnames(raster_df)[3] <- "value"
    raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
    
    LFBB_point <- st_sfc(st_point(c(391748.223282, -1293198.163127)))
    LFBB_point_sf <- st_sf(geom = LFBB_point)
    
    LHBB_point <- st_sfc(st_point(c(396517.85394052055, -1289740.3825915689)))
    LHBB_point_sf <- st_sf(geom = LHBB_point)
    
    ELBBB_point <- st_sfc(st_point(c(404047.2666197109, -1277516.4884229063)))
    ELBBB_point_sf <- st_sf(geom = ELBBB_point)
    
    WLBBB_point <- st_sfc(st_point(c(407169.73944380396, -1275776.8988470172)))
    WLBBB_point_sf <- st_sf(geom = WLBBB_point)
    
    buffer_distance <- 300
    LF_buffered_point <- st_buffer(LHBB_point_sf, dist = buffer_distance)
    LH_buffered_point <- st_buffer(LHBB_point_sf, dist = buffer_distance)
    EL_buffered_point <- st_buffer(ELBBB_point_sf, dist = buffer_distance)
    WL_buffered_point <- st_buffer(WLBBB_point_sf, dist = buffer_distance)
    
    LF_crop <- st_intersection(LF_buffered_point, raster_sf)
    LH_crop <- st_intersection(LH_buffered_point, raster_sf)
    EL_crop <- st_intersection(EL_buffered_point, raster_sf)
    WL_crop <- st_intersection(WL_buffered_point, raster_sf)
    
    LF_final <- st_rasterize(LF_crop |> dplyr::select(value, geom))
    LH_final <- st_rasterize(LH_crop |> dplyr::select(value, geom))
    EL_final <- st_rasterize(EL_crop |> dplyr::select(value, geom))
    WL_final <- st_rasterize(WL_crop |> dplyr::select(value, geom))
    
    LF_buffered <- as.data.frame(LF_final, xy = TRUE)
    LH_buffered <- as.data.frame(LH_final, xy = TRUE)
    EL_buffered <- as.data.frame(EL_final, xy = TRUE)
    WL_buffered <- as.data.frame(WL_final, xy = TRUE)
    
    
    mean_LF = mean(LF_buffered$value, na.rm = T)
    mean_LH = mean(LH_buffered$value, na.rm = T)
    mean_EL = mean(EL_buffered$value, na.rm = T)
    mean_WL = mean(WL_buffered$value, na.rm = T)
    
    date[[i]] <- str_extract(files[[i]], "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]")
    
    output = tibble(
      date = year, 
      fryxell = mean_LF,
      hoare = mean_LH, 
      eastlobe = mean_EL, 
      westlobe = mean_WL
    )
    
    print(i)
}


output_df = data.frame(output) |> 
  mutate(date = ymd(date), 
         value = as.numeric(value), 
         converted_value = 1-value)

library(raster)
library(sf)
library(dplyr)
library(tibble)
library(stringr)

# Loop through each file
for (i in 1:length(files)) {
  # Load raster and convert to data frame
  raster_file <- raster(paste0("Google Drive/My Drive/SMA_EarthEngine/", files[[i]]))
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> drop_na()
  colnames(raster_df)[3] <- "value"
  
  # Convert to sf object
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  # Define points and create buffered zones
  points <- list(
    LFBB = st_sfc(st_point(c(391748.223282, -1293198.163127))),
    LHBB = st_sfc(st_point(c(396517.85394052055, -1289740.3825915689))),
    ELBBB = st_sfc(st_point(c(404047.2666197109, -1277516.4884229063))),
    WLBBB = st_sfc(st_point(c(407169.73944380396, -1275776.8988470172)))
  )
  
  points_sf <- st_sf(geom = st_sfc(points))
  
  buffer_distance <- 300
  buffered_points_sf <- st_buffer(points_sf, dist = buffer_distance)
  
  # Initialize list to store results
  results <- list()
  
  # Process each point
  for (j in 1:length(points)) {
    tryCatch({
      cropped_sf <- st_intersection(buffered_points_sf[j, ], raster_sf)
      final_raster <- st_rasterize(cropped_sf |> dplyr::select(value, geom))
      buffered_df <- as.data.frame(final_raster, xy = TRUE)
      mean_value <- mean(buffered_df$value, na.rm = TRUE)
      results[[names(points)[j]]] <- mean_value
    }, error = function(e) {
      message(paste("Error in st_rasterize for point", names(points)[j], ":", e$message))
      next
    })
  }
  
  # Extract date from filename
  date[[i]] <- str_extract(files[[i]], "20[:digit:]{2}-[:digit:]{2}-[:digit:]{2}")
  
  # Create output tibble
  output <- tibble(
    date = date[[i]], 
    fryxell = results$LFBB,
    hoare = results$LHBB,
    eastlobe = results$ELBBB,
    westlobe = results$WLBBB
  )
  
  # Print iteration index and output
  print(i)
  print(output)
}


## updated code from chagpt

# Initialize date list
date <- list()

library(raster)
library(sf)
library(dplyr)
library(tibble)
library(stringr)
library(purrr) # For map functions

# Initialize date list
date <- list()


# Initialize date list
date <- list()

# Loop through each file
library(raster)
library(sf)
library(dplyr)
library(tibble)
library(stringr)

# Initialize date list
date <- list()

# Loop through each file
for (i in 1:length(files)) {
  # Load raster and convert to data frame
  raster_file <- raster(paste0("Google Drive/My Drive/SMA_EarthEngine/", files[[i]]))
  #raster_crs <- crs(raster_file)  # Extract CRS from raster
  
  raster_df <- as.data.frame(raster_file, xy = TRUE) %>% drop_na()
  colnames(raster_df)[3] <- "value"
  
  # Convert to sf object
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  # Define and buffer points
  points <- list(
    LFBB = st_point(c(391748.223282, -1293198.163127)),
    LHBB = st_point(c(396517.85394052055, -1289740.3825915689)),
    ELBBB = st_point(c(404047.2666197109, -1277516.4884229063)),
    WLBBB = st_point(c(407169.73944380396, -1275776.8988470172))
  )
  
  points_sf <- st_sf(name = names(points), geom = st_sfc(points))
  
  buffer_distance <- 300
  buffered_points_sf <- st_buffer(points_sf, dist = buffer_distance)
  
  # Initialize list to store results
  results <- list()
  
  # Process each buffered point
  for (point_name in names(points)) {
    tryCatch({
      # Extract the specific buffered point
      buffered_point <- buffered_points_sf %>% filter(name == point_name)
      
      # Check if the buffered point intersects with the raster
      if (st_intersects(buffered_point, raster_sf, sparse = FALSE) %>% any()) {
        cropped_sf <- st_intersection(buffered_point, raster_sf)
        
        if (nrow(cropped_sf) > 0) {
          final_raster <- st_rasterize(cropped_sf %>% dplyr::select(value, geom))
          buffered_df <- as.data.frame(final_raster, xy = TRUE)
          mean_value <- mean(buffered_df$value, na.rm = TRUE)
          results[[point_name]] <- mean_value
        } else {
          results[[point_name]] <- NA  # No data in intersection
        }
      } else {
        results[[point_name]] <- NA  # No intersection with raster
      }
      
    }, error = function(e) {
      message(paste("Error in processing point", point_name, ":", e$message))
      results[[point_name]] <- NA  # Assign NA in case of error
    })
  }
  
  # Extract date from filename
  date[[i]] <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Create output tibble
  output <- tibble(
    date = date[[i]], 
    fryxell = results$LFBB,
    hoare = results$LHBB, 
    eastlobe = results$ELBBB, 
    westlobe = results$WLBBB
  )
  
  # Print iteration index and output
  print(i)
  print(output)
}



   