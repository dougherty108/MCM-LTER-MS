# create a mean output value
# Loop through each file
library(raster)
library(stars)
library(sf)
library(dplyr)
library(tibble)
library(stringr)

setwd("~charliedougherty")

files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/20250218", pattern = ".tif")

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250218")

output <- tibble(
  date = character(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)


# Loop through each file
for (i in 1:length(files)) {
  raster_file <- raster(files[[i]])
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
  date <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Create output tibble
  output <- bind_rows(output, tibble(
    date = date, 
    `Lake Fryxell` = results$LFBB,
    `Lake Hoare` = results$LHBB, 
    `East Lake Bonney` = results$ELBBB, 
    `West Lake Bonney` = results$WLBBB
  ))
  
  # Print iteration index and output
  print(i)
}

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

output_tosave = output |> 
  pivot_longer(cols = c(`East Lake Bonney`, `Lake Hoare`, `Lake Fryxell`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |> 
  mutate(date = ymd(date))

write_csv(output_tosave, "data/sediment abundance data/LANDSAT_sediment_abundances_20250228.csv")

#plot
ggplot(output_tosave, aes(date, sediment)) + 
  geom_path() + 
  facet_wrap(vars(lake))




