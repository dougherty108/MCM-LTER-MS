###### sediment analysis #######

# library
library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)

setwd("~/Documents/R-Repositories/MCM-LTER")

# create lists
output <- list()
mean_value <- list()
year <- list()

# LF BB coords -77.610275	163.146877
# EPSG:3031 -1293198.163127, 391748.223282

setwd("~/Documents/R-Repositories/MCM-LTER/data/ENVI Unmixing")
files <- list.files(pattern = ".tif")
year = integer()
mean_value = integer()
output <- list()
year <- list()

buffer_distance <- c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300, 500, 1000)

buffer.output = list()
for(i in 1:length(buffer_distance)) {
  file.output = list()
  
  for(j in 1:length(files)) {
    raster_file <- raster(files[[j]])
    raster_df <- as.data.frame(raster_file, xy = TRUE)
    colnames(raster_df)[3] <- "value"
    raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
    
    point <- st_sfc(st_point(c(391748.223282, -1293198.163127)))
    point_sf <- st_sf(geom = point)
    
    buffered_point <- st_buffer(point_sf, dist = buffer_distance[[i]])
    sf_cropped <- st_intersection(buffered_point, raster_sf)
    
    final_raster <- st_rasterize(sf_cropped |> dplyr::select(value, geom))
    df_buffered <- as.data.frame(final_raster, xy = TRUE)
    
    mean_value <- mean(df_buffered$value, na.rm = T)
    #mean_value[[i]] <- mean(df_buffered$value, na.rm = T)
    year <- str_extract(files[[j]], "20[:digit:][:digit:]")
    
    file.output[[j]] = data.frame(mean_value = mean_value, 
                                  year = year, 
                                  buffer_distance = buffer_distance[[i]])
  }
  # unlist to dataframe 
  buffer.output[[i]] = bind_rows(file.output)
  print("complete")
}

# unlist to dataframe. 
final.output = bind_rows(buffer.output) |> 
  mutate(mean_value = mean_value*100, 
         year = as.numeric(year), 
         buffer_distance = as.factor(buffer_distance))

# boxplot to assess variability
ggplot(final.output, aes(year, mean_value, fill = buffer_distance)) + 
  geom_boxplot() + 
  ggtitle("Mean sediment coverage by year over different buffering distances")

# linear regression by year faceted
ggplot(final.output, aes(year, mean_value)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) + 
  facet_wrap(~buffer_distance) + 
  ggtitle("Sediment coverage values over time by different buffering distances")

# plot all together with differnt lines for each buffer distance
ggplot(final.output, aes(year, mean_value)) + 
  geom_smooth(method = "lm", aes(color = buffer_distance), se = F) + 
  geom_point() + 
  ggtitle("lm by buffer distance")

# apply linear regression to each different buffer distance

lm_all <- final.output |> 
  group_by(buffer_distance) |> 
  reframe(coef = coef(lm(year ~ mean_value))) |> 
  filter(coef < 1.00)

# plot of coefficient 
ggplot(lm_all, aes(buffer_distance, coef)) + 
  geom_col() + 
  ggtitle("coefficient of variance by buffer distance")

# 
fitted_models = final.output |> 
  group_by(buffer_distance) |> 
  do(model = lm(year~mean_value)) |> 
  print()


