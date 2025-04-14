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
mean_value <= integer()
output <- list()

for(i in 2:length(files)) {
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE)
  colnames(raster_df)[3] <- "value"
  raster_sf <- st_as_sf(raster_df, coords = c("x", "y"))
  
  point <- st_sfc(st_point(c(391748.223282, -1293198.163127)))
  point_sf <- st_sf(geom = point)
  
  buffer_distance <- 50:100
  buffered_point <- st_buffer(point_sf, dist = buffer_distance)
  print("complete")
  
  sf_cropped <- st_intersection(buffered_point, raster_sf)
  
  final_raster <- st_rasterize(sf_cropped |> dplyr::select(value, geom))
  df_buffered <- as.data.frame(final_raster, xy = TRUE)
  
  mean_value <- mean(df_buffered$value, na.rm = T)
  
  year <- str_extract(files[[i]], "20[:digit:][:digit:]")
  print("complete")
  
  output[[i]] <- c(
    mean_value = mean_value, 
    year = year
  )
}

output_df <- bind_rows(output) 

setwd("~/Documents/R-Repositories/MCM-LTER/data")

write_delim(output_df, "FRYX_sed_cover.csv", delim = ",")

df <- output_df |> 
  mutate(mean_value = as.numeric(mean_value)*100, 
         year = as.numeric(year))

# assign "high" and "low" sediment coverage values. 

# plot sediment values over time with lin reg (appears to be increasing)
ggplot(df, aes(year, mean_value)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("Sediment coverage over time.")

## eyeball and see where a good cut off for "low" and "high" values might be
# for a first stab; set it at 15% coverage. 
ggplot(df, (aes(year, mean_value))) + 
  geom_col()

# create the linear regression model
lm <- lm(year~mean_value, df)
summary(lm)

### add in some stuff about radiation to see if there is anything going on there
setwd("~/Documents/R-Repositories/MCM-LTER")

# add in lake ice thickness 
lakeice <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |> 
  mutate(date_time = mdy_hm(date_time),
         month = month(date_time, label = T), 
         year = year(date_time), 
         str = str_detect(location_name, "Outside Hole"), 
         z_water_m = z_water_m*-1, 
         month = month(date_time)
         ) |> 
  filter(lake == "Lake Fryxell", 
         str == "TRUE", 
         date_time > "2013-09-01") |> 
  select(-str) |> 
  mutate(year = if_else(month >= 10, year + 1, year)) 


# plot the linear regression of ice thickness over time by season (inclusive of January)
ggplot(lakeice, aes(date_time, z_water_m)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(~year, scales = "free_x") + 
  theme_minimal()

#Create linear regression data for each year
models <- lakeice |> 
  group_by(year) |> 
  do(model = lm(date_time ~ z_water_m, data = .))

model_summaries <- models %>%
  summarise(year, 
            intercept = coef(model)[1],
            slope = coef(model)[2],
            r_squared = summary(model)$r.squared
            )

## summary ice thickness data
lake_summary = lakeice |> 
  group_by(year) |> 
  summarize(mean_thickness = abs(mean(z_water_m, na.rm = T))) |> 
  mutate(year = as.character(year))


# thickness vs. sediment coverage
ggplot(total_summary, aes(mean_value, mean_thickness)) + 
  geom_point(size = 2.5) + 
  theme_minimal() + 
  geom_smooth(method = "lm", size = 2) +
  theme(text = element_text(size = 30)) + 
  ggtitle("Sediment coverage (%) vs mean January ice thickness", 
          subtitle = "Lake Fryxell") + 
  ylab("Ice thickness (m)") + xlab("Percentage Sediment (%)")
ggsave("plots/sed_thick_plot.png", width = 14, height = 8, units = "in", dpi = 700)


## create a new column within the df that identifies values as "high" or "low"
# setting this threshold at 15, this seems to be roughly middle. In the future, i'll need to find a better way to classify this

threshold = 15
df <- df |> 
  mutate(category = case_when(
    mean_value >= threshold ~ "high",
    TRUE ~ "low"
  ))

## join to the lake ice thickness dataset. 
lakeice_thresh <- lakeice |> 
  left_join(df, by = "year")  |> 
  drop_na(z_water_m)

# what is the difference between early season measurement to the final season's measurement year by year? 
year_diff <- lakeice_thresh |> 
  group_by(year, category
           )|> 
  summarize(mean = mean(z_water_m, na.rm = T), 
            difference = last(z_water_m) - first(z_water_m)#, 
            #sed_storage = category
            ) |> 
  print()

# plot this up in a barplot and see what's going on
# a boxplot could also be good to see level of variance each year

ggplot(year_diff, aes(year, difference, fill = category)) + 
  geom_col() + 
  ggtitle("Difference between first and last ice thickness measuremenet", 
          subtitle = "At the Lake Fryxell Outside LimnoHole")
  
## add classifications over this plot to see if anything appears interesting
ggplot(lakeice_thresh, aes(date_time, z_water_m)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(~year, scales = "free") + 
  #facet_wrap(~category) +
  theme_minimal()

