###### sediment analysis #######

# library
library(raster)
library(sf)
library(tidyverse)
library(lubridate)

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

df <- output_df |> 
  mutate(mean_value = as.numeric(mean_value)*100, 
         year = as.numeric(year))

# plot sediment values over time with lin reg (appears to be increasing)
ggplot(df, aes(year, mean_value)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  ggtitle("Sediment coverage over time.")


# create the linear regression model
lm <- lm(year~mean_value, df)
summary(lm)

### add in some stuff about radiation to see if there is anything going on there
setwd("~/Documents/R-Repositories/MCMLTER")


# load files
radn_daily <- read_csv("data/FRLM/mcmlter-clim-frlm_radn-daily-20230605.csv") |> 
  mutate(date_time = mdy(date_time), 
         month = month(date_time, label = T), 
         year = year(date_time)) |> 
  filter(month == c("Oct", "Nov", "Dec", "Jan"))

radn_15 <- read_csv("data/FRLM/mcmlter-clim-frlm_radn-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time, label = T))


radn_summary <- radn_daily |> 
  filter(date_time > "2013-01-10") |> 
  group_by(year) |> 
  summarize(mean_sw = mean(avg_swradin)) |> 
  mutate(year = as.character(year))


ggplot(radn_summary, aes(year, mean_sw)) + 
  geom_col(fill = "lightblue") + 
  ggtitle("Mean Shortwave Radiation NDJF", 
          subtitle = "FRLM")


# add in lake ice thickness 
lakeice <- read_csv("data/Lake Ice/mcmlter-lake-ice_thickness-20230726.csv") |> 
  mutate(date_time = mdy_hm(date_time),
         month = month(date_time, label = T), 
         year = year(date_time)) |> 
  filter(lake == "Lake Fryxell" &
           date_time > "2014-01-01", 
         month == "Dec" |
           month == "Jan")


lake_summary = lakeice |> 
  group_by(year) |> 
  summarize(mean_thickness = abs(mean(z_water_m, na.rm = T))) |> 
  mutate(year = as.character(year))

total_summary <- full_join(radn_summary, lake_summary, by = "year")
df2 <- df |> 
  mutate(year = as.character(year))

total_summary <- full_join(df2, total_summary, by = "year")

# plot shortwave against thickness
ggplot(total_summary, aes(mean_sw, mean_thickness)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# bar graph of sediment coverage
ggplot(df, aes(year, mean_value)) + 
  geom_col(fill = "#4287f5") + 
  theme_minimal() +
  ggtitle("Sediment coverage (%)", 
          subtitle = "Lake Fryxell, 2014-2024") + 
  ylab("Percentage Sediment (%)") + xlab("Year") + 
  theme(text = element_text(size = 15)) 

ggsave("plots/sed_plot.png", width = 14, height = 8, units = "in", dpi = 700)

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


# run a bunch of linear regressions on stuff
lm_thicksed <- lm(mean_value~mean_thickness, total_summary) |> 
  print()
summary(lm_thicksed)

lm_sedsw <- lm(mean_value~mean_sw, total_summary) |> 
  print()
summary(lm_sedsw)

lm_thicksw <- lm(mean_sw~mean_thickness, total_summary) |> 
  print()
summary(lm_thicksw)
