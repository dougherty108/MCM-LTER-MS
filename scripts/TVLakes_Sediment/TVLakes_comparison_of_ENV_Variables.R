####### SCRIPT TO INVESTIGATE THE INFLUENCE OF WIND, AIR TEMP, OR SOLAR RADIANCE 
# ON ICE THICKNESS. important because if there's a strong relation between these values, then
# that changes how we understand the sediment stuff. 

# libraries
library(tidyverse)
library(lubridate)

# met data
setwd("~charliedougherty")
BOYM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "East Lake Bonney") |> 
  select(c(metlocid, year, month, week, lake, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))

HOEM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Hoare") |> 
  select(c(metlocid, year, month, week, lake, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))

FRLM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_frlm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Fryxell") |> 
  select(c(metlocid, year, month, week, lake, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))

# add in data
setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ice_thickness <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20250218_0.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         year = year(date_time), 
         month = month(date_time), 
         week = week(date_time))

# goal here is to compare weather variables with ice thicknesses. Could do a 
# weekly mean
ice_thickness_weekly <- ice_thickness |> 
  rename(lake = location_name) |> 
  group_by(year, week, lake) |> 
  summarize(z_water_m = mean((z_water_m*-1), na.rm = T), 
            z_ice_m = mean(z_ice_m, na.rm = T))

met_stations <- rbind(FRLM, HOEM, BOYM) |> 
  group_by(year, week, lake) |> 
  summarize(mean_wspd = mean(wspd_ms), 
            mean_airtemp = mean(airtemp_3m_degc), 
            mean_swradin = mean(swradin_wm2))

# join the two data sets
ice_met_peaksolar <- ice_thickness_weekly |> 
  full_join(met_stations, by = join_by(year, week, lake)) |> 
  filter(week == 50 | week == 51 | week == 52 | 
           week == 1 | week == 2 | week == 3)

# plot comparison
# mean air temp, all summer, all time
ggplot(ice_met, aes(mean_airtemp, z_water_m)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# mean sw rad, all summer, all time
ggplot(ice_met, aes(z_water_m, mean_swradin)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# mean wspd rad, all summer, all time
ggplot(ice_met, aes(z_water_m, mean_wspd)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ice_met_studyperiod <- ice_met |> 
  filter(year > 2016)


# mean air temp vs. ice thick
ggplot(ice_met_studyperiod, aes(mean_airtemp, z_water_m)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# mean 
ggplot(ice_met_studyperiod, aes(mean_swradin, z_water_m)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(ice_met_studyperiod, aes(mean_wspd, z_water_m)) + 
  geom_point() + 
  geom_smooth(method = "lm")
