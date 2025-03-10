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
         week = week(date_time)) |> 
  select(c(metlocid, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))

HOEM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time)) |> 
  select(c(metlocid, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))

FRLM <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_frlm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time)) |> 
  select(c(metlocid, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))

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

