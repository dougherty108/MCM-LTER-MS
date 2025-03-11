########### TV_Lakes Wind and Sediment ###########

# goal here is to find the biggest difference in sediment cover year 
# and see what the wind is doing when the jumps are super big


#libraries
library(tidyverse)
library(lubridate)

# load weather data and bind into one file
bonney_met <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time))

BOYM <- bonney_met |> 
  mutate(year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "East Lake Bonney") |> 
  select(c(metlocid, year, month, week, lake, date_time, wspd_ms, wspdmax_ms, wspdmin_ms, wdir_deg))

hoare_met <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") 

HOEM <- hoare_met |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Hoare") |> 
  select(c(metlocid, year, month, week, lake, date_time, wspd_ms, wspdmax_ms, wspdmin_ms, wdir_deg))

fryxell_met <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_frlm_15min-20250205.csv") 

FRLM <- fryxell_met |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Fryxell") |> 
  select(c(metlocid, year, month, week, lake, date_time, airtemp_3m_degc, wspd_ms, swradin_wm2))