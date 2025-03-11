########### TV_Lakes Wind and Sediment ###########

# goal here is to find the biggest difference in sediment cover year 
# and see what the wind is doing when the jumps are super big


#libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)


# load weather data and bind into one file
bonney_met <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv")

hoare_met <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") 

fryxell_met <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_frlm_15min-20250205.csv") 

# create function to make a season column: 
## Define a season function to plot data by season. Makes data viz a lot easier. 
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


BOYM <- bonney_met |> 
  mutate(date_time = ymd_hms(date_time),
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Bonney") |> 
  select(c(metlocid, year, month, week, lake, date_time, wspd_ms, wspdmax_ms, wspdmin_ms, wdir_deg))

HOEM <- hoare_met |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Hoare") |> 
  select(c(metlocid, year, month, week, lake, date_time, wspd_ms, wspdmax_ms, wspdmin_ms, wdir_deg))

FRLM <- fryxell_met |> 
  mutate(date_time = ymd_hms(date_time), 
         year = year(date_time),
         month = month(date_time),
         week = week(date_time), 
         lake = "Lake Fryxell") |> 
  select(c(metlocid, year, month, week, lake, date_time, wspd_ms, wspdmax_ms, wspdmin_ms, wdir_deg))

met_stations <- rbind(BOYM, HOEM, FRLM) |> 
  mutate(season = sapply(date_time, get_season))

# load sediment abundance data 
wholelake <- read_csv("data/sediment abundance data/LANDSAT_wholelake_mean_20250307.csv") |> 
  mutate(date = ymd(date), 
         type = "whole_lake", 
         week = week(date), 
         month = month(date), 
         year = year(date), 
         season = sapply(date, get_season)) |> 
  group_by(week, year, lake) |> 
  summarize(weekly_mean = mean(sediment_abundance, na.rm = T), 
            season = season) |> 
  group_by(lake) |> 
  mutate(weekly_difference = weekly_mean - lag(weekly_mean))

sed_BB <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250308.csv") |> 
  mutate(date = ymd(date), 
         type = "blue_box", 
         week = week(date), 
         month = month(date), 
         year = year(date), 
         season = sapply(date, get_season)) |> 
  group_by(week, year, lake) |> 
  summarize(weekly_mean = mean(sediment_abundance, na.rm = T), 
            season = season) |> 
  group_by(lake) |> 
  mutate(weekly_difference = weekly_mean - lag(weekly_mean))

sediment_estimates <- rbind(wholelake, sed_BB)

# plot the weekly difference
ggplot(sediment_estimates, aes(week, weekly_difference, color = lake)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") +
  facet_wrap(vars(season), scales = "free")







