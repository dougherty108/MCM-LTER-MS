#### TV Lakes wind vs. Whole lake average ####

# load packages
library(tidyverse)
library(lubridate)
library(MetBrewer)
library(ggpubr)

# load files
#set working directory
setwd("~/Documents/R-Repositories/MCM-LTER")

## define function to create seasons
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Winter ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Winter ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Winter ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Spring ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Spring ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Summer ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Summer ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Fall ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Winter ", year))  # October is Fall
  }
}

#load file
alllakes <- read_csv("data/sediment abundance data/sediment_abundance_wholelake_20250211.csv") |> 
  mutate(date = ymd(date), 
         mean_coverage = mean_coverage*100, 
         year = year(date), 
         month = month(date))



### add in the wind data (Bonney, Fryxell, Hoare)
FRLM <- read_csv("data/met data/FRLM/mcmlter-clim_frlm_hourly-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > "2015-01-01 00:00:00") |> 
  select(date_time, metlocid, wspd_ms, wspdmax_ms, wdir_deg)

HOEM <- read_csv("data/met data/HOEM/mcmlter-clim_hoem_hourly-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > "2015-01-01 00:00:00") |> 
  select(date_time, metlocid, wspd_ms, wspdmax_ms, wdir_deg)

BOYM <- read_csv("data/met data/BOYM/mcmlter-clim_boym_hourly-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > "2015-01-01 00:00:00") |> 
  select(date_time, metlocid, wspd_ms, wspdmax_ms, wdir_deg)

# join into one big wind file
big3 <- rbind(FRLM, HOEM, BOYM) |> 
  mutate(month = month(date_time), 
         year = year(date_time)) |> 
  mutate(season = sapply(date_time, get_season), 
         lake = metlocid) |> 
  select(-c(metlocid))

big3$lake[big3$lake== "frlm"] = "Lake Fryxell"
big3$lake[big3$lake== "hoem"] = "Lake Hoare"
big3$lake[big3$lake== "boym"] = "Lake Bonney"

# plot wind data for all three stations to find data gaps (if they exist)
ggplot(big3, aes(date_time, wspd_ms)) + 
  geom_point() + 
  facet_wrap(vars(lake)) +
  theme_minimal()

# plot sediment data to get a vibe of what was going on
ggplot(alllakes, aes(date, mean_coverage)) + 
  geom_point() + 
  facet_wrap(vars(lake, season), scales = "free") + 
  theme_minimal()

# what metric to use to compare wind to sediment
# want to get an idea of how wind might be driving surface sediment
# if the theory is that large wind events do the bulk of work, then we need to find the large wind events
# although this may not be true. In fluvial geomorphology, the majority of work is done by mid-level flows that occur more 
# frequently. The same concept may hold true for aeolian dispersement, but who knows
wind_summary <- big3 |> 
  group_by(month, year, lake) |> 
  summarize(date_time = date_time, 
            mean_wspd = mean(wspd_ms, na.rm = T), 
            mean_wspdmax = mean(wspdmax_ms, na.rm = T),
            max_wspdmax = max(wspdmax_ms, na.rm = T), 
            mean_dir = mean(wdir_deg, na.rm = T), 
            season = season) 

# plot summaries

windplot <- ggplot(wind_summary, aes(date_time, max_wspdmax, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  theme_minimal()

sedplot <- ggplot(alllakes, aes(date, mean_coverage, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(lake, season), scales = "free") + 
  theme_minimal()

ggarrange(windplot, sedplot, nrow = 2)










