########## landsat and sentinel comparison #########

library(tidyverse)

setwd("~/Documents/R-Repositories/MCM-LTER")

# load files
sentinel <- read_csv('data/sediment abundance data/SENTINEL_sediment_abundances_20250217.csv') |> 
  mutate(date = ymd(date), 
         satellite = "Sentinel 2", 
         sediment = 1-sediment)

landsat <- read_csv("data/sediment abundance data/sediment_abundances_20250207.csv") |> 
  mutate(date = ymd(date), 
         satellite = "Landsat 8", 
         sediment = 1-sediment)

# quick plot of landsat
ggplot(landsat, aes(date, sediment)) + 
  geom_line() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat") + 
  theme_minimal()

# join datasets
df <- full_join(sentinel, landsat)

## test to fix the seasons issue
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
    return(paste0("Spring ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Spring ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Winter ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Winter ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Spring ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Spring ", year))  # October is Fall
  }
}

# Apply the function and group by season
df_seasons <- df |> 
  mutate(season = sapply(date, get_season)) |> 
  filter(season == "Summer 2015" | season == "Summer 2016" | season == "Summer 2017" | 
           season == "Summer 2018" | season == "Summer 2019" |
           season == "Summer 2020" | season == "Summer 2021" | season == "Summer 2022" | 
           season == "Summer 2023" | season == "Summer 2024")

# plot data
ggplot(df_seasons, aes(date, sediment, color = satellite)) + 
  geom_line() + 
  facet_wrap(vars(season), scales = "free")


# do a filtering join to only retain dates where there was coverage from landsat and sentinel
comparison <- inner_join(landsat, sentinel)






