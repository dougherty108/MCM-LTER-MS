###### Sediment Rates compared to Underwater PAR ####

#Author: Charlie Dougherty

#libraries
library(tidyverse)
library(MetBrewer)


# load sediment file
setwd("~/Documents/R-Repositories/MCM-LTER-MS/")

sed = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250218.csv") |> 
  mutate(date = ymd(date))

#sed$lake[sed$lake== "fryxell"] = "Lake Fryxell"
#sed$lake[sed$lake== "hoare"] = "Lake Hoare"
#sed$lake[sed$lake== "eastlobe"] = "East Lake Bonney"
#sed$lake[sed$lake== "westlobe"] = "West Lake Bonney"

# load UW PAR File (s)
LFBB <- read_csv("data/Blue Box/LIMNO_BLUE_BOX_FRYXELL.csv") |> 
  mutate(DATE_TIME = mdy_hm(DATE_TIME)) |> 
  filter(DATE_TIME > "2016-01-01 00:00") |> 
  select(c(DATE_TIME, LOCATION.NAME, UW_PAR, UW_TEMPERATURE))

LHBB <- read_csv("data/Blue Box/LIMNO_BLUE_BOX_HOARE.csv") |> 
  mutate(DATE_TIME = mdy_hm(DATE_TIME)) |> 
  filter(DATE_TIME > "2016-01-01 00:00") |> 
  select(c(DATE_TIME, LOCATION.NAME, UW_PAR, UW_TEMPERATURE))

ELBBB <- read_csv("data/Blue Box/LIMNO_BLUE_BOX_ELBONNEY_0.csv") |> 
  mutate(DATE_TIME = mdy_hm(DATE_TIME), 
         LOCATION.NAME = LOCATION.ME) |> 
  filter(DATE_TIME > "2016-01-01 00:00") |> 
  select(c(DATE_TIME, LOCATION.NAME, UW_PAR, UW_TEMPERATURE))

WLBBB <- read_csv("data/Blue Box/LIMNO_BLUE_BOX_WLBONNEY_0.csv") |> 
  mutate(DATE_TIME = mdy_hm(DATE_TIME)) |> 
  filter(DATE_TIME > "2016-01-01 00:00") |> 
  select(c(DATE_TIME, LOCATION.NAME, UW_PAR, UW_TEMPERATURE))

# join all BB data into one big dataset and create a daily average 
BB <- rbind(LFBB, LHBB, ELBBB, WLBBB) |> 
  mutate(date = date(DATE_TIME), 
         lake = LOCATION.NAME) |> 
  group_by(date, lake) |> 
  summarize(UW_PAR = mean(UW_PAR, na.rm = T), 
            UW_TEMPERATURE = mean(UW_TEMPERATURE, na.rm = T))

# create a function to 
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

# create season column in both data frames
BB_sed = sed |> 
  left_join(BB, by = c("date", "lake")) |> 
  mutate(season = sapply(date, get_season), 
         log_PAR = log(UW_PAR))


# do some data viz
ggplot(BB_sed, aes(sediment, log_PAR)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(vars(lake), scales = "free_x") + 
  ggtitle("UW PAR vs. Sed Abundance") + 
  theme_minimal()

# linear regression
PAR_to_sediment = lm(sediment ~ log_PAR, BB_sed)
summary(PAR_to_sediment)

# filter out October and November values where light is limited and sediment could be artificially high
BB_sed_summer <- BB_sed |> 
  mutate(month = month(date)) |> 
  filter(month != 10:11)

par_to_sed_summer = lm(sediment ~ UW_PAR, BB_sed_summer)
summary(par_to_sed_summer)

# plot data 
ggplot(BB_sed_summer, aes(sediment, log_PAR)) + 
  geom_point() + 
  geom_smooth(method = "lm")


