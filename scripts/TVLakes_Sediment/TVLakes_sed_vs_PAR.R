###### Sediment Rates compared to Underwater PAR ####

#Author: Charlie Dougherty

#libraries
library(tidyverse)
library(MetBrewer)


# load sediment file\
setwd("~charliedougherty")

sed = read_csv("Documents/R-Repositories/MCM-LTER-MS/data/sediment abundance data/LANDSAT_sediment_abundances_20250301.csv") |> 
  mutate(date = ymd(date))

#sed$lake[sed$lake== "fryxell"] = "Lake Fryxell"
#sed$lake[sed$lake== "hoare"] = "Lake Hoare"
#sed$lake[sed$lake== "eastlobe"] = "East Lake Bonney"
#sed$lake[sed$lake== "westlobe"] = "West Lake Bonney"

# load UW PAR File (s)
#compare this station to FRLM
FRLM <- read_csv("Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_frlm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-01-01 00:00:00') |> 
  select(metlocid, date_time, par_umolm2s1)

LFBB <- read_csv("Documents/R-Repositories/MCM-LTER-MS/data/Blue Box/LIMNO_BLUE_BOX_FRYXELL.csv") |> 
  mutate(date_time = mdy_hm(DATE_TIME)) |> 
  filter(date_time > "2016-01-01 00:00") |> 
  select(c(date_time, LOCATION.NAME, UW_PAR, UW_TEMPERATURE))

LFBB_joined <- LFBB |> 
  left_join(FRLM, by = join_by("date_time")) |> 
  mutate(net_par = par_umolm2s1 / UW_PAR) |> 
  select(date_time, net_par, LOCATION.NAME)


#compare this station to HOEM PAR
HOEM <- read_csv("Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_hoem_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-01-01 00:00:00') |> 
  select(c(date_time, metlocid, par_umolm2s1)) 

LHBB <- read_csv("Documents/R-Repositories/MCM-LTER-MS/data/Blue Box/LIMNO_BLUE_BOX_HOARE.csv") |> 
  mutate(date_time = mdy_hm(DATE_TIME)) |> 
  filter(date_time > "2016-01-01 00:00") |> 
  select(c(date_time, LOCATION.NAME, UW_PAR))

LHBB_joined <- LHBB |> 
  left_join(HOEM, by = join_by("date_time")) |> 
  mutate(net_par = par_umolm2s1 / UW_PAR) |> 
  select(date_time, net_par, LOCATION.NAME)

#compare this station to BOYM PAR
BOYM <- read_csv("Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time)) |> 
  filter(date_time > '2016-01-01 00:00:00') |> 
  select(c(date_time, metlocid, par_umolm2s1))

ELBBB <- read_csv("Documents/R-Repositories/MCM-LTER-MS/data/Blue Box/LIMNO_BLUE_BOX_ELBONNEY_0.csv") |> 
  mutate(date_time = mdy_hm(DATE_TIME), 
         LOCATION.NAME = LOCATION.ME) |> 
  filter(date_time > "2016-01-01 00:00") |> 
  select(c(date_time, LOCATION.NAME, UW_PAR))

ELBBB_joined <- ELBBB |> 
  left_join(BOYM, by = join_by("date_time")) |> 
  mutate(net_par = par_umolm2s1 / UW_PAR) |> 
  select(date_time, net_par, LOCATION.NAME)

#compare this station to BOYM PAR
WLBBB <- read_csv("Documents/R-Repositories/MCM-LTER-MS/data/Blue Box/LIMNO_BLUE_BOX_WLBONNEY_0.csv") |> 
  mutate(date_time = mdy_hm(DATE_TIME)) |> 
  filter(date_time > "2016-01-01 00:00") |> 
  select(c(date_time, LOCATION.NAME, UW_PAR))

WLBBB_joined <- WLBBB |> 
  left_join(BOYM, by = join_by("date_time")) |> 
  drop_na(c(par_umolm2s1, UW_PAR)) |> 
  mutate(net_par = par_umolm2s1 / UW_PAR) |> 
  select(date_time, net_par, LOCATION.NAME)

# join all BB data into one big dataset and create a daily average 
BB <- rbind(LFBB_joined, LHBB_joined, ELBBB_joined, WLBBB_joined) |> 
  mutate(date = date(date_time), 
         lake = LOCATION.NAME) |> 
  group_by(date, lake) |> 
  summarize(net_par = mean(net_par, na.rm = T))

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
         log_PAR = log(net_par)) |> 
  drop_na(net_par)

# plot net par by season
ggplot(BB_sed, aes(date, net_par, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(season), scales = "free")

# do some data viz
ggplot(BB_sed, aes(sediment_abundance, net_par)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("Surface PAR / UW_PAR vs. Sed Abundance") + 
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


