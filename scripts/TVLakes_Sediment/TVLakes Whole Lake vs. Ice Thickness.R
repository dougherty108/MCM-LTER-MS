### TVLakes Whole Lake vs. Ice Thickness #######
#libraries
library(tidyverse)
library(lubridate)
library(broom)
library(ggpubr)

#set working directory
setwd("~/Documents/R-Repositories/MCM-LTER-MS")

#load file
sedi <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_600m_20250301.csv") |> 
  mutate(date = ymd(date), 
         mean_coverage = sediment_abundance*100, 
         year = year(date), 
         month = month(date))

#un-comment this out if using a file with the old lake naming
#sed$lake[sed$lake== "fryxell"] = "Lake Fryxell"
#sed$lake[sed$lake== "hoare"] = "Lake Hoare"
#sed$lake[sed$lake== "eastlobe"] = "East Lake Bonney"
#sed$lake[sed$lake== "westlobe"] = "West Lake Bonney"


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

#plot sediment abundances
ggplot(sedi, aes(date, mean_coverage)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 15) + 
  ggtitle("Sediment Estimate", 
          subtitle = "Landsat") + 
  xlab("Date") + ylab("Percent Coverage (%)")

ggsave("plots/manuscript_plots/wholelakesed.png", width = 6.5, height = 3.5, units = "in", dpi = 500)


# Apply the function and group by season
alllakes <- sedi |> 
  mutate(season = sapply(date, get_season), 
         year = year(date), 
         month = month(date))

## load lake ice: 
lakeice <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time), 
         year = year(date_time),
         year = as.numeric(year),
         z_water_m = z_water_m*-1) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney") |> 
  filter(year >= 2016)

ggplot(lakeice, aes(date_time, z_water_m)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 15) + 
  xlab("Date") + ylab("Percent Coverage (%)") + 
  ggtitle("Ice thickness (m) 2016-2023", 
          subtitle = "ice to water measurement")

li_summary = lakeice |> 
  group_by(year, month, lake) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T)) |> 
  filter(year > 2014)

## sediment
sed <- alllakes |> 
  group_by(year, month, lake) |> 
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  print()

fulljoined = full_join(sed, li_summary) |> 
  drop_na()

##### plot
plot1 = ggplot(fulljoined, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("October-February", 
          subtitle = "whole lake average") + 
  theme_linedraw()

### now remove November and October Values
fulljoin_filter <- fulljoined |> 
  filter(month == 12 | month == 1)

plot2 = ggplot(fulljoin_filter, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("December - January",
          subtitle = "whole lake average") + 
  theme_linedraw()

ggarrange(plot1, plot2)

ggsave("plots/GEE/alllakes/analysis plots/wholelakesed_vsthickness.png", width = 6.5, height = 3.5, units = "in", dpi = 500)









