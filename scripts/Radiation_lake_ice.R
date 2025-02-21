## Fryxell Radiation #####

# trying to look at the change in radiation over 2014-2024 to see if anything is going on there

#libraries
library(tidyverse)
library(lubridate)

# set wd
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

ggplot

total_summary <- full_join(radn_summary, lake_summary, by = "year")


ggplot(total_summary, aes(mean_sw, mean_thickness)) + 
  geom_point() + 
  geom_smooth(method = "lm")

lm <- lm(mean_sw~mean_thickness, total_summary) |> 
  print()

