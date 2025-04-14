######### wind characteristics between landsat captures in January of each year ######

# images were captured in roughly mid January (14-16) from 2014-2024 (11 years)

# libraries
library(tidyverse)
library(lubridate)

setwd("~/Documents/R-Repositories/MCMLTER")

# load files
# start with just fryxell met and build from there, this is also closest to the blue box

fryxell <- read_csv("data/FRLM/mcmlter-clim-frlm_wind-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time))

yoi <- fryxell |> 
  filter(date_time > "2013-01-01 00:00") |> 
  mutate(year = year(date_time))

###### boxplots for wind speeds 
plot_yoi <- yoi |> 
  group_by(year) |> 
  mutate(year = as.character(year)) |> 
  pivot_longer(cols = c(wspd, wspdmax), 
               names_to = "measurement_type", 
               values_to = "value")

ggplot(plot_yoi, aes(year, value, fill = measurement_type)) + 
  geom_boxplot() + 
  ggtitle("wind speeds by year at Fryxell")

ggplot(plot_yoi, aes(x = year, y = wspdmax)) + 
  geom_boxplot() + 
  ggtitle("WSPDMAX by year Fryxell")


# wind run = total distance of wind over a given amount of time
# wind is in meters per second, there are 3.15 E7 seconds in a year (31540000)

summary_yoi = yoi |> 
  group_by(year) |> 
  summarize(sum_wspd = sum(wspd), 
            wind_run = sum(wspd)*31540000) |> 
  print() |> 
  mutate(year = as.character(year))

# plot wind run
ggplot(summary_yoi, aes(year, wind_run)) + 
  geom_col() + 
  ggtitle("wind run per year at Fryxell met")


# other wind descriptive statistics
summary_wind_stats <- yoi |> 
  group_by(year) |> 
  summarize(wspd_mean = mean(wspd, na.rm = T), 
            wspdmax_mean = mean(wspdmax, na.rm = T), 
            wdir_mean = mean(wdir, na.rm = T), 
            ) |> 
  print() |> 
  mutate(year = as.character(year))


# plotting descriptive statistics about wind
# mean wind speed per year
ggplot(summary_wind_stats, aes(year, wspd_mean)) + 
  geom_col() + 
  ggtitle("average mean wind speed Fryxell met")

#mean maximum wind speed per year
ggplot(summary_wind_stats, aes(year, wspdmax_mean)) + 
  geom_col() + 
  ggtitle("average max wind speed Fryxell met")

