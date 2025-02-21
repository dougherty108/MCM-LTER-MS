### Wind events in relation to sediment coverage

# taking the rough percentage of sediment cover from the data mark sent me, see how total wind, and 
#wind severity was different through those years

# libraries
library(tidyverse)
library(lubridate)

setwd("~/Documents/R-Repositories/MCM-LTER")

# load files
air <- read_csv("data/met data/FRLM/mcmlter-clim-frlm_airt-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time))
wind <- read_csv("data/met data/FRLM/mcmlter-clim-frlm_wind-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time))
rad <-  read_csv("data/met data/FRLM/mcmlter-clim-frlm_radn-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time))

# join into one large dataframe
clim <- merge(air, wind, by = "date_time", all = T) 
clim <- merge(clim, rad, by = "date_time") |> 
  filter(date_time > "2013-01-01 00:00:00") |> 
  mutate(year = year(date_time))

# look at wind run by year
wind_summary <- clim |> 
  drop_na(wspd) |> 
  group_by(year) |> 
  summarize(wind_run = sum(wspd), 
            wind_run_max = sum(wspdmax), 
            rows = nrow(clim)) |> 
  mutate(year = as.character(year))

# bar plot of wind runs

ggplot(wind_summary, aes(x = year, y = wind_run)) + 
  geom_col(position = position_dodge(width = 1)) + 
  ggtitle("Wind Run") 


# https://agupubs-onlinelibrary-wiley-com.ezproxy.library.wisc.edu/doi/full/10.1002/jgrf.20007
# the above paper has a documented entrainment speed for the dry valleys
# paper states that saltation occurs when wind is over 10m/s, regardless of termp. 
#

wind_saltation <- clim |> 
  filter(wspd > 10) |> 
  group_by(year) |> 
  summarize(wind_run = sum(wspd), 
            mean_wspd = mean(wspd)) |> 
  mutate(year = as.numeric(year) + 1)

ggplot(wind_saltation, aes(year, wind_run)) + 
  geom_col() + 
  ggtitle("Wind Run over saltation threshold", 
          "Year given is the sum of previous years wind, to better represent January maps")
# load in sediment abundance file
sed_cover <- read_csv("data/FRYX_sed_cover.csv")

sed_wind <- merge(wind_saltation, sed_cover, by = "year")

# linear regressions of mean wind speed and wind run against mean percentage sediment cover
# the wind run and mean wind speed are this are the ones calculated above the saltation threshold. 

lm_run <- lm(wind_run ~ mean_value, sed_wind)
lm_meanw <- lm(mean_wspd ~ mean_value, sed_wind)

# seems that wind run is a better predictor of sediment coverage
# than the average wind speed above saltation threshold of 10. 
summary(lm_run)
ggplot(sed_wind, aes(wind_run, mean_value)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  ggtitle("wind run above saltation threshold vs. mean sediment coverage") + 
  xlab("wind run ") + ylab("mean sediment coverage")

# mean wind speed above saltation threshold
summary(lm_meanw)
ggplot(sed_wind, aes(mean_wspd, mean_value)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  ggtitle("mean wind speed above saltation vs. mean sediment coverage") + 
  xlab("average wind speed") + ylab("mean sediment coverage")

# find number of extreme wind events
# start simple; just based off increases in wind speed. 
# 
clim_10 <- clim |> 
  filter(wspd > 20) |> 
  group_by(year) |> 
  summarize(wind_run = sum(wspd), 
            mean_wspd = mean(wspd))

# bar plot of wind_run for high wind events
ggplot(clim_10, aes(year, wind_run)) + 
  geom_col()

ggplot(clim_10, aes(date_time, wspd)) + 
  geom_point()




