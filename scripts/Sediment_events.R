##### wind direction analysis

# specific dates with sediment dumps: 
# April 15, 2020, April 23, 2024, April 24, 2024
# June 17, 2021, March 18, 2022

# libraries

library(tidyverse)
library(lubridate)

# Load data

wind <- read_csv("data/climatedata/mcmlter-clim-boym_wind-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         wspdmax = as.numeric(wspdmax))
wind <- wind[-c(1,2),]


wind_events <- wind |> 
  filter(wdir > 0 &
           wdir < 75, 
         wspd > 7) |> 
  mutate(date = as.Date(date_time)) |> 
  group_by(date) 

number_dates <- wind_events |> 
  unique(wind_events$date)

wind_events <- wind_events[!duplicated(wind_events[c('date')]),]


wind_dates <- wind |> 
  inner_join(wind_events, 
            by = "date_time")


ggplot(wind_dates, aes(date_time, wspd.x)) + 
  geom_point() + 
  facet_wrap(~date, scales = "free")
