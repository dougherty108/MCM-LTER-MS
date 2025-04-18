######## Soil Albedo Calculation from BOYM #########

# libraries
library(tidyverse)

bonney <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_boym_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         month = month(date_time)) |> 
  filter(date_time > "1995-01-01 00:00:00") |> 
  filter(month == 10 | month == 11 | month == 12 | month == 1 | month == 2) |> 
  drop_na(swradout_wm2, swradin_wm2) |>  
  mutate(albedo = swradout_wm2 / swradin_wm2) |> 
  filter(albedo < 1)

# plot albedo
ggplot(bonney, aes(date_time, albedo)) + 
  geom_line()

#summary to find a defendable soil albedo value
summary(bonney$albedo)


#### Find a defendable ice albedo value
taylor <- read_csv("~/Google Drive/My Drive/MCMLTER_Met/met stations/mcmlter-clim_tarm_15min-20250205.csv") |> 
  mutate(date_time = ymd_hms(date_time), 
         albedo = swradout_wm2 / swradin_wm2, 
         month = month(date_time)) |> 
  filter(month == 10 | month == 11 | month == 12 | month == 1 | month == 2) |> 
  drop_na(swradout_wm2, swradin_wm2) |> 
  filter(albedo <1 & albedo > 0.4)

# plot albedo
#ggplot(taylor, aes(date_time, albedo)) + 
#  geom_line()

#summary to find a defendable ice albedo value
summary(taylor$albedo) |> 
  print()

