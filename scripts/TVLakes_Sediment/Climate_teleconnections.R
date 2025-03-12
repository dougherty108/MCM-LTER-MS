########### exploratory oscillation analysis #########

library(tidyverse)
library(readr)

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

# load file

AAO_init <- read_table("data/Oscillation/AAO_monthly_index.txt"
                  #, header = TRUE, sep = "", dec = "."
                  )
#rename due to crazy column issue
colnames(AAO_init) <- c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
                   "Aug", "Sep", "Oct", "Nov", "Dec")

AAO <- AAO_init |> 
  select(c("Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
           "Aug", "Sep", "Oct", "Nov", "Dec")) |> 
  mutate(Dec = as.character(Dec)) |> 
  pivot_longer(cols = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", 
           "Aug", "Sep", "Oct", "Nov", "Dec"), names_to = "month", values_to = "geopotential_height") |> 
  mutate(month = match(month, month.abb), 
         date = ymd(paste0(Year, month, "-01"))) |> 
  select(-c(Year, month)) |> 
  mutate(Year = year(date), 
         Month = month(date)) |> 
  select(date)

### load cleaned AAO file
AAO_clean <- read_csv("data/Oscillation/AAO_monthly_index_long.csv") |> 
  mutate(geopotential_height = as.numeric(geopotential_height)) |> 
  pivot_wider(names_from = "Month", values_from = "geopotential_height") |> 
  filter(Year > 1996 & Year < 2023)

#AAO$geopotential_height <- trimws(AAO$geopotential_height)

### load ice thickness data
ice_thickness <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time), 
         year = year(date_time)) |> 
  filter(lake == "Lake Fryxell") |> 
  group_by(month, year) |> 
  summarize(mean_ice_water = mean(z_water_m, na.rm = T)*-1, 
            mean_ice_ice = mean(z_ice_m, na.rm = T)) |> 
  select(-mean_ice_ice)

# Interpolation for missing values in November and December
df_interpolated <- ice_thickness |> 
  filter(month %in% c(11, 12)) |>   # Keep only November and December
  group_by(month) |> 
  complete(year = full_seq(year, 1)) |>   # Ensure all years are present
  mutate(mean_ice_water = na.approx(mean_ice_water, na.rm = FALSE)) |>   # Interpolate missing values
  ungroup()

# Pivot to wide format (years as columns)
df_wide <- df_interpolated %>%
  pivot_wider(names_from = month, values_from = mean_ice_water) |> 
  drop_na()

cor_test_results <- cor(df_wide$`12`, AAO_clean$`12`, method = "pearson") |> 
  print()



#December of NOAA data: -0.37 for november of ice thickness data
#December of NOAA data: -0.25 for december of ice thickness data



