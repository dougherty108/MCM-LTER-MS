##### just doing this bit in a new script to simplify things ####
#The other figure that would be interesting. Is the mean sediment abundance (same x-axis as here), 
#but the difference in ice thickness between that year and the next. So like 
#Dec 2017/Jan 2018 sediment abundance vs. (Dec 2017/Jan 2018 ice thickness - Nov 2018 ice thickness)

library(tidyverse)

setwd("/Users/charliedougherty/Documents/R-Repositories/MCM-LTER-MS")

#sediment
mean_bluebox_sediment = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(date = ymd(date))

#lake ice
li = read_csv("data/lake ice/mcmlter-lake-ice_thickness-20250218_0_2025.csv") |> 
  mutate(date = mdy_hm(date_time), 
         year = year(date), 
         month = month(date),
         z_water_m = z_water_m*-1) 

li_again = li |> 
  filter(str_detect(location, " L[0-9]+")) |> 
  group_by(location_name, month, year) |> 
  reframe(z_water_m = mean(z_water_m, na.rm = T), 
            date = date)


li_filtered <- li %>%
  filter(year > 2015, 
         month == 12) %>%
  arrange(location_name, date) %>%
  mutate(year = year(date))

# Last measurement of each year per location
last_of_year <- li_filtered %>%
  group_by(location_name, year) %>%
  slice_max(order_by = date, n = 1) %>%
  rename(last_thickness = z_water_m, last_date = date)

# First measurement of each year per location (with shifted year)
first_of_year <- li_filtered %>%
  group_by(location_name, year) %>%
  slice_min(order_by = date, n = 1) %>%
  rename(first_thickness = z_water_m, first_date = date) %>%
  mutate(prev_year = year - 1)

# Join and calculate change
annual_change <- last_of_year %>%
  inner_join(first_of_year, by = c("location_name", "year" = "prev_year")) %>%
  transmute(
    location_name = location_name,
    year = year,
    last_date,
    last_thickness,
    next_year = year + 1,
    first_date,
    first_thickness,
    change = first_thickness - last_thickness
  ) |> 
  distinct(last_date, first_date, .keep_all = TRUE)

ggplot(annual_change, aes(year, change)) + 
  geom_col() + 
  facet_wrap(vars(location_name))

