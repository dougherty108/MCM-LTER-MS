##### comparing panchromatic data to sma data ####

library(tidyverse)
library(lubridate)
library(viridisLite)

# load data
sma = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(type = "sma") |> 
  select(date, lake, sediment_abundance, type)

pan = read_csv("data/sediment abundance data/LANDSAT_panchromatic.csv") |> 
  mutate(sediment_abundance = sediment_corrected) |> 
  mutate(type = "panchromatic") |> 
  select(date, lake, sediment_abundance, type)

total_data = rbind(sma, pan) |> 
  mutate(year = year(date))

ggplot(total_data, aes(date, sediment_abundance*100, color = type)) + 
  geom_point(size = 3) + 
  scale_colour_viridis_d(option = "E") + 
  facet_wrap(vars(lake)) +
  xlab("Date") + ylab("Sediment Estimate (%)") + 
  theme_linedraw(base_size = 20)

ggsave("plots/manuscript/chapter 1/pan_vs_sma.png", 
       dpi = 500, height = 8, width = 14)

# shows that the pan band really doesn't work. estaimtes up towards 0.75 sed at times (may not be the correc way to interpret)

# load ice thickness data 
lakeice = read_csv('data/lake ice/mcmlter-lake-ice_thickness-20250218_0_2025.csv') |> 
  mutate(date_time = mdy_hm(date_time), 
         year = year(date_time), 
         month = month(date_time)) |> 
  filter(date_time >= "2016-05-01") |> 
  #mutate(season = sapply(date_time, get_season)) |> 
  filter(str_detect(string = location, pattern = "incubation hole") | 
           str_detect(string = location, pattern = "sample hole")) |> 
  filter(grepl("^O", location)) |> 
  drop_na(z_water_m)

li_summary = lakeice |> 
  mutate(week = week(date)) |> 
  group_by(year, month, location_name) |> 
  summarize(mean_thickness = -1*mean(z_water_m, na.rm = T))

## sediment
sed_monthly <- total_data |> 
  mutate(week = week(date), 
         month = month(date)) |> 
  group_by(year, month, lake, type) |>
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  filter(type == "panchromatic")

fulljoined = full_join(sed_monthly, li_summary) |> 
  drop_na() |> 
  mutate(month = as.character(month))

##### plot
ggplot(fulljoined, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm", se = T) + 
  geom_point(size = 3) + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("October-February") + 
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  theme_linedraw(base_size = 28) 

ggsave("plots/manuscript/chapter 1/panchromatic_vs_ice.png", 
       height = 8, width = 12, dpi = 500)
