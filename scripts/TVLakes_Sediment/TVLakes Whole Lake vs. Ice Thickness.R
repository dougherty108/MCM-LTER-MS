### TVLakes Whole Lake vs. Ice Thickness #######
#libraries
library(tidyverse)
library(lubridate)
library(broom)
library(ggpubr)
library(RColorBrewer)
library(scales)

#set working directory
setwd("~/Documents/R-Repositories/MCM-LTER-MS")

#load file
sedi <- read_csv("data/sediment abundance data/LANDSAT_all_buffer_distances_20250301.csv") |> 
  mutate(date = ymd(date), 
         year = year(date), 
         month = month(date), 
         buffer_distance = as.character(buffer_distance))

#un-comment this out if using a file with the old lake naming
#sed$lake[sed$lake== "fryxell"] = "Lake Fryxell"
#sed$lake[sed$lake== "hoare"] = "Lake Hoare"
#sed$lake[sed$lake== "eastlobe"] = "East Lake Bonney"
#sed$lake[sed$lake== "westlobe"] = "West Lake Bonney"

#plot sediment abundances
ggplot(sedi, aes(date, ice_abundance, color = buffer_distance)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 15) + 
  ggtitle("Ice Abundance", 
          subtitle = "Landsat") + 
  scale_color_brewer(palette = "Set1") + 
  xlab("Date") + ylab("Percent Coverage (%)")

#ggsave("plots/manuscript_plots/wholelakesed.png", width = 6.5, height = 3.5, units = "in", dpi = 500)

## load lake ice: 
lakeice1 <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time), 
         year = year(date_time),
         year = as.numeric(year),
         z_water_m = z_water_m*-1) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney") |> 
  #filter(year >= 2016) |> 
  filter(!grepl("^B", location_name))

ggplot(lakeice1, aes(date_time, z_water_m)) + 
  geom_point() + 
  geom_smooth(se = T) + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 15) + 
  xlab("Date") + ylab("Percent Coverage (%)") + 
  ggtitle("Ice thickness (m) 1993-2024", 
          subtitle = "ice to water measurement")

ggsave("plots/manuscript/chapter 1/ice_thickness_total_years.png", dpi = 700, 
       height = 8, width = 12)

lakeice = lakeice1 |> 
  filter(year >= 2016)

li_summary = lakeice |> 
  group_by(year, month, lake) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

## sediment
sed_monthly <- sedi |> 
  group_by(year, month, lake) |> 
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  print()

fulljoined = full_join(sed_monthly, li_summary) |> 
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









