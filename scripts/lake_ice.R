#### Lake Ice Thickness Figure Creation #####
library(tidyverse)
library(lubridate)
library(ggplot2) 

lakeice <- read_csv("data/Lake Ice/mcmlter-lake-ice_thickness-20230726.csv")

ICE <- read_csv("data/Lake Ice/lake_ice_total_dataset.csv") |> 
  mutate(date = mdy(date)) |> 
  filter(date < '2023-01-19' & date > "1993-01-01") |> 
  filter(lake == c("Lake Hoare", "Lake Bonney", "Lake Fryxell", "East Lake Bonney", "West Lake Bonney"))

ggplot(ICE, aes(date, z_ice_m)) + 
  geom_point(aes(method = "lm", color = lake), size = 3) + 
  geom_smooth(aes(color = lake), se = F, size = 2) +
  ylab("Ice Thickness (m)") + xlab("Year") +
  ggtitle("MDVs Lake Ice Thickness over time") + 
  theme_bw() +
  theme(text = element_text(size = 30),
        axis.text.x = element_text(angle = 90, hjust = 1))
  

ggsave(plot = last_plot(), "plots/MDV_ice.png", dpi = 700, 
       height = 8, width = 14, units = "in")

# Lake Fryxell

thickness <- read_csv("data/Lake Ice/mcmlter-lake-ice_thickness-20230726.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         year = year(date_time), 
         z_water_m = abs(z_water_m))

fryxell <- thickness |> 
  filter(lake == "Lake Fryxell")

 # plot that muv
ggplot(fryxell, aes(year, z_ice_m)) + 
  geom_col(fill = "#BF0000") + 
  ylab("Ice Thickness (m)") + xlab("Year") +
  ggtitle("Lake Fryxell Ice Thickness (m) through time",
          subtitle = "from 1993-2023") + 
  theme(text = element_text(size = 30),
        axis.text.x = element_text(angle = 90, hjust = 1))  + 
  scale_y_reverse()

ggsave("fryxell_li.png")

ice_model <- subset(ICE, select = c(date, lake, z_ice_m)) |> 
  na.omit() |> 
  filter(lake != 'Lake Trough')
  

model_ICE <- ice_model |> 
  group_by(lake) |> 
  summarise(n = n(), 
            mean = mean(z_ice_m),
            linear_model = lm(formula = date ~ z_ice_m, data = ice_model))
model_ICE
