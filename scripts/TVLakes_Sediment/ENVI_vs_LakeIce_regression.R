# recreate the ENVI vs. Lake Ice plot
library(lubridate)
setwd("~/Documents/R-Repositories/MCM-LTER")
mark <- read_csv("data/FRYX_sed_cover.csv") |> 
  mutate(year = as.numeric(year), 
         mean_value_sed = mean_value*100)

li <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time), 
         year = year(date_time),
         year = as.numeric(year),
         z_water_m = z_water_m*-1) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney")

li_summary = li |> 
  group_by(year) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

fulljoined = full_join(mark, li_summary) |> 
  drop_na()


# create a plot to replace the bad one in the paper
ggplot(fulljoined, aes(mean_value_sed, mean_thickness)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  xlab("Estimate Sediment Coverage (%)") + ylab("Ice Thickness (m)") + 
  ggtitle("Percentage Sediment Coverage vs. Mean January Ice Thickness") + 
  theme_bw(base_size = 10)

ggsave("ENVI_mark.png", width =6.5, height = 3.75, units = "in", dpi = 500)

### recreate long term ice thickness plot
ggplot(li, aes(date_time, z_water_m)) + 
  geom_smooth() + 
  geom_point() +
  facet_wrap(vars(lake)) + 
  xlab("Ice Thickness (m)") + ylab("Date") + 
  ggtitle("MDVs Lake Ice Thickness over time") + 
  theme_bw(base_size = 10)

ggsave("MDV_lter_ice.png", width =6.5, height = 3.75, units = "in", dpi = 500)

