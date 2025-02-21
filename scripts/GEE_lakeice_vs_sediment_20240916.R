#### GEE vs. Ice Thickness for all BB sites #######

# load the necessary libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(MetBrewer)

#set working directory
setwd("/Users/charliedougherty")

sed <- read_csv("Documents/R-Repositories/MCM-LTER/data/sediment abundance data/sediment_abundances_20250207.csv") |> 
  mutate(sediment = sediment*100, 
         date = ymd(date), 
         month=month(date), 
         year=year(date))

sed$lake[sed$lake== "fryxell"] = "Lake Fryxell"
sed$lake[sed$lake== "hoare"] = "Lake Hoare"
sed$lake[sed$lake== "eastlobe"] = "East Lake Bonney"
sed$lake[sed$lake== "westlobe"] = "West Lake Bonney"

## test to fix the seasons issue
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Summer ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Summer ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Summer ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Fall ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Fall ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Winter ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Winter ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Spring ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Summer ", year))  # October is Fall
  }
}

# Apply the function and group by season
alllakes <- sed |> 
  mutate(season = sapply(date, get_season))

## create plots
ggplot(alllakes, aes(date, sediment, color = lake, fill = lake)) + 
  geom_smooth(se = F) +
  geom_point() +
  ylab("Percentage Sediment Coverage (%)") + xlab("Date") +
  ggtitle("Mean Sediment Percentage across Taylor Valley Lakes", 
          subtitle = "for 10 pixel radius around lake monitoring site") + 
  facet_wrap(vars(season), scales = "free") + 
  theme_bw(base_size = 10)

ggsave("Documents/R-Repositories/MCM-LTER/plots/GEE_all_sed.png", dpi = 500, 
       width = 6.5, units = "in")

ggplot(alllakes, aes(date, sediment)) + 
  geom_smooth(se = F) +
  geom_point() +
  ylab("Percentage Sediment Coverage (%)") + xlab("Date") +
  ggtitle("Mean Sediment Percentage across Taylor Valley Lakes", 
          subtitle = "for 10 pixel radius around lake monitoring site") + 
  facet_wrap(vars(lake)) + 
  theme_bw(base_size = 10)

# load lake ice file
lakeice <- read_csv("Documents/R-Repositories/MCM-LTER/data/lake ice/mcmlter-lake-ice_thickness-20230726 (1).csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time), 
         year = year(date_time), 
         z_water_m = z_water_m*-1) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney") |> 
  filter(year > 2015) |> 
  filter(location_name != "B-011 Hole A", 
         location_name != "B-011 Hole B",
         location_name != "B-011 Hole C",
         location_name != "B-011 Hole D",
         location_name != "B-011 Hole E",
         location_name != "B-011 Hole F",
         location_name != "B-011 Hole I",
  )

# summarize to mean values for easier comparison
lakeice2 = lakeice |> 
  drop_na(z_water_m) |> 
  group_by(year, lake) |>
  summarize(mean_z_water = mean(z_water_m), 
            mean_z_ice = mean(z_ice_m))

## reformatting the sediment column because its a huge mess
sedi <- sed |> 
  group_by(year, lake) |>
  summarize(
    sediment = mean(sediment, na.rm = T)
    ) |> 
  drop_na(sediment)

joined = full_join(lakeice2, sedi)

## plot the joined lake ice and sediment coverage data
ice_to_ice <- ggplot(joined, aes(sediment, mean_z_ice)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  xlab("Estimated Sediment Coverage") + ylab("Ice Thickness (m)") + 
  ggtitle("Ice to Ice") + 
  theme_bw(base_size = 10)

ice_to_water <- ggplot(joined, aes(sediment, mean_z_water)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  xlab("Estimated Sediment Coverage") + ylab("Ice Thickness (m)") + 
  ggtitle("ice to water") + 
  theme_bw(base_size = 10)

ggarrange(ice_to_ice, ice_to_water)
setwd("~/Documents/R-Repositories/MCM-LTER/plots")
ggsave("ice_to_water_annual.png", width =6.5, units = "in", dpi = 500)


# filter out values in November and October, when the effect of albedo is muted
joined |> 
  filter()

## all lakes together, no faceting
ggplot(joined, aes(sediment, mean_z_ice)) +
  geom_smooth(method = "lm") + 
  geom_point() + 
  xlab("Estimated Sediment Coverage") + ylab("Ice Thickness (m)") + 
  ggtitle("All lakes together") + 
  theme_bw(base_size = 10)

setwd("~/Documents/R-Repositories/MCM-LTER/plots")

ggsave("monthly_sediment_data_corrected_alllakes.png", width =6.5, units = "in", dpi = 500)

## create lm

model_iceice = lm(sediment ~ mean_z_ice, joined)
model_icewater = lm(sediment ~ mean_z_water, joined)

summary(model_iceice)
summary(model_icewater)
