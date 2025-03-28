######### TVLakes Sediment Output Comparison ###########
library(tidyverse)
library(RColorBrewer)
library(scales)

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

## Define a season function to plot data by season. Makes data viz a lot easier. 
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

mean_BB <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250328.csv") |> 
  mutate(date = ymd(date), 
         type = 'lake_monitoring_station', 
         season = sapply(date, get_season))

#####plot by lake
ggplot(mean_BB, aes(date, sediment_abundance*100)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  #scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 20)

ggsave("plots/manuscript/chapter 1/BB_sed_by_lake.png", dpi = 700, 
       height = 8, width = 12)

mean_wholelake <- read_csv("data/sediment abundance data/LANDSAT_wholelake_mean_20250328.csv") |> 
  mutate(date = ymd(date), 
         type = "whole_lake", 
         season = sapply(date, get_season))

# join the two files for easy comparison and plotting
means <- rbind(mean_BB, mean_wholelake) |> 
  mutate(sediment_abundance = sediment_abundance*100, 
         ice_abundance = ice_abundance*100) |> 
  mutate(year = year(date), 
         month = month(date))

## plot the raw output for sediment abundance/ice abundance against each other and see how the outputs compare
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") 

#By season for just the lake monitoring site
ggplot(mean_BB, aes(date, sediment_abundance*100, color = lake)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 20)

ggsave("plots/manuscript/chapter 1/BB_sed_by_season.png", dpi = 700, 
       height = 8, width = 12)

# facet by season for sed abundance
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 15)

ggsave("plots/manuscript/chapter 1/whole_lake_vs_BB_sed_abundance.png", dpi = 700, 
       height = 8, width = 12)

# facet by season for ice abundance
ggplot(means, aes(date, ice_abundance, color = type)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free_x") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Ice Abundance (%)") + 
  theme_linedraw(base_size = 15)

ggsave("plots/manuscript/chapter 1/whole_lake_vs_BB_ice_abundance.png", dpi = 700, 
       height = 8, width = 12)


## load lake ice: 
lakeice1 <- read_csv("data/lake ice/mcmlter-lake-ice_thickness-20250218_0_2025.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time), 
         year = year(date_time),
         year = as.numeric(year),
         z_water_m = z_water_m*-1) |> 
  rename("lake" = location_name) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney") |> 
  #filter(year >= 2016) |> 
  filter(!grepl("^B", location))

ggplot(lakeice1, aes(date_time, z_water_m)) + 
  geom_point() + 
  geom_smooth(se = T) + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 15) + 
  xlab("Date") + ylab("Ice Thickness (m)") + 
  ggtitle("Ice thickness (m) 1993-2024", 
          subtitle = "ice to water measurement")

#ggsave("plots/manuscript/chapter 1/ice_thickness_total_years.png", dpi = 700, 
#       height = 8, width = 12)

lakeice = lakeice1 |> 
  filter(year >= 2017) |> 
  mutate(season = sapply(date_time, get_season)) |> 
  filter(str_detect(string = location, pattern = "incubation hole")) |> 
  #filter(grepl("^O", location)) |> 
  drop_na(z_water_m)

#FIND SEASONAL DROPOFF OF ICE THICKNESS
seasonal_changes <- lakeice1 %>%
  group_by(season, lake) |> 
  arrange(season, date_time) %>%
  summarise(
    First_Measurement = first(z_water_m*-1),
    Last_Measurement = last(z_water_m*-1),
    Difference = Last_Measurement - First_Measurement
  )

ggplot(seasonal_changes, aes(season, Difference), fill = lake) + 
  geom_col() + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Seasonal") + ylab("Change in Ice Thickness")

ggsave("plots/manuscript/chapter 1/seasonal_ice_drop.png", 
       height = 8, width = 12, dpi = 300)

li_summary = lakeice |> 
  mutate(week = week(date_time)) |> 
  group_by(year, month, lake) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

## sediment
sed_monthly <- means |> 
  mutate(week = week(date)) |> 
  group_by(year, month, lake, type) |> 
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  print()

fulljoined = full_join(sed_monthly, li_summary) |> 
  drop_na()

##### plot
ggplot(fulljoined, aes(mean_sed, mean_thickness, color= type)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("October-February", 
          subtitle = "whole lake average") + 
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20) 

ggsave("plots/manuscript/chapter 1/sed_vs_ice_thickness.png", 
       width = 12, height = 8, dpi = 300)

### now remove November and October Values
#fulljoin_filter <- fulljoined |> 
#  filter(month == 12 | month == 1)
fulljoin_peak_solar <- fulljoined |> 
  filter(month == 1)

ggplot(fulljoin_peak_solar, aes(mean_sed, mean_thickness, color = type)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("January") + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20) 

ggsave("plots/manuscript/chapter 1/wholelakesed_vsthickness.png", 
       width = 12, height = 8, dpi = 300)

# now filter for only early year estimates, October - December
oct_dec_fulljoin <- fulljoined |> 
  filter(month == 10 | month == 11 | month == 12)

ggplot(oct_dec_fulljoin, aes(mean_sed, mean_thickness, color = type)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("October - December", 
          subtitle = "whole lake average") + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 15) 

jan_fulljoin <- fulljoined |> 
  filter(month == 1)

ggplot(jan_fulljoin, aes(mean_sed, mean_thickness, color = type)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("January") + 
  scale_color_brewer(palette = "Set1") +
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  theme_linedraw(base_size = 20)  

ggsave("plots/manuscript/chapter 1/jan_only_sed.png", 
       dpi = 300, height = 8, width = 12)


####### do restructuring to look at this by week ###
li_summary_2 = lakeice |> 
  mutate(week = week(date_time)) |> 
  group_by(year, week, lake) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

## sediment
sed_monthly_2 <- means |> 
  mutate(week = week(date)) |> 
  group_by(year, week, lake, type) |> 
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  print()

fulljoined_2 = full_join(sed_monthly_2, li_summary_2) |> 
  drop_na() |> 
  filter(type == "lake_monitoring_station")

peak_solar_week <- fulljoined_2 |> 
  filter(
    #week == 50 |
      week == 51 | 
      week == 52 | 
      week == 1 | 
      week == 2 #|
      #week == 3
      )

ggplot(peak_solar_week, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("Peak Solar") + 
  scale_color_brewer(palette = "Set1") +
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  theme_linedraw(base_size = 20)  

ggsave("plots/manuscript/chapter 1/Peak_Solar_Comparison_20250328.png", 
       width = 12, height = 8, dpi = 300)

#### Create a dual panel plot where only the lake ice thickness from 2016-2024 to sediment abundannce
# Need to join the sed data frame to the lake ice data frame
mean_BB_week <- mean_BB |> 
  filter(date < "2023-02-01")

library(ggpubr)

plot1 <- ggplot(mean_BB_week, aes(date, sediment_abundance*100)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  ylab("Sediment Abundance") + xlab("Date") + 
  ggtitle("Sediment Abundance (%)") + 
  facet_wrap(vars(lake), scales = "free") + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20) 

plot2 <- ggplot(lakeice, aes(date_time, z_water_m)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  ylab("Ice Thickness (m)") + xlab("Date") + 
  ggtitle("Ice Thickness") + 
  facet_wrap(vars(lake), scales = "free") + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20)

ggarrange(plot1, plot2)

ggsave("plots/manuscript/chapter 1/sed_abundance_ice_thick_timeseries.png", 
       dpi = 300, height = 8, width = 12)

#ggplot() + 
#  geom_point(data = lakeice, aes(x = as.Date(date_time), y = z_water_m)) + 
#  geom_point(data = mean_BB, aes(x = date, y = sediment_abundance)) + 
##  facet_wrap(vars(lake)) + 
#  xlab("Date") + ylab("Ice thickness (m)") +
#  scale_color_brewer(palette = "Set1") +
#  theme_linedraw(base_size = 20) 




######### investing different buffering distances #########
mean_150m <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_150m_20250301.csv") |> 
  mutate(date = ymd(date), 
         buffer_distance = '150')

mean_300m = mean_BB |> 
  mutate(buffer_distance = '300') |> 
  select(-type)

mean_450m = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_450m_20250301.csv") |> 
  mutate(date = ymd(date), 
         buffer_distance = '450')

mean_600m = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_600m_20250301.csv") |> 
  mutate(date = ymd(date), 
         buffer_distance = '600') |> 
  select(-approx_albedo)

mean_900m = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_900m_20250301.csv") |> 
  mutate(date = ymd(date), 
         buffer_distance = '900')

buffers <- rbind(mean_150m, mean_300m, mean_450m, mean_600m, mean_900m) |> 
  mutate(ice_abundance = ice_abundance*100, 
         sediment_abundance = sediment_abundance*100, 
         season = sapply(date, get_season)) |> 
  select(-sediment)

write_csv(buffers, "data/sediment abundance data/LANDSAT_all_buffer_distances_20250301.csv")

### plot cropping distance comparisons
#sed
ggplot(buffers, aes(date, sediment_abundance, color = buffer_distance)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(lake)) + 
 # scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 15)

ggsave("plots/manuscript/chapter 1/comparison_of_buffer_distances_sed_bylake.png", dpi = 700, 
       height = 8, width = 10)

#ice
ggplot(buffers, aes(date, ice_abundance, color = buffer_distance)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(lake)) + 
  # scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Ice Abundance (%)") + 
  theme_linedraw(base_size = 15)

ggsave("plots/manuscript/chapter 1/comparison_of_buffer_distances_sed_bylake.png", dpi = 700, 
       height = 8, width = 10)

## facet by season
#ice
ggplot(buffers, aes(date, ice_abundance, color = buffer_distance)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(lake, season), scales = "free") + 
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Ice Abundance (%)") + 
  theme_linedraw(base_size = 10)

ggsave("plots/manuscript/chapter 1/comparison_of_buffer_distances_sed_byseason.png", dpi = 700, 
       height = 10, width = 10)

#sed
ggplot(buffers, aes(date, sediment_abundance, color = buffer_distance)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(lake, season), scales = "free") + 
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 10)

ggsave("plots/manuscript/chapter 1/comparison_of_buffer_distances_sed_byseason.png", dpi = 700, 
       height = 10, width = 10)
