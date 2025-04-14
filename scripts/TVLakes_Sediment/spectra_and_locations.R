#### Spectra and location analysis ###
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(lubridate)
library(sf)

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


split_and_expand <- function(col) {
  col %>%
    str_replace_all("\\[|\\]", "") %>%   # Remove brackets
    str_split(",\\s*")                   # Split on commas
}


LB_spectra <- read_csv("~/Google Drive/My Drive/EarthEngine/endmembers_output_LB_20250325.csv") |> 
  mutate(
    brightest_band_means = split_and_expand(brightest_band_means),
    dimmest_band_means = split_and_expand(dimmest_band_means),
    brightest_geometry = split_and_expand(brightest_geometry), 
    dimmest_geometry = split_and_expand(dimmest_geometry)
  ) |> 
  # Unnest the lists into separate columns
  unnest_wider(brightest_band_means, names_sep = "_") %>%
  unnest_wider(dimmest_band_means, names_sep = "_") %>%
  unnest_wider(brightest_geometry, names_sep = "_") |> 
  unnest_wider(dimmest_geometry, names_sep = "_") |> 
  rename(id = "system:index", 
         B2 = "brightest_band_means_1", 
         B3 = "brightest_band_means_2", 
         B4 = "brightest_band_means_3", 
         B5 = "brightest_band_means_4", 
         B6 = "brightest_band_means_5", 
         B7 = "brightest_band_means_6", 
         B8 = "brightest_band_means_7") |> 
  mutate(id = as.numeric(id), 
         date = ymd(date), 
         season = sapply(date, get_season)) |> 
  pivot_longer(cols = c(B2, B3, B4, B5, B6, B7, B8), names_to = "brightness_band_names", 
               values_to = "bright_band_values") |> 
  mutate(bright_band_values = as.numeric(bright_band_values)) |> 
  rename(B2 = "dimmest_band_means_1", 
         B3 = "dimmest_band_means_2", 
         B4 = "dimmest_band_means_3", 
         B5 = "dimmest_band_means_4", 
         B6 = "dimmest_band_means_5", 
         B7 = "dimmest_band_means_6", 
         B8 = "dimmest_band_means_7") |> 
  pivot_longer(cols = c(B2, B3, B4, B5, B6, B7, B8), names_to = "dimmest_band_names", 
               values_to = "dim_band_values") |> 
  mutate(dim_band_values = as.numeric(dim_band_values), 
         lake = "Lake Bonney")


bon_ice <- ggplot(LB_spectra, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  scale_color_brewer(palette = "Set1") + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Bonney, Ice Endmember Values")

bon_soil <- ggplot(LB_spectra, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Lake Bonney, Soil Endmember Values")

ggarrange(bon_ice, bon_soil)


ggsave("plots/manuscript/chapter 1/bonney_spectra_comparison_values.png", 
       height = 8, width = 16, dpi = 300)

# by season 
ggplot(LB_spectra, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  facet_wrap(vars(season), scales = "free_x") + 
  theme_linedraw(base_size = 20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Bonney, by season")

ggsave("plots/manuscript/chapter 1/bonney_season_spectra_comparison_values_soil.png", 
       height = 8, width = 16, dpi = 300)

# by season 
ggplot(LB_spectra, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  facet_wrap(vars(season), scales = "free_x") + 
  theme_linedraw(base_size = 20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Bonney, by season")

ggsave("plots/manuscript/chapter 1/bonney_season_spectra_comparison_values_ice.png", 
       height = 8, width = 16, dpi = 300)

# Lake Hoare
LH_spectra <- read_csv("~/Google Drive/My Drive/EarthEngine/endmembers_output_LH_20250325.csv") |> 
  mutate(
    brightest_band_means = split_and_expand(brightest_band_means),
    dimmest_band_means = split_and_expand(dimmest_band_means),
    brightest_geometry = split_and_expand(brightest_geometry), 
    dimmest_geometry = split_and_expand(dimmest_geometry)
  ) |> 
  # Unnest the lists into separate columns
  unnest_wider(brightest_band_means, names_sep = "_") %>%
  unnest_wider(dimmest_band_means, names_sep = "_") %>%
  unnest_wider(brightest_geometry, names_sep = "_") |> 
  unnest_wider(dimmest_geometry, names_sep = "_") |> 
  rename(id = "system:index", 
         B2 = "brightest_band_means_1", 
         B3 = "brightest_band_means_2", 
         B4 = "brightest_band_means_3", 
         B5 = "brightest_band_means_4", 
         B6 = "brightest_band_means_5", 
         B7 = "brightest_band_means_6", 
         B8 = "brightest_band_means_7") |> 
  mutate(id = as.numeric(id), 
         date = ymd(date), 
         season = sapply(date, get_season)) |> 
  pivot_longer(cols = c(B2, B3, B4, B5, B6, B7, B8), names_to = "brightness_band_names", 
               values_to = "bright_band_values") |> 
  mutate(bright_band_values = as.numeric(bright_band_values)) |> 
  rename(B2 = "dimmest_band_means_1", 
         B3 = "dimmest_band_means_2", 
         B4 = "dimmest_band_means_3", 
         B5 = "dimmest_band_means_4", 
         B6 = "dimmest_band_means_5", 
         B7 = "dimmest_band_means_6", 
         B8 = "dimmest_band_means_7") |> 
  pivot_longer(cols = c(B2, B3, B4, B5, B6, B7, B8), names_to = "dimmest_band_names", 
               values_to = "dim_band_values") |> 
  mutate(dim_band_values = as.numeric(dim_band_values), 
         lake = "Lake Hoare")


hor_ice <- ggplot(LH_spectra, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  scale_color_brewer(palette = "Set1") + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Hoare, Ice Endmember Values")

hor_soil <- ggplot(LH_spectra, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Hoare, Soil Endmember Values")

ggarrange(hor_ice, hor_soil)

ggsave("plots/manuscript/chapter 1/hoare_spectra_comparison_values.png", 
       height = 8, width = 14, dpi = 300)


# by season 
ggplot(LH_spectra, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  facet_wrap(vars(season), scales = "free_x") + 
  theme_linedraw(base_size = 20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Hoare, by season")

ggsave("plots/manuscript/chapter 1/hoare_season_spectra_comparison_values_soil.png", 
       height = 8, width = 16, dpi = 300)

# by season 
ggplot(LH_spectra, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  facet_wrap(vars(season), scales = "free_x") + 
  theme_linedraw(base_size = 20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Hoare, by season")

ggsave("plots/manuscript/chapter 1/hoare_season_spectra_comparison_values_ice.png", 
       height = 8, width = 16, dpi = 300)


# Lake Fryxell
LF_spectra <- read_csv("~/Google Drive/My Drive/EarthEngine/endmembers_output_LF_20250325.csv") |> 
  mutate(
    brightest_band_means = split_and_expand(brightest_band_means),
    dimmest_band_means = split_and_expand(dimmest_band_means),
    brightest_geometry = split_and_expand(brightest_geometry), 
    dimmest_geometry = split_and_expand(dimmest_geometry)
  ) |> 
  # Unnest the lists into separate columns
  unnest_wider(brightest_band_means, names_sep = "_") %>%
  unnest_wider(dimmest_band_means, names_sep = "_") %>%
  unnest_wider(brightest_geometry, names_sep = "_") |> 
  unnest_wider(dimmest_geometry, names_sep = "_") |> 
  rename(id = "system:index", 
         B2 = "brightest_band_means_1", 
         B3 = "brightest_band_means_2", 
         B4 = "brightest_band_means_3", 
         B5 = "brightest_band_means_4", 
         B6 = "brightest_band_means_5", 
         B7 = "brightest_band_means_6", 
         B8 = "brightest_band_means_7") |> 
  mutate(id = as.numeric(id), 
         date = ymd(date), 
         season = sapply(date, get_season)) |> 
  pivot_longer(cols = c(B2, B3, B4, B5, B6, B7, B8), names_to = "brightness_band_names", 
               values_to = "bright_band_values") |> 
  mutate(bright_band_values = as.numeric(bright_band_values)) |> 
  rename(B2 = "dimmest_band_means_1", 
         B3 = "dimmest_band_means_2", 
         B4 = "dimmest_band_means_3", 
         B5 = "dimmest_band_means_4", 
         B6 = "dimmest_band_means_5", 
         B7 = "dimmest_band_means_6", 
         B8 = "dimmest_band_means_7") |> 
  pivot_longer(cols = c(B2, B3, B4, B5, B6, B7, B8), names_to = "dimmest_band_names", 
               values_to = "dim_band_values") |> 
  mutate(dim_band_values = as.numeric(dim_band_values), 
         lake = "Lake Fryxell")


fry_soil <- ggplot(LF_spectra, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  scale_color_brewer(palette = "Set1") +
  ggtitle("Lake Fryxell, Soil Endmember Values")

fry_ice <- ggplot(LF_spectra, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  scale_color_brewer(palette = "Set1") +
  ggtitle("Lake Fryxell, Ice Endmember Values")

ggarrange(fry_soil, fry_ice)

ggsave("plots/manuscript/chapter 1/fryxell_spectra_comparison_values.png", 
       height = 8, width = 14, dpi = 300)

# by season 
ggplot(LF_spectra, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  facet_wrap(vars(season), scales = "free_x") + 
  theme_linedraw(base_size = 20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Fryxell, by season")

ggsave("plots/manuscript/chapter 1/fryxell_season_spectra_comparison_values_soil.png", 
       height = 8, width = 16, dpi = 300)

# by season 
ggplot(LF_spectra, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  facet_wrap(vars(season), scales = "free_x") + 
  theme_linedraw(base_size = 20) + 
  scale_color_brewer(palette = "Set1") + 
  ggtitle("Fryxell, by season")

ggsave("plots/manuscript/chapter 1/fryxell_season_spectra_comparison_values_ice.png", 
       height = 8, width = 16, dpi = 300)


######### MAPS ###############
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggmap)


all_lakes <- rbind(LB_spectra, LF_spectra, LH_spectra) |> 
  select(-c(.geo, image_id)) |> 
  mutate(brightest_lat = as.numeric(brightest_geometry_2), 
         brightest_lon = as.numeric(brightest_geometry_1), 
         dimmest_lat = as.numeric(dimmest_geometry_2), 
         dimmest_lon = as.numeric(dimmest_geometry_1))

#### make a plot by season of spectra
ggplot(all_lakes, aes(date, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size=20) + 
  scale_color_brewer(palette = "Set1") +
  ggtitle("Soil Endmember Values")

ggsave("plots/manuscript/chapter 1/alllakes_spectra_comparison_values_soil.png", 
       height = 8, width = 16, dpi = 300)
  
ggplot(all_lakes, aes(date, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size=20) + 
  scale_color_brewer(palette = "Set1") +
  ggtitle("Ice Endmember Values")

ggsave("plots/manuscript/chapter 1/alllakes_spectra_comparison_values_ice.png", 
       height = 8, width = 16, dpi = 300)

### summary information ####
dim_summ <- all_lakes |> 
  group_by(lake, dimmest_band_names) |> 
  summarize(mean_dim = mean(dim_band_values), 
            min_dim = min(dim_band_values), 
            max_dim = max(dim_band_values)) |> 
  pivot_longer(cols = c(mean_dim, min_dim, max_dim), values_to = "summary_value", names_to = "summary_stat")

ggplot(dim_summ, aes(dimmest_band_names, summary_value, fill = lake)) + 
  geom_col() + 
  facet_wrap(vars(summary_stat, lake)) + 
  theme_linedraw()

# 
bright_summ <- all_lakes |> 
  group_by(lake, brightness_band_names) |> 
  summarize(mean_bright = mean(bright_band_values), 
            min_bright = min(bright_band_values), 
            max_bright = max(bright_band_values)) |> 
  pivot_longer(cols = c(mean_bright, min_bright, max_bright), values_to = "summary_value", names_to = "summary_stat")

ggplot(bright_summ, aes(brightness_band_names, summary_value, fill = lake)) + 
  geom_col() + 
  facet_wrap(vars(summary_stat, lake)) + 
  theme_linedraw()

ggplot() +
  # Add points
  geom_point(data = all_lakes, 
             aes(x = dimmest_lon, y = dimmest_lat, color = lake),
             size = 1.5) +
  
  # Customize appearance
  labs(title = "Dimmest_points",
       x = "Longitude", y = "Latitude") +
  theme_minimal()


ggplot() +
  # Add points
  geom_point(data = all_lakes, 
             aes(x = brightest_lon, y = brightest_lat, color = lake),
             size = 1.5) +
  
  # Customize appearance
  labs(title = "Brightest_points",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
