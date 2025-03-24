#### Spectra and location analysis ###
library(tidyverse)
library(ggpubr)

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

split_and_expand <- function(col) {
  col %>%
    str_replace_all("\\[|\\]", "") %>%   # Remove brackets
    str_split(",\\s*")                   # Split on commas
}


LB_spectra <- read_csv("~/Google Drive/My Drive/EarthEngine/endmembers_output_LB.csv") |> 
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
  rename(id = "system:index", 
         B2 = "brightest_band_means_1", 
         B3 = "brightest_band_means_2", 
         B4 = "brightest_band_means_3", 
         B5 = "brightest_band_means_4", 
         B6 = "brightest_band_means_5", 
         B7 = "brightest_band_means_6", 
         B8 = "brightest_band_means_7") |> 
  mutate(id = as.numeric(id)) |> 
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
  mutate(dim_band_values = as.numeric(dim_band_values))


bon_ice <- ggplot(LB_spectra, aes(id, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Bonney, Ice Endmember Values")

bon_soil <- ggplot(LB_spectra, aes(id, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Bonney, Soil Endmember Values")

ggarrange(bon_ice, bon_soil)

ggsave("plots/manuscript/chapter 1/bonney_spectra_comparison_values.png", 
       height = 8, width = 16, dpi = 300)


# Lake Hoare
LH_spectra <- read_csv("~/Google Drive/My Drive/EarthEngine/endmembers_output_LH.csv") |> 
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
  rename(id = "system:index", 
         B2 = "brightest_band_means_1", 
         B3 = "brightest_band_means_2", 
         B4 = "brightest_band_means_3", 
         B5 = "brightest_band_means_4", 
         B6 = "brightest_band_means_5", 
         B7 = "brightest_band_means_6", 
         B8 = "brightest_band_means_7") |> 
  mutate(id = as.numeric(id)) |> 
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
  mutate(dim_band_values = as.numeric(dim_band_values))


hor_ice <- ggplot(LB_spectra, aes(id, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Hoare, Ice Endmember Values")

hor_soil <- ggplot(LB_spectra, aes(id, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Hoare, Soil Endmember Values")

ggarrange(hor_ice, hor_soil)

ggsave("plots/manuscript/chapter 1/hoare_spectra_comparison_values.png", 
       height = 8, width = 14, dpi = 300)


# Lake Hoare
LF_spectra <- read_csv("~/Google Drive/My Drive/EarthEngine/endmembers_output_LF.csv") |> 
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
  rename(id = "system:index", 
         B2 = "brightest_band_means_1", 
         B3 = "brightest_band_means_2", 
         B4 = "brightest_band_means_3", 
         B5 = "brightest_band_means_4", 
         B6 = "brightest_band_means_5", 
         B7 = "brightest_band_means_6", 
         B8 = "brightest_band_means_7") |> 
  mutate(id = as.numeric(id)) |> 
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
  mutate(dim_band_values = as.numeric(dim_band_values))


fry_ice <- ggplot(LB_spectra, aes(id, bright_band_values, color = brightness_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Fryxell, Ice Endmember Values")

fry_soil <- ggplot(LB_spectra, aes(id, dim_band_values, color = dimmest_band_names)) + 
  geom_path() + 
  theme_linedraw(base_size=20) + 
  ggtitle("Lake Fryxell, Soil Endmember Values")

ggarrange(fry_ice, fry_soil)

ggsave("plots/manuscript/chapter 1/fryxell_spectra_comparison_values.png", 
       height = 8, width = 14, dpi = 300)


