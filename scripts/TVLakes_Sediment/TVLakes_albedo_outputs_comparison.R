######### TVLakes Sediment Output Comparison ###########

library(tidyverse)
library(RColorBrewer)

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

mean_BB <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250301.csv") |> 
  mutate(date = ymd(date), 
         type = 'lake_monitoring_station') |> 
  select(-`approx_albedo`)

mean_wholelake <- read_csv("data/sediment abundance data/LANDSAT_wholelake_mean_20250307.csv") |> 
  mutate(date = ymd(date), 
         type = "whole_lake")

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

# join the two files for easy comparison and plotting
means <- rbind(mean_BB, mean_wholelake) |> 
  mutate(season = sapply(date, get_season), 
         sediment_abundance = sediment_abundance*100, 
         ice_abundance = ice_abundance*100)

## plot the raw output for sediment abundance/ice abundance against each other and see how the outputs compare
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") 

# facet by season for sed abundance
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free") +
  theme_linedraw(base_size = 15) + 
  xlab("Date") + ylab("Sediment Abundance (%)")

ggsave("plots/manuscript/chapter 1/whole_lake_vs_BB_sed_abundance.png", dpi = 700, 
       height = 8, width = 10)

# facet by season for ice abundance
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free") +
  theme_linedraw(base_size = 15) + 
  xlab("Date") + ylab("Sediment Abundance (%)")

ggsave("plots/manuscript/chapter 1/whole_lake_vs_BB_sed_abundance.png", dpi = 700, 
       height = 8, width = 10)











