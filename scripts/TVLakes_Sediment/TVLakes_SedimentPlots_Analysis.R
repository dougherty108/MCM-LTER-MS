#### Sediment output Analyses
# no ice thickness data included yet

#add the libraries
library(tidyverse)
library(lubridate)
library(RColorBrewer)

#set worviridis#set working directory
setwd("~/Documents/R-Repositories/MCM-LTER-MS")

# load mean sediment abundance plots
alllakes <- read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250301.csv") |> 
  mutate(date = ymd(date), 
         mean_sed = sediment_abundance*100, 
         year = year(date), 
         month = month(date))

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
alllakes <- alllakes |> 
  mutate(season = sapply(date, get_season))

#plotting
#sediment coverage by each lake for whole record
ggplot(alllakes, aes(date, mean_sed, color = lake)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(vars(lake)) + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") + 
  theme(legend.position = "none")

#sediment coverage by season
ggplot(alllakes, aes(date, mean_sed, color = lake)) + 
  geom_smooth(SE = T) + 
  geom_point() + 
  facet_wrap(vars(season), scales = "free") + 
  scale_color_brewer(palette = "Set1") + 
  theme_minimal() 

#summary statistics
summary_lakes <- alllakes |> 
  drop_na() |> 
  group_by(lake, season) |> 
  summarize(
    mean = mean(mean_sed), 
    min = min(mean_sed), 
    max = max(mean_sed), 
    median = median(mean_sed)
  ) |> 
  pivot_longer(cols = c(mean, min, max, median), names_to = "stats", values_to = "value")

#plot summary statistics
ggplot(summary_lakes, aes(x = season, y = value, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(stats), scales = "free") + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#plot summary statistics
ggplot(summary_lakes, aes(x = stats, y = value, fill = lake)) + 
  geom_col(position = "jitter") + 
  facet_wrap(vars(season), scales = "free") + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set1") 




