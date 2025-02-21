library(tidyverse)
library(lubridate)

# load file: 
alllakes <- read_csv("data/whole_lake_sediment_abundance.csv") |> 
  mutate(date = ymd(date), 
         mean_coverage = mean_coverage*100, 
         year = year(date), 
         month = month(date))


## test to fix the seasons issue
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Winter ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Winter ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Winter ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Spring ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Spring ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Summer ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Summer ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Fall ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Winter ", year))  # October is Fall
  }
}

# Apply the function and group by season
dates_grouped <- alllakes |> 
  mutate(season = sapply(date, get_season))

#create a plot to visualize the output by lake over all years. 
ggplot(dates_grouped, aes(date, mean_coverage)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  xlab("Date") + ylab("Estimate Sediment Coverage (%)") + 
  ggtitle("Whole Lake Sediment Concentration")

# create one that visualizes sediment concentration by year, with multiple facets for year and all lakes on the same plot
ggplot(dates_grouped, aes(date, mean_coverage, color = lake)) + 
  geom_point() + 
  facet_wrap(vars(year), scales = "free")

## create one that visualizes sediment concentration by month, with multiple facets for month and all lakes on the same plot
ggplot(dates_grouped, aes(date, mean_coverage)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  facet_wrap(season ~ lake, scales = "free") + 
  theme_bw(base_size = 10) + 
  xlab("Date") + ylab("Whole Lake Sediment Percentage (%)") + 
  ggtitle("Whole Lake Sediment Concentration by season across TV Lakes")


## seems like there is something happening in December (real or imagined) that causes a decrease in sediment concentration. 
## Winter 2020: Lake Hoare, Lake Fryxell, sediment concentration goes up. What's up with that? 
## Need to confirm this isn't just the model breaking down at a certain point and creating fake signals
## but how to do that? 


