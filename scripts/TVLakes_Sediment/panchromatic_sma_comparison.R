##### comparing panchromatic data to sma data ####

library(tidyverse)
library(lubridate)

# load data
sma = read_csv("data/sediment abundance data/LANDSAT_sediment_abundances_20250403.csv")

pan = read_csv("data/sediment abundance data/LANDSAT_panchromatic.csv") |> 
  mutate(sediment_corrected = 1-sediment_corrected)


total_data = sma |> 
  left_join(pan, by = join_by(date, lake))


# plot data against each other
ggplot(total_data, aes(sediment_abundance, sediment_corrected)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 15)
