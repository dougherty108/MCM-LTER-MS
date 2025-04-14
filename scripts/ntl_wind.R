library(tidyverse)
library(lubridate)

ntl <- read_csv("data/ntl129_1_v11.csv") |> 
  mutate(sampledate = ymd(sampledate))

ggplot(ntl, aes(sampledate, avg_wind_speed)) + 
  geom_point()

summary(ntl$avg_wind_speed)
