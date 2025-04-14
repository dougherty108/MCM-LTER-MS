# 
library(tidyverse)
library(lubridate)


# load file

test <- read_csv("data/test/Old_LFBB_test_MISM15.dat", skip = 1) |> 
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP)) |> 
  drop_na() |> 
  pivot_longer(cols = c("Pressure1", "Pressure2", "Pressure3", "Pressure4"), 
               names_to = "sensor", values_to = "press") |> 
  mutate(press = as.numeric(press)) |> 
  filter(press > 1015)


ggplot(test, aes(TIMESTAMP, press, color = sensor)) + 
  geom_point() + 
  ggtitle("Pressure over time during freezer test", 
          subtitle = "Pressure 1 and 2 were in freezer, 3 and 4 in the open air")
