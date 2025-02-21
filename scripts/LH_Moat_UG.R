###### Lake Hoare BB Stage Moat Ungrounding ######

library(tidyverse)
library(lubridate)


## load files ###

file = "data/LHBB/LIMNO_BLUE_BOX_HOARE.csv"
site = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T) |> 
  select(2) |> pull(1)

LH <- read_csv("data/LHBB/LIMNO_BLUE_BOX_HOARE.csv") |> 
  mutate(DATE_TIME = mdy_hm(DATE_TIME))


ggplot(LH, aes(DATE_TIME, STAGE)) + 
  geom_point() +
  ggtitle("Stage LHBB")

ggplot(LH, aes(DATE_TIME, ABLATION)) + 
  geom_point() + 
  ggtitle("Ablation LHBB")
