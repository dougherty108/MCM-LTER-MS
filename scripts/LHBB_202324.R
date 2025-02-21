###### LHBB Blue Box Exploratory ########

# packages
library(tidyverse)
library(lubridate)
library(ggpubr)

file = "data/LHBB/LH_BB_CR1000X_LH1min.dat"
site = read.csv(file, skip = 1, header = F, nrows = 1, as.is = T) |> 
  select(2) |> pull(1)

## Read files ####
LHBB1min <- read_delim("data/LHBB/LH_BB_CR1000X_LH1min.dat", 
                       skip = 1) |> 
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP), 
         RECORD = as.numeric(RECORD), 
         stage_Avg = as.numeric(stage_Avg), 
         ablation_Avg = as.numeric(ablation_Avg)) |> 
  filter(RECORD > 5000)


LHBB15min <- read_delim("data/LHBB/LH_BB_CR1000X_LH15min.dat", 
                        skip = 1)

stage <- ggplot(LHBB1min, aes(TIMESTAMP, stage_Avg)) +
  geom_point() + 
  ggtitle("Lake Hoare BB Stage 22/23") +
  scale_y_reverse()


ablation <- ggplot(LHBB1min, aes(TIMESTAMP, ablation_Avg)) +
  geom_point() + 
  ggtitle("Lake Hoare BB Ablation 22/23") +
  scale_y_reverse()


ggarrange(stage, ablation)
