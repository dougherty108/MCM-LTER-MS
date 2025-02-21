

#### Exploratory Blue Box Data #####

# According to video from Anne B., the moat ice on Lake Hoare became ungrounded on
# 12-24-2021. Looking to see if this event can be found in the pressure data from the blue box. 
# don't have the 1min LHBB data, which would make finding this easier. The resolution of 
# 15 minutes is likely too low. 
# packages
library(tidyverse)
library(lubridate)

# Lake Hoare Pressure Exploring
LHBB <- read.csv("data/LHBB/TOA5_31558.Minute.dat", skip = 1)
LHBB <- LHBB[-c(1,2),]

LHBB_filtered <- LHBB |> 
  mutate(TIMESTAMP = ymd_hms(TIMESTAMP), 
         stage_Avg = as.numeric(stage_Avg), 
         ablation_Avg = as.numeric(ablation_Avg)) |> 
  filter(TIMESTAMP <= "2021-12-20 00:00:00" & TIMESTAMP >= "2021-12-15 00:00:00")

ggplot(LHBB_filtered, aes(TIMESTAMP, stage_Avg)) + 
  geom_point()



##### WLBBB ######

WLB <- read.csv("data/WLBBB/WLB_BB_CR1000X_WLB15min.dat", skip = 1)
WLBB<- WLB[-c(1,2),]

WLBBB <- WLBB |> 
  mutate(OSat_DOdeep_Avg = as.numeric(OSat_DOdeep_Avg), 
         OSat_Dshallow_Avg = as.numeric(OSat_Dshallow_Avg), 
         UW_PAR_moored_Avg = as.numeric(UW_PAR_moored_Avg), 
         UW_PAR_hanging_Avg = as.numeric(UW_PAR_hanging_Avg), 
         Temp_stg_Avg = as.numeric(Temp_stg_Avg), 
         TIMESTAMP = ymd_hms(TIMESTAMP))

winterWLB <- WLBBB |> 
  filter(TIMESTAMP <= "2023-08-01 00:00:00" & TIMESTAMP >= "2023-05-11 00:00:00")


ggplot(winterWLB, aes(x = TIMESTAMP)) + 
  geom_point(aes(y = OSat_DOdeep_Avg), color = "blue") + 
  geom_point(aes(y = OSat_Dshallow_Avg), color = "red")

ggplot(winterWLB, aes(x = TIMESTAMP)) + 
  geom_smooth(method = lm, aes(y = OSat_DOdeep_Avg), color = "blue") + 
  geom_smooth(method = lm, aes(y = OSat_Dshallow_Avg), color = "red")
  
winterlm = lm(formula = TIMESTAMP~OSat_DOdeep_Avg, data = winterWLB)

summary(winterlm)

