### Ultrasonic ranger distance drift assessment

library(tidyverse)
library(lubridate)


# load files
# sites that have an ultrasonic ranger: 
# BOYM, BRHM, FRLM, HODM, VAAM, VIAM, TARM

BOYM <- read_csv("data/Ultrasonic data/mcmlter-clim-boym_snowht-15min-20230605.csv") |> 
  mutate(date_time = mdy_hm(date_time))
colnames(BOYM)[4:5] = c("ultrasonic", "notes")
BOYM <- BOYM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")
BRHM <- read_csv("data/Ultrasonic data/mcmlter-clim-brhm_surfchange-15min-20230630.csv") |> 
  mutate(date_time = mdy_hm(date_time))
colnames(BRHM)[4:5] = c("ultrasonic", "notes")
BRHM <- BRHM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")
FRLM <- read_csv("data/Ultrasonic data/mcmlter-clim-frlm_surf-15min-20230605.csv")|> 
  mutate(date_time = mdy_hm(date_time))
colnames(FRLM)[4:5] = c("ultrasonic", "notes")
FRLM <- FRLM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")
HODM <- read_csv("data/Ultrasonic data/mcmlter-clim-hodm_icesurfchange-15min-20230706.csv")|> 
  mutate(date_time = mdy_hm(date_time))
colnames(HODM)[4:5] = c("ultrasonic", "notes")
HODM <- HODM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")
TARM <- read_csv("data/Ultrasonic data/mcmlter-clim-tarm_depth-15min-20230705.csv")|> 
  mutate(date_time = mdy_hm(date_time)) 
colnames(TARM)[4:5] = c("ultrasonic", "notes")
TARM <- TARM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")
VAAM <- read_csv("data/Ultrasonic data/mcmlter-clim-vaam_surfchange-15min-20230621.csv")|> 
  mutate(date_time = mdy_hm(date_time))
colnames(VAAM)[4:5] = c("ultrasonic", "notes")
VAAM <- VAAM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")
VIAM <- read_csv("data/Ultrasonic data/mcmlter-clim-viam_surfchange-15min-20230706.csv")|> 
  mutate(date_time = mdy_hm(date_time))
colnames(VIAM)[4:5] = c("ultrasonic", "notes")
VIAM <- VIAM |> 
  select("date_time", "metlocid", "ultrasonic", "notes")

### seasons
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

total = rbind(BOYM, BRHM, FRLM, VAAM, VIAM) |> 
  mutate(month = month(date_time), 
         year = year(date_time)) |> 
  filter(month == 1 | month == 10 | month == 11 | month == 12) |> 
  filter(ultrasonic < 200, 
         ultrasonic > 0) |> 
  mutate(season = sapply(date_time, get_season))

ggplot(total, aes(date_time, ultrasonic,color = metlocid)) + 
  geom_point(size = 2, shape = 1) + 
  ggtitle("Ultrasonic change of all stations that have US", 
          "assuming the change is the change from base value, not change from last measurement ? ")


ggplot(total, aes(date_time, ultrasonic, color = metlocid)) + 
  geom_line() + 
  facet_wrap(~season, scales = "free") + 
  ggtitle("Ultrasonic measurements at main stations by season")
