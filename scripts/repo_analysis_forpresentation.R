### landsat image repo ######

library(tidyverse)
library(lubridate)

##### load file

setwd("~/Documents/R-Repositories/MCM-LTER")

repo = read_csv("data/GEE/Sentinel Image Repo.csv") |> 
  drop_na(`Cloud Free (Y/N/P)`)
    
test <- repo |> 
  filter(`Cloud Free (Y/N/P)` == "Y" &
           `Snow Cover (Y/N/P)` == "N" &
           `Coverage of lakes` == "All") |> 
  mutate(fileid = `File ID`)

summary <- repo |> 
  mutate(row = 1) |> 
  group_by(`Cloud Free (Y/N/P)`) |> 
  summarize(sum = sum(row)) 


summary_snow <- repo |> 
  mutate(row = 1) |> 
  drop_na(`Snow Cover (Y/N/P)`) |> 
  group_by(`Snow Cover (Y/N/P)`) |> 
  summarize(sum = sum(row))


repod <- repo |> 
  mutate(date = str_extract(repo$`File ID`, "20[:digit:][:digit:]-[:digit:][:digit:]-[:digit:][:digit:]"), 
         month = month(date), 
         year = year(date), 
         row = 1) |> 
  filter(`Cloud Free (Y/N/P)` == 'Y' & `Snow Cover (Y/N/P)` == "N") |> 
  group_by(year) |> 
  summarize('number of images' = sum(row)) |> 
  print()

## add 2024 extra scenes


