# Sonar data lookout

require(tidyverse)
library(lubridate)

#set the working directory from which the files will be read from
setwd("~/Documents/R-Repositories/MCMLTER")

# start with just one file

#create a list of the files from your target directory
file_list <- list.files(path="~/Documents/R-Repositories/MCMLTER/data/SONAR")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

for (i in 1:length(file_list)){
  sonar <- read_csv(file_list[i], col_names = TRUE, skip = 1) |> 
    mutate(TIMESTAMP = mdy_hm(TIMESTAMP))
  dataset <- rbind(dataset, sonar, fill = TRUE)
}


#### All in one file
setwd("~/Documents/R-Repositories/MCMLTER")

sonar <- read_csv("data/SONAR/Total_Data.csv") |> 
  mutate(TIMESTAMP = mdy_hm(TIMESTAMP)) |> 
  filter(IceThickness_Avg < 7) |> 
  distinct()

LF_thickness <- read_csv("data/lake_ice.csv") |> 
  filter(lake == 'Lake Fryxell')

ggplot(sonar, aes(TIMESTAMP, IceThickness_Avg)) + 
  geom_point()
  
