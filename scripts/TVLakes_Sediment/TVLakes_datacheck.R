# TVLakes_datainvestigation
library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(MetBrewer)
library(progress)

setwd("~/Google Drive/My Drive/EarthEngine")
files <- list.files(pattern = ".tif")

#initialize output object
output <- tibble(
  date = character(length(files)), 
  min = numeric(length(files)), 
  max = numeric(length(files)), 
  mean = numeric(length(files)), 
  median = numeric(length(files))
)

for(i in 1:length(files)) {
  #load raster file
  raster_file <- raster(files[[i]])
  
  #convert raster file to dataframe for data manipulation
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  
  #clean up the column name, sometimes it's saved as B2, sometimes at band_0
  colnames(raster_df)[3] <- "value"
  
  #extract date for labeling in file
  date <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Create output tibble
  output <- output |> 
    add_row(
      date = date, 
      min = min(raster_df$value),
      max = max(raster_df$value), 
      mean = mean(raster_df$value, na.rm = TRUE),
      median = median(raster_df$value)
    )
}

#set working directory again to easily save the plot output
setwd("~/Documents/R-Repositories/MCM-LTER")

#visualization of outputs
output2 <- output |> 
  mutate(date = ymd(date)) |> 
  pivot_longer(cols = c(min, max, mean, median), names_to = "summary_type", 
               values_to = "value") |> 
  filter(value >0)

ggplot(output2, aes(date, value)) + 
  geom_path() + 
  facet_wrap(vars(summary_type)) + 
  ggtitle("With SumtoOne and nonNegative turned on")

setwd("~/Documents/R-Repositories/MCM-LTER")

ggsave("plots/GEE/datacheck_withTT.png")


