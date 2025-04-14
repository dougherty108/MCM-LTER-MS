library(tidyverse) # definitely need this
library(terra)
library(sp)
library(sf)
library(raster)
library(ggplot2)

library(gridExtra)
library(patchwork)
library(ggpubr)
library(viridis)

setwd("~/Documents/R-Repositories/MCMLTER/data/Landsat_Unmixed")
ldf <- list() # creates a list
cropped <- list()
names <- list.files()
lakeplots <- list()

int <- list()
outlist <- list() #create empty list to store outputs from loop
r <- list()
names <- list()
m <- list()
listtif <- dir(pattern = "*.tif") # creates the list of all the csv files in the directory
type <- list()

boundary <- raster(ymx = 155, xmn = 150, 
                   ymn = 135, xmx = 170)

# for loop to crop rasters and extract mean sediment cover value. 
for(k in 1:length(listtif)) {
  r[[k]] <- raster(listtif[[k]])
  int <- crop(r[[k]], boundary)
  val <- getValues(int)
  m[[k]] <- mean(val, na.rm = F)
  names[[k]] <- substr(listtif[[k]], start=18, stop=25)
  type[[k]] <- substr(listtif[[k]], start = 65, stop = 67)
  output <- tibble(
    date = names, 
    percentage = m, 
    measured = type
  )
  print("complete")
}

# reorder output a little bit to make later analysis easy
sed_cover <- output %>% 
  pivot_wider(names_from = measured, 
              values_from = percentage) %>% 
  mutate(sed = as.numeric(sed), 
         ice = as.numeric(ice), 
         date = ymd(date))

# create lists to be called in the for loop
names <- list.files()
lakeplots <- list()
int <- list()
listtif <- dir(pattern = "*.tif") # creates the list of all the csv files in the directory


# for loop that plots every file and names each plot as its file name
for (k in 1:length(listtif)){
  ldf[[k]] <- raster(listtif[k])
  int <- crop(ldf[[k]], boundary)
  names[[k]] <- substr(listtif[[k]], start=18, stop=25)
  type[[k]] <- substr(listtif[[k]], start = 65, stop = 67)
  temp = plot(ldf[[k]], 
              breaks = c(0, 20, 40, 60, 80, 100),
              col = gray.colors(5, rev = TRUE), 
              main = paste0(type[[k]], " ", names[[k]]))
  print("woohoo!")
}

write_csv(sed_cover, file = "data.csv")

sed_c <- read_csv("data.csv") %>% 
  mutate(date = mdy(date), 
         year = year(date))

# plot sediment cover
ggplot(sed_cover, aes(date, sed)) + 
  geom_smooth(se = F) + 
  geom_point() + 
  xlab("Date") + ylab("Percentage Sediment Coverage (%)") + 
  ggtitle("Percentage Sediment Cover", 
          subtitle = "Lake Fryxell") + 
  theme(
    plot.title = element_text(size = 20),
    plot.subtitle = element_text(size = 15), 
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    axis.title=element_text(size=14), 
    #panel.spacing.x = unit(20, "mm"), 
    #strip.text.x = element_text(size = 15)
  ) 

ggsave(filename = "sediment_cover_projected.png", path = "../../plots")



ggplot(sed_c, aes(date, sed)) + 
  geom_col(fill = "#0889C9")+ 
  xlab("Date") + ylab("Percentage Sediment Coverage (%)") + 
  ggtitle("Percentage Sediment Cover", 
          subtitle = "Lake Fryxell") + 
  theme(text = element_text(size = 25),
        axis.text.x = element_text(angle = 90, hjust = 1), 
        plot.margin = margin(50,50,10,10))

ggsave(filename = "sediment_cover.png", path = "../../plots", 
       width = 18, height = 8, dpi = 500)


# add in lake thickness data
setwd("~/Documents/R-Repositories/MCMLTER")
thickness <- read_csv("data/Limno Team Data/mcmlter-lake-ice_thickness-20230726.csv") %>% 
  mutate(date_time = mdy_hm(date_time), 
         year = year(date_time), 
         month = month(date_time))

thickness <- thickness%>% 
  filter(lake == "Lake Fryxell", 
         date_time > '2014-01-01') %>% 
  mutate(z_water_m = abs(z_water_m), 
         z_ice_m = abs(z_ice_m)) %>% 
  drop_na(z_water_m) %>% 
  filter(location_name != "B-011 Hole A", 
         location_name != "B-011 Hole B", 
         location_name != "B-011 Hole C", 
         location_name != "B-011 Hole D", 
         location_name != "B-011 Hole E", 
         location_name != "B-011 Hole F")

#doesn't work
sed_cover <- sed_cover %>% 
  mutate(year = year(date), 
         month = month(date)) 

thick_sed <- full_join(thickness, sed_cover,
                       by = join_by(year)) %>%
  group_by(year) %>% 
  mutate(mean_thickness = abs(mean(z_ice_m)), 
         mean_thick_log = log(abs(z_ice_m)))

# plot sediment cover against ice thickness

ggplot(thick_sed, aes(mean_thickness, sed)) + 
  geom_smooth(se = T) + 
  geom_point() +
  xlab("Mean ice thickness by year (m)") + ylab("percentage sediment cover(%)") + 
  ggtitle("Mean Ice Thickness vs. Percentage Sediment Cover by Year", 
          subtitle = "Lake Fryxell") +
  theme(text = element_text(size = 30)
          )

ggplot(thick_sed, aes(x = date_time)) + 
  geom_point(aes(y = sed), color = "blue") + 
  geom_point(aes(y = z_ice_m))

# ice thickness plot

ggplot(thickness, aes(date_time, z_water_m)) + 
  geom_col()

