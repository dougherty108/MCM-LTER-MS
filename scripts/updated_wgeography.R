library(tidyverse) # definitely need this
library(terra)
library(sp)
library(sf)
library(raster)
library(ggpubr)
library(viridis)

setwd("~/Documents/R-Repositories/MCMLTER/data/ENVI Unmixing")

####### analysis
ldf <- list() # creates a list
cropped <- list()
names <- list.files()

int <- list()
outlist <- list() #create empty list to store outputs from loop
r <- list()
names <- list()
m <- list()
listtif <- dir(pattern = "*.tif") # creates the list of all the csv files in the directory
type <- list()

# coordinates of LFBB in lat/long: S 77.610355 E 163.147163
# coordinates of LFBB in UTM: 455628.56, 1384413.59
# coordinates in EPSG:3031; 391739.21960283903, -1293191.705610413

#coords = raster(as.numeric(c(-77.610355, 163.147163)))

#boundary <- st_buffer(x = c(), dist = 100)

##boundary <- raster(ymn = -1293200,xmx =392050, 
#                 ymx = -1293150, xmn = 392000, 
#                 crs = "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
#-1293196.9, 392147.2
#-1293141.8, 391478.7

str_name <- "LC08_L2SR_054116_20150122_20201016_02_T2_SR_stack_fryxell_refl_unmix-sed.tif"

coords = as.numeric(c(-1293196.9, 392147.2))

boundary <- st_buffer(x = coords, dist = 200)
plot(boundary)


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
    percentage = m
  )
  print("complete")
}


output2 <- output %>% 
  mutate(date = ymd(date),
         percentage = as.numeric(percentage)*100, 
         year = year(date))

# plot sediment cover
ggplot(output2, aes(date, percentage)) + 
  #geom_smooth(se = F) + 
  geom_col() + 
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
    strip.text.x = element_text(size = 15)
    )

ggsave(filename = "sediment_cover_projected.png", path = "../../plots")


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


thickness_avg <- thickness %>% 
  group_by(year) %>% 
  summarize(mean_thickness = mean(z_water_m))

total <- full_join(thickness_avg, output2)


ggplot(total, aes(mean_thickness, percentage)) + 
  geom_smooth(method = "lm") + 
  geom_point() + 
  ylab("Percentage Sediment Coverage (%)") + xlab("Annual mean Ice thickness") + 
  ggtitle("Percentage Sediment Cover vs. Ice Thickness", 
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

sed_ice <- lm(mean_thickness ~ percentage, data = total)
summary(sed_ice)
