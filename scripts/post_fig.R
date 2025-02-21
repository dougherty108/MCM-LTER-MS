#creating sediment plot

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

setwd("~/Documents/R-Repositories/MCMLTER")

rast_2014 <- raster("data/ENVI Unmixing/LC08_L2SR_058115_20140115_20201016_02_T2_SR_stack_fryxell_refl_unmix-sed.tif")
rast_2017 <- raster("data/ENVI Unmixing/LC08_L2SR_059115_20170114_20201016_02_T2_SR_stack_fryxell_refl_unmix-sed.tif")
rast_2020 <- raster("data/ENVI Unmixing/LC08_L2SR_055116_20200111_20201016_02_T2_SR_stack_fryxell_refl_unmix-sed.tif")
rast_2023 <- raster("data/ENVI Unmixing/LC08_L2SR_059115_20230115_20230131_02_T2_SR_stack_fryxell_refl_unmix-sed.tif")


rast_2014 <- as.data.frame(rast_2014, 
                      xy = TRUE) 
names(rast_2014) <- c("x", "y", "percentage")
rast_2014 <- rast_2014 |> 
  mutate(percentage = percentage*100)

plot2014 <- ggplot() + 
  geom_raster(data = rast_2014, aes(x = x, y = y, fill = `percentage`)) + 
  scale_fill_gradientn(colours=c("lightgray", "black")) +
  guides(fill=guide_legend(title="% Sediment")) + 
  coord_fixed(ratio = 1) + 
  ggtitle("January 2014") + 
  xlab("") + ylab("") + 
  theme(text = element_text(size = 20), 
        legend.position = "none", 
        axis.text = element_text(size = 15), 
        axis.x.ticks = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.y.ticks = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

rast_2017 <- as.data.frame(rast_2017, 
                           xy = TRUE)
names(rast_2017) <- c("x", "y", "percentage")
rast_2014 <- rast_2017 |> 
  mutate(percentage = percentage*100)

plot2017 <- ggplot() + 
  geom_raster(data = rast_2017, aes(x = x, y = y, fill = `percentage`)) + 
  scale_fill_gradientn(colours=c("lightgray", "black")) +
  guides(fill=guide_legend(title="% Sediment")) + 
  coord_fixed(ratio = 1) + 
  ggtitle("January 2017") + 
  xlab("") + ylab("") + 
  theme(text = element_text(size = 20), 
        legend.position = "none", 
        axis.text = element_text(size = 15), 
        axis.x.ticks = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.y.ticks = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank())


rast_2020 <- as.data.frame(rast_2020, 
                           xy=TRUE) 
names(rast_2020) <- c("x", "y", "percentage")
rast_2020 <- rast_2020 |> 
  mutate(percentage = percentage*100)

plot2020 <- ggplot() + 
  geom_raster(data = rast_2020, aes(x = x, y = y, fill = `percentage`)) + 
  scale_fill_gradientn(colours=c("lightgray", "black")) + 
  guides(fill=guide_legend(title="% Sediment")) + 
  coord_fixed(ratio = 1) + 
  ggtitle("January 2020") + 
  xlab("") + ylab("") + 
  theme(text = element_text(size = 20), 
        legend.position = "none", 
        axis.text = element_text(size = 15), 
        axis.x.ticks = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.y.ticks = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank())

rast_2023 <- as.data.frame(rast_2023, 
                          xy=TRUE) 
names(rast_2023) <- c("x", "y", "percentage")
rast_2023 <- rast_2023 |> 
  mutate(percentage = percentage*100)

plot2023 <- ggplot() + 
  geom_raster(data = rast_2023, aes(x = x, y = y, fill = `percentage`)) + 
  scale_fill_gradientn(colours=c("lightgray", "black")) + 
  guides(fill=guide_legend(title="%")) + 
  coord_fixed(ratio = 1) + 
  ggtitle("January 2023") + 
  xlab("") + ylab("") + 
  theme(text = element_text(size = 20), 
        legend.position = "none", 
        #axis.text.x = element_text(angle = 90), 
        axis.text = element_text(size = 15), 
        axis.x.ticks = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(), 
        axis.y.ticks = element_blank(), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank())


all <- ggarrange(plot2014, plot2017, plot2020, plot2023) + 
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm"))

ggsave("plots/raster_plot.png", plot = all, width = 8, height = 8, units = "in", dpi = 700)



