####### plotting landsat data ########

library(raster)
library(sp)
library(tidyverse)
library(terra)
library(sf)
library(rgdal)


# plot individual file 
sed2015 <- raster("data/Landsat Unmixed/LC08_L2SR_054116_20150122_20201016_02_T2_SR_stack_fryxell_unmix_sed.tif")

plot(sed2015, 
     main = "sediment 2015")

sat <- list.files(path = "data/Landsat Unmixed")
plots <- list()

for(i in 1:length(sat)) {
  p = raster(paste0('data/Landsat Unmixed/',sat[i]))
  plots <- plot(p[[i]], 
               breaks = c(0, 20, 40, 60, 80, 100), 
               col = hcl.colors(5),
               main = paste0(sat[[i]]))
  plots$plot
}

paste0('Data/WQP_spc/spcWQPsites_',usestate,'.csv')