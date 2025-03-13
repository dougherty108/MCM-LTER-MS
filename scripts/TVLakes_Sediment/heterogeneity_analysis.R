######## TV LAKES Heterogeneity Test ###########
library(terra)
library(tidyverse)

setwd("~charliedougherty")

# Load raster
r <- rast("Google Drive/My Drive/EarthEngine/landsat/20250308/LANDSAT_FRY_unmix_mar28_2023-12-07.tif")


# Define a focal function (e.g., standard deviation for heterogeneity)
heterogeneity_sd <- focal(r, w = matrix(1, 3, 3), fun = "sd", na.policy = "omit")

# Plot result
plot(heterogeneity_sd, main = "Local Standard Deviation (Heterogeneity)")

# Compute local variance in a moving window
heterogeneity_var <- focal(r, w = matrix(1, 5, 5), fun = var, na.policy = "omit")

# Plot result
plot(heterogeneity_var, main = "Local Variance (Heterogeneity)")
