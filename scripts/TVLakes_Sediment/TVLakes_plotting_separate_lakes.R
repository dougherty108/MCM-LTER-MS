library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)
library(MetBrewer)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250301")
files <- list.files(pattern = ".tif")

# Select color palette
met_palette <- MetBrewer::met.brewer("Hokusai2")

# Extract type from filename
get_type <- function(filename) {
  str_extract(filename, "(?<=LANDSAT_)(FRY|HOA|BON)(?=_unmix)")
}

# Create output directories for each type
output_base <- "~/Documents/R-Repositories/MCM-LTER-MS/plots/LANDSAT/20250306"
dir.create(output_base, showWarnings = FALSE)

types <- unique(na.omit(sapply(files, get_type)))
for (t in types) {
  dir.create(file.path(output_base, t), showWarnings = FALSE)
}

# Loop to create and save plots
for (i in 1:length(files)) {
  setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250301")
  
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  #colnames(raster_df)[3] <- ""
  
  raster_df = raster_df |> 
    mutate(sediment_abundance = (1 - ice_endmember))
  
  year <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  type <- get_type(files[[i]])
  
  if (!is.na(type)) {
    plot_path <- file.path(output_base, type, paste0("LANDSAT_plot_", type, "_", year, ".png"))
    
    ggplot() +
      geom_raster(data = raster_df, aes(x = x, y = y, fill = sediment_abundance)) +
      coord_sf() +
      scale_fill_gradientn(colors = met_palette) +
      labs(title = paste0(type, " - ", year), x = "Easting", y = "Northing") +
      theme_minimal()
    
    setwd("~/Documents/R-Repositories/MCM-LTER-MS/plots/LANDSAT/20250306")
    ggsave(filename = plot_path)
    print(paste0("Saved plot for ", type, " - ", year, " (", i, "/", length(files), ")"))
  }
}


