library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)
library(MetBrewer)
library(RColorBrewer)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")
files <- list.files(pattern = ".tif")

# Select color palette
met_palette <- MetBrewer::met.brewer("Hokusai2")

# Extract type from filename
get_type <- function(filename) {
  str_extract(filename, "(?<=LANDSAT_)(FRY|HOA|BON)(?=_unmix)")
}

# Create output directories for each type
output_base <- "~/Google Drive/My Drive/EarthEngine/plots/final_manuscript_plots/Hokusai2"
dir.create(output_base, showWarnings = FALSE)

types <- unique(na.omit(sapply(files, get_type)))
for (t in types) {
  dir.create(file.path(output_base, t), showWarnings = FALSE)
}

# Loop to create and save plots
for (i in 1:length(files)) {
  setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")
  
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  
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
      #scale_fill_distiller(palette = "RdBu", direction = -1, na.value = "transparent") +
      labs(title = paste0(type, " - ", year), x = "Easting", y = "Northing") +
      scale_x_reverse() + 
      scale_y_reverse() + 
      theme_minimal()
    
    setwd("~/Google Drive/My Drive/EarthEngine/plots/final_manuscript_plots/Hokusai2")
    ggsave(filename = plot_path)
    print(paste0("Saved plot for ", type, " - ", year, " (", i, "/", length(files), ")"))
  }
}


######## USE THE BELOW IF YOU HAVE TO PLOT ONLY A SINGLE LAKE OUT OF THE DIRECTORY #########

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")

# Filter files to only include those with "HOA"
files <- list.files(pattern = "LANDSAT_HOA.*\\.tif$")

# Select color palette
met_palette <- MetBrewer::met.brewer("Hokusai2")

# Extract type from filename
get_type <- function(filename) {
  str_extract(filename, "(?<=LANDSAT_)(FRY|HOA|BON)(?=_unmix)")
}

# Create output directory for HOA
output_base <- "~/Google Drive/My Drive/EarthEngine/plots/20250328"
hoa_output <- file.path(output_base, "HOA")
dir.create(hoa_output, showWarnings = FALSE, recursive = TRUE)

# Loop to create and save plots
for (i in seq_along(files)) {
  setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")
  
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  
  raster_df <- raster_df |> 
    mutate(sediment_abundance = (1 - ice_endmember))
  
  year <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  type <- get_type(files[[i]])
  
  if (!is.na(type) && type == "HOA") {
    plot_path <- file.path(hoa_output, paste0("LANDSAT_plot_HOA_", year, ".png"))
    
    ggplot() +
      geom_raster(data = raster_df, aes(x = x, y = y, fill = sediment_abundance)) +
      coord_sf() +
      scale_fill_gradientn(colors = met_palette) +
      labs(title = paste0("HOA - ", year), x = "Easting", y = "Northing") +
      theme_minimal()
    
    setwd(hoa_output)
    ggsave(filename = plot_path)
    print(paste0("Saved plot for HOA - ", year, " (", i, "/", length(files), ")"))
  }
}





