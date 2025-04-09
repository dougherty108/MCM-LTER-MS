library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)
library(MetBrewer)
library(RColorBrewer)
library(ggspatial)

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
output_base <- "~/Google Drive/My Drive/EarthEngine/plots/20250409"
hoa_output <- file.path(output_base, "HOA")
dir.create(hoa_output, showWarnings = FALSE, recursive = TRUE)

# Loop to create and save plots
for (i in seq_along(files)) {
  setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")
  
  raster_file <- raster(files[[i]])
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  
  raster_df <- raster_df |> 
    mutate(sediment_coverage = (1 - ice_endmember)#, 
          # y = y*-1
           )
  
  year <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  type <- get_type(files[[i]])
  
  if (!is.na(type) && type == "HOA") {
    plot_path <- file.path(hoa_output, paste0("LANDSAT_plot_HOA_", year, ".png"))
    
    ggplot() +
      geom_raster(data = raster_df, aes(x = x, y = y, fill = sediment_coverage)) +
      coord_sf(crs = 3031,
               datum = sf::st_crs("EPSG:3031")
               ) +
      #scale_y_reverse() + 
      #scale_x_reverse() + 
      scale_fill_gradientn(colors = met_palette) +
      labs(title = paste0("HOA - ", year), x = "Easting", y = "Northing") +
      annotation_north_arrow(location = "tr", which_north = "true",
                             style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", width_hint = 0.3) + 
      theme_linedraw()
    
    setwd(hoa_output)
    ggsave(filename = plot_path)
    print(paste0("Saved plot for HOA - ", year, " (", i, "/", length(files), ")"))
  }
}



raster_df_flipped <- raster_df %>%
  mutate(y_plot = -y)  # This flips it vertically for plotting only

ggplot() +
  geom_raster(data = raster_df_flipped, aes(x = x, y = y_plot, fill = sediment_coverage)) +
  #scale_y_continuous(
  #  name = "Northing",
  ##  breaks = scales::pretty_breaks(n = 5),
  #  labels = function(b) format(-b, scientific = FALSE)  # Restore original y-labels
  #) +
  scale_x_continuous(name = "Easting") +
  scale_fill_gradientn(colors = met_palette) +
  #labs(title = paste0("HOA - ", year)) +
  coord_sf(crs = 3031) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) +
  theme_linedraw()


