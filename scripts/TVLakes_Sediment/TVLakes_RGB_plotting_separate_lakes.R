
#############
###### plotting for RGB images ######
library(terra)
library(sf)
library(tidyverse)
library(lubridate)
library(stars)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/RGB_images")
files <- list.files(pattern = ".tif")

# Extract type from filename
get_type <- function(filename) {
  str_extract(filename, "(?<=LANDSAT_)(FRY|HOA|BON)(?=_RGB)")
}

# Create output directories for each type
output_base <- "~/Documents/R-Repositories/MCM-LTER-MS/plots/LANDSAT/RGB_images"
dir.create(output_base, showWarnings = FALSE)

types <- unique(na.omit(sapply(files, get_type)))
for (t in types) {
  dir.create(file.path(output_base, t), showWarnings = FALSE)
}

# Loop to create and save plots
for (i in 1:length(files)) {
  tryCatch({
    setwd("~/Google Drive/My Drive/EarthEngine/landsat/RGB_images")
    
    raster_file <- rast(files[[i]])
    raster_df <- as.data.frame(raster_file, xy = TRUE)
    
    # Check if required bands exist
    if (!all(c("B4", "B3", "B2") %in% names(raster_df))) {
      stop("Missing required bands (B4, B3, B2)")
    }
    
    raster_df <- raster_df %>%
      mutate(
        B4 = scales::rescale(B4, to = c(0, 1)),
        B3 = scales::rescale(B3, to = c(0, 1)),
        B2 = scales::rescale(B2, to = c(0, 1))
      )
    
    # Ensure no NA values before using rgb()
    if (any(is.na(raster_df$B4) | is.na(raster_df$B3) | is.na(raster_df$B2))) {
      stop("NA values found in RGB bands")
    }
    
    year <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
    type <- get_type(files[[i]])
    
    if (!is.na(type)) {
      plot_path <- file.path(output_base, type, paste0("RGB_plot_", type, "_", year, ".png"))
      
      ggplot(raster_df, aes(x = x, y = y)) +
        geom_raster(aes(fill = rgb(B4, B3, B2))) +
        scale_fill_identity() +
        labs(title = paste0(type, " - ", year), x = "Easting", y = "Northing") +
        coord_fixed() +
        theme_minimal()
      
      setwd("~/Documents/R-Repositories/MCM-LTER-MS/plots/LANDSAT/RGB_images")
      ggsave(filename = plot_path)
      print(paste0("Saved plot for ", type, " - ", year, " (", i, "/", length(files), ")"))
    }
    
  }, error = function(e) {
    message(paste0("Error processing file: ", files[[i]], " - ", e$message))
  })
}






