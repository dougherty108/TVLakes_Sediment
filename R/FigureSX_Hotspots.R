####### TV Lakes HotSpot Analysis #####
# goal of script is to create hotspot maps of the TV Lakes. You will need to run the SMA scripts in the Python folder
# first before running this, so the script has tif files in Google Drive to use. 
# library
library(terra)
library(tidyverse)
library(sf)
library(ggpubr)
library(ggspatial)
library(MetBrewer)


###################### Create mean from raster stack #################

process_mean_raster <- function(lake) {
  tif_dir <- "Data/landsat/20250325"
  
  # Define directory and search pattern
  pattern <- paste0("LANDSAT_", lake, ".*\\.tif$")
  tif_files <- list.files(tif_dir, pattern = pattern, full.names = TRUE)
  
  if (length(tif_files) == 0) {
    stop("No matching .tif files found.")
  }
  
  # Load the specified band (2nd) from each raster
  raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[2]]))
  # Calculate mean across all layers (including NAs)
  mean_raster <- app(raster_stack, fun = mean, na.rm = TRUE)
  
  # Reproject to EPSG:32758
  mean_raster <- project(mean_raster, "EPSG:32758")
  # Convert to tibble
  mean_df <- as_tibble(as.data.frame(mean_raster, xy = TRUE)) |>
    rename(sediment_mean = 3)
  
  return(mean_df)
}

mean_df_LB = process_mean_raster('BON')
mean_df_LH = process_mean_raster('HOA')
mean_df_LF = process_mean_raster('FRY')

########################## PLOTS #############################

ph.bon = ggplot() +
  geom_raster(data = mean_df_LB, aes(x = x, y = y, fill = sqrt((sediment_mean)*100))) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_met_c(name = "Isfahan1", direction = -1) +
  labs(title = "Lake Bonney", x = "Easting", y = "Northing", fill = "mean") +
  annotation_scale(location = "br", width_hint = 0.3) + 
  theme_bw(base_size = 8) + 
  theme(axis.text = element_blank(), 
        legend.position = "none")

ph.hor <- ggplot() +
  geom_raster(data = mean_df_LH, aes(x = x, y = y, fill = sqrt((sediment_mean)*100))) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_met_c(name = "Isfahan1", direction = -1) +
  labs(title = "Lake Hoare", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  annotation_scale(location = "br", width_hint = 0.3) + 
  theme_bw(base_size = 8) + 
  theme(axis.text = element_blank(), 
        legend.position = "none")

ph.frx <- ggplot() +
  geom_raster(data = mean_df_LF, aes(x = x, y = y, fill = sqrt((sediment_mean)*100))) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_met_c(name = "Isfahan1", direction = -1) +
  labs(title = "Lake Fryxell", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  annotation_scale(location = "br", width_hint = 0.3) + 
  theme_bw(base_size = 8) + 
  theme(axis.text = element_blank(), 
        legend.position = "none")

ph.frx + ph.hor + ph.bon +
plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))

ggsave('Figures/FigureSX_Hotspots.png', width = 6.5, height = 2.2, dpi = 500)
