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
# These dates are bad for Lake Fryxell due to snow or cloud cover
baddates = as.Date(c( '2016-12-08', '2017-01-09', '2018-12-25', '2019-12-10', '2019-12-12', '2019-12-29', '2021-11-29', '2016-12-17', '2023-01-01'))

process_mean_raster <- function(lake) {
  tif_dir <- "Data/landsat/20250325"
  
  # Define directory and search pattern
  pattern <- paste0("LANDSAT_", lake, ".*\\.tif$")
  tif_files <- list.files(tif_dir, pattern = pattern, full.names = TRUE)
  
  # Remove bad dates 
  date_string <- str_extract(tif_files, "\\d{4}-\\d{2}-\\d{2}")
  tif_files = tif_files[!date_string %in% baddates]
  
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

########################## End Members #############################
# Define band names in order
band_names <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2", "Panchromatic")

getBright.sf <- function(filename) {
  lf.bright = read_csv(filename) |> 
    mutate(band_values = str_remove_all(brightest_band_means, "\\[|\\]")) %>%
    separate(band_values, into = band_names, sep = ",\\s*", convert = TRUE) |> 
    dplyr::select(date, Blue:Panchromatic, brightest_geometry) |> 
    mutate(date = as.Date(date))
  
  lf.bright.sf <- lf.bright %>%
    mutate(
      # Remove brackets and split coordinates
      coords = str_remove_all(brightest_geometry, "\\[|\\]"),
      lon = as.numeric(str_split_fixed(coords, ",\\s*", 2)[,1]),
      lat = as.numeric(str_split_fixed(coords, ",\\s*", 2)[,2])
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::select(-coords, -brightest_geometry) |> 
    st_transform(crs = 32758)
  
  return(lf.bright.sf)
}

getDim.sf <- function(filename) {
  
  lf.dim = read_csv(filename) |> 
    mutate(band_values = str_remove_all(dimmest_band_means, "\\[|\\]")) %>%
    separate(band_values, into = band_names, sep = ",\\s*", convert = TRUE) |> 
    dplyr::select(date, Blue:Panchromatic, dimmest_geometry) |> 
    mutate(date = as.Date(date))
  
  lf.dim.sf <- lf.dim %>%
    mutate(
      # Remove brackets and split coordinates
      coords = str_remove_all(dimmest_geometry, "\\[|\\]"),
      lon = as.numeric(str_split_fixed(coords, ",\\s*", 2)[,1]),
      lat = as.numeric(str_split_fixed(coords, ",\\s*", 2)[,2])
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::select(-coords, -dimmest_geometry) |> 
    st_transform(crs = 32758)
  return(lf.dim.sf)
}

lf.bright.sf = getBright.sf('Data/endMembers/endmembers_output_LF_20250325.csv')
lh.bright.sf = getBright.sf('Data/endMembers/endmembers_output_LH_20250325.csv')
lb.bright.sf = getBright.sf('Data/endMembers/endmembers_output_LB_20250325.csv')

lf.dim.sf = getDim.sf('Data/endMembers/endmembers_output_LF_20250325.csv')
lh.dim.sf = getDim.sf('Data/endMembers/endmembers_output_LH_20250325.csv')
lb.dim.sf = getDim.sf('Data/endMembers/endmembers_output_LB_20250325.csv')

########################## PLOTS #############################

ph.bon = ggplot() +
  geom_raster(data = mean_df_LB, aes(x = x, y = y, fill = sqrt((sediment_mean)*100))) +
  # geom_sf(data = lb.dim.sf, size = 2, col = 'gold') +
  # geom_sf(data = lb.bright.sf, size = 2, col = 'gold3') +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_met_c(name = "Isfahan1", direction = -1) +
  labs(title = "Lake Bonney", x = "Easting", y = "Northing", fill = "mean") +
  annotation_scale(location = "br", width_hint = 0.3) + 
  theme_bw(base_size = 8) + 
  theme(axis.text = element_blank(), 
        legend.position = "none"); ph.bon

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
  # geom_sf(data = lf.dim.sf, size = 2, col = 'gold') +
  # geom_sf(data = lf.bright.sf, size = 2, col = 'gold3') +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_met_c(name = "Isfahan1", direction = -1) +
  labs(title = "Lake Fryxell", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  annotation_scale(location = "br", width_hint = 0.3) + 
  theme_bw(base_size = 8) + 
  theme(axis.text = element_blank(), 
        legend.position = "none"); ph.frx

ph.frx + ph.hor + ph.bon +
plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))

ggsave('Figures/FigureSX_Hotspots.png', width = 6.5, height = 2.2, dpi = 500)
