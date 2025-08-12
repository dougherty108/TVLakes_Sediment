###### Plotting of SMA outputs #####

library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(terra)
library(MetBrewer)
library(RColorBrewer)
library(ggspatial)
library(patchwork)

files <- list.files(path = 'Data/20250325/', pattern = ".tif", full.names = TRUE)

# Create dataframe of lake blue box locations 
lakes_df <- data.frame(
  lake = c("Lake Fryxell", "Lake Hoare", "East Lake Bonney", "West Lake Bonney"),
  lat = c(-77.610275, -77.627703, -77.713515, -77.720000),
  lon = c(163.146877, 162.910475, 162.449109, 162.299291)
)
# Convert to sf object
lakes_sf <- st_as_sf(lakes_df, coords = c("lon", "lat"), crs = 4326)  # WGS84
lakes_utm <- st_transform(lakes_sf, crs = 32758)


# Select color palette
met_palette <- MetBrewer::met.brewer("Hokusai2")

# # Extract type from filename
# get_type <- function(filename) {
#   str_extract(filename, "(?<=LANDSAT_)(FRY|HOA|BON)(?=_unmix)")
# }
# 
# # Create output directories for each type
# output_base <- "Data/OutPlots/20250610"
# dir.create(output_base, showWarnings = FALSE)
# 
# types <- unique(na.omit(sapply(files, get_type)))
# for (t in types) {
#   dir.create(file.path(output_base, t), showWarnings = FALSE)
# }
# 
# # Loop to create and save plots
# for (i in 1:length(files)) {
#   # setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")
#   
#   raster_file <- rast(files[[i]])
#   raster_file <- project(raster_file, "EPSG:32758")
#   
#   raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
#     drop_na()
#   
#   raster_df = raster_df |> 
#     mutate(sediment_coverage = (1 - ice_endmember))
#   
#   year <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
#   type <- get_type(files[[i]])
#   
#   if (!is.na(type)) {
#     plot_path <- file.path(output_base, type, paste0("LANDSAT_plot_", type, "_", year, ".png"))
#     
#     ggplot() +
#       geom_raster(data = raster_df, aes(x = x, y = y, fill = sediment_coverage)) +
#       coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
#       scale_fill_gradientn(colors = met_palette, name = 'Sed %') +
#       labs(#title = paste0(type, " ", year), 
#            x = "Easting", y = "Northing") +
#       # annotation_north_arrow(location = "tr", which_north = "true",
#       #                        style = north_arrow_fancy_orienteering, height = unit(0.5, "cm"), width = unit(0.5, "cm")) +
#       annotation_scale(location = "br", width_hint = 0.3) + 
#       theme_bw(base_size = 8) + 
#       theme(#axis.text.x = element_text(angle = 45, hjust = 1), 
#             legend.position = 'bottom', 
#             legend.key.height = unit(0.2, 'cm'),
#             legend.key.width = unit(1, 'cm'),
#             legend.margin = margin(t = 0, unit='cm'),
#             axis.text = element_blank())
#     
#     # setwd("~/Google Drive/My Drive/EarthEngine/plots/20250414")
#     ggsave(filename = plot_path, width = 3, height = 3, dpi = 500)
#     print(paste0("Saved plot for ", type, " - ", year, " (", i, "/", length(files), ")"))
#   }
# }



######## plotting for comparison figure in manuscript; SMA vs RGB

smaPlot <- function(SMA_name) {
  FRY_raster_SMA = rast(SMA_name) # load files as raster
  FRY_project_SMA <- project(FRY_raster_SMA, "EPSG:32758") # reproject files for correct orientation
  
  # convert to dataframe in order to plot with ggplot
  FRY_raster_SMA_df = as.data.frame(FRY_project_SMA, xy = TRUE) |> 
    drop_na() |> 
    mutate(sediment_coverage = 100*(1 - ice_endmember))
  
  FRY_plot_SMA = ggplot() +
    geom_raster(data = FRY_raster_SMA_df, aes(x = x, y = y, fill = sediment_coverage)) +
    coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
    scale_fill_gradientn(colors = met_palette, name = 'Sed %') +
    labs(x = "Easting", y = "Northing") +
    annotation_scale(location = "br", width_hint = 0.3) + 
    theme_bw(base_size = 8) + 
    theme(
      legend.position = "inside", 
      legend.position.inside =  c(.05, .95), 
      legend.justification.inside = c(0, 1),
      legend.key.height = unit(0.5, 'cm'),
      legend.key.width = unit(0.5, 'cm'),
      legend.margin = margin(t = 0, unit='cm'),
      axis.text = element_blank())
}

print(smaPlot("Data/landsat/20250325/LANDSAT_BON_unmix_mar25_2023-01-10.tif"))

rgbPlot <- function(RGB_name, label) {
  FRY_raster_RGB = rast(RGB_name) # load files as raster
  FRY_project_RGB <- project(FRY_raster_RGB, "EPSG:32758")
  
  FRY_raster_RGB_df = as.data.frame(FRY_project_RGB, xy = TRUE) |> 
    mutate(
      B4 = scales::rescale(B4, to = c(0, 1)),
      B3 = scales::rescale(B3, to = c(0, 1)),
      B2 = scales::rescale(B2, to = c(0, 1))
    )
  
  p1 = ggplot(FRY_raster_RGB_df) +
    geom_raster(aes(x = x, y = y, fill = rgb(B4, B3, B2))) +
    scale_fill_identity() +
    labs(x = "Easting", y = "Northing") +
    coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) + 
    annotation_scale(location = "br", width_hint = 0.3) + 
    annotate('text', x = -Inf, y = Inf, 
             label = label, size = 3, 
             hjust = -0.1, vjust = 1.3,) +
    theme_bw(base_size = 8) + 
    theme(
      legend.position = 'bottom', 
      legend.key.height = unit(0.2, 'cm'),
      legend.key.width = unit(1, 'cm'),
      legend.margin = margin(t = 0, unit='cm'),
      axis.text = element_blank())
  return(p1)
}

print(rgbPlot("Data/landsat/RGB_images/LANDSAT_BON_RGB_mar06_2023-01-10.tif", label = 'test')) #test

smaBonney = smaPlot("Data/landsat/20250325/LANDSAT_BON_unmix_mar25_2023-01-10.tif")
rbgBonney = rgbPlot(RGB_name = "Data/landsat/RGB_images/LANDSAT_BON_RGB_mar06_2023-01-10.tif", label = 'Lake Bonney, 2023-01-10') +
  geom_sf(data = lakes_utm |> filter(lake %in% c('East Lake Bonney', 'West Lake Bonney')), shape = 21, fill = 'gold2', stroke = 0.2)

smaHoare = smaPlot("Data/landsat/20250325/LANDSAT_HOA_unmix_mar25_2020-12-24.tif")
rbgHoare = rgbPlot("Data/landsat/RGB_images/LANDSAT_HOA_RGB_mar06_2020-12-24.tif", label = 'Lake Hoare, 2020-12-14') +
  geom_sf(data = lakes_utm |> filter(lake %in% c('Lake Hoare')), shape = 21, fill = 'gold2', stroke = 0.2)

smaFryxell = smaPlot("Data/landsat/20250325/LANDSAT_FRY_unmix_mar25_2019-11-06.tif")
rbgFryxell = rgbPlot("Data/landsat/RGB_images/LANDSAT_FRY_RGB_mar07_2019-11-06.tif", label = 'Lake Fryxell, 2019-11-06') +
  geom_sf(data = lakes_utm |> filter(lake %in% c('Lake Fryxell')), shape = 21, fill = 'gold2', stroke = 0.2)


rbgFryxell + smaFryxell + 
  rbgHoare + smaHoare +
  rbgBonney + smaBonney +
  plot_layout(byrow = TRUE, ncol = 2, nrow = 3, guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag = element_text(size = 8))
  
ggsave('Figures/Figure1_LandsatScenes.png', width = 6.5, height = 7, dpi = 500)


