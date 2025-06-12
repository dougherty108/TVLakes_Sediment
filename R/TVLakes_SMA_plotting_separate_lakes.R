###### Plotting of SMA outputs #####

library(raster)
library(sf)
library(tidyverse)
library(lubridate)
library(terra)
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
output_base <- "~/Google Drive/My Drive/EarthEngine/plots/20250414"
dir.create(output_base, showWarnings = FALSE)

types <- unique(na.omit(sapply(files, get_type)))
for (t in types) {
  dir.create(file.path(output_base, t), showWarnings = FALSE)
}

# Loop to create and save plots
for (i in 1:length(files)) {
  setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")
  
  raster_file <- rast(files[[i]])
  raster_file <- project(raster_file, "EPSG:32758")
  
  raster_df <- as.data.frame(raster_file, xy = TRUE) |> 
    drop_na()
  
  raster_df = raster_df |> 
    mutate(sediment_coverage = (1 - ice_endmember))
  
  year <- str_extract(files[[i]], "20\\d{2}-\\d{2}-\\d{2}")
  type <- get_type(files[[i]])
  
  if (!is.na(type)) {
    plot_path <- file.path(output_base, type, paste0("LANDSAT_plot_", type, "_", year, ".png"))
    
    ggplot() +
      geom_raster(data = raster_df, aes(x = x, y = y, fill = sediment_coverage)) +
      coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
      scale_fill_gradientn(colors = met_palette) +
      labs(title = paste0(type, " ", year), x = "Easting", y = "Northing") +
      annotation_north_arrow(location = "tr", which_north = "true",
                             style = north_arrow_fancy_orienteering) +
      annotation_scale(location = "bl", width_hint = 0.3) + 
      theme_linedraw(base_size = 15) + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    setwd("~/Google Drive/My Drive/EarthEngine/plots/20250414")
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
  
  raster_file <- rast(files[[i]])
  raster_file <- project(raster_file, "EPSG:32758")
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
      coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
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


######## plotting for comparison figure in manuscript; SMA vs RGB
# Lake Fryxell
library(ggpubr)

# load files as raster
FRY_raster_SMA = rast("LANDSAT_FRY_unmix_mar25_2019-11-06.tif")
FRY_raster_RGB = rast("../RGB_images/LANDSAT_FRY_RGB_mar07_2019-11-06.tif")

#reproject files for correct orientation
FRY_project_SMA <- project(FRY_raster_SMA, "EPSG:32758")

FRY_project_RGB <- project(FRY_raster_RGB, "EPSG:32758")

# convert to dataframe in order to plot with ggplot
FRY_raster_SMA_df = as.data.frame(FRY_project_SMA, xy = TRUE) |> 
  drop_na() |> 
  mutate(sediment_coverage = (1 - ice_endmember))

FRY_raster_RGB_df = as.data.frame(FRY_project_RGB, xy = TRUE) |> 
  mutate(
    B4 = scales::rescale(B4, to = c(0, 1)),
    B3 = scales::rescale(B3, to = c(0, 1)),
    B2 = scales::rescale(B2, to = c(0, 1))
  )

FRY_plot_SMA = ggplot() +
  geom_raster(data = FRY_raster_SMA_df, aes(x = x, y = y, fill = sediment_coverage)) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = paste0("2019-11-06"), x = "Easting", y = "Northing") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

FRY_plot_RGB = ggplot(raster_RGB_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = rgb(B4, B3, B2))) +
  scale_fill_identity() +
  labs(title = paste0(""), x = "Easting", y = "Northing") +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) + 
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_linedraw(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(FRY_plot_SMA, FRY_plot_RGB)


# Lake Hoare

# load files as raster
HOA_raster_SMA = rast("LANDSAT_HOA_unmix_mar25_2020-12-24.tif")
HOA_raster_RGB = rast("../RGB_images/LANDSAT_HOA_RGB_mar06_2020-12-24.tif")

#reproject files for correct orientation
HOA_project_SMA <- project(HOA_raster_SMA, "EPSG:32758")

HOA_project_RGB <- project(HOA_raster_RGB, "EPSG:32758")

# convert to dataframe in order to plot with ggplot
HOA_raster_SMA_df = as.data.frame(HOA_project_SMA, xy = TRUE) |> 
  drop_na() |> 
  mutate(sediment_coverage = (1 - ice_endmember))

HOA_raster_RGB_df = as.data.frame(HOA_project_RGB, xy = TRUE) |> 
  mutate(
    B4 = scales::rescale(B4, to = c(0, 1)),
    B3 = scales::rescale(B3, to = c(0, 1)),
    B2 = scales::rescale(B2, to = c(0, 1))
  )

HOA_plot_SMA = ggplot() +
  geom_raster(data = HOA_raster_SMA_df, aes(x = x, y = y, fill = sediment_coverage)) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = paste0("2023-01-10"), x = "Easting", y = "Northing") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

HOA_plot_RGB = ggplot(HOA_raster_RGB_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = rgb(B4, B3, B2))) +
  scale_fill_identity() +
  labs(title = paste0(""), x = "Easting", y = "Northing") +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) + 
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_linedraw(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(HOA_plot_SMA, HOA_plot_RGB)

# Lake Bonney

# load files as raster
BON_raster_SMA = rast("LANDSAT_BON_unmix_mar25_2023-01-10.tif")
BON_raster_RGB = rast("../RGB_images/LANDSAT_BON_RGB_mar06_2023-01-10.tif")

#reproject files for correct orientation
BON_project_SMA <- project(BON_raster_SMA, "EPSG:32758")

BON_project_RGB <- project(BON_raster_RGB, "EPSG:32758")

# convert to dataframe in order to plot with ggplot
BON_raster_SMA_df = as.data.frame(BON_project_SMA, xy = TRUE) |> 
  drop_na() |> 
  mutate(sediment_coverage = (1 - ice_endmember))

BON_raster_RGB_df = as.data.frame(BON_project_RGB, xy = TRUE) |> 
  mutate(
    B4 = scales::rescale(B4, to = c(0, 1)),
    B3 = scales::rescale(B3, to = c(0, 1)),
    B2 = scales::rescale(B2, to = c(0, 1))
  )

BON_plot_SMA = ggplot() +
  geom_raster(data = BON_raster_SMA_df, aes(x = x, y = y, fill = sediment_coverage)) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = paste0("2023-01-10"), x = "Easting", y = "Northing") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_linedraw() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

BON_plot_RGB = ggplot(BON_raster_RGB_df, aes(x = x, y = y)) +
  geom_raster(aes(fill = rgb(B4, B3, B2))) +
  scale_fill_identity() +
  labs(title = paste0(""), x = "Easting", y = "Northing") +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) + 
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme_linedraw(base_size = 15) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(BON_plot_SMA, BON_plot_RGB)

