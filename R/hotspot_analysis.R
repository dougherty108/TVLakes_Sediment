####### TV Lakes HotSpot Analysis #####
# goal of script is to create hotspot maps of the TV Lakes. You will need to run the SMA scripts in the Python folder
# first before running this, so the script has tif files in Google Drive to use. 
# library
library(terra)
library(tidyverse)
library(sf)
library(ggpubr)
library(ggspatial)

setwd("~charliedougherty")

# Set the directory containing .tif files
# change to your output directory with satellite outputs

########################## BONNEY ########################## 
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250612"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_MIE.*\\.tif$", full.names = TRUE)

# Use the extent and resolution of the first raster as a reference
ref_rast <- rast(tif_files[1])[[1]]

# Resample all rasters to match the reference
aligned_rasters <- lapply(tif_files, function(f) {
  r <- rast(f)[[1]]
  resample(r, ref_rast, method = "bilinear")  # or "near" for categorical data
})

raster_stack <- rast(aligned_rasters)

# Load only the first band of each raster
#raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[1]]))  # Adjust `[[1]]` to desired band index

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)

mean_raster = project(mean_raster, "EPSG:32758")

# Save the output raster
mean_df <- as.data.frame(mean_raster, xy = TRUE) 

colnames(mean_df)[3] = "sediment_var"

mean_df_LB = mean_df #|> 
  #filter(sediment_var < 0.10)

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

ggplot() +
  geom_raster(data = mean_df_LB, aes(x = x, y = y, fill = (sediment_var)*100)) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Miers", x = "Easting", y = "Northing",
       fill = "variance") +
  theme_linedraw(base_size = 20) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)#, 
        #legend.position = "none"
        )

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_bonney_hotspot.png", 
       plot = bonney,
       dpi = 400, width = 8, height = 8)

########################## HOARE ########################## 

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250325"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_HOA.*\\.tif$", full.names = TRUE)

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[2]]))  # Adjust `[[1]]` to desired band index

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F) 
mean_raster <- project(mean_raster, "EPSG:32758")

# Save the output raster
mean_df_LH <- as.data.frame(mean_raster, xy = TRUE) 

colnames(mean_df_LH)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

hoare <- ggplot() +
  geom_raster(data = mean_df_LH, aes(x = x, y = y, fill = (sediment_mean)*100)) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Hoare", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  theme_linedraw(base_size = 20) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)#, 
        #legend.position = "none"
  )

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_hoare_hotspot.png", 
       plot = hoare,
       dpi = 700)


########################## FRYXELL ########################## 

setwd("~charliedougherty")

# Set the directory containing .tif files
tif_dir <- "Google Drive/My Drive/EarthEngine/landsat/20250325"

# Get list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "LANDSAT_FRY.*\\.tif$", full.names = TRUE)

# Load only the first band of each raster
raster_stack <- rast(lapply(tif_files, function(f) rast(f)[[2]]))  # Adjust `[[1]]` to desired band index

# Compute the mean across all layers (ignoring NA values)
mean_raster <- app(raster_stack, fun=mean, na.rm = F)
mean_raster = project(mean_raster, "EPSG:32758")

# Save the output raster
mean_df_LF <- as.data.frame(mean_raster, xy = TRUE)

colnames(mean_df_LF)[3] = "sediment_mean"

# Select color palette
met_palette <- MetBrewer::met.brewer("Derain")

fryxell <- ggplot() +
  geom_raster(data = mean_df_LF, aes(x = x, y = y, fill = (sediment_mean)*100)) +
  coord_sf(crs = sf::st_crs(32758), datum = sf::st_crs(32758)) +
  scale_fill_gradientn(colors = met_palette) +
  labs(title = "Lake Fryxell", x = "Easting", y = "Northing",
       fill = "Sediment (%)") +
  theme_linedraw(base_size = 20) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.3) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)#, 
       # legend.position = "none"
  )

setwd("~/Documents/R-Repositories/MCM-LTER-MS")

ggsave("plots/hotspot/lk_fryxell_hotspot.png", 
       plot = fryxell, dpi = 400, width = 8, height = 9)


legend <- get_legend(dummy)

final_fig = ggarrange(bonney, hoare, fryxell, #legend,
          nrow = 1#, widths = c(1, 1, 1)
          )


annotate_figure(final_fig,
                top = text_grob("Hotspots", face = "bold.italic", size = 20, 
                                y = -2.0))

setwd("~/Documents/R-Repositories/MCM-LTER-MS/plots/manuscript/chapter 1")
ggsave("hotpots_redone_foraxes.png", 
       dpi = 700, height = 7, width = 14)







