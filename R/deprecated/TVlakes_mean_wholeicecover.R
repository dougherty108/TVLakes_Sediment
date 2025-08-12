########################## 
## libraries
library(raster)
library(sf)
library(tidyverse)

# change to your work directory
setwd("~charliedougherty")

files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/20250325", pattern = ".tif", full.names = TRUE)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")

# Predefine output tibble
output <- tibble(
  date = character(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

# shapefiles
setwd("/Users/charliedougherty")

# Load required libraries
library(sf)
library(raster)
library(dplyr)
library(stringr)

files <- list.files(path = "Google Drive/My Drive/EarthEngine/landsat/20250325", pattern = ".tif", full.names = TRUE)

# Load and transform polygons
lake_shapefiles <- list(
  "Lake Fryxell" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/Lake Fryxell Shapefile.kml",
  "Lake Hoare" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/Lake Hoare Shapefile.kml",
  "East Lake Bonney" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/East Lake Bonney.kml",
  "West Lake Bonney" = "Documents/R-Repositories/MCM-LTER-MS/data/shapefiles/West Lake Bonney.kml"
)

lakes_sf <- lapply(lake_shapefiles, function(shp) {
  read_sf(shp) |> 
    st_cast("POLYGON") |> 
    st_transform(crs = st_crs(raster(files[1]))) |> 
    st_zm()
})

setwd("~charliedougherty")
output <- tibble()

# Loop through each raster file
for (i in seq_along(files)) {
  raster_file <- raster(files[i])
  
  # Extract mean values within each lake polygon
  extracted_values <- sapply(lakes_sf, function(lake) {
    cropped_raster <- mask(crop(raster_file, lake), lake)
    cellStats(cropped_raster, stat = 'mean', na.rm = TRUE)
  })
  
  # Extract date from filename
  date <- str_extract(files[i], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Append results to output tibble
  output <- bind_rows(output, tibble(
    date = date, 
    `Lake Fryxell` = extracted_values["Lake Fryxell"],
    `Lake Hoare` = extracted_values["Lake Hoare"], 
    `East Lake Bonney` = extracted_values["East Lake Bonney"], 
    `West Lake Bonney` = extracted_values["West Lake Bonney"]
  ))
  
  print(i)  # Keep track of progress
}

output_for_save <- output |> 
  pivot_longer(cols = c(`East Lake Bonney`, `Lake Hoare`, `Lake Fryxell`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |>
  drop_na() |> 
  mutate(date = ymd(date), 
         ice_abundance = sediment, 
         sediment_abundance = 1-sediment) |> 
  drop_na()

Save output
#write_csv(output_for_save, "")

# Plot results
ggplot(output_for_save, aes(date, sediment_abundance)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat") + 
  theme_minimal()

ggplot(output_for_save, aes(date, ice_abundance)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat") + 
  theme_minimal()
