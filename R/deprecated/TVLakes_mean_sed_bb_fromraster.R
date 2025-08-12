## libraries
library(raster)
library(sf)
library(tidyverse)

########################## For a single buffer distance ########################## 

# change to whatever your machine is
setwd("~charliedougherty")

#replace with your google drive outputs of the GEE side of the script. 
files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/panchromatic", pattern = ".tif", full.names = TRUE)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/panchromatic")

# Predefine output tibble
output <- tibble(
  date = character(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

# Define point coordinates
points_df <- data.frame(
  name = c("Lake Fryxell", "Lake Hoare", "East Lake Bonney", "West Lake Bonney"),
  x = c(391748.223282, 396517.85394052055, 404047.2666197109, 407169.73944380396),
  y = c(-1293198.163127, -1289740.3825915689, -1277516.4884229063, -1275776.8988470172)
)

# Convert to sf object and buffer
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)  

# Buffer after ensuring the correct CRS. buffer distance is in meters
buffered_points_sf <- st_buffer(points_sf, dist = 300)

# Convert `sf` buffer object to `Spatial` before using extract()
buffered_points_sp <- as(buffered_points_sf, "Spatial")  

setwd("~charliedougherty")
# Loop through each raster file
for (i in seq_along(files)) {
  raster_file <- raster(files[i])

  # Extract mean values using the corrected object
  extracted_values <- raster::extract(raster_file, buffered_points_sp, fun = mean, na.rm = TRUE)
  
  # Extract date from filename
  date <- str_extract(files[i], "20\\d{2}-\\d{2}-\\d{2}")
  
  # Append results to output tibble
  output <- bind_rows(output, tibble(
    date = date, 
    `Lake Fryxell` = extracted_values[1],
    `Lake Hoare` = extracted_values[2], 
    `East Lake Bonney` = extracted_values[3], 
    `West Lake Bonney` = extracted_values[4]
  ))
  
  print(i)  # Keep track of progress
}


setwd("~/Documents/R-Repositories/TVLakes_Sediment")

# Transform and save output
output_to_save <- output |> 
  pivot_longer(cols = c(`East Lake Bonney`, `Lake Hoare`, `Lake Fryxell`, `West Lake Bonney`), names_to = "lake", values_to = "sediment") |>
  drop_na() |> 
  mutate(date = ymd(date),
         ) |> 
  drop_na()

# Plot results
ggplot(output_to_save, aes(date, sediment_corrected)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat Panchromatic") + 
  theme_linedraw()

ggplot(output_to_save, aes(date, ice_abundance)) + 
  geom_point() + 
  facet_wrap(vars(lake)) + 
  ggtitle("Landsat") + 
  theme_minimal()


########################## for multiple buffer distances ########################## 

library(sf)
library(raster)
library(tibble)
library(stringr)
library(dplyr)

setwd("~charliedougherty")

files <- list.files(path = "~/Google Drive/My Drive/EarthEngine/landsat/20250325", pattern = ".tif", full.names = TRUE)

setwd("~/Google Drive/My Drive/EarthEngine/landsat/20250325")

# Define point coordinates
points_df <- data.frame(
  name = c("Lake Fryxell", "Lake Hoare", "East Lake Bonney", "West Lake Bonney"),
  x = c(391748.223282, 396517.85394052055, 404047.2666197109, 407169.73944380396),
  y = c(-1293198.163127, -1289740.3825915689, -1277516.4884229063, -1275776.8988470172)
)

# Convert to sf object and buffer
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)  

setwd("~charliedougherty")

# Define buffer distances
buffer_distances <- c(50, 100, 150, 200, 300, 400, 500)  # Modify distances as needed
#buffer_distances = 100
# Convert to sf object and buffer
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)  


# Convert points to sf object
points_sf <- st_as_sf(points_df, coords = c("x", "y"), crs = 3031)


output_buffers <- tibble(
  date = character(),
  buffer_distance = numeric(),
  `Lake Fryxell` = numeric(),
  `Lake Hoare` = numeric(),
  `East Lake Bonney` = numeric(),
  `West Lake Bonney` = numeric()
)

for (buffer_dist in buffer_distances) {
  buffered_points_sf <- st_buffer(points_sf, dist = buffer_dist)
  buffered_points_sp <- as(buffered_points_sf, "Spatial")
  
  for (i in seq_along(files)) {
    raster_file <- raster(files[i])
    extracted_values <- raster::extract(raster_file, buffered_points_sp, fun = mean, na.rm = TRUE)
    date <- str_extract(files[i], "20\\d{2}-\\d{2}-\\d{2}")
    
    output_buffers <- bind_rows(output_buffers, tibble(
      date = date,
      buffer_distance = buffer_dist,
      `Lake Fryxell` = extracted_values[1],
      `Lake Hoare` = extracted_values[2],
      `East Lake Bonney` = extracted_values[3],
      `West Lake Bonney` = extracted_values[4]
    ))
    
    print(paste("Processed file", i, "for buffer", buffer_dist))
  }
}

########################## do a little plotting of different buffer distances ########################## 

# define function for seasons
## Define a season function to plot data by season. Makes data viz a lot easier. 
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Summer ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Summer ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Summer ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Fall ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Fall ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Winter ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Winter ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Spring ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Summer ", year))  # October is Fall
  }
}



buffer_plot <- output_buffers |> 
  pivot_longer(cols = c(`Lake Fryxell`, `Lake Hoare`, `East Lake Bonney`, `West Lake Bonney`), 
               names_to = "lake", values_to = "sediment") |> 
  drop_na(sediment) |> 
  mutate(date = ymd(date), 
         buffer_distance = as.character(buffer_distance),
         season = sapply(date, get_season))

ggplot(buffer_plot, aes(date, (1-sediment)*100, color = buffer_distance)) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  xlab("Date") + ylab("Sediment Coverage (%)") + 
  theme_linedraw(base_size = 20) 

buffer_pivoted = buffer_plot |> 
  pivot_wider(names_from = buffer_distance, values_from = sediment)

# 50, 100, 150, 200, 300
ggplot(buffer_pivoted, aes(`150`, `300`)) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free")


