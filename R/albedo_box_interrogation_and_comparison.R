###### Initial Visualization and Munging of Albedo Box data from Bergstrom Thesis (2020) ###

library(terra)
library(tidyverse)
library(lubridate)
library(ggmap)

# load dataset
setwd("~/Documents/R-Repositories/TVLakes_Sediment")

# load albedo box dataset
# start by just looking at the ELB buffered site
albedo_box = read_csv("Data/ALBEDO_BOX.csv") |> 
  mutate(DATE_TIME = mdy_hms(DATE_TIME))
  
# Convert to sf object (WGS84)
sf_points <- st_as_sf(albedo_box, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
# Transform to EPSG:3031
sf_proj <- st_transform(sf_points, crs = 3031)

coords <- st_coordinates(sf_proj)

# Bind back into data.frame
df_proj <- cbind(data.frame(coords), RADIATION = sf_proj$RADIATION, 
                 FLIGHT_DATE = sf_proj$FLIGHT_DATE, DATE_TIME = sf_proj$DATE_TIME)
  
# plot data
ggplot(df_proj, aes(x = X, y = Y, color = RADIATION)) + 
  geom_point(shape = 1) + 
  scale_color_viridis_c() +
  coord_fixed() +
  facet_wrap(~FLIGHT_DATE) + 
  theme_minimal() +
  labs(title = "Radiation Levels by Location",
       x = "Longitude", y = "Latitude", color = "Radiation")


