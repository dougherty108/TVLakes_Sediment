###### Initial Visualization and Munging of Albedo Box data from Bergstrom Thesis (2020) ###

library(terra)
library(tidyverse)
library(lubridate)
library(ggmap)

# load dataset
setwd("~/Documents/R-Repositories/TVLakes_Sediment")

# load albedo box dataset

albedo_box = read_csv("Data/ALBEDO_BOX.csv") |> 
  mutate(DATE_TIME = mdy_hms(DATE_TIME)) |> 
  drop_na()

#bounding box
bbox <- make_bbox(lon = albedo_box$LONGITUDE, lat = albedo_box$LATITUDE, f = 0.1)

# add the basemap
basemap <- get_stadiamap(bbox = bbox, zoom = 10)

# plot data
ggplot(albedo_box, aes(x = LATITUDE, y = LONGITUDE, fill = RADIATION)) + 
  geom_point() + 
  scale_color_viridis_c() +
  coord_fixed() +
  facet_wrap(~FLIGHT_DATE) + 
  theme_minimal() +
  labs(title = "Radiation Levels by Location",
       x = "Longitude", y = "Latitude", color = "Radiation")


ggmap(basemap) +
  geom_point(data = albedo_box, aes(x = LONGITUDE, y = LATITUDE, color = RADIATION), size = 3) +
  scale_color_viridis_c() +
  labs(title = "Radiation Levels with Basemap",
       x = "Longitude", y = "Latitude", color = "Radiation") +
  theme_minimal()
