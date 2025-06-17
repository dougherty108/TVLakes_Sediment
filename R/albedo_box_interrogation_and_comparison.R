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
                 FLIGHT_DATE = sf_proj$FLIGHT_DATE, DATE_TIME = sf_proj$DATE_TIME) |> 
  filter((X < 392248 & X > 391248) | (X < 397017 & X > 396017) | (X < 404547 & X > 403547) | (X < 407669 & X > 406669)) |> 
  filter((Y < -1292698 & Y > -1293698) | (Y < -1289240 & Y > -1289740) | (Y < -1277016 & Y > -1278016) | (Y < -1275276 & Y > -1276276)) |> 
  mutate(
    lake = case_when(
    X < 392248 & X > 391248 ~ "Lake Fryxell", 
    X < 397017 & X > 396017 ~ "Lake Hoare", 
    X < 404547 & X > 403547 ~ "East Lake Bonney", 
    X < 407669 & X > 406669 ~ "West Lake Bonney"
  )
  )
  
df_albedo = df_proj |> 
  group_by(FLIGHT_DATE) |> 
  summarize(mean_radiation = mean(RADIATION), 
            mean_rad_diff = 1-mean_radiation) |> 
  mutate(FLIGHT_DATE = mdy(FLIGHT_DATE), 
         week = week(FLIGHT_DATE), 
         year = year(FLIGHT_DATE))

# plot
ggplot(df_albedo) + 
  geom_col(aes(x = FLIGHT_DATE, y = mean_radiation)) + 
  theme_linedraw()
  
# plot data in x and y
ggplot(df_proj, aes(x = X, y = Y, color = RADIATION)) + 
  geom_point(shape = 1) + 
  scale_color_viridis_c() +
  coord_fixed() +
  facet_wrap(~FLIGHT_DATE) + 
  theme_minimal() +
  labs(title = "Radiation Levels by Location",
       x = "Longitude", y = "Latitude", color = "Radiation")

# load in the GEE albedo estimates and find match ups
gee_albedo = read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(FLIGHT_DATE = date, 
         week = week(FLIGHT_DATE), 
         year = year(FLIGHT_DATE)) 

gee_albedo_wholelake= read_csv("Data/LANDSAT_wholelake_mean_20250403.csv") |> 
  mutate(FLIGHT_DATE = date, 
         week = week(FLIGHT_DATE), 
         year = year(FLIGHT_DATE))

# do a filtering join between the albedo box measurements and gee measurements and we'll compare
comparison_albedo = df_albedo |> 
  left_join(gee_albedo_wholelake, by = join_by(week, year))


ggplot(comparison_albedo, aes(sediment_abundance, mean_rad_diff)) + 
  geom_point() + 
  geom_abline() + 
  theme_linedraw()


comparison_albedo_normal = df_albedo |> 
  left_join(gee_albedo, by = join_by(week, year))

ggplot(comparison_albedo_normal, aes(ice_abundance, mean_radiation)) + 
  geom_point() + 
  geom_abline(intercept = 0, slope = 1) + 
  labs(title = "mean albedo box vs. ice abundance") + 
  facet_wrap(vars(lake)) + 
  theme_linedraw()
