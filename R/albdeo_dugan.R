library(sf)
library(dplyr)
library(fuzzyjoin)
library(raster)
library(terra)

# NOTE: In some cases, pilots flew too close to the edge of a lake, which necessitated
# discarding measurements that should be classified as a lake and resulted in low total counts of lake
# measurements (e.g. 7 Dec 15, Table B.1).

# Create a data frame with BB lat/long
lakes <- data.frame(
  lake = c(
    "Lake Fryxell",
    "Lake Hoare",
    "East Lake Bonney",
    "West Lake Bonney"
  ),
  lat = c(-77.610275, -77.627703, -77.713515, -77.720000),
  lon = c(163.146877, 162.910475, 162.449109, 162.299291)
)
# Convert to sf object (WGS 84 coordinate system)
lakes_sf <- st_as_sf(lakes, coords = c("lon", "lat"), crs = 4326)

# Read in albedo box data
abox = read_csv('Data/ALBEDO_BOX.csv') |> 
  mutate(date = as.Date(mdy_hms(DATE_TIME))) |> 
  filter(!is.na(LATITUDE))
# Convert box tibble to sf using LONGITUDE and LATITUDE
box_sf <- st_as_sf(abox, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Transform to projection WGS 84 / UTM zone 58S
lakes_proj <- st_transform(lakes_sf, 32758)
box_proj <- st_transform(box_sf, 32758)

# Join box points with lake points using a spatial filter (within 300 m)
within_300m <- st_join(
  box_proj, lakes_proj,
  join = st_is_within_distance,
  dist = 300
)
# drop NAs for only matched rows
within_300m_filtered <- within_300m %>% filter(!is.na(lake)) |> 
  select(date, ALTITUDE, RADIATION, lake)

# Load sediment data
# These dates are bad for Lake Fryxell due to snow or cloud cover
baddates = as.Date(c( '2016-12-08', '2017-01-09', '2018-12-25', '2019-12-10', '2019-12-12', '2019-12-29', '2021-11-29'))

# Read in CD GEE sed data
sed = read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(wateryear = if_else(month(date) >= 10, year(date) + 1, year(date))) |> 
  filter(!date %in% baddates)

# Example: fuzzy join on dates within 2 days
joined <- difference_inner_join(
  within_300m_filtered,
  sed,
  by = c("date"),  
  max_dist = list(date = 3),
  distance_col = "date_diff") |> 
  filter(lake.x == lake.y) |>  # keep only matching lakes 
  rename(lake = lake.x, LSdate = date.y) |> 
  group_by(LSdate, lake) |> 
  summarise_if(is.numeric, mean, na.rm = TRUE) |> 
  mutate(month = month(LSdate, label = TRUE)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb')))

joined |> filter(lake == 'Lake Fryxell') |> pull(date)

ggplot(joined) +
  geom_point(aes(x = sediment_abundance, y = RADIATION, color = month), size = 3) +
  scale_color_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(~lake)


############ Find overlapping dates ##################
unique(box_proj$date)

unique.dates = difference_inner_join(
  box_proj,
  sed,
  by = c("date"),  
  max_dist = list(date = 2),
  distance_col = "date_diff") |> 
  dplyr::select(date.albedo = date.x, date.sed = date.y) |> 
  distinct(date.albedo, date.sed)

# Read the shapefile
fryxell <- st_read("data/gis/fryxell.shp")
# Transform to EPSG:32758
fryxell_utm <- st_transform(fryxell, crs = 32758) |> 
  st_buffer(dist = -50) # shrink by 50 m

# Read old lake shapefiles
lakes = st_read('Data/gis/Lakes_and_Poonds_1970.shp')
hoare_utm = lakes |> filter(NAME == 'Lake Hoare') |> 
  st_transform(fryxell, crs = 32758) |> 
  st_buffer(dist = -50) # shrink by 50 m
bonney_utm = lakes |> filter(NAME == 'Lake Bonney') |> 
  st_transform(fryxell, crs = 32758) |> 
  st_buffer(dist = -50) # shrink by 50 m

plotMatch <- function(lake, seddate, albedodate) {
  # Test albedo box vs. SMA on 2017-11-22
  if (!file.exists(paste0('Data/landsat/20250325/LANDSAT_',lake,'_unmix_mar25_',seddate,'.tif'))) {
    return(NULL)
  }
  
  FRY_raster_SMA = rast(paste0('Data/landsat/20250325/LANDSAT_',lake,'_unmix_mar25_',seddate,'.tif')) # load files as raster
  FRY_trimmed <- trim(FRY_raster_SMA)
  FRY_project_SMA <- project(FRY_trimmed, "EPSG:32758") # reproject files for correct orientation
  # Convert raster extent to sf polygon
  raster_extent <- st_as_sfc(st_bbox(FRY_project_SMA))  # makes an sf bbox polygon
  
  
  # Filter points that fall within the lake extent 
  if (lake == 'FRY') {
    shape_utm = fryxell_utm
  } else if (lake == 'HOA') {
    shape_utm = hoare_utm
  } else if (lake == 'BON') {
    shape_utm = bonney_utm
  } 
  
  # Generate logical vector: TRUE if point is inside raster extent
  inside <- st_within(box_proj, shape_utm, sparse = FALSE)[, 1]
  # Filter those points
  points_within <- box_proj[inside, ] |> 
    filter(date == as.Date(albedodate))
  
  # Convert raster to data frame for ggplot
  r_df <- as.data.frame(FRY_project_SMA[[1]], xy = TRUE, na.rm = TRUE)
  colnames(r_df)[3] <- "value"  # rename the raster value column
  # ggplot
  p1 = ggplot() +
    geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "C") +
    geom_sf(data = points_within, aes(color = RADIATION), size = 1.2) +
    theme_minimal(base_size = 9) +
    labs(title = seddate) +
    theme(legend.position = 'none', 
          legend.text = element_text(size = 7))
  
  print(p1)
  # Convert sf points to SpatVector (terra format)
  points_vect <- vect(points_within)
  # Extract raster values at point locations
  vals <- extract(FRY_project_SMA, points_vect)
  # Step 3: Combine with original point attributes
  points_with_vals <- cbind(points_within, vals[,-1])  # Remove ID column from extract()
  p2 = ggplot(points_with_vals) +
    geom_point(aes(x = RADIATION, y = ice_endmember)) +
    ylab(paste0('ice, ', seddate)) +
    xlab(paste0('albedo, ', albedodate)) +
    ylim(0,1) +
    labs(title = lake) +
    theme_bw(base_size = 9)
  p1 + p2
  ggsave(paste0('plots/albedoMatchup/',lake,'_',seddate,'.png'), width = 6.5, height = 4, dpi = 500)
}

# plotMatch('FRY','2017-11-21', '2017-11-22')
# Data/landsat/20250325/LANDSAT_HOA_unmix_mar25_2016-11-13.tif: Why doesn't this exist
for (i in 1:nrow(unique.dates)) {
  plotMatch('FRY', as.character(unique.dates[[i,2]]), as.character(unique.dates[[i,1]]))
  plotMatch('BON', as.character(unique.dates[[i,2]]), as.character(unique.dates[[i,1]]))
  plotMatch('HOA', as.character(unique.dates[[i,2]]), as.character(unique.dates[[i,1]]))
  
}

