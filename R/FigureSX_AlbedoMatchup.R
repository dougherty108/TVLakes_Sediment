library(sf)
library(dplyr)
library(fuzzyjoin)
library(raster)
library(terra)
library(stringr)
library(broom)
library(zoo)

# NOTE: In some cases, pilots flew too close to the edge of a lake, which necessitated
# discarding measurements that should be classified as a lake and resulted in low total counts of lake
# measurements (e.g. 7 Dec 15, Table B.1).

# Local Identifier:	knb-lter-mcm.2016.2
# Title:	McMurdo Dry Valleys LTER: Landscape Albedo in Taylor Valley, Antarctica from 2015 to 2019
# Alternate Identifier:	doi:10.6073/pasta/728016d29b9a7df1eec1cf1ac9b17c23
# The measured reflected shortwave radiation can be normalized to incoming solar radiation (measured in-situ at meteorological stations) to calculate landscape albedo.
# Read in albedo box data
abox = read_csv('Data/ALBEDO_BOX.csv') |> 
  filter(!is.na(LATITUDE)) |> 
  mutate(
    DATE_TIME = mdy_hms(DATE_TIME),
    DATE_TIME_HOUR = round_date(DATE_TIME, unit = "hour"), 
    albedo.date = as.Date(DATE_TIME))

# Get meteorological data for Lake Fryxell
source('R/0_GetMet.R')

# Convert box tibble to sf using LONGITUDE and LATITUDE
box_sf <- st_as_sf(abox, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Transform to projection WGS 84 / UTM zone 58S
lakes_proj <- st_transform(lakes_sf, 32758)
box_proj <- st_transform(box_sf, 32758)

# Load sediment data
# These dates are bad for Lake Fryxell due to snow or cloud cover
baddates = as.Date(c( '2016-12-08', '2017-01-09', '2018-12-25', '2019-12-10', '2019-12-12', '2019-12-29', '2021-11-29'))

# Read in CD GEE sed data
sed = read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(wateryear = if_else(month(date) >= 10, year(date) + 1, year(date))) |> 
  filter(!date %in% baddates)

############ Find overlapping dates ##################
unique(box_proj$albedo.date)

unique.dates = 
  data.frame(date.albedo = unique(box_proj$albedo.date)) |> 
  mutate(date.sed = date.albedo - 1) |> 
  filter(date.albedo != '2017-12-07') |> 
  filter(date.sed %in% sed$date)

# Read old lake shapefiles
lakes = st_read('Data/gis/Lakes_and_Poonds_1970.shp')
hoare_utm = lakes |> filter(NAME == 'Lake Hoare') |> 
  st_transform(crs = 32758) |> 
  st_buffer(dist = -100) # shrink by 100 m
bonney_utm = lakes |> filter(NAME == 'Lake Bonney') |> 
  st_transform(crs = 32758) |> 
  st_buffer(dist = -100) # shrink by 100 m
fryxell_utm = lakes |> filter(NAME == 'Lake Fryxell') |> 
  st_transform(crs = 32758) |> 
  st_buffer(dist = -100) # shrink by 100 m

# Plotting function
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
    usemet = met |> dplyr::select(date_time, swradin_wm2 = FRX.swradin_wm2)
  } else if (lake == 'HOA') {
    shape_utm = hoare_utm
    usemet = met |> dplyr::select(date_time, swradin_wm2 = HOR.swradin_wm2)
  } else if (lake == 'BON') {
    shape_utm = bonney_utm
    usemet = met |> dplyr::select(date_time, swradin_wm2 = TAY.swradin_wm2)
  } 
    
  # Generate logical vector: TRUE if point is inside raster extent
  inside <- st_within(box_proj, shape_utm, sparse = FALSE)[, 1]
  # Filter those points
  points_within <- box_proj[inside, ] |> 
    filter(albedo.date == as.Date(albedodate)) |> 
    left_join(usemet, join_by(DATE_TIME_HOUR == date_time)) |> 
    mutate(albedo = RADIATION * 1000/swradin_wm2)
  
  # Convert raster to data frame for ggplot
  r_df <- as.data.frame(FRY_project_SMA[[1]], xy = TRUE, na.rm = TRUE)
  colnames(r_df)[3] <- "value"  # rename the raster value column
  # ggplot
  p1 = ggplot() +
    geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
    scale_fill_viridis_c(option = "C") +
    geom_sf(data = points_within, aes(color = albedo), size = 1.2) +
    theme_minimal(base_size = 8) +
    # labs(title = seddate) +
    theme(legend.position = 'none', 
          axis.title = element_blank(),
          axis.text = element_blank())
  
  # print(p1)
  
  # Convert sf points to SpatVector (terra format)
  points_vect <- vect(points_within)
  # Extract raster values at point locations
  vals <- extract(FRY_project_SMA, points_vect)
  # Step 3: Combine with original point attributes
  points_with_vals <- cbind(points_within, vals[,-1]) |>   # Remove ID column from extract()
    mutate(sed.date = as.Date(seddate), albedo.date = as.Date(albedo.date))
  
  p2 = ggplot(points_with_vals) +
    geom_point(aes(x = albedo, y = ice_endmember)) +
    ylab('LS8 Ice Abundance') +
    xlab(paste0('Albedo Box, ', albedodate)) +
    ylim(0,1) +
    xlim(0,1) +
    # labs(title = lake) +
    theme_bw(base_size = 8) +
    theme(axis.title = element_text(size = 8))
  # plotList[[i]] = 
  # ggsave(paste0('plots/albedoMatchup/',lake,'_',seddate,'.png'), width = 6.5, height = 4, dpi = 500)
  
  return(p1 + p2)
}


# Make SI figures of matchups 
albedo.FRX.list = list()
for (i in 1:nrow(unique.dates)) {
  albedo.FRX.list[[i]] = plotMatch(lake = 'FRY', seddate = as.character(unique.dates[[i,2]]), albedodate = as.character(unique.dates[[i,1]]))
}

patchwork::wrap_plots(albedo.FRX.list, nrow = 4, ncol = 1)
ggsave(paste0('Figures/FiguresX_AlbedoFryxell.png'), width = 5, height = 8, dpi = 500)

# Make SI figures of matchups 
albedo.HOR.list = list()
for (i in 1:nrow(unique.dates)) {
  albedo.HOR.list[[i]] = plotMatch(lake = 'HOA', seddate = as.character(unique.dates[[i,2]]), albedodate = as.character(unique.dates[[i,1]]))
}

patchwork::wrap_plots(albedo.HOR.list, nrow = 4, ncol = 1)
ggsave(paste0('Figures/FiguresX_AlbedoHoare.png'), width = 5, height = 8, dpi = 500)


# Make SI figures of matchups 
albedo.BON.list = list()
for (i in 1:nrow(unique.dates)) {
  albedo.BON.list[[i]] = plotMatch(lake = 'BON', seddate = as.character(unique.dates[[i,2]]), albedodate = as.character(unique.dates[[i,1]]))
}

patchwork::wrap_plots(albedo.BON.list, nrow = 4, ncol = 1)
ggsave(paste0('Figures/FiguresX_AlbedoBonney.png'), width = 5, height = 8, dpi = 500)


