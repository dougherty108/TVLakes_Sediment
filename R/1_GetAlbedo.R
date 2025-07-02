library(sf)
library(dplyr)
library(fuzzyjoin)
library(raster)
library(terra)
library(stringr)
library(broom)

# Get meteorological data for Lake Fryxell, Hoare, and Bonney
source('R/0_GetMet.R')

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
  filter(!is.na(LATITUDE)) |> 
  mutate(
    DATE_TIME = mdy_hms(DATE_TIME),
    DATE_TIME_HOUR = round_date(DATE_TIME, unit = "hour"), 
    albedo.date = as.Date(DATE_TIME))

# # Join with abox data
# abox = abox |> left_join(FRXmet, join_by(DATE_TIME_HOUR == date_time)) |> 
#   mutate(albedo = RADIATION * 1000/swradin_wm2)

# Convert box tibble to sf using LONGITUDE and LATITUDE
box_sf <- st_as_sf(abox, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# Transform to projection WGS 84 / UTM zone 58S
lakes_proj <- st_transform(lakes_sf, 32758)
box_proj <- st_transform(box_sf, 32758)

# Load sediment data
# These dates are bad for Lake Fryxell due to snow or cloud cover
baddates = as.Date(c( '2016-12-08', '2017-01-09', '2018-12-25', '2019-12-10', '2019-12-12', '2019-12-29', '2021-11-29', '2016-12-17', '2023-01-01'))

# Read in CD GEE sed data
# sed.wholelake = read_csv('data/LANDSAT_wholelake_mean_20250403.csv') |> 
#   dplyr::select(date, lake, sed_wholelake = sediment_abundance, ice_wholelake = ice_abundance)
# 
# sed = read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  # mutate(wateryear = if_else(month(date) >= 10, year(date) + 1, year(date))) |>
  # filter(!date %in% baddates) |>
#   left_join(sed.wholelake)

sed = read_csv('DataOut/sedimentResults.csv') |> 
    mutate(wateryear = if_else(month(date) >= 10, year(date) + 1, year(date))) |>
    filter(!date %in% baddates)

############ Find overlapping dates ##################
unique.dates = 
  data.frame(date.albedo = unique(box_proj$albedo.date)) |> 
  mutate(date.sed = date.albedo - 1) |> 
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
    theme_minimal(base_size = 9) +
    # labs(title = seddate) +
    theme(legend.position = 'none', 
          legend.text = element_text(size = 7))
  
  print(p1)
  
  # Convert sf points to SpatVector (terra format)
  points_vect <- vect(points_within)
  # Extract raster values at point locations
  vals <- extract(FRY_project_SMA, points_vect)
  # Step 3: Combine with original point attributes
  points_with_vals <- cbind(points_within, vals[,-1]) |>   # Remove ID column from extract()
    mutate(sed.date = as.Date(seddate), albedo.date = as.Date(albedo.date))
  
  p2 = ggplot(points_with_vals) +
    geom_point(aes(x = albedo, y = ice_endmember)) +
    ylab(paste0('landsat , ', seddate)) +
    xlab(paste0('albedo, ', albedodate)) +
    ylim(0,1) +
    labs(title = lake) +
    theme_bw(base_size = 9)
  p1 + p2
  ggsave(paste0('plots/albedoMatchup/',lake,'_',seddate,'.png'), width = 6.5, height = 4, dpi = 500)
  
  return(points_with_vals)
}

# plotMatch('FRY','2017-11-21', '2017-11-22')
# Data/landsat/20250325/LANDSAT_HOA_unmix_mar25_2016-11-13.tif: Why doesn't this exist
albedoMatch.FRY.list = list()
albedoMatch.BON.list = list()
albedoMatch.HOA.list = list()
for (i in 1:nrow(unique.dates)) {
  albedoMatch.FRY.list[[i]] = plotMatch(lake = 'FRY', seddate = as.character(unique.dates[[i,2]]), albedodate = as.character(unique.dates[[i,1]]))
  albedoMatch.BON.list[[i]] = plotMatch(lake = 'BON', seddate = as.character(unique.dates[[i,2]]), albedodate = as.character(unique.dates[[i,1]]))
  albedoMatch.HOA.list[[i]] = plotMatch('HOA', as.character(unique.dates[[i,2]]), as.character(unique.dates[[i,1]]))
}

# Mixing analysis end points
# band_names <- c("Blue", "Green", "Red", "NIR", "SWIR1", "SWIR2", "Panchromatic")
# 
# getDim <- function(filename) {
#   df.dim = read_csv(filename) |> 
#     mutate(band_values = str_remove_all(dimmest_band_means, "\\[|\\]")) %>%
#     separate(band_values, into = paste0(band_names, '_dim'), sep = ",\\s*", convert = TRUE) |> 
#     mutate(sed.date = as.Date(date)) |> 
#     dplyr::select(sed.date, Blue_dim:Panchromatic_dim) 
#     
#   return(df.dim)
# }
# # Mixing analysis end points
# getBright <- function(filename) {
#   df.bright = read_csv(filename) |> 
#     mutate(band_values = str_remove_all(brightest_band_means, "\\[|\\]")) %>%
#     separate(band_values, into = paste0(band_names, '_bright'), sep = ",\\s*", convert = TRUE) |> 
#     mutate(sed.date = as.Date(date)) |> 
#     dplyr::select(sed.date, Blue_bright:Panchromatic_bright) 
#   
#   return(df.bright)
# }
# 
# lf.dim = getDim('Data/endMembers/endmembers_output_LF_20250325.csv')
# lf.bright = getBright('Data/endMembers/endmembers_output_LF_20250325.csv')
# lb.dim = getDim('Data/endMembers/endmembers_output_LB_20250325.csv')

# source('R/0_GetRGB.R')
# write_csv(RGB.LF, 'DataOut/RGBscenes_LF.csv')
# write_csv(RGB.LH, 'DataOut/RGBscenes_LH.csv')
# write_csv(RGB.LB, 'DataOut/RGBscenes_LB.csv')
RGB.LF = read_csv('DataOut/RGBscenes_LF.csv')
RGB.LH = read_csv('DataOut/RGBscenes_LH.csv')
RGB.LB = read_csv('DataOut/RGBscenes_LB.csv')

albedoMatch.FRY = bind_rows(albedoMatch.FRY.list) |> mutate(lake = 'Lake Fryxell') |>
  left_join(RGB.LF |> dplyr::select(-lake))
summary(lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.FRY))
a.LF.model = lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.FRY)
# predict albedo for existing data 
albedoMatch.FRY = albedoMatch.FRY |> bind_cols(albedo.predict = predict(a.LF.model, newdata = albedoMatch.FRY))


albedoMatch.BON = bind_rows(albedoMatch.BON.list) |> mutate(lake = 'Lake Bonney') |> 
  left_join(RGB.LB |> dplyr::select(-lake)) |> 
  filter(albedo.date != as.Date('2017-11-22'))
summary(lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.BON))
a.LB.model = lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.BON)
# predict albedo for existing data 
albedoMatch.BON = albedoMatch.BON |> bind_cols(albedo.predict = predict(a.LB.model, newdata = albedoMatch.BON))


albedoMatch.HOA = bind_rows(albedoMatch.HOA.list) |> mutate(lake = 'Lake Hoare')|>
  left_join(RGB.LF |> dplyr::select(-lake))
summary(lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.HOA))
a.LH.model = lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.HOA)
# predict albedo for existing data 
albedoMatch.HOA = albedoMatch.HOA |> bind_cols(albedo.predict = predict(a.LH.model, newdata = albedoMatch.HOA))

# # Overall model using Lake Fryxell and Lake Bonney
# albedoMatch.FRYBON = albedoMatch.FRY |> bind_rows(albedoMatch.BON)
# summary(lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.FRYBON))
# albedo.model = lm(albedo ~ ice_endmember + B2mean, data = albedoMatch.FRYBON)
# 
# ggplot(albedoMatch.FRYBON) +
#   geom_point(aes(albedo, y = ice_endmember, color = lake), size = 3) +
#   scale_color_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) + 
#   theme_bw(base_size = 9)

# Ok now use this model to predict albedo for all scenes
RGB.join = RGB.LB |> dplyr::select(lake, sed.date, B2mean) |> mutate(lake = 'West Lake Bonney') |> 
  bind_rows(RGB.LB |> dplyr::select(lake, sed.date, B2mean) |> mutate(lake = 'East Lake Bonney')) |> 
  bind_rows(RGB.LF |> dplyr::select(lake, sed.date, B2mean) |> mutate(lake = 'Lake Hoare')) |>
  # bind_rows(RGB.LH |> dplyr::select(lake, sed.date, B2mean)) |> 
  bind_rows(RGB.LF |> dplyr::select(lake, sed.date, B2mean))

# Lake Hoare and Fryxell scenes
sed.LFLH = sed |> 
  filter(lake %in% c('Lake Fryxell', 'Lake Hoare')) |> 
  rename(sed.date = date) |> 
  mutate(ice_endmember = 1-sed_mean) |> 
  left_join(RGB.join) |> 
  mutate(month = month(sed.date, label = TRUE)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) %>%
  mutate(albedo.predict.wholelake = predict(a.LF.model, newdata = .)) |> 
  mutate(ice_endmember = 1-sed_mean_bb) %>%
  mutate(albedo.predict.bb = predict(a.LF.model, newdata = .))

sed.LB = sed |> 
  filter(!lake %in% c('Lake Fryxell', 'Lake Hoare')) |> 
  rename(sed.date = date) |> 
  mutate(ice_endmember = 1-sed_mean) |> 
  left_join(RGB.join) |> 
  mutate(month = month(sed.date, label = TRUE)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) %>%
  mutate(albedo.predict.wholelake = predict(a.LB.model, newdata = .)) |> 
  mutate(ice_endmember = 1-sed_mean_bb) %>%
  mutate(albedo.predict.bb = predict(a.LF.model, newdata = .))


# sed.RGB = sed |>
#   rename(sed.date = date) |>
#   mutate(ice_endmember = 1-sed_mean) |>
#   left_join(RGB.join) |>
#   mutate(month = month(sed.date, label = TRUE)) |>
#   mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) %>%
#   mutate(albedo.predict.wholelake = predict(albedo.model, newdata = .)) |>
#   mutate(ice_endmember = 1-sed_mean_bb) %>%
#   mutate(albedo.predict.bb = predict(albedo.model, newdata = .))

sed.RGB = sed.LFLH |> bind_rows(sed.LB) |> 
  arrange(sed.date)

ggplot(sed.RGB) +
  geom_point(aes(x = albedo.predict.wholelake, y = ice_endmember, color = month), size = 3) +
  scale_color_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(~lake) +
  theme_bw(base_size = 9)

ggplot(sed.RGB) +
  geom_point(aes(x = albedo.predict.bb, y = ice_endmember, color = month), size = 3) +
  scale_color_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(~lake) +
  theme_bw(base_size = 9)

write_csv(sed.RGB, 'DataOut/AlbedoModel.csv')


