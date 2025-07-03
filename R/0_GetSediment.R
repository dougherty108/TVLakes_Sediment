
# Create dataframe of lake blue box locations 
lakes_df <- data.frame(
  lake = c("Lake Fryxell", "Lake Hoare", "East Lake Bonney", "West Lake Bonney"),
  # lat = c(-77.610275, -77.627703, -77.713515, -77.720000),
  # lon = c(163.146877, 162.910475, 162.449109, 162.299291)
  lat = c(-77.610275, -77.627703, -77.713515, -77.720000), # Move Lake Hoare slightly to the left to avoid buffer going into Canada Glacier
  lon = c(163.146877, 162.900475, 162.449109, 162.299291)
)

# Convert to sf object
lakes_sf <- st_as_sf(lakes_df, coords = c("lon", "lat"), crs = 4326)  # WGS84
lakes_utm <- st_transform(lakes_sf, crs = 32758)

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

# Check buffers for lakes 
# lakes_buffer <- st_buffer(lakes_utm, dist = 200)
# # Check buffer
# ggplot(lakes |> filter(NAME %in% c('Lake Fryxell', 'Lake Hoare', 'Lake Bonney'))) +
#   geom_sf() +
#   geom_sf(data = lakes_buffer)

# Separate West Lobe
wlb_utm = st_cast(bonney_utm, "POLYGON")[1,]
elb_utm = st_cast(bonney_utm, "POLYGON")[2,]

# Get SMA files
usefiles = list.files('Data/landsat/20250325/',  pattern = "\\.tif$")

# Function to get values from SMA files 
getSediment <- function(usefile) {
  
  date_string <- str_extract(usefile, "\\d{4}-\\d{2}-\\d{2}")
  lake <- str_extract(usefile, "(?<=LANDSAT_)[A-Z]+")
  
  raster_SMA = rast(paste0('Data/landsat/20250325/',usefile)) # load files as raster
  raster_trimmed <- trim(raster_SMA)
  raster_project_SMA <- project(raster_trimmed, "EPSG:32758") # reproject files for correct orientation
  
  
  # Convert raster extent to sf polygon
  raster_extent <- st_as_sfc(st_bbox(raster_project_SMA))  # makes an sf bbox polygon
  
  # Filter points that fall within the lake extent 
  if (lake == 'FRY') {
    shape_utm = fryxell_utm
    lakename = 'Lake Fryxell'
  } else if (lake == 'HOA') {
    shape_utm = hoare_utm
    lakename = 'Lake Hoare'
  } else if (lake == 'BON') {
    shape_utm = wlb_utm
    lakename = 'West Lake Bonney'
  } 
  
  uselake = lakes_utm |> filter(lake == lakename)
  # Create 300 m buffer
  lakes_buffer <- st_buffer(uselake, dist = 100)
  # Check buffer

  # Mask raster to lake 
  raster_masked <- mask(raster_project_SMA, shape_utm)
  raster_cropped <- crop(raster_masked, shape_utm)
  values <- as_tibble(extract(raster_cropped, shape_utm))
  
  # Mask raster to buffer
  bb_masked <- mask(raster_project_SMA, lakes_buffer)
  bb_cropped <- crop(bb_masked, lakes_buffer)
  bb_values <- as_tibble(extract(bb_cropped, lakes_buffer))
  
  # Summarise 
  values.out = values |> 
    summarise(sed_mean = mean(soil_endmember)) |> 
    mutate(date = date_string, lake = lakename) |> 
    dplyr::select(date, lake, everything())
  
  bb_values.out = bb_values |> 
    summarise(sed_mean_bb = mean(soil_endmember)) |> 
    mutate(date = date_string, lake = lakename) |> 
    dplyr::select(date, lake, everything())
  
  values.out = values.out |> left_join(bb_values.out)
  
  if (lake == 'BON') {
    shape_utm = elb_utm
    lakename = 'East Lake Bonney'
    
    uselake = lakes_utm |> filter(lake == lakename)
    # Create 300 m buffer
    lakes_buffer <- st_buffer(uselake, dist = 200)
    
    # Mask raster to lake 
    raster_masked <- mask(raster_project_SMA, shape_utm)
    raster_cropped <- crop(raster_masked, shape_utm)
    values <- as_tibble(extract(raster_cropped, shape_utm))
    # Mask raster to buffer
    bb_masked <- mask(raster_project_SMA, lakes_buffer)
    bb_cropped <- crop(bb_masked, lakes_buffer)
    bb_values <- as_tibble(extract(bb_cropped, lakes_buffer))
    
    
    values.out2 =  values |> 
      summarise(sed_mean = mean(soil_endmember)) |> 
      mutate(date = date_string, lake = lakename) |> 
      dplyr::select(date, lake, everything())
    
    bb_values.out2 = bb_values |> 
      summarise(sed_mean_bb = mean(soil_endmember)) |> 
      mutate(date = date_string, lake = lakename) |> 
      dplyr::select(date, lake, everything())
    
    values.out2 = values.out2 |> left_join(bb_values.out2)
    
    values.out = values.out |> bind_rows(values.out2)
  }
  
  return(values.out)
} 

# Apply to file list 
results_list <- map(usefiles, getSediment)
# Bind rows
smaResults = bind_rows(results_list) |> 
  mutate(date = as.Date(date))

ggplot(smaResults) +
  geom_point(aes(x = sed_mean, y = sed_mean_bb)) +
  geom_abline() +
  facet_wrap(~lake)

write_csv(smaResults, 'DataOut/sedimentResults.csv')
  
