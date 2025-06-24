library(tidyverse)
library(terra)

# Step 1: List all .tif files
files <- list.files("Data/landsat/RGB_images", pattern = "\\.tif$", full.names = TRUE)

# Step 2: Extract date using regex
df.files <- tibble(
  filename = files,
  lake = str_extract(files, "(?<=LANDSAT_)[A-Z]+(?=_)"),
  date = str_extract(files, "\\d{4}-\\d{2}-\\d{2}")) |> 
  mutate(date = as.Date(date))

# fryxell_utm is an sf object, convert it to SpatVector
fryxell_vect <- vect(fryxell_utm)
hoare_vect <- vect(hoare_utm)
bonney_vect <- vect(bonney_utm)

# B2 = blue, B3 = green, B4 = red
getRGB <- function(RGB_name, usefun = 'mean', shapefile) {
  FRY_raster_RGB = rast(RGB_name) # load files as raster
  FRY_project_RGB <- project(FRY_raster_RGB, "EPSG:32758")
  
  # Crop the raster to the extent of the polygon
  fry_cropped <- crop(FRY_project_RGB, shapefile)
  # Mask the cropped raster to the exact shape of the polygon
  fry_masked <- mask(fry_cropped, shapefile)
  
  mean_vals <- global(fry_masked, fun = usefun, na.rm = TRUE)
  mean.tib = as_tibble(t(mean_vals)) |> 
    rename_with(.cols = c("B2", "B3", "B4"), .fn = ~ paste0(.x, usefun))
  return(mean.tib)
}  
  
# Get max RGB values for Lake Fryxell
RGB.LF.list = list()
df.files.LF = df.files |> filter(lake == 'FRY')
  # filter(date %in% unique.dates$date.sed)
for (i in 1:nrow(df.files.LF)) {
  RGB.LF.list[[i]] = data.frame(df.files.LF[i,2:3]) |> 
  bind_cols(getRGB(RGB_name = as.character(df.files.LF[i,1]), usefun = 'mean', shapefile = fryxell_vect))
}

RGB.LF = bind_rows(RGB.LF.list) |> 
  rename(sed.date = date) |>
  as_tibble() |> 
  mutate(lake = 'Lake Fryxell')

# Get max RGB values for Lake Hoare
RGB.LH.list = list()
df.files.LH = df.files |> filter(lake == 'HOA') |> 
  filter(date %in% unique.dates$date.sed)
for (i in 1:nrow(df.files.LH)) {
  RGB.LH.list[[i]] = data.frame(df.files.LH[i,2:3]) |> 
    bind_cols(getRGB(RGB_name = as.character(df.files.LH[i,1]), usefun = 'mean', shapefile = hoare_vect)) |> 
    bind_cols(getRGB(RGB_name = as.character(df.files.LH[i,1]), usefun = 'max', shapefile = hoare_vect)) |> 
    bind_cols(getRGB(RGB_name = as.character(df.files.LH[i,1]), usefun = 'min', shapefile = hoare_vect))
}

RGB.LH = bind_rows(RGB.LH.list) |> 
  rename(sed.date = date) |>
  as_tibble() |> 
  rename(lake = 'Lake Hoare')

# Get max RGB values for Lake Bonney
RGB.LB.list = list()
df.files.LB = df.files |> filter(lake == 'BON') |> 
  filter(date %in% unique.dates$date.sed)
for (i in 1:nrow(df.files.LB)) {
  RGB.LB.list[[i]] = data.frame(df.files.LB[i,2:3]) |> 
    bind_cols(getRGB(RGB_name = as.character(df.files.LB[i,1]), usefun = 'mean', shapefile = bonney_vect)) |> 
    bind_cols(getRGB(RGB_name = as.character(df.files.LB[i,1]), usefun = 'max', shapefile = bonney_vect)) |> 
    bind_cols(getRGB(RGB_name = as.character(df.files.LB[i,1]), usefun = 'min', shapefile = bonney_vect))
}

RGB.LB = bind_rows(RGB.LB.list) |> 
  rename(sed.date = date) |>
  as_tibble() |> 
  rename(lake = 'Lake Bonney')
 


