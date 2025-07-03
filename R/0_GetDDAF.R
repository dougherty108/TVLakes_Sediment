# Get meteorological data for Lake Fryxell
# Dugan, H.A., P.T. Doran, and A.G. Fountain. 2025. High-frequency, hourly, and daily measurements from Lake Fryxell Meteorological Station (FRLM), 
# McMurdo Dry Valleys, Antarctica (1993-2025, ongoing) ver 19. Environmental Data Initiative. https://doi.org/10.6073/pasta/12c8b7dff536c07ee61ccbc65a4154f8 (Accessed 2025-06-26).
# inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/19/576d95d3e9fb41ceec3821ae6f5072b1" # 1 hour
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7010/19/0b60fddb80da8bb222b08084e154e1ca" # 15 min
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

# Take 3 hour rolling mean of SWradIN
FRXmet <- read_csv(infile2) |> 
  filter(date_time >= as.POSIXct('2014-10-01')) |> 
  mutate(wateryear = if_else(month(date_time) >= 10, year(date_time) + 1, year(date_time)))

############### Degree days ##################
## Function where you can input cutoff
deg.days.date <- function(df, cutoff) {
  df |> mutate(timestep = as.numeric(date_time - lag(date_time))) |>  # what is time step 
    filter(airtemp_3m_degc >= cutoff) |> 
    mutate(airtemp_3m_degc = airtemp_3m_degc - cutoff) |> 
    mutate(year = year(date_time)) |> 
    group_by(wateryear) |> 
    summarise(dd = sum(airtemp_3m_degc*(timestep/60/24), na.rm = T)) 
}

dd_cutoff <- function(df.hourly, usecutoff) {
  df.hourly |>  
    # Remove rows that match site + wateryear in missing data
    group_by(metlocid) %>%
    group_split() %>%
    map_dfr(function(group_df) {
      dds = deg.days.date(group_df, cutoff = usecutoff)
      dds |> mutate(metlocid = unique(group_df$metlocid)) |> mutate(cutoff = usecutoff)
    })
}
ddm5 = dd_cutoff(FRXmet, -2)
ddm0 = dd_cutoff(FRXmet, 0)
ddm3 = dd_cutoff(FRXmet, 3)

dd = ddm5 |> bind_rows(ddm0)

ggplot(dd) +
  geom_path(aes(x = wateryear, y = dd, col = as.factor(cutoff), group = as.factor(cutoff))) +
  geom_point(aes(x = wateryear, y = dd, col = as.factor(cutoff))) +
  ylab('Deg days') +
  theme_bw(base_size = 9) +
  scale_color_manual(values = c('lightblue4','lightblue2'), name = 'cutoff') 
  # facet_wrap(~site)

# write_csv(dd, 'dataout/degdays.csv')

############### Warm sunny days ##################

# Stone paper 
# WSdays were calculated as the number of days each year when mean daily air temperature was greater than −1.5°C (x1 = −1.5°C) 
# and mean daily incoming shortwave radiation was greater than 367 W m−2 (x2 = 367 W m−2).

FRXmet.daily = FRXmet |> 
  group_by(wateryear, date = as.Date(date_time)) |> 
  summarise(air.mean = mean(airtemp_3m_degc, na.rm = T), sw.mean = mean(swradin_wm2, na.rm = T)) |> 
  filter(air.mean >= -1.5 & sw.mean >= 367) |> 
  group_by(wateryear) |> 
  summarise(warmsunnies = n()) |> 
  left_join(ddm3)

