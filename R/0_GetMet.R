library(tidyverse)

# Read in albedo box data
abox = read_csv('Data/ALBEDO_BOX.csv') |> 
  filter(!is.na(LATITUDE)) |> 
  mutate(
    DATE_TIME = mdy_hms(DATE_TIME),
    DATE_TIME_HOUR = round_date(DATE_TIME, unit = "15 minutes"), 
    albedo.date = as.Date(DATE_TIME))

abox.hours = unique(abox$DATE_TIME_HOUR)

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
  filter(year(date_time) >= 2015) |> 
  dplyr::select(date_time, swradin_wm2) |> 
  filter(date_time %in% abox$DATE_TIME_HOUR) |> 
  rename(FRX.swradin_wm2 = swradin_wm2)

# Dugan, H.A., P.T. Doran, and A.G. Fountain. 2025. High-frequency, hourly, and daily measurements from Lake Hoare Meteorological Station (HOEM), 
# McMurdo Dry Valleys, Antarctica (1987-2025, ongoing) ver 21. Environmental Data Initiative. https://doi.org/10.6073/pasta/a094e5c7c1a507ef1a574ae02b6ee2b8 (Accessed 2025-06-26).

# inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7011/21/8f4c1c307f617ac1e8cd78158a0a560f" # 1 hour
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7011/21/90d844ff46de094c905bbdc4e7db1ac8" # 15 min
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
# SWradIN
HORmet <- read_csv(infile2) |> 
  filter(year(date_time) >= 2015) |> 
  dplyr::select(date_time, swradin_wm2) |> 
  filter(date_time %in% abox$DATE_TIME_HOUR) |> 
  rename(HOR.swradin_wm2 = swradin_wm2)

# Dugan, H.A., P.T. Doran, and A.G. Fountain. 2025. High-frequency, hourly, and daily measurements from Lake Bonney Meteorological Station (BOYM), 
# McMurdo Dry Valleys, Antarctica (1993-2025, ongoing) ver 22. Environmental Data Initiative. https://doi.org/10.6073/pasta/3e680ff4a17518b1f391a599552362cf (Accessed 2025-06-26).

# inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7003/22/77fa74c6cc7140a20666670430758026" # 1 hour
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7003/22/56fd122d4e61c0bfa414b3050bd6a7d5" # 15 min
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
# SWradIN
BONmet <- read_csv(infile2) |> 
  filter(year(date_time) >= 2015) |> 
  dplyr::select(date_time, swradin_wm2) |> 
  filter(date_time %in% abox$DATE_TIME_HOUR) |> 
  rename(BON.swradin_wm2 = swradin_wm2)


# inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7003/22/77fa74c6cc7140a20666670430758026" # 1 hour
inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7003/22/56fd122d4e61c0bfa414b3050bd6a7d5" # 15 min
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")
# SWradIN
BONmet <- read_csv(infile2) |> 
  filter(year(date_time) >= 2015) |> 
  dplyr::select(date_time, swradin_wm2) |> 
  filter(date_time %in% abox$DATE_TIME_HOUR) |> 
  rename(BON.swradin_wm2 = swradin_wm2)

# Dugan, H.A., P.T. Doran, and A.G. Fountain. 2025. High-frequency, hourly, and daily measurements from Taylor Glacier Meteorological 
# Station (TARM), McMurdo Dry Valleys, Antarctica (1994-2025, ongoing) ver 20. Environmental Data Initiative. https://doi.org/10.6073/pasta/f31d224c846e28d556ee5c395dfd461b 
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-mcm/7013/20/db11c6825f5b15897d6b868bb92e3788" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
# SWradIN
TAYmet <- read_csv(infile1) |> 
  filter(year(date_time) >= 2015) |> 
  dplyr::select(date_time, swradin_wm2) |> 
  filter(date_time %in% abox$DATE_TIME_HOUR) |> 
  rename(TAY.swradin_wm2 = swradin_wm2)

met = FRXmet |> left_join(HORmet) |> left_join(BONmet) |> left_join(TAYmet)

ggplot(met) +
  geom_point(aes(x = FRX.swradin_wm2, HOR.swradin_wm2))

ggplot(met) +
  geom_point(aes(x = BON.swradin_wm2, TAY.swradin_wm2))

