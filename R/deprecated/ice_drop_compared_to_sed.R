library(tidyverse)
library(broom)
library(purrr)

## Authors
# Hilary Dugan, Charlie Dougherty

# Read in CD GEE sed data
sed = read_csv("~/Documents/R-Repositories/TVLakes_Sediment/Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(wateryear = if_else(month(date) >= 10, year(date) + 1, year(date))) 

# Plot sed by month and lake
ggplot(sed) +
  geom_point(aes(x = date, y = sediment_abundance, col = as.factor(month(date)))) +
  facet_wrap(~lake)

# Take Dec-Jan mean sediment for each wateryear 
sed2 = sed |> 
  filter(month(date) %in% c(12,1)) |> 
  group_by(lake, wateryear) |> 
  summarise(sed = mean(sediment_abundance, na.rm = T))

# Read in ice thickness data from MCM database 
ice = read_csv('~/Documents/R-Repositories/TVLakes_Sediment/Data/mcmlter-lake-ice_thickness-20250218_0_2025.csv') |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  rename(lake = location_name) |> 
  filter(lake %in% c('East Lake Bonney', 'West Lake Bonney', 
                     'Lake Fryxell', 'Lake Hoare')) |> 
  filter(year(date_time) >= 2012) |> 
  mutate(year = year(date_time), month = month(date_time))

# Take first ice thickness measurements in the fall and calculate mean for each lake 
ice3 = ice |> 
  mutate(wateryear = if_else(month >= 10, year + 1, year)) |> 
  mutate(yday = yday(date_time)) |> 
  mutate(group = case_when(yday > 200 & yday <= 340 ~ 'first',
                           yday >= 355 | yday <= 100 ~ 'last')) |> 
  filter(group == 'first') |> 
  arrange(lake, date_time) |> 
  group_by(lake, wateryear) |> 
  summarise(z_water_m = mean(z_water_m, na.rm = T),
            z_ice_m = mean(z_ice_m, na.rm = T),
            z_diff_m = mean(z_diff_m, na.rm = T)) |> 
  group_by(lake) |> 
  mutate(ice.diff = c(-diff(z_water_m), NA)) # calculate difference between years

# Join sediment and ice thickness data 
sed.join = ice3 |> left_join(sed2, by = join_by(lake, wateryear)) |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare',
                                        'East Lake Bonney', 'West Lake Bonney')))

ggplot(sed.join) +
  geom_smooth(aes(x = sed, y = ice.diff), method = 'lm', 
              color = 'black', linetype = 2, linewidth = 0.4) +
  geom_point(aes(x = sed, y = ice.diff), size = 3) +
  xlab('Mean Dec-Jan sediment coverage') +
  ylab('∆ Ice thickness between years') +
  facet_wrap(~lake, scales = "free_x") +
  theme_linedraw(base_size = 28)


# None of these are significant, but linear models aren't very robust with only 6 values 
sed.join |> group_by(lake) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, ~ lm(ice.diff ~ sed, data = .x)),
    results = purrr::map(model, tidy)
  ) %>%
  unnest(results)
