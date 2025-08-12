library(tidyverse)
library(broom)
library(purrr)

source('R/0_GetDDAF.R')

## Authors
# Hilary Dugan, Charlie Dougherty
# Read in CD GEE sed data
# Albedo model data
sed = #read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  read_csv('DataOut/AlbedoModel.csv') |> rename(date = sed.date) |>
  mutate(wateryear = if_else(month(date) >= 10, year(date) + 1, year(date)))

# Take Dec-Jan mean sediment/albedo for each wateryear 
sed2 = sed |> 
  filter(month(date) %in% c(11,12,1)) |>
  # filter(yday(date) >= 350 | yday(date) <= 15) |>
  group_by(lake, wateryear) |> 
  summarise(sed_mean = mean(sed_mean, na.rm = T), 
            sed_mean_bb = mean(sed_mean_bb, na.rm = T), 
            # sed.wholelake = mean(sed_wholelake, na.rm = T), 
            albedo.predict.wholelake = mean(albedo.predict.wholelake, na.rm = T),
            albedo.predict.bb = median(albedo.predict.bb, na.rm = T))

# Read in ice thickness data from MCM database 
ice = read_csv('Data/mcmlter-lake-ice_thickness-20250218_0_2025.csv') |> 
  mutate(date_time = as.Date(mdy_hm(date_time))) |> 
  rename(lake = location_name) |> 
  filter(lake %in% c('East Lake Bonney', 'West Lake Bonney', 
                     'Lake Fryxell', 'Lake Hoare')) |> 
  filter(year(date_time) >= 2012) |> 
  mutate(year = year(date_time), month = month(date_time)) |> 
  filter(is.na(comments) | !str_detect(comments, "B-011")) # Filter out B-011 measurements in another location

# Take first ice thickness measurements in the fall and calculate mean for each lake 
ice3 = ice |> 
  mutate(wateryear = if_else(month >= 10, year + 1, year)) |> 
  mutate(yday = yday(date_time)) |> 
  mutate(group = case_when(yday > 200 & yday <= 350 ~ 'first',
                           yday >= 1 & yday <= 60 ~ 'last')) |> 
  # filter(group == 'first') |> 
  arrange(lake, date_time) |> 
  group_by(lake, wateryear, group) |> 
  arrange(lake, date_time) |> 
  summarise(z_water_m = mean(head(z_water_m, 2), na.rm = TRUE)) |> #Instead of taking mean, take mean of first two values
  # summarise(z_water_m = mean(z_water_m, na.rm = T)) |>
  pivot_wider(names_from = group, values_from = z_water_m) |> 
  group_by(lake) |> 
  mutate(ice.year = -(first - last)) |> 
  mutate(ice.diff = c(-diff(first), NA)) # calculate difference between years

# climate
dd.wide = dd |> dplyr::select(-metlocid) |> pivot_wider(names_from = cutoff, values_from = dd, names_prefix = 'dd_')

# Join sediment and ice thickness data 
sed.join = ice3 |> left_join(sed2, by = join_by(lake, wateryear)) |> 
  left_join(FRXmet.daily) |> 
  mutate(lake = factor(lake, levels = c('West Lake Bonney',  'East Lake Bonney', 'Lake Hoare', 'Lake Fryxell')))

# p1 = ggplot(sed.join) +
#   geom_smooth(data = sed.join |> filter(wateryear != 2020), 
#               aes(x = sed, y = ice.diff), method = 'lm', 
#               color = 'black', linetype = 2, linewidth = 0.4) +
#   geom_point(aes(x = sed, y = ice.diff), size = 3) +
#   geom_point(data = sed.join |> filter(wateryear == 2020), aes(x = sed, y = ice.diff), size = 3, col = 'red3') +
#   xlab('Mean Dec-Jan sediment coverage') +
#   ylab('∆ Ice thickness between years') +
#   facet_wrap(~lake, scales = "free_x", nrow = 1) +
#   theme_bw(base_size = 9)


p2 = ggplot(sed.join) +
  geom_smooth(data = sed.join |> filter(wateryear != 2020), 
              aes(x = albedo.predict.wholelake, y = ice.diff), method = 'lm', 
              color = 'black', linetype = 2, linewidth = 0.4) +
  geom_point(aes(x = albedo.predict.wholelake, y = ice.diff), size = 3) +
  geom_point(data = sed.join |> filter(wateryear == 2020), aes(x = albedo.predict.wholelake, y = ice.diff), size = 3, col = 'red3') +
  xlab('Mean Nov-Jan albedo') +
  ylab('∆ Ice thickness between years') +
  facet_wrap(~lake, scales = "free_x", nrow = 1) +
  theme_bw(base_size = 9)

p3 = ggplot(sed.join) +
  geom_smooth(data = sed.join |> filter(wateryear != 2020), 
              aes(x = albedo.predict.wholelake, y = ice.year), method = 'lm', 
              color = 'black', linetype = 2, linewidth = 0.4) +
  geom_point(aes(x = albedo.predict.wholelake, y = ice.year), size = 3) +
  xlab('Mean Nov-Jan albedo') +
  ylab('∆ Ice thickness within year') +
  facet_wrap(~lake, scales = "free_x", nrow = 1) +
  theme_bw(base_size = 9)

p2 / p3 +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8))

ggsave("Figures/Figure4_iceDrop.png", height = 4, width = 6.5, dpi = 500)

# None of these are significant, but linear models aren't very robust with only 6 values 
sed.join |> group_by(lake) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, ~ lm(ice.diff ~ albedo.predict.wholelake, data = .x)),
    results = purrr::map(model, tidy)
  ) %>%
  unnest(results)


sed.join |> group_by(lake) %>%
  nest() %>%
  mutate(
    model = purrr::map(data, ~ lm(ice.year ~ albedo.predict.bb, data = .x)),
    results = purrr::map(model, tidy)
  ) %>%
  unnest(results)

# Test on blue box 200 m buffer
p2_bb = ggplot(sed.join) +
  geom_smooth(data = sed.join |> filter(wateryear != 2020), 
              aes(x = albedo.predict.bb, y = ice.diff), method = 'lm', 
              color = 'black', linetype = 2, linewidth = 0.4) +
  geom_point(aes(x = albedo.predict.bb, y = ice.diff), size = 3) +
  geom_point(data = sed.join |> filter(wateryear == 2020), aes(x = albedo.predict.bb, y = ice.diff), size = 3, col = 'red3') +
  xlab('Mean Nov-Jan albedo') +
  ylab('∆ Ice thickness between years') +
  facet_wrap(~lake, scales = "free_x", nrow = 1) +
  theme_bw(base_size = 9)

p3_bb = ggplot(sed.join) +
  geom_smooth(data = sed.join |> filter(wateryear != 2020), 
              aes(x = albedo.predict.bb, y = ice.year), method = 'lm', 
              color = 'black', linetype = 2, linewidth = 0.4) +
  geom_point(aes(x = albedo.predict.bb, y = ice.year), size = 3) +
  xlab('Mean Nov-Jan Albedo') +
  ylab('∆ Ice thickness between seasons') +
  facet_wrap(~lake, scales = "free_x", nrow = 1) +
  theme_bw(base_size = 9)

# p2_bb / p3_bb

