# 2019-12-10 SNOWY
# 2019-12-12 SNOWY

# Libraries
library(tidyverse)
library(patchwork)

####################### Ice thickness ############################
mcmIce <- read_csv("Data/mcmlter-lake-ice_thickness-20250218_0_2025.csv") |> 
  mutate(date_time = as.Date(mdy_hm(date_time)), 
         month = month(date_time, label = TRUE), 
         year = year(date_time),
         year = as.numeric(year),
         z_water_m = z_water_m*-1) |> 
  filter(month(date_time) %in% c(10,11,12,1,2)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) |> 
  rename("lake" = location_name) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney") |> 
  filter(!grepl("^B", location)) |> 
  filter(year(date_time) >= 1995) |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

  
mcmIce.fall = mcmIce |> filter(month(date_time) >= 11) 

# Full timeseries
p.ice = ggplot(mcmIce, aes(date_time, z_water_m)) + 
  geom_point(aes(fill = month), size = 1.5, shape = 21, stroke = 0.2) + 
  geom_smooth(data = mcmIce.fall, aes(date_time, z_water_m), 
              se = T, color = 'black', method = 'gam') + 
  scale_fill_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(vars(lake), nrow = 1) + 
  xlab("Date") + 
  ylab("Ice Thickness (m)") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

# Shorten to time period that overlaps with LS
p.ice2 = ggplot(mcmIce, aes(date_time, z_water_m)) + 
  geom_point(aes(fill = month), size = 1.5, shape = 21, stroke = 0.2) + 
  geom_smooth(data = mcmIce.fall, aes(date_time, z_water_m), 
              se = T, color = 'black', method = 'gam') + 
  scale_fill_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(vars(lake), nrow = 1) + 
  xlim(as.Date('2016-10-01'), as.Date('2025-02-01')) +
  xlab("Date") + 
  ylab("Ice Thickness (m)") +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())


# ggsave("Figures/Figure1_iceTimeseries.png", dpi = 500, height = 2, width = 6.5)

####################### Sediment abundance ############################
mean_BB <- #read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |>
  read_csv('DataOut/AlbedoModel.csv') |> rename(date = sed.date) |> 
  # mutate(date = ymd(date), 
  #        type = 'lake_monitoring_station', 
  #        month = month(date, label = TRUE)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) |>
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


# Calculate lake-specific means
lake_means <- mean_BB |> 
  group_by(lake, year = year(date)) |> 
  summarise(mean_sediment = mean(sed_mean * 100, na.rm = TRUE), 
            mean_albedo = mean(albedo.predict.wholelake, na.rm = T)) |> 
  group_by(lake) |> 
  summarise(mean_sediment = mean(mean_sediment, na.rm = T), 
            mean_albedo = mean(mean_albedo, na.rm = T))

##### Plot by lake with lake-specific mean lines
p.sed = ggplot(mean_BB, aes(date, sed_mean * 100, fill = month)) + 
  geom_hline(data = lake_means, aes(yintercept = mean_sediment), 
             linetype = "dashed", color = "red4", linewidth = 0.8) +
  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 1.5, shape = 21, stroke = 0.2) + 
  scale_fill_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(vars(lake), nrow = 1) + 
  xlim(as.Date('2016-10-01'), as.Date('2025-02-01')) +
  xlab("Date") + 
  ylab("Sediment Abundance (%)") + 
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank())

p.albedo = ggplot(mean_BB, aes(date, albedo.predict.wholelake, fill = month)) + 
  geom_hline(data = lake_means, aes(yintercept = mean_albedo), 
             linetype = "dashed", color = "red4", linewidth = 0.8) +
  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 1.5, shape = 21, stroke = 0.2) + 
  scale_fill_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(vars(lake), nrow = 1) + 
  xlim(as.Date('2016-10-01'), as.Date('2025-02-01')) +
  xlab("Date") + 
  ylab("Estimated Albedo") + 
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank()); p.albedo

####################### Combine plots ############################
p.ice / p.ice2 / p.sed / p.albedo + 
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8), 
        legend.position = 'bottom',
        legend.margin = margin(0, 0, 0, 0),  # remove inner margin around the legend box
        legend.box.margin = margin(0, 0, 0, 0))  # remove margin around the legend box area

ggsave("Figures/Figure1_icesedimentTimeseries.png", dpi = 500, 
       height = 7, width = 6.5)
