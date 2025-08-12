####################### Sediment abundance ############################
mean_BB <- #read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |>
  read_csv('DataOut/AlbedoModel.csv') |> rename(date = sed.date) |> 
  # mutate(date = ymd(date), 
  #        type = 'lake_monitoring_station', 
  #        month = month(date, label = TRUE)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) |>
  mutate(lake = factor(lake, levels = c('West Lake Bonney',  'East Lake Bonney', 'Lake Hoare', 'Lake Fryxell')))


# Calculate lake-specific means
lake_means <- mean_BB |> 
  group_by(lake, year = year(date), month) |> 
  summarise(mean_sediment = mean(sed_mean * 100, na.rm = TRUE), 
            mean_albedo_bb = mean(albedo.predict.bb, na.rm = T), 
            mean_albedo = mean(albedo.predict.wholelake, na.rm = T)) |> 
  group_by(lake, month) |> 
  summarise(mean_sediment = mean(mean_sediment, na.rm = T), 
            mean_albedo_bb = mean(mean_albedo_bb, na.rm = T),
            mean_albedo = mean(mean_albedo, na.rm = T))

##### Plot by lake with lake-specific mean lines
p1 = ggplot(mean_BB, aes(month, sed_mean * 100, fill = month)) + 

  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 1.5, shape = 21, stroke = 0.2) + 
  geom_point(data = lake_means, aes(y = mean_sediment), shape = 22, fill = 'black') +
  scale_fill_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(vars(lake), nrow = 1) + 
  # xlim(as.Date('2016-10-01'), as.Date('2025-02-01')) +
  xlab("Month") + 
  ylab("Sediment Abundance (%)") + 
  theme_bw(base_size = 9) +
  theme(legend.position = 'none')

p2 = ggplot(mean_BB, aes(month, albedo.predict.wholelake, fill = month)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 1.5, shape = 21, stroke = 0.2) + 
  geom_point(data = lake_means, aes(y = mean_albedo), shape = 22, fill = 'black') +
  scale_fill_manual(values = c('#238a9e','#4ea35e','#d9d138','#eba534','#eb4034')) +
  facet_wrap(vars(lake), nrow = 1) + 
  # xlim(as.Date('2016-10-01'), as.Date('2025-02-01')) +
  xlab("Month") + 
  ylab("Albedo") + 
  theme_bw(base_size = 9) +
  theme(legend.position = 'none')


p1 / p2 +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8))

ggsave("Figures/Figure2_monthlySed.png", height = 3.5, width = 6.5, dpi = 500)


