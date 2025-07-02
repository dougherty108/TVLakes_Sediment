
albedo = read_csv('DataOut/AlbedoModel.csv') |> 
  mutate(fakedate = if_else(month(sed.date) >= 10, update(sed.date, year = 2022), update(sed.date, year = 2023))) |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


usecolors = c(
  "#238a9e",  # blue-green
  "#2e9b8d",  # teal
  "#45a862",  # green
  "#7dbb49",  # yellow-green
  "#d3cd3b",  # yellow
  "#e6b736",  # goldenrod
  "#eba534",  # orange
  "#ec6a34",  # orange-red
  "#eb4034"   # red
)


ggplot(albedo) +
  geom_path(aes(x = fakedate, y = sed_mean, group = as.factor(wateryear), col = as.factor(wateryear))) +
  geom_point(aes(x = fakedate, y = sed_mean, group = as.factor(wateryear), col = as.factor(wateryear))) +
  scale_color_manual(values = usecolors) +
  facet_wrap(~lake)

ggplot(albedo) +
  geom_path(aes(x = fakedate, y = albedo.predict.wholelake, group = as.factor(wateryear), col = as.factor(wateryear))) +
  geom_point(aes(x = fakedate, y = albedo.predict.wholelake, group = as.factor(wateryear), col = as.factor(wateryear))) +
  scale_color_manual(values = usecolors) +
  ylab('Albedo Estimate') +
  facet_wrap(~lake) +
  theme_bw(base_size = 9) +
  theme(axis.title.x = element_blank(), 
        legend.title = element_blank())

