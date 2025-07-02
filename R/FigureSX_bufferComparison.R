# Compare sediment mean for whole lake vs. 300 m buffered area 

# sedBB <- read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
#   mutate(type = 'lake_monitoring_station') |> 
#   mutate(sediment_abundance = sediment_abundance*100)
# 
# sedwholelake <- read_csv("Data/LANDSAT_wholelake_mean_20250403.csv") |> 
#   mutate(type = "whole_lake") |> 
#   mutate(sediment_abundance = sediment_abundance*100)

# join the two files for easy comparison and plotting
# sedJoin <- full_join(sedBB, sedwholelake,by = join_by(date, lake)) |> 
#   mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

sedJoin = read_csv('DataOut/sedimentResults.csv') |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

#plot of buffered mean vs whole lake mean by lake. 
ggplot(sedJoin, aes(x = sed_mean * 100, y = sed_mean_bb * 100)) + 
  geom_abline(size = 0.8) +
  geom_point(size = 1, shape = 21) + 
  ylab("Sediment estimate 200 m buffer mean (%)") + 
  xlab("Sediment estimate whole lake mean  (%)") + 
  facet_wrap(~lake, scales = "free") + 
  theme_bw(base_size = 9)

ggsave("Figures/FigureSX_bufferComparison.png", 
       height = 4, width = 6.5, dpi = 500)

