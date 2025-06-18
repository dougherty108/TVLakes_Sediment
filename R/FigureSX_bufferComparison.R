# Compare sediment mean for whole lake vs. 300 m buffered area 

mean_BB <- read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(type = 'lake_monitoring_station')

mean_wholelake <- read_csv("Data/LANDSAT_wholelake_mean_20250403.csv") |> 
  mutate(type = "whole_lake")

# join the two files for easy comparison and plotting
means <- rbind(mean_BB, mean_wholelake) |> 
  mutate(sediment_abundance = sediment_abundance*100, 
         ice_abundance = ice_abundance*100) |> 
  mutate(year = year(date), 
         month = month(date))

## plot the raw output for sediment abundance/ice abundance against each other and see how the outputs compare
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") 

mean_bluebox = mean_BB |> 
  mutate(sediment_abundance_bb = sediment_abundance) |> 
  dplyr::select(c(date, lake, sediment_abundance_bb))

mean_whole = mean_wholelake |> 
  mutate(sediment_abundance_wholelake = sediment_abundance) |> 
  dplyr::select(c(date, lake, sediment_abundance_wholelake))

means_pivot = mean_bluebox |> 
  full_join(mean_whole) |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))

#plot of buffered mean vs whole lake mean by lake. 
ggplot(means_pivot, aes(sediment_abundance_bb, sediment_abundance_wholelake)) + 
  geom_abline(size = 0.8) +
  geom_point(size = 1, shape = 21) + 
  xlab("Sediment estimate 300 m buffered mean (%)") + ylab("Sediment estimate whole lake mean  (%)") + 
  facet_wrap(~lake, scales = "free") + 
  theme_linedraw(base_size = 9)

ggsave("Figures/FigureSX_bufferComparison.png", 
       height = 4, width = 6.5, dpi = 500)

