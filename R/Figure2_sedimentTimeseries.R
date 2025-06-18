######### TVLakes Sediment Output Comparison ###########
library(tidyverse)
library(RColorBrewer)
library(scales)

## Define a season function to plot data by season. Makes data viz a lot easier. 
get_season <- function(date) {
  month <- month(date)
  year <- year(date)
  
  if (month %in% c(11, 12)) {
    return(paste0("Summer ", year))  # November and December belong to the current winter
  } else if (month == 1) {
    return(paste0("Summer ", year - 1))  # January belongs to the previous winter
  } else if (month == 2) {
    return(paste0("Summer ", year - 1))  # February belongs to the previous winter
  } else if (month == 3) {
    return(paste0("Fall ", year))  # March is Spring
  } else if (month %in% 4:5) {
    return(paste0("Fall ", year))  # April and May are Spring
  } else if (month == 6) {
    return(paste0("Winter ", year))  # June is Summer
  } else if (month %in% 7:8) {
    return(paste0("Winter ", year))  # July and August are Summer
  } else if (month == 9) {
    return(paste0("Spring ", year))  # September is Fall
  } else if (month %in% 10) {
    return(paste0("Summer ", year))  # October is Fall
  }
}

mean_BB <- read_csv("Data/LANDSAT_sediment_abundances_20250403.csv") |> 
  mutate(date = ymd(date), 
         type = 'lake_monitoring_station', 
         season = sapply(date, get_season), 
         month = month(date, label = TRUE)) |> 
  mutate(month = factor(month, levels = c('Oct','Nov','Dec','Jan','Feb'))) |> 
  mutate(lake = factor(lake, levels = c('Lake Fryxell', 'Lake Hoare', 'East Lake Bonney', 'West Lake Bonney')))


# Calculate lake-specific means
lake_means <- mean_BB |> 
  group_by(lake) |> 
  summarise(mean_sediment = mean(sediment_abundance * 100, na.rm = TRUE))

##### Plot by lake with lake-specific mean lines
ggplot(mean_BB, aes(date, sediment_abundance * 100, fill = month)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 1.5, shape = 21) + 
  geom_hline(data = lake_means, aes(yintercept = mean_sediment), 
             linetype = "dashed", color = "red4", size = 0.8, inherit.aes = FALSE) +
  facet_wrap(vars(lake)) + 
  xlab("Date") + 
  ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 9)

ggsave("Figures/Figure2_sedimentTimeseries.png", dpi = 500, 
       height = 4, width = 6.5)
