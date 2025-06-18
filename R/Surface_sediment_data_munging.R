######### TVLakes Sediment Output Comparison ###########
library(tidyverse)
library(RColorBrewer)
library(scales)
library(ggpubr)

setwd("~/Documents/R-Repositories/TVLakes_Sediment")

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
         month = month(date, label = TRUE))

# Calculate lake-specific means
lake_means <- mean_BB |> 
  group_by(lake) |> 
  summarise(mean_sediment = mean(sediment_abundance * 100, na.rm = TRUE))

##### Plot by lake with lake-specific mean lines
ggplot(mean_BB, aes(date, sediment_abundance * 100, fill = month)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.1), size = 4, shape = 21) + 
  geom_hline(data = lake_means, aes(yintercept = mean_sediment), 
             linetype = "dashed", color = "red", size = 1, inherit.aes = FALSE) +
  facet_wrap(vars(lake)) + 
  xlab("Date") + 
  ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 28)

ggsave("plots/BB_sed_by_lake_with_mean_line.png", dpi = 700, 
       height = 8, width = 12)

#####plot by lake
ggplot(mean_BB, aes(date, sediment_abundance*100, color = month)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.1)) + 
  facet_wrap(vars(lake)) + 
  #scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  #scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20)

ggsave("plots/BB_sed_by_lake.png", dpi = 700, 
       height = 8, width = 12)

mean_wholelake <- read_csv("Data/LANDSAT_wholelake_mean_20250403.csv") |> 
  mutate(date = ymd(date), 
         type = "whole_lake", 
         season = sapply(date, get_season), 
         month = month(date, label = TRUE))

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
  full_join(mean_whole)

#plot of buffered mean vs whole lake mean by lake. 
ggplot(means_pivot, aes(sediment_abundance_bb, sediment_abundance_wholelake)) + 
  geom_point(size = 2, shape = 1) + 
  geom_abline(size = 2) +
  xlab("Sediment estimate 300m buffered mean") + ylab("Sediment estimate whole lake mean") + 
  facet_wrap(~lake, scales = "free") + 
  theme_linedraw(base_size = 20)

ggsave("plots/wholelake_vs_bb_comparison.png", 
       height = 8, width = 8, dpi = 300)

#By season for just the lake monitoring site
ggplot(mean_BB, aes(date, sediment_abundance*100, color = lake)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free_x") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("plots/BB_sed_by_season.png", dpi = 700, 
       height = 8, width = 12)

# facet by season for sed abundance
ggplot(means, aes(date, sediment_abundance, color = type)) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  theme_linedraw(base_size = 15)

ggsave("plots/whole_lake_vs_BB_sed_abundance.png", dpi = 700, 
       height = 8, width = 12)

# facet by season for ice abundance
ggplot(means, aes(date, ice_abundance, color = type)) + 
  geom_point() + 
  geom_smooth(se = F) + 
  scale_color_brewer(palette = "Set1") + 
  facet_wrap(vars(season), scales = "free_x") +
  scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Ice Abundance (%)") + 
  theme_linedraw(base_size = 15)

ggsave("plots/whole_lake_vs_BB_ice_abundance.png", dpi = 700, 
       height = 8, width = 12)


## load lake ice: 
lakeice1 <- read_csv("Data/mcmlter-lake-ice_thickness-20250218_0_2025.csv") |> 
  mutate(date_time = mdy_hm(date_time), 
         month = month(date_time, label = TRUE), 
         year = year(date_time),
         year = as.numeric(year),
         z_water_m = z_water_m*-1, 
         season = sapply(date_time, get_season)) |> 
  rename("lake" = location_name) |> 
  filter(lake == "Lake Fryxell" | lake == "Lake Hoare" | lake == "East Lake Bonney" | lake == "West Lake Bonney") |> 
  #filter(year >= 2016) |> 
  filter(!grepl("^B", location))

ggplot(lakeice1, aes(date_time, z_water_m)) + 
  geom_point(size = 2.0) + 
  geom_smooth(se = T) + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 20) + 
  #scale_color_brewer(palette = "Set1") +
  xlab("Date") + ylab("Ice Thickness (m)")

ggsave("plots/MCM_LTER_ice_thickness_total_years.png", dpi = 700, 
       height = 8, width = 12)

####### lake ice vs. sediment values Figure #####
sediment_plot = ggplot(mean_BB, aes(date, sediment_abundance*100, color = month)) + 
  geom_point(position = position_jitter(width = 0.2, height = 0.1), 
             size = 2.0) + 
  facet_wrap(vars(lake)) + 
  #scale_x_date(labels = date_format("%b"), breaks = "1 month") + 
  xlab("Date") + ylab("Sediment Abundance (%)") + 
  #scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20)

lakeice_plot = ggplot(lakeice1, aes(date_time, z_water_m)) + 
  geom_point(size = 2.0) + 
  geom_smooth(se = T) + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 20) + 
  #scale_color_brewer(palette = "Set1") +
  xlab("Date") + ylab("Ice Thickness (m)")

ggarrange(sediment_plot, lakeice_plot, 
          align = "hv", 
          widths = c(1, 1))

ggsave("plots/sediment_ts_lakeice_ts.png", 
       width = 16, height = 8, dpi = 300)


lakeice = lakeice1 |> 
  filter(date_time >= "2016-05-01") |> 
  #mutate(season = sapply(date_time, get_season)) |> 
  filter(str_detect(string = location, pattern = "incubation hole") | 
           str_detect(string = location, pattern = "sample hole")) |> 
  filter(grepl("^O", location)) |> 
  drop_na(z_water_m)

#FIND SEASONAL DROPOFF OF ICE THICKNESS
seasonal_changes <- lakeice %>%
  group_by(season, lake) |> 
  arrange(season, date_time) %>%
  summarise(
    First_Measurement = first(z_water_m),
    Last_Measurement = last(z_water_m),
    Difference = Last_Measurement - First_Measurement
  )

ggplot(seasonal_changes, aes(season, Difference)) + 
  geom_col(aes(fill = lake)) + 
  scale_fill_brewer(palette = "Set1") + 
  facet_wrap(vars(lake)) + 
  theme_linedraw(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none") + 
  xlab("Seasonal") + ylab("Change in Ice Thickness")

ggsave("plots/seasonal_ice_drop.png", 
       height = 8, width = 12, dpi = 300)

li_summary = lakeice |> 
  mutate(week = week(date_time)) |> 
  group_by(year, month, lake) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

## sediment
sed_monthly <- means |> 
  mutate(week = week(date), 
         month = month(date, label = TRUE)) |> 
  group_by(year, month, lake, type) |>
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  print()

fulljoined = full_join(sed_monthly, li_summary) |> 
  drop_na() |> 
  filter(type == "lake_monitoring_station")

##### plot
ggplot(fulljoined, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm", se = T) + 
  geom_point(aes(color = month), 
             size = 3#, 
             #shape = 21
             ) + 
  facet_wrap(vars(lake), scales = "free") + 
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  theme_linedraw(base_size = 28) 

ggsave("plots/sed_vs_ice_thickness_comparison.png", 
       width = 12, height = 8, dpi = 500)

all_data_lm = fulljoined |> 
  group_by(lake) |> 
  nest() |> 
  mutate(
    model = map(data, ~ lm(mean_sed ~mean_thickness, data = .x)),
    tidied = map(model, broom::tidy)
  )

all_data_lm %>% select(lake, tidied) %>% unnest(tidied)


### now remove November and October Values
#fulljoin_filter <- fulljoined |> 
#  filter(month == 12 | month == 1)
fulljoin_peak_solar <- fulljoined |> 
  filter(month == "Jan")

ggplot(fulljoin_peak_solar, aes(mean_sed, mean_thickness, color = type)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("January") + 
  scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20) 

ggsave("plots/wholelakesed_vsthickness_jan.png", 
       width = 12, height = 8, dpi = 300)

# now filter for only early year estimates, October - December
jan_dec_fulljoin <- fulljoined |> 
  filter(
    month == "Jan" | 
      month == "Dec")

ggplot(jan_dec_fulljoin, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point(aes(color = month)) + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("January - December") + 
  #scale_color_brewer(palette = "Set1") +
  theme_linedraw(base_size = 20) 

jan_dec_lm = jan_dec_fulljoin |> 
  group_by(lake) |> 
  nest() |> 
  mutate(
    model = map(data, ~ lm(mean_sed ~ mean_thickness, data = .x)),
    tidied = map(model, broom::tidy),
    glanced = map(model, broom::glance)  # this adds R², adj R², etc.
  )

save = jan_dec_lm %>% select(lake, tidied, glanced) |> 
  unnest(tidied, names_sep = "_coef") |> 
  unnest(glanced, names_sep = "_model")


jan_fulljoin <- fulljoined |> 
  filter(month == 1)

ggplot(jan_fulljoin, aes(mean_sed, mean_thickness, color = type)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point() + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("January") + 
  scale_color_brewer(palette = "Set1") +
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  theme_linedraw(base_size = 20)  

ggsave("plots/jan_only_sed.png", 
       dpi = 300, height = 8, width = 12)


####### do restructuring to look at this by week ###
li_summary_2 = lakeice |> 
  mutate(week = week(date_time), 
         #month = month(date, label = TRUE)
         ) |> 
  group_by(year, week, lake) |> 
  summarize(mean_thickness = mean(z_water_m, na.rm = T))

## sediment
sed_monthly_2 <- means |> 
  mutate(week = week(date), 
         month = month(date, label = TRUE)) |> 
  group_by(year, week, lake, type) |> 
  summarize(mean_sed = mean(sediment_abundance, na.rm = T)) |> 
  print()

fulljoined_2 = full_join(sed_monthly_2, li_summary_2) |> 
  drop_na() |> 
  filter(type == "lake_monitoring_station")

peak_solar_week <- fulljoined_2 |> 
  filter(
    week == 44 |
      week == 45 |
      week == 46 |
      week == 47 |
      week == 48 |
      week == 49 |
      week == 50 |
      week == 51 | 
      week == 52 | 
      week == 1 | 
      week == 2 #|
      #week == 3
      )

peak_solar_lm <- peak_solar_week |> 
  group_by(lake) |> 
  nest() |> 
  mutate(
    model = map(data, ~ lm(mean_sed ~ mean_thickness, data = .x)),
    tidied = map(model, broom::tidy),
    glanced = map(model, broom::glance)  # this adds R², adj R², etc.
  )

# To see both coefficients and R² together
peak_solar_lm |> 
  select(lake, tidied, glanced) |> 
  unnest(tidied, names_sep = "_coef") |> 
  unnest(glanced, names_sep = "_model") 


ggplot(peak_solar_week, aes(mean_sed, mean_thickness)) + 
  geom_smooth(method = "lm", se = F) + 
  geom_point(size = 3) + 
  facet_wrap(vars(lake), scales = "free") + 
  ggtitle("Peak Solar") + 
  scale_color_brewer(palette = "Set1") +
  xlab("Mean Sediment Abundance (%)") + ylab("Mean Ice Thickness (m)") + 
  theme_linedraw(base_size = 20)  

ggsave("plots/Peak_Solar_Comparison_20250408.png", 
       width = 12, height = 8, dpi = 300)

#### Create a dual panel plot where only the lake ice thickness from 2016-2024 to sediment abundannce
# Need to join the sed data frame to the lake ice data frame
mean_BB_week <- mean_BB |> 
  filter(date < "2025-02-01")

library(ggpubr)
library(viridisLite)
library(scales)

# Step 1: Define all possible months (in your case)
all_months <- c("Jan", "Feb", "Oct", "Nov", "Dec")

# Generate Viridis colors for all_months
palette <- viridis(n = length(all_months), option = "D")  # or "A", "B", "C", "E", etc.
month_colors <- setNames(palette, all_months)

# Plot 1
months1 <- c("Jan", "Feb", "Oct", "Nov", "Dec")
plot1 <- ggplot(mean_BB_week, aes(date, sediment_abundance * 100)) + 
  geom_point(aes(color = month)) + 
  geom_smooth(se = FALSE) + 
  ylab("Sediment Abundance") + xlab("Date") + 
  ggtitle("Sediment Coverage (%)") + 
  facet_wrap(vars(lake)) + 
  scale_fill_manual(values = month_colors[months1]) + 
  theme_linedraw(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_date(
    breaks = seq(as.Date("2010-01-01"), as.Date("2025-01-01"), by = "year"),
    labels = date_format("%Y")
  )

# Plot 2
months2 <- c("Jan", "Nov", "Dec")
plot2 <- ggplot(lakeice, aes(date_time, z_water_m)) + 
  geom_point(aes(color = month)) + 
  geom_smooth(se = FALSE) + 
  ylab("Ice Thickness (m)") + xlab("Date") + 
  ggtitle("Ice Thickness") + 
  facet_wrap(vars(lake)) +
  scale_fill_manual(values = month_colors[months2]) + 
  theme_linedraw(base_size = 20) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_datetime(
    breaks = as.POSIXct(paste0(2010:2025, "-01-01")),
    labels = date_format("%Y")
  )

ggarrange(plot1, plot2)


ggsave("plots/sed_abundance_ice_thick_timeseries.png", 
       dpi = 300, height = 8, width = 15)
