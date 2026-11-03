library(tidyverse)
library(lubridate)
library(patchwork)

#Load data
tntp <- read_csv('data/water_quality/TNTP.csv')
chloro_all<- read_csv("data/water_quality/chlorophyll.csv")

# Site type, same definition and colours as the main-text figures: pelagic vs littoral from
# geo_site (P = pelagic, L = littoral); littoral sites split at the 45-98 g gap in macrophyte
# biomass. Legend shows only the biomass range of each type.
type_levels <- c("pelagic", "littoral", "dense")
type_labels <- c("0–9", "16–45", "98–227")
type_names  <- c(pelagic = "Pelagic", littoral = "Littoral", dense = "Dense littoral")
type_fills  <- c(pelagic = "#e5f5e0", littoral = "#74c476", dense = "#006d2c")

crosswalk <- read_csv("data/map/crosswalk_biomass.csv") %>%
  mutate(
    site_type = case_when(
      startsWith(geo_site, "P") ~ "pelagic",
      biomass < 80              ~ "littoral",
      TRUE                      ~ "dense"),
    site_type = factor(site_type, levels = type_levels))

# x-axis labels: site number with its site type underneath, e.g. "Site 1\nPelagic"
site_axis_labels <- setNames(paste0(crosswalk$rating_site, "\n", type_names[as.character(crosswalk$site_type)]),
                             crosswalk$rating_site)

#Assign season and join nutrient + biomass data
nutrients <- tntp %>%
  left_join(crosswalk, by = c("Site" = "site")) %>%
  mutate(season = case_when(
    month(Date) %in% 3:5   ~ "Spring", # March-May
    month(Date) %in% 6:8   ~ "Summer", # June-August
    month(Date) %in% 9:10  ~ "Fall",   # September-October
    month(Date) %in% 1:2   ~ "Winter")) %>%  # January-February, under ice
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "Winter")))

# Summarize unfiltered nutrients by season for results
nutrients.mean = nutrients %>% 
  group_by(season) %>% 
  filter(Filtered == "UF") %>% 
  summarize(mean.tp = mean(TP_ug_l, na.rm = TRUE),
            mean.tn = mean(TN_ug_l, na.rm = TRUE))

#Summer TN/TP joined with biomass rating
summer_tptn <- nutrients %>%
  filter(season == "Summer")

#Summer chlorophyll joined with biomass rating
chloro_summer <- chloro_all %>%
  mutate(season = case_when(
    month(date) %in% 3:5   ~ "Spring", # March-May
    month(date) %in% 6:8   ~ "Summer", # June-August
    month(date) %in% 9:10  ~ "Fall",   # September-October
    month(date) %in% 1:2   ~ "Winter")) %>%  # January-February, under ice
  filter(season == "Summer") %>%
  left_join(crosswalk, by = c("site"))

#TP plot
tp_plot <- ggplot(summer_tptn %>% filter(Filtered == "UF"), aes(x = rating_site, fill = site_type,
                                   y = TP_ug_l / 1000, group = rating_site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.2) +
  geom_jitter(alpha = 0.5, size = 0.8) +
  scale_fill_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  scale_x_discrete(labels = site_axis_labels) +
  ylab(expression(paste('Total Phosphorus (mg ', 'L'^-1, ')'))) +
  xlab("") +
  theme_bw(base_size = 9)

#TN plot
tn_plot <- ggplot(summer_tptn %>% filter(Filtered == "UF"), aes(x = rating_site, fill = site_type,
                                   y = TN_ug_l / 1000, group = rating_site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.2) +
  geom_jitter(alpha = 0.5, size = 0.8) +
  scale_fill_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  scale_x_discrete(labels = site_axis_labels) +
  xlab("") +
  ylab(expression(paste('Total Nitrogen (mg ', 'L'^-1, ')'))) +
  theme_bw(base_size = 9)

#Chlorophyll plot
chloro_plot <- ggplot(chloro_summer, aes(x = rating_site, fill = site_type,
                                         y = chla_correct_ugl, group = rating_site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.2) +
  geom_jitter(alpha = 0.5, size = 0.8) +
  scale_fill_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  scale_x_discrete(labels = site_axis_labels) +
  xlab("") +
  ylab(expression(paste('Chlorophyll a (', mu, 'g ', 'L'^-1, ')'))) +
  theme_bw(base_size = 9)

## ---- Combine and save ----
tp_plot / tn_plot / chloro_plot +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom",
        plot.tag = element_text(size = 8),
        legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(2, 0, 0, 0),
        legend.box.margin = margin(-4, 0, 0, 0))

ggsave(filename = 'figures_SI/FigureS2.png',
       width = 6.5, height = 7, units = 'in', dpi = 500)
