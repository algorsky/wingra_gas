library(tidyverse)
library(lubridate)
library(patchwork)

#Load data
tntp <- read_csv('data/water_quality/TNTP.csv')
chloro_all<- read_csv("data/water_quality/chlorophyll.csv")

crosswalk <- read_csv("data/map/crosswalk_biomass.csv") %>%
  mutate(
    biomass_rake_fulness = case_when(
      biomass < 1 ~ "0",
      biomass < 30 ~ "1to29",
      biomass < 80 ~ "30to79",
      biomass >= 80 ~ "80plus"
    ),
    biomass_rake_fulness = factor(biomass_rake_fulness, levels = c("0", "1to29", "30to79", "80plus"))
  )

#Assign season and join nutrient + biomass data
nutrients <- tntp %>%
  left_join(crosswalk, by = c("Site" = "site")) %>%
  mutate(season = case_when(
    month(Date) %in% 3:5   ~ "Spring", # March-May
    month(Date) %in% 6:8   ~ "Summer", # June-August
    month(Date) %in% 9:10  ~ "Fall",   # September-October
    month(Date) %in% 1:2   ~ "Winter"  # January-February, under ice
  )) %>%
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
    month(date) %in% 1:2   ~ "Winter"  # January-February, under ice
  )) %>%
  filter(season == "Summer") %>%
  left_join(crosswalk, by = c("site"))

#TP plot
tp_plot <- ggplot(summer_tptn %>% filter(Filtered == "UF"), aes(x = rating_site, fill = biomass_rake_fulness,
                                   y = TP_ug_l / 1000, group = rating_site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.2) +
  geom_jitter(alpha = 0.5, size = 0.8) +
  scale_fill_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
  ylab(expression(paste('Total Phosphorus (mg ', 'L'^-1, ')'))) +
  xlab("") +
  theme_bw(base_size = 9)

#TN plot
tn_plot <- ggplot(summer_tptn %>% filter(Filtered == "UF"), aes(x = rating_site, fill = biomass_rake_fulness,
                                   y = TN_ug_l / 1000, group = rating_site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.2) +
  geom_jitter(alpha = 0.5, size = 0.8) +
  scale_fill_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
  xlab("") +
  ylab(expression(paste('Total Nitrogen (mg ', 'L'^-1, ')'))) +
  theme_bw(base_size = 9)

#Chlorophyll plot
chloro_plot <- ggplot(chloro_summer, aes(x = rating_site, fill = biomass_rake_fulness,
                                         y = chla_correct_ugl, group = rating_site)) +
  geom_boxplot(outlier.shape = NA, linewidth = 0.2) +
  geom_jitter(alpha = 0.5, size = 0.8) +
  scale_fill_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
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
