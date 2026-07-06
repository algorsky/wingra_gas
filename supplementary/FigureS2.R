library(tidyverse)
library(lubridate)
library(patchwork)

#Load data 
tntp      <- read_csv('supplementary/data/TNTP.csv')
macrophyte<- read_csv("supplementary/data/sites.csv")
chloro_all<- read_csv("supplementary/data/chlorophyll.csv")
crosswalk <- read_csv("supplementary/data/crosswalk_biomass.csv")

#Assign season and join nutrient + macrophyte data
nutrients <- tntp %>%
  left_join(macrophyte, by = c("Site")) %>%
  mutate(season = ifelse(month(Date) < 3, "under-ice",
                         ifelse(month(Date) > 2 & month(Date) < 6, "Spring",
                                ifelse(month(Date) > 8, "Fall", "Summer")))) %>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "under-ice")))

#Summer TN/TP joined with biomass rating 
summer_tptn <- nutrients %>%
  filter(season == "Summer") %>%
  left_join(crosswalk, by = c("Site" = "site"))

#Summer chlorophyll joined with biomass rating
chloro_summer <- chloro_all %>%
  mutate(season = ifelse(month(date) < 3, "winter",
                         ifelse(month(date) > 2 & month(date) < 6, "spring",
                                ifelse(month(date) > 8, "fall", "summer")))) %>%
  filter(season == "summer") %>%
  left_join(crosswalk, by = c("site"))

#Shared color palette (biomass rating gradient) 
biomass_colors <- c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B",
                             "#74C476", "#41AB5D", "#238B45", "#005A32")
                             
#TP plot
tp_plot <- ggplot(summer_tptn, aes(x = as.factor(rating_site), fill = as.factor(biomass.x),
                                   y = TP_ug_l / 1000, group = rating_site)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.5) +
  scale_fill_manual(values = biomass_colors) +
  ylab(expression(paste('Total Phosphorus (mg', 'L'^-1, ')'))) +
  xlab("") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

#TN plot 
tn_plot <- ggplot(summer_tptn, aes(x = as.factor(rating_site), fill = as.factor(biomass.x),
                                   y = TN_ug_l / 1000, group = rating_site)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.5) +
  scale_fill_manual(values = biomass_colors) +
  xlab("") +
  ylab(expression(paste('Total Nitrogen (mg', 'L'^-1, ')'))) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

#Chlorophyll plot
chloro_plot <- ggplot(chloro_summer, aes(x = as.factor(rating_site), fill = as.factor(rating),
                                         y = chla_correct_ugl, group = rating)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.5) +
  scale_fill_manual(values = biomass_colors) +
  xlab("") +
  ylab(expression(paste('Chlorophyll a (', mu, 'g ', 'L'^-1, ')'))) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

## ---- Combine and save ----
tp_plot / tn_plot / chloro_plot +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") +
  plot_layout(guides = 'collect')

ggsave(filename = 'supplementary/figures/FigureS2.png',
       width = 7, height = 8, units = 'in')
