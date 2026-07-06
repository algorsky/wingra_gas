library(tidyverse)

crosswalk <- read_csv("data/map/crosswalk_biomass.csv")

profiles <- read_csv("data/water_quality/profiles_all.csv") |>
  mutate(season = case_when(
    month(sampledate) %in% 3:5   ~ "Spring", # March-May
    month(sampledate) %in% 6:8   ~ "Summer", # June-August
    month(sampledate) %in% 9:10  ~ "Fall",   # September-October
    month(sampledate) %in% 1:2   ~ "Under-ice"  # January-February, under ice
  )) %>%
  left_join(crosswalk, by = c("Site" = "site"))


ggplot(filter(profiles, !(Site == 4 & Depth > 0.9)&!(Site == 8 & Depth > 0.9)&!(Site == 6 & sampledate == as.Date("2022-07-25")))) + 
  geom_path(aes(x = DO_perc, y = Depth, group = sampledate, color = season), linewidth = 0.3) +
  geom_point(aes(x = DO_perc, y = Depth, fill = as.factor(season)), shape = 21, stroke = 0.2) +
  
  scale_fill_manual(values =c("#D55E00", "#CC79A7", "#009E73","darkblue"), name = "") +
  scale_color_manual(values =c("#D55E00", "#CC79A7", "#009E73","darkblue")) +
  geom_vline(xintercept = 100, linetype = "dashed")+
  facet_wrap(~rating_site, scales = "free_y") +
  xlab("Dissolved oxygen saturation (%)") +
  guides(size = FALSE, color = 'none') +
  scale_y_reverse(name = "Depth (m)") +
  # scale_x_continuous(name = ((expression(paste("C", O[2], " (", mu,"mol ", L^-1,")")))), limits = c(0, 2000))+
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom", 
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(0, 0, 0, 0))

ggsave('supplementary/figures/FigureS3.png', width = 6.5, height = 6, dpi = 500)

