library(tidyverse)

ice <- read_csv('supplementary/data/ice.csv')|>
  rename(sample_date = sampledate)%>%
  dplyr::select(site, sample_date, avsnow, totice, whiteice, blackice,secchi)

ice_long <- ice %>%
  pivot_longer(
    cols = c(avsnow, whiteice, blackice),
    names_to = "icetype",
    values_to = "thickness"
  ) %>%
  mutate(icetype = dplyr::recode(icetype,
                                 "avsnow" = "snow",
                                 "whiteice" = "white",
                                 "blackice" = "black"),
         icetype = factor(icetype, levels = c("black", "white", "snow")))
# Order by snow and ice type
ice_stacked <- ice_long %>%
  mutate(icetype = factor(icetype, levels = c("black", "white", "snow")),
         thickness = ifelse(icetype %in% c("black", "white"), -thickness, thickness))|>
  left_join(crosswalk, by = "site")

ggplot(ice_stacked, aes(x = as.factor(sample_date), y = thickness, fill = icetype)) +
  geom_bar(stat = "identity", width=  0.5, size = 0.2) +
  facet_wrap(~rating_site, nrow = 2) +
  scale_y_continuous(labels = abs) +
  scale_fill_manual(values = c('#404040','#E0E0E0','lightblue3'), name = '') +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(y = "Thickness (cm)") +
  xlab("") +
  geom_hline(yintercept = 0, size = 0.3) +
  theme_bw(base_size = 9) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = 'figures_SI/FigureS6.png', width = 6.5, height = 4, units = 'in')
