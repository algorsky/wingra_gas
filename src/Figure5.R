library(tidyverse)
library(patchwork)

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv") |>
  left_join(macrophyte, by = c("site")) |>
  mutate(
    # multcompView::vec2mat() splits Tukey comparison names on "-", so these levels must stay
    # hyphen-free; the "0"/"1-29"/... labels are applied via scale_fill/color_brewer() below
    biomass_rake_fulness = case_when(
      biomass < 1 ~ "0",
      biomass < 30 ~ "1to29",
      biomass < 80 ~ "30to79",
      biomass >= 80 ~ "80plus"
    ),
    biomass_rake_fulness = factor(biomass_rake_fulness, levels = c("0", "1to29", "30to79", "80plus"))
  )

#Carbon dioxide
ggplot() +
  geom_rect(aes(xmin = as.Date("2022-12-18"), xmax = as.Date("2023-03-20"), ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.3) +
  geom_line(data = co2_dissolved, aes(x = date, y = CO2_mean, color = biomass_rake_fulness, group = rating_site)) +
  geom_line(data = co2_dissolved, aes(x = date, y = total_mean_co2), linetype = "dashed", alpha = 0.7) +
  geom_point(data = co2_dissolved, aes(x = date, y = CO2_mean, fill = biomass_rake_fulness),
             shape = 21, size = 2, stroke = 0.2) +
  scale_fill_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
  scale_color_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'M)'))) +
  xlab("") +
  theme_bw(base_size = 9) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", colour = 'black', linewidth = 0.1),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(1,1,1,1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

ggsave(filename = 'figures/Figure5.png',width = 6,height = 3,units = 'in', dpi = 500)
 



#### Compare under-ice CO2 by category using ANOVA ####

# make seasonal categories
co2_dissolved = co2_dissolved %>% 
  mutate(season = case_when(
    month(date) %in% 3:5   ~ "Spring", # March-May
    month(date) %in% 6:8   ~ "Summer", # June-August
    month(date) %in% 9:10  ~ "Fall",   # September-October
    month(date) %in% 1:2   ~ "Winter"  # January-February, under ice
  )) %>%
  # filter(Date != as.Date("2022-06-28")) %>%
  mutate(Site_num = gsub("_", " ", Site)) %>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "Winter"))) 

# quick boxplot to visually check the data
ggplot(co2_dissolved %>% filter(season == "Winter"), aes(x = biomass_rake_fulness, y = CO2_mean))+
  geom_boxplot()

winter.co2 = co2_dissolved %>% 
  filter(season == "Winter")

response = "CO2_mean"

# run an ANOVA for pCO2 across binned macrophyte biomass
mod <- aov(as.formula(paste(response, "~ biomass_rake_fulness")), data = winter.co2)
cld <- multcompLetters4(mod, TukeyHSD(mod))[["biomass_rake_fulness"]]
data.frame(biomass_rake_fulness = names(cld$Letters), Letters = cld$Letters, row.names = NULL)
