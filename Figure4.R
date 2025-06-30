library(tidyverse)
library(patchwork)

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv")|>
  left_join(macrophyte, by = c("site"))
#Carbon dioxide
ggplot()+
  geom_rect(aes(xmin = as.Date("2022-12-18"), xmax = as.Date("2023-03-20"), ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.3)+
  geom_line(data = co2_dissolved, aes(x = date, y = CO2_mean, color = as.factor(biomass)), show.legend = FALSE)+
  geom_line(data = co2_dissolved, aes(x = date, y = total_mean_co2), linetype = "dashed", alpha = 0.7)+
  geom_point(data = co2_dissolved, aes(x = date, y = CO2_mean, fill = as.factor(biomass)), shape = 21, size = 3, show.legend = FALSE)+
  scale_fill_brewer(palette = "Greens")+
  scale_color_brewer(palette = "Greens")+
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'M)')))+
  xlab("")+
  theme_bw(base_size = 16)
ggsave(filename = 'figures/Figure4.png',width = 8,height = 6,units = 'in')


