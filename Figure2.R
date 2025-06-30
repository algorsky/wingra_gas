################################# Calculate bicarbonate #################################
source('0_alkalinity.R')

ggplot(filter(alk, site != 6 & sampledate != as.Date("2022-07-25")))+
  geom_rect(aes(xmin = as.Date("2022-12-18"), xmax = as.Date("2023-03-20"), ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.1)+
  geom_point(aes(x = sampledate, y = alkalinity_calculated, fill = as.factor(biomass)), size = 2, shape = 21)+
  scale_fill_brewer(palette = "Greens")+
  xlab("")+
  ylab(expression(paste("Alkalinity (", µ,"eq ", L^-1,")")))+
  theme_bw(base_size = 14)+
  theme(legend.position = "none")

ggsave(filename = 'figures/Figure2.png',width = 6,height = 4,units = 'in')
