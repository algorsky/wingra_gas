#################### Figure 3 ####################
source('0_diffusive.R')

ggplot(df_figure)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_boxplot(aes(x = geo_site, y = co2_mean*1000*24, fill = as.factor(biomass)))+
  ylab(((expression(paste("C", O[2], " (mmol ", m^-2, d^-1,")")))))+
  scale_fill_brewer(palette = "Greens")+
  # scale_fill_manual(values =c("#009E73","#D55E00", "#CC79A7"), labels = c("Summer (n = 8)",
  #  "Fall (n = 4)",
  #  "Spring (n = 4)"))+
  xlab("")+
  theme_bw(base_size = 14)+
  facet_wrap(~season_n)+
  # guides(fill = guide_legend(override.aes = list(lwd=1)))+
  theme(legend.position = "none" ,
        axis.text.x=element_text(angle=45, hjust=1))
ggsave('figures/Figure3.png',width = 10,height = 4,units = 'in')
