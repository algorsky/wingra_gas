#################### Figure 3 ####################
source('src/0_diffusive.R')

ggplot(df_figure) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # geom_boxplot(aes(x = Site, y = co2_mean*1000*24, fill = as.factor(biomass)), linewidth = 0.2) +
  geom_boxplot(aes(x = Site, y = co2_mmol.m2.d, fill = as.factor(biomass)), linewidth = 0.2) +
  ylab(((expression(paste("C", O[2], " (mmol ", m^-2, d^-1,")"))))) +
  scale_fill_brewer(palette = "Greens") +
  # scale_fill_manual(values =c("#009E73","#D55E00", "#CC79A7"), labels = c("Summer (n = 8)",
  #  "Fall (n = 4)",
  #  "Spring (n = 4)")) +
  xlab("") +
  theme_bw(base_size = 9) +
  facet_wrap(~season_n) +
  theme(legend.position = "none" ,
        axis.text.x=element_text(angle=45, hjust=1))

ggsave('figures/Figure4.png', width = 6, height = 3, units = 'in', dpi = 500)
