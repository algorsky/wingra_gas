library(tidyverse)
source('0_diffusive.R')
ph_dic = read_csv("data/temp_dic_ph.csv")|>
  rename(temp = Temp_C)%>%
  rename(ph = pH)%>%
  dplyr::select(sampledate, site, temp, ph, dic)
data<- df%>%
  left_join(crosswalk, by = c("Site"))%>%
  left_join(ph_dic, by = c("site", "Date" = "sampledate"))


p1<-ggplot(data)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(aes(y = co2_mean, x = dic,  fill = (season)), size = 3, shape = 21)+
  scale_fill_manual(values =c("#009E73","#D55E00", "#CC79A7"))+
  ylab(((expression(paste("C", O[2] , " Flux (mmol ", m^-2, d^-1,")")))))+
  xlab(((expression(paste("Dissolved Inorganic Carbon", " (mg ","",, L^-1,")")))))+
  theme_bw(base_size = 18)+
  theme(legend.title = element_blank(), legend.position = 'right')
#linear model
co2_dic_lm <- lm(co2_mean ~dic, data = data)
summary(co2_dic_lm)

p2<-ggplot(data)+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(aes(y = co2_mean, x = ph,  fill = (season)), size = 3, shape = 21)+
  scale_fill_manual(values =c("#009E73","#D55E00", "#CC79A7"))+
  ylab(((expression(paste("C", O[2] , " Flux (mmol ", m^-2, d^-1,")")))))+
  xlab("Surface water pH")+
  theme_bw(base_size = 18)+
  theme(legend.title = element_blank(), legend.position = 'right')
#Linear model
co2_pH_lm <- lm(co2_mean ~ph, data = data)
summary(co2_pH_lm)

p3<-ggplot(filter(data, season == "Summer"))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_point(aes(y = co2_mean, x = ph,  fill = as.factor(biomass)), size = 3, shape = 21)+
  scale_fill_brewer(palette = "Greens")+
  ylab(((expression(paste("C", O[2] , " Flux (mmol ", m^-2, d^-1,")")))))+
  xlab("Surface water pH")+
  theme_bw(base_size = 18)+
  theme(legend.position = 'none')
#Linear model
co2_pH_lm_summer <- lm(co2_mean ~ph, data = filter(data, season == "Summer"))
summary(co2_pH_lm_summer)

top <- (p1 + p2) + 
  plot_layout(ncol = 2, guides = "collect") & 
  theme(legend.position = "bottom")
top / p3+
  plot_annotation(tag_levels = 'a', tag_prefix = "(",tag_suffix = ")")

ggsave(filename = 'figures/Figure5.png',width = 11,height = 9,units = 'in')



