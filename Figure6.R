################################# Polygons #################################
source('0_PolygonsVoronoi.R')

# Full plot of polygons
polygon_plot<-ggplot() +
  geom_sf(data = filter(lake_macrophytes, rake < 10), aes(color = as.factor(rake), 
                                                          shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  geom_sf(data = sites_sf, color = 'black', fill = "yellow", size = 2, shape = 24, stroke = 1.2)+
  geom_sf(data = cluster_polygons, alpha = 0.2, linewidth = 1) +
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(2,2,2,2, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  # scale_y_continuous(breaks = c(43.05, 43.08)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 11)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv")|>
  left_join(macrophyte, by = c("site"))
# 13 days of freeze in December (13 + DOY = Days since Ice-On)
co2_winter<- co2_dissolved%>%
  filter(season == "ice" | date == as.Date("2023-03-23"))%>%
  mutate(doy = yday(date))%>%
  mutate(days_ice = 13 + doy)%>%
  mutate(mM = CO2_mean/1000)%>%
  mutate(CO2_max = CO2_mean + CO2_sd)
co2_winter_plot<-ggplot()+
  geom_smooth(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, group = site, color = as.factor(biomass)), method = "lm", se = F)+
  geom_vline(xintercept = 91, linetype = "dashed", size = 1.4)+
  geom_segment(aes(x = 61, xend = 91, y = -2.36 + 0.31*61, yend = -2.36 + 0.31*91), linetype ="dashed", color = "#F7FCF5")+
  geom_segment(aes(x = 61, xend = 91, y = 0.01 + 0.57*61, yend = 0.01 + 0.57*91), linetype ="dashed", color = "#A1D99B")+
  geom_segment(aes(x = 61, xend = 91, y = -8.68 + 0.53*61, yend = -8.68 + 0.53*91), linetype ="dashed", color = "#C7E9C0")+
  geom_segment(aes(x = 61, xend = 91, y = -36.9 + 1.37*61, yend = -36.9 + 1.37*91), linetype ="dashed", color = "#41AB5D")+
  geom_segment(aes(x = 61, xend = 91, y = -11.7 + 0.68*61, yend = -11.7 + 0.68*91), linetype ="dashed", color = "#E5F5E0")+
  geom_segment(aes(x = 61, xend = 91, y = -207 + 6.59*61, yend = -207 + 6.59*91), linetype ="dashed", color = "#238B45")+
  geom_segment(aes(x = 61, xend = 91, y = -174 + 5.48*61, yend = -174 + 5.48*91), linetype ="dashed", color = "#005A32")+
  geom_segment(aes(x = 61, xend = 91, y = -7.29 + 0.92*61, yend = -7.29 + 0.92*91), linetype ="dashed", color = "#74C476")+
  geom_point(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, fill = as.factor(biomass)), size = 2, shape = 21)+
  geom_pointrange(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, ymin = CO2_mean - CO2_sd, ymax = CO2_mean + CO2_sd, fill = as.factor(biomass)), shape = 21)+
  #geom_point(data = filter(co2_winter, season == "open"), aes(x = days_ice, y = CO2_mean, fill = as.factor(biomass)), size = 2, shape = 21)+
  geom_pointrange(data = filter(co2_winter, season == "open"), aes(x = days_ice, y = CO2_mean, ymin = CO2_mean - CO2_sd, ymax = CO2_mean + CO2_sd,fill = as.factor(biomass)), shape = 21)+
  xlab("Days since ice-on")+
  scale_color_brewer(palette = "Greens")+
  scale_fill_brewer(palette = "Greens")+
  ylab(expression(paste("Surface C", O[2], " (", µ,"mol ", L^-1,")")))+
  theme_bw(base_size = 14)+
  theme(legend.position = 'none')

polygon_plot+co2_winter_plot+ plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") 

ggsave(filename = 'figures/Figure6.png',width = 12,height = 5.5,units = 'in')
