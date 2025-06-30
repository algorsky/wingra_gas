library(tidyverse)
library(patchwork)
#################### Figure 1 ####################
source('0_maps.R')

bathy_map<-ggplot() +
  geom_sf(data = wingra, color = "black", lwd = 5)+
  geom_sf(data = tprs_sf, aes(color = depth), size = 3) +  # Plot depth using color
  shadowtext::geom_shadowtext(data = sites_sf, aes(st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], 
                                                   label = geo_site), color = "white", bg.color = "black", size = 8) +
  scale_color_viridis_c(name = "Depth (m)", option = "D") +  # Use viridis color scale
  xlab("")+ylab("")+
  scale_y_continuous(breaks = c(43.05, 43.054, 43.058)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  # guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 18)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal"
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")

macrophyte_map<-ggplot() +
  geom_sf(data = wingra, color = 'black', lwd = 2) +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = filter(points_within_lake), aes(color = as.factor(rake), shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  # geom_sf(data = sites.sf, color = 'black', fill = "white", size = 4, shape = 8)+
  shadowtext::geom_shadowtext(data = sites_sf, aes(st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], 
                                                   label = geo_site), color = "white", bg.color = "black", size = 8) +
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(3,3,3,3, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  scale_y_continuous(breaks = c(43.05, 43.054, 43.058)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 18)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=12),
    legend.text = element_text(size=12),
    legend.direction="horizontal",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
    
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")


(bathy_map + macrophyte_map)  +
  plot_annotation(tag_levels = 'a', tag_prefix = "(",tag_suffix = ")")
ggsave('figures/Figure1.png',width = 16,height = 6,units = 'in')
