library(tidyverse)
library(patchwork)
#################### Figure 1 ####################
source('0_maps.R')

bathy_map <- ggplot() +
  geom_sf(data = wingra, color = 'grey99') +  
  geom_sf(data = tprs_sf, aes(color = depth), size = 3) +  # Plot depth using color
  shadowtext::geom_shadowtext(data = sites_sf, aes(st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], 
                                                   label = rating), color = "white", bg.color = "black", size = 3) +
  scale_color_viridis_c(name = "Depth (m)", option = "D") +  # Use viridis color scale
  xlab("")+ylab("")+
  scale_y_continuous(breaks = c(43.05, 43.054, 43.058)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  # guides(color = guide_legend(override.aes = list(size = 5), title.position = "top", nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 9)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.height = unit(0.3,'cm'),
    legend.position = 'bottom',
    legend.title = element_text(size=8),
    legend.text = element_text(size=8),
    legend.direction="horizontal",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-4,0,0,0)
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br", text_cex = 0.5, pad_y = unit(0.15, "cm"))

macrophyte_map <- ggplot() +
  # geom_sf(data = wingra, color = 'black', lwd = 2) +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = filter(points_within_lake), aes(color = as.factor(rake), shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake))) +
  geom_sf(data = sites_sf, color = 'black', fill = "white", size = 2.5, shape = 21, stroke = 0.2)+
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(1.4, 1.4, 1.4, 1.4, 2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top"))+
  scale_y_continuous(breaks = c(43.05, 43.054, 43.058)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41))+
  # guides(color = guide_legend(override.aes = list(size = 2), nrow = 1, byrow = TRUE))+
  theme_bw(base_size = 9)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.key.width = unit(0.1,'cm'),
    legend.position = 'bottom',
    legend.title = element_text(size=8),
    legend.text = element_text(size=8),
    legend.direction="horizontal",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-4,0,0,0),
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
    
  )+
  ggspatial::annotation_scale( bar_cols = c("grey", "white"),  location = "br")


# (bathy_map + macrophyte_map)  +
#   plot_annotation(tag_levels = 'a', tag_prefix = "(",tag_suffix = ")")

plot_grid(
  bathy_map + theme(plot.tag = element_text(size = 8)),
  macrophyte_map + theme(plot.tag = element_text(size = 8)),
  labels = c("(a)", "(b)"),
  label_size = 9,
  label_fontface = "plain",
  ncol = 2
)

ggsave('figures/Figure1.png',width = 6,height = 2.5,units = 'in', dpi = 500, bg = 'white')
