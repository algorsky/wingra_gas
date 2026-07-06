library(sf)
library(tidyverse)
library(patchwork)

#Load lake boundary
lakes <- st_read('supplementary/data/yld_study_lakes.shp')
wingra <- lakes %>% filter(LAKEID == "WI")

#Load Eurasian watermilfoil/coontail rake data 
species <- read_csv('supplementary/data/macrophyte_eurasian_coon22.csv')
species.sf<- st_as_sf(species, coords = c("longitude", "latitude"), crs = 4326)

species.sf$eurasian<- factor(species.sf$eurasian, levels = c(0.5, 0.0, 1.0, 2.0, 3.0, 8.0, 10.0))
species.sf$coontail<- factor(species.sf$coontail, levels = c(0.5, 0.0, 1.0, 2.0, 3.0, 8.0, 10.0))
points_sf_macrophyte <- st_transform(species.sf, st_crs(wingra))
points_within_lake_macrophyte <- st_intersection(wingra, points_sf_macrophyte)

#Scale values for color, labels, shapes, and sizes for both plots
rake_colors <- c("darkgoldenrod1", "lightgray", '#c2e699', "#78c679", "#006837", "gray77", "red3")
rake_labels <- c("Visual", "0", "1", "2", "3", "Not sampled", "Non-navigable")
rake_shapes <- c(19, 19, 19, 19, 19, 4, 8)
rake_sizes  <- c(1, 1, 2, 3, 4, 2, 2)

#Eurasian watermilfoil plot
eurasian <- ggplot() +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = points_within_lake_macrophyte,
          aes(color = as.factor(eurasian), shape = as.factor(eurasian),
              fill = as.factor(eurasian), size = as.factor(eurasian)),
          stroke = 1.1) +
  scale_color_manual(values = rake_colors, labels = rake_labels, name = "",
                     guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = rake_colors, labels = rake_labels, name = "",
                    guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = rake_shapes, labels = rake_labels, name = "",
                     guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = rake_sizes, labels = rake_labels, name = "",
                    guide = guide_legend(title.position = "top")) +
  scale_y_continuous(breaks = c(43.05, 43.06)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41)) +
  ggtitle('Eurasian water-milfoil') +
  guides(color = guide_legend(override.aes = list(size = 3), title.position = "top",
                              nrow = 1, byrow = TRUE)) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.direction = "horizontal",
    plot.title = element_text(size = 12, face = "bold")
  ) +
  ggspatial::annotation_scale(bar_cols = c("grey", "white"), location = "br")

#Coontail plot
coontail <- ggplot() +
  geom_sf(data = wingra, color = 'grey99') +
  geom_sf(data = points_within_lake_macrophyte,
          aes(color = as.factor(coontail), shape = as.factor(coontail),
              fill = as.factor(coontail), size = as.factor(coontail)),
          stroke = 1.1) +
  scale_color_manual(values = rake_colors, labels = rake_labels, name = "",
                     guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = rake_colors, labels = rake_labels, name = "",
                    guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = rake_shapes, labels = rake_labels, name = "",
                     guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = rake_sizes, labels = rake_labels, name = "",
                    guide = guide_legend(title.position = "top")) +
  scale_y_continuous(breaks = c(43.05, 43.06)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41)) +
  ggtitle('Coontail') +
  guides(color = guide_legend(override.aes = list(size = 3), title.position = "top",
                              nrow = 1, byrow = TRUE)) +
  theme_bw(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.direction = "horizontal",
    plot.title = element_text(size = 12, face = "bold")
  ) +
  ggspatial::annotation_scale(bar_cols = c("grey", "white"), location = "br")

#Combine and save 
eurasian / coontail +
  plot_layout(axes = 'collect', guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = 'bottom')

ggsave(filename = 'supplementary/figures/FigureS1.png',
       width = 9, height = 8, units = 'in', dpi = 300)
