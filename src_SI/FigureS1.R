library(sf)
library(tidyverse)
library(patchwork)

#Load lake boundary
lakes <- st_read('data/gis/YaharaLakes/yld_study_lakes.shp')
wingra <- lakes %>% filter(LAKEID == "WI")

#Load Eurasian watermilfoil/coontail rake data 
species <- read_csv('supplementary/data/macrophyte_eurasian_coon22.csv')
species.sf<- st_as_sf(species, coords = c("longitude", "latitude"), crs = 4326)

species.sf$eurasian<- factor(species.sf$eurasian, levels = c(0.5, 0.0, 1.0, 2.0, 3.0, 8.0, 10.0))
species.sf$coontail<- factor(species.sf$coontail, levels = c(0.5, 0.0, 1.0, 2.0, 3.0, 8.0, 10.0))
points_sf_macrophyte <- st_transform(species.sf, st_crs(wingra))
points_within_lake_macrophyte <- st_intersection(wingra, points_sf_macrophyte)

#Scale values for color, labels, shapes, and sizes for both plots
rake_colors <- c("darkgoldenrod1", "lightblue3", "#78c679", "#157521",
                 "#074a0f", "gray37", "red3")
rake_labels <- c("Visual", "0", "1", "2", "3", "Not sampled", "Non-navigable")
rake_shapes <- c(16, 16, 16, 18, 18, 4, 8)
rake_sizes  <- c(1, 1, 1.5, 1.8, 2, 2, 2)
# rake_sizes  <- c(0.3, 0.3, 0.7, 0.8, 1, 0.8, 0.4)

#Rake map plot, parameterized by species column and panel title
plot_rake_map <- function(species_col, title) {
  ggplot() +
    geom_sf(data = wingra, color = 'grey99') +
    geom_sf(data = points_within_lake_macrophyte,
            aes(color = as.factor(.data[[species_col]]), shape = as.factor(.data[[species_col]]),
                fill = as.factor(.data[[species_col]]), size = as.factor(.data[[species_col]])),
            stroke = 0.2) +
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
    ggtitle(title) +
    guides(color = guide_legend(title.position = "top",nrow = 1, byrow = TRUE)) +
    theme_bw(base_size = 9) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = 'bottom',
      legend.key.width = unit(0.2,'cm'),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.direction = "horizontal",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 0, 0),
      plot.title = element_text(size = 9, face = "bold")
    ) +
    ggspatial::annotation_scale(bar_cols = c("grey", "white"), location = "br", text_cex = 0.5, pad_y = unit(0.15, "cm"))
}

eurasian <- plot_rake_map("eurasian", "Eurasian water-milfoil")
coontail <- plot_rake_map("coontail", "Coontail")

#Combine and save
eurasian / coontail +
  plot_layout(axes = 'collect', guides = 'collect') +
  plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = 'bottom', plot.tag = element_text(size = 8))

ggsave(filename = 'figures_SI/FigureS1.png',
       width = 6.5, height = 5, units = 'in', dpi = 500, bg = 'white')

