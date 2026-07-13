library(cowplot)
################################# Polygons #################################
source('src/0_PolygonsVoronoi.R')

# Full plot of polygons
polygon_plot <- ggplot() +
  geom_sf(data = filter(lake_macrophytes, rake < 10), aes(color = as.factor(rake), 
                                                          shape = as.factor(rake), fill = as.factor(rake), size = as.factor(rake)), stroke = 1.1) +
  geom_sf(data = cluster_polygons, alpha = 0.2, linewidth = 0.5) +
  geom_sf(data = sites_sf, color = 'black', fill = "white", size = 2.4, shape = 22, stroke = 0.5, alpha = 0.8) +
  geom_text(data = sites_sf, aes(st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], 
       label = rating), color = "black", size = 1.8, fontface = 1) +
  
  scale_color_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"),  name = "", guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("lightgray",'#c2e699',"#78c679",  "#006837", "gray77"),labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_shape_manual(values = c(19, 19, 19, 19, 4), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  scale_size_manual(values = c(0.2,0.2,0.2,0.2,0.2), labels = c("0", "1", "2", "3", "Not sampled"), name = "", guide = guide_legend(title.position = "top")) +
  # scale_y_continuous(breaks = c(43.05, 43.08)) +
  scale_x_continuous(breaks = c(-89.43, -89.42, -89.41)) +
  guides(color = guide_legend(override.aes = list(size = 2), title.position = "top", nrow = 1, byrow = TRUE)) +
  theme_bw(base_size = 9) +
  theme(
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_text(size=8),
    legend.text = element_text(size=8),
    legend.direction="horizontal",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.box.spacing = unit(0, "pt"),
    legend.margin = margin(0,0,0,0),
    legend.box.margin = margin(-4,0,0,0)
  ) +
  ggspatial::annotation_scale( bar_cols = c("grey", "white"), 
                               location = "br", text_cex = 0.5, pad_y = unit(0.15, "cm")); polygon_plot

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv")|>
  left_join(macrophyte, by = c("site")) |>
  mutate(
    # multcompView::vec2mat() splits Tukey comparison names on "-", so these levels must stay
    # hyphen-free; the "0"/"1-29"/... labels are applied via scale_fill/color_brewer() below
    biomass_rake_fulness = case_when(
      biomass < 1 ~ "0",
      biomass < 30 ~ "1to29",
      biomass < 80 ~ "30to79",
      biomass >= 80 ~ "80plus"
    ),
    biomass_rake_fulness = factor(biomass_rake_fulness, levels = c("0", "1to29", "30to79", "80plus"))
  )
# 13 days of freeze in December (13 + DOY = Days since Ice-On)
co2_winter <- co2_dissolved%>%
  filter(season == "ice" | date == as.Date("2023-03-23"))%>%
  mutate(doy = yday(date))%>%
  mutate(days_ice = 13 + doy)%>%
  mutate(mM = CO2_mean/1000)%>%
  mutate(CO2_max = CO2_mean + CO2_sd)

co2_winter_plot <- ggplot() +
  geom_smooth(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, group = site, color = biomass_rake_fulness), method = "lm", se = F) +
  geom_vline(xintercept = 91, linetype = "dashed", linewidth = 0.8) +
  geom_segment(aes(x = 61, xend = 91, y = -2.36 + 0.31*61, yend = -2.36 + 0.31*91), linetype ="dashed", color = "#F7FCF5") +
  geom_segment(aes(x = 61, xend = 91, y = 0.01 + 0.57*61, yend = 0.01 + 0.57*91), linetype ="dashed", color = "#A1D99B") +
  geom_segment(aes(x = 61, xend = 91, y = -8.68 + 0.53*61, yend = -8.68 + 0.53*91), linetype ="dashed", color = "#C7E9C0") +
  geom_segment(aes(x = 61, xend = 91, y = -36.9 + 1.37*61, yend = -36.9 + 1.37*91), linetype ="dashed", color = "#41AB5D") +
  geom_segment(aes(x = 61, xend = 91, y = -11.7 + 0.68*61, yend = -11.7 + 0.68*91), linetype ="dashed", color = "#E5F5E0") +
  geom_segment(aes(x = 61, xend = 91, y = -207 + 6.59*61, yend = -207 + 6.59*91), linetype ="dashed", color = "#238B45") +
  geom_segment(aes(x = 61, xend = 91, y = -174 + 5.48*61, yend = -174 + 5.48*91), linetype ="dashed", color = "#005A32") +
  geom_segment(aes(x = 61, xend = 91, y = -7.29 + 0.92*61, yend = -7.29 + 0.92*91), linetype ="dashed", color = "#74C476") +
  # geom_point(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, fill = biomass_rake_fulness),
  #   size = 2, shape = 21, stroke = 0.2) +
  geom_pointrange(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, ymin = CO2_mean - CO2_sd, ymax = CO2_mean + CO2_sd, fill = biomass_rake_fulness), shape = 21, stroke = 0.2) +
  #geom_point(data = filter(co2_winter, season == "open"), aes(x = days_ice, y = CO2_mean, fill = biomass_rake_fulness), size = 2, shape = 21) +
  geom_pointrange(data = filter(co2_winter, season == "open"), aes(x = days_ice, y = CO2_mean, ymin = CO2_mean - CO2_sd, ymax = CO2_mean + CO2_sd,fill = biomass_rake_fulness), shape = 21, stroke = 0.2) +
  xlab("Days since ice-on") +
  scale_color_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
  scale_fill_brewer(palette = "Greens", name = "Biomass (g)", labels = c("0", "1-29", "30-79", "80+")) +
  ylab(expression(paste("Surface C", O[2], " (", µ,"mol ", L^-1,")"))) +
  theme_bw(base_size = 9) +
  theme(legend.position = "inside",
        legend.position.inside = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_rect(fill = "white", colour = 'black', linewidth = 0.1),
        legend.key.size = unit(0.3, "cm"),
        legend.spacing.y = unit(0, "cm"),
        legend.margin = margin(1,1,1,1),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8))

# patchwork doesn't work well for maps 
# polygon_plot + co2_winter_plot + plot_annotation(tag_levels = 'a', tag_prefix = "(", tag_suffix = ")") 

# ggsave(filename = 'figures/Figure6.png', width = 6, height = 2, units = 'in', dpi = 500)
plot_grid(
  polygon_plot +
    theme(plot.tag = element_text(size = 8)),
  co2_winter_plot +
    theme(plot.tag = element_text(size = 8)),
  labels = c("(a)", "(b)"),
  label_size = 8,
  label_fontface = "plain",
  ncol = 2
)
ggsave(filename = 'figures/Figure7.png', width = 6, height = 2.5, units = 'in', dpi = 500, bg = 'white')
