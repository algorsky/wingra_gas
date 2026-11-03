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
    legend.box.margin = margin(-4,0,0,0)) +
  ggspatial::annotation_scale( bar_cols = c("grey", "white"), 
                               location = "br", text_cex = 0.5, pad_y = unit(0.15, "cm")); polygon_plot

# Site type, same definition and colours as Figures 1, 2, 4 and 5: pelagic vs littoral from
# geo_site (P = pelagic, L = littoral); littoral sites split at the 45-98 g gap in macrophyte
# biomass. Legend shows only the biomass range of each type.
type_levels <- c("pelagic", "littoral", "dense")
type_labels <- c("0–9", "16–45", "98–227")
type_fills  <- c(pelagic = "#e5f5e0", littoral = "#74c476", dense = "#006d2c")

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv")|>
  left_join(macrophyte, by = c("site")) |>
  mutate(
    site_type = case_when(
      startsWith(geo_site, "P") ~ "pelagic",
      biomass < 80              ~ "littoral",
      TRUE                      ~ "dense"),
    site_type = factor(site_type, levels = type_levels))
# 13 days of freeze in December (13 + DOY = Days since Ice-On)
co2_winter <- co2_dissolved%>%
  filter(season == "ice" | date == as.Date("2023-03-23"))%>%
  mutate(doy = yday(date))%>%
  mutate(days_ice = 13 + doy)%>%
  mutate(mM = CO2_mean/1000)%>%
  mutate(CO2_max = CO2_mean + CO2_sd)

# Per-site linear fit of under-ice CO2 vs days since ice-on, extended (dashed) from the last
# under-ice sample (day 61) to ice-off (day 91)
ice_fits <- co2_winter |>
  filter(season == "ice") |>
  group_by(site, site_type) |>
  summarise(int = coef(lm(CO2_mean ~ days_ice))[1],
            slope = coef(lm(CO2_mean ~ days_ice))[2], .groups = "drop") |>
  mutate(x = 61, xend = 91, y = int + slope * x, yend = int + slope * xend)

co2_winter_plot <- ggplot() +
  geom_smooth(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, group = site, color = site_type), method = "lm", se = F) +
  geom_vline(xintercept = 91, linetype = "dashed", linewidth = 0.8) +
  geom_segment(data = ice_fits, aes(x = x, xend = xend, y = y, yend = yend, color = site_type), linetype = "dashed") +
  # geom_point(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, fill = site_type),
  #   size = 2, shape = 21, stroke = 0.2) +
  geom_pointrange(data = filter(co2_winter, season == "ice"), aes(x = days_ice, y = CO2_mean, ymin = CO2_mean - CO2_sd, ymax = CO2_mean + CO2_sd, fill = site_type), shape = 21, stroke = 0.2) +
  #geom_point(data = filter(co2_winter, season == "open"), aes(x = days_ice, y = CO2_mean, fill = site_type), size = 2, shape = 21) +
  geom_pointrange(data = filter(co2_winter, season == "open"), aes(x = days_ice, y = CO2_mean, ymin = CO2_mean - CO2_sd, ymax = CO2_mean + CO2_sd,fill = site_type), shape = 21, stroke = 0.2) +
  xlab("Days since ice-on") +
  scale_color_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  scale_fill_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
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
  ncol = 2)
ggsave(filename = 'figures/Figure7.png', width = 6, height = 2.5, units = 'in', dpi = 500, bg = 'white')
