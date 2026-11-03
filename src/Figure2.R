################################# Calculate bicarbonate #################################
source('src/0_alkalinity.R')

# Site type, same definition and colours as Figures 1 and 4: pelagic vs littoral from geo_site
# (P = pelagic, L = littoral); littoral sites split at the 45-98 g gap in macrophyte biomass.
# Legend shows only the biomass range of each type.
type_levels <- c("pelagic", "littoral", "dense")
type_labels <- c("0–9", "16–45", "98–227")
type_fills  <- c(pelagic = "#e5f5e0", littoral = "#74c476", dense = "#006d2c")

alk <- alk |>
  mutate(
    site_type = case_when(
      startsWith(geo_site, "P") ~ "pelagic",
      biomass < 80              ~ "littoral",
      TRUE                      ~ "dense"),
    site_type = factor(site_type, levels = type_levels))

ggplot(filter(alk, site != 6 & sampledate != as.Date("2022-07-25"))) +
  annotate("rect",
    xmin = as.Date("2022-12-18"), xmax = as.Date("2023-03-20"),
    ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3) +
  geom_point(aes(x = sampledate, y = alkalinity_calculated, fill = site_type),
    size = 2, shape = 21, stroke = 0.2) +
  scale_fill_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  xlab("") +
  ylab(expression(paste("Alkalinity (", µ,"eq ", L^-1,")"))) +
  theme_bw(base_size = 9) +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_blank())

ggsave(filename = 'figures/Figure2.png',width = 6,height = 2.5,units = 'in', dpi = 500)
