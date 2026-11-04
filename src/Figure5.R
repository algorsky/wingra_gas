library(tidyverse)
library(patchwork)
library(lmerTest)
library(multcompView)

# Site type, same definition and colours as Figures 1, 2 and 4: pelagic vs littoral from geo_site
# (P = pelagic, L = littoral); littoral sites split at the 45-98 g gap in macrophyte biomass.
# Legend shows only the biomass range of each type.
type_levels <- c("pelagic", "littoral", "dense")
type_labels <- c("0–9", "16–45", "98–227")
type_fills  <- c(pelagic = "#e5f5e0", littoral = "#74c476", dense = "#006d2c")

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv") |>
  left_join(macrophyte, by = c("site")) |>
  mutate(
    site_type = case_when(
      startsWith(geo_site, "P") ~ "pelagic",
      biomass < 80              ~ "littoral",
      TRUE                      ~ "dense"),
    site_type = factor(site_type, levels = type_levels))

#Carbon dioxide
ggplot() +
  geom_rect(aes(xmin = as.Date("2022-12-18"), xmax = as.Date("2023-03-20"), ymin = -Inf, ymax = Inf), fill = "gray", alpha = 0.3) +
  geom_line(data = co2_dissolved, aes(x = date, y = CO2_mean, color = site_type, group = rating_site)) +
  geom_line(data = co2_dissolved, aes(x = date, y = total_mean_co2), linetype = "dashed", alpha = 0.7) +
  geom_point(data = co2_dissolved, aes(x = date, y = CO2_mean, fill = site_type),
             shape = 21, size = 2, stroke = 0.2) +
  scale_fill_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  scale_color_manual(values = type_fills, labels = type_labels, name = "Biomass (g)") +
  ylab(expression(paste('Dissolved Carbon Dioxide (',mu,'M)'))) +
  xlab("") +
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

ggsave(filename = 'figures/Figure5.png',width = 6,height = 3,units = 'in', dpi = 500)
 



#### Compare under-ice CO2 among site types (mixed model) ####

# make seasonal categories
co2_dissolved = co2_dissolved %>% 
  mutate(season = case_when(
    month(date) %in% 3:5   ~ "Spring", # March-May
    month(date) %in% 6:8   ~ "Summer", # June-August
    month(date) %in% 9:10  ~ "Fall",   # September-October
    month(date) %in% 1:2   ~ "Winter")) %>%  # January-February, under ice
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "Winter"))) 

# under-ice samples: 8 sites x 3 dates (2023-01-23, 02-10, 02-17)
winter.co2 <- co2_dissolved %>% 
  filter(season == "Winter") %>%
  mutate(site = factor(site), date_f = factor(date))

# quick boxplot to visually check the data
ggplot(winter.co2, aes(x = site_type, y = CO2_mean)) +
  geom_boxplot()

# log(CO2) ~ site type, random intercepts for site and sampling date (as in Figure 4).
# log scale because dense-littoral CO2 is ~4x higher and ~3x more variable than other sites;
# on the raw scale residual variance is very unequal and the site variance collapses to zero.
f <- log(CO2_mean) ~ site_type + (1|site) + (1|date_f)
mod <- lmer(f, data = winter.co2)
anova(mod)   # overall test of site type (F test, Satterthwaite df)

# All pairwise site-type contrasts (refit with each type as reference), Holm-corrected.
# Estimates are differences in log(CO2); exp(estimate) = ratio of geometric means.
pairs <- combn(type_levels, 2)
winter_contrasts <- map_dfr(seq_len(ncol(pairs)), function(j) {
  m_ref <- lmer(f, data = mutate(winter.co2, site_type = relevel(site_type, pairs[1, j])))
  cf <- coef(summary(m_ref))[paste0("site_type", pairs[2, j]), ]
  tibble(contrast = paste(pairs[2, j], "vs", pairs[1, j]),
         ratio = exp(cf[["Estimate"]]), p = cf[["Pr(>|t|)"]])
}) %>%
  mutate(p_holm = p.adjust(p, "holm"))
winter_contrasts

# Compact letters: site types sharing a letter do not differ (Holm p >= 0.05)
multcompLetters(setNames(winter_contrasts$p_holm < 0.05,
                         paste(pairs[1, ], pairs[2, ], sep = "-")))$Letters

# Under ice, dissolved CO2 differed among site types (linear mixed-effects model on log-transformed 
# CO2 with site and sampling date as random effects; F₂,₅ = 18.4, p = 0.005). 
# Dense littoral sites had 5.4 and 3.5 times the CO2 of pelagic and littoral sites, 
# respectively (Holm-corrected p = 0.007 and 0.009), while pelagic and littoral sites did not differ (p = 0.14).



