################################# Calculate bicarbonate #################################
source('src/0_alkalinity.R')

alk <- alk |>
  mutate(
    # multcompView::vec2mat() splits Tukey comparison names on "-", so these levels must stay
    # hyphen-free; the "0"/"1-29"/... labels are applied via scale_fill_brewer() below
    biomass_rake_fulness = case_when(
      biomass < 1 ~ "0",
      biomass < 30 ~ "1to29",
      biomass < 80 ~ "30to79",
      biomass >= 80 ~ "80plus"
    ),
    biomass_rake_fulness = factor(biomass_rake_fulness, levels = c("0", "1to29", "30to79", "80plus"))
  )

ggplot(filter(alk, site != 6 & sampledate != as.Date("2022-07-25"))) +
  annotate("rect",
    xmin = as.Date("2022-12-18"), xmax = as.Date("2023-03-20"),
    ymin = -Inf, ymax = Inf, fill = "gray", alpha = 0.3) +
  geom_point(aes(x = sampledate, y = alkalinity_calculated, fill = biomass_rake_fulness),
    size = 2, shape = 21, stroke = 0.2) +
  scale_fill_brewer(palette = "Greens", name = "Biomass (g)",
                     labels = c("0", "1-29", "30-79", "80+")) +
  xlab("") +
  ylab(expression(paste("Alkalinity (", µ,"eq ", L^-1,")"))) +
  theme_bw(base_size = 9) +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_blank())

ggsave(filename = 'figures/Figure2.png',width = 6,height = 2.5,units = 'in', dpi = 500)
