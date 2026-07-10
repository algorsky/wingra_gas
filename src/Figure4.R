library(tidyverse)
library(multcompView)
library(patchwork)

source('src/0_SiteRakeMatch.R')

df <- read_csv("data/diffusive.csv") |>
  mutate(season = case_when(
    month(Date) %in% 3:5   ~ "Spring", # March-May
    month(Date) %in% 6:8   ~ "Summer", # June-August
    month(Date) %in% 9:10  ~ "Fall",   # September-October
    month(Date) %in% 1:2   ~ "Winter"  # January-February, under ice
  )) %>%
  # filter(Date != as.Date("2022-06-28")) %>%
  mutate(Site_num = gsub("_", " ", Site)) %>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "Winter"))) %>% 
  mutate(co2_mean = co2_mean*1000*24)  # convert units of CO2 to mmol m-2 d-1


#crosswalk
crosswalk <- read_csv("data/map/crosswalk_biomass.csv")

ph_dic <- read_csv("data/temp_dic_ph.csv") |>
  rename(temp = Temp_C, ph = pH) |>
  dplyr::select(sampledate, site, temp, ph, dic)

data <- df |>
  left_join(crosswalk, by = "Site") |>
  left_join(ph_dic, by = c("site", "Date" = "sampledate")) |>
  left_join(site_rake_mean) |> 
  mutate(
    # multcompView::vec2mat() splits Tukey comparison names on "-", so these levels must stay
    # hyphen-free for get_letters() to work; the "0"/"1-29"/... labels are applied in make_plot()
    biomass_rake_fulness = case_when(
      biomass < 1 ~ "0",
      biomass < 30 ~ "1to29",
      biomass < 80 ~ "30to79",
      biomass >= 80 ~ "80plus"
    )) |>
  mutate(biomass_rake_fulness = factor(biomass_rake_fulness, levels = c("0", "1to29", "30to79", "80plus")))

# Compact letter display from Tukey HSD for one response variable
get_letters <- function(df, response) {
  mod <- aov(as.formula(paste(response, "~ biomass_rake_fulness")), data = df)
  cld <- multcompLetters4(mod, TukeyHSD(mod))[["biomass_rake_fulness"]]
  data.frame(biomass_rake_fulness = names(cld$Letters), Letters = cld$Letters, row.names = NULL)
}

seasons <- c("Spring", "Summer", "Fall")

plot_df <- data |>
  filter(season %in% seasons) |>
  drop_na(biomass_rake_fulness, co2_mean, ph) |>
  mutate(season = factor(season, levels = seasons))

# Fixed fraction of each response's overall range, so CO2 and pH letters sit equally far above
# their boxes regardless of the two variables' very different scales
co2_range <- diff(range(plot_df$co2_mean, na.rm = TRUE))
ph_range  <- diff(range(plot_df$ph, na.rm = TRUE))

# MANOVA (co2_mean, ph ~ biomass class) for one season; returns Tukey letters (with a label y-position)
# for each response, or NULL if that response's univariate ANOVA isn't significant
analyze_season <- function(s) {
  df_s <- plot_df |>
    filter(season == s) |>
    mutate(biomass_rake_fulness = droplevels(biomass_rake_fulness))

  manova_model <- manova(cbind(co2_mean, ph) ~ biomass_rake_fulness, data = df_s)
  print(summary(manova_model, test = "Wilks"))
  aov_results <- summary.aov(manova_model)
  print(aov_results)

  make_letters <- function(response, p_value, range_) {
    if (is.na(p_value) || p_value >= 0.05) return(NULL)
    get_letters(df_s, response) |>
      left_join(
        df_s |> group_by(biomass_rake_fulness) |> summarise(y = max(.data[[response]], na.rm = TRUE) + 0.08 * range_),
        by = "biomass_rake_fulness"
      ) |>
      mutate(season = s)
  }

  list(
    co2 = make_letters("co2_mean", aov_results[[1]][["Pr(>F)"]][1], co2_range),
    ph  = make_letters("ph", aov_results[[2]][["Pr(>F)"]][1], ph_range)
  )
}

results <- set_names(seasons) |> map(analyze_season)

co2_letters <- bind_rows(map(results, "co2"))
ph_letters  <- bind_rows(map(results, "ph"))

# Boxplot of `response` by season, with biomass classes dodged side-by-side and Tukey letters overlaid
dodge <- position_dodge(width = 0.8)

make_plot <- function(df, letters_df, response, ylab, hline = FALSE) {
  p <- ggplot(df, aes(season, .data[[response]], fill = biomass_rake_fulness))

  if (hline) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.2)
  }

  p <- p +
    geom_boxplot(position = dodge, linewidth = 0.2, outlier.size = 0.2) +
    scale_fill_brewer(palette = "Greens", name = "Biomass (g)",
                       labels = c("0", "1-29", "30-79", "80+")) +
    xlab("") + ylab(ylab) +
    theme_bw(base_size = 9)

  if (nrow(letters_df) > 0) {
    p <- p + geom_text(
      data = letters_df,
      aes(x = season, y = y, label = Letters, group = biomass_rake_fulness),
      position = dodge, inherit.aes = FALSE, size = 2
    )
  }
  p
}


co2_plot <- make_plot(plot_df, co2_letters, "co2_mean",   expression(paste("C", O[2] , " Flux (mmol ", " ", m^-2, d^-1,")")))+
  geom_hline(yintercept = 0, linetype = "dashed")

ph_plot  <- make_plot(plot_df, ph_letters, "ph", "pH")

combined_plot <- (co2_plot | ph_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8), 
        legend.position = "bottom",
        legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(2,0,0,0),
        legend.box.margin = margin(-4,0,0,0))

combined_plot

ggsave(filename = 'figures/Figure4.png', plot = combined_plot, width = 6, height = 2.5, units = 'in', dpi = 500)
