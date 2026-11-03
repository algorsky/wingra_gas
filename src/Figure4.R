library(tidyverse)
library(lmerTest)
library(multcompView)
library(patchwork)

# Figure 4: CO2 flux and pH by season and site type (pelagic / littoral / dense littoral).
# Previous version (biomass bins, MANOVA + Tukey) is in src/Figure4_old.R

df <- read_csv("data/diffusive.csv") |>
  mutate(season = case_when(
    month(Date) %in% 3:5   ~ "Spring", # March-May
    month(Date) %in% 6:8   ~ "Summer", # June-August
    month(Date) %in% 9:10  ~ "Fall",   # September-October
    month(Date) %in% 1:2   ~ "Winter")) |>  # January-February, under ice
  mutate(co2_mean = co2_mean * 1000 * 24)  # convert units of CO2 to mmol m-2 d-1

crosswalk <- read_csv("data/map/crosswalk_biomass.csv")

ph_dic <- read_csv("data/temp_dic_ph.csv") |>
  rename(temp = Temp_C, ph = pH) |>
  dplyr::select(sampledate, site, temp, ph, dic)

# Site type: pelagic vs littoral from geo_site (P = pelagic, L = littoral);
# littoral sites split at the 45-98 g gap in macrophyte biomass
type_levels <- c("pelagic", "littoral", "dense")
type_labels <- c("Pelagic (0–9 g)", "Littoral (16–45 g)", "Dense littoral (98–227 g)")

seasons <- c("Spring", "Summer", "Fall")

plot_df <- df |>
  left_join(crosswalk, by = "Site") |>
  left_join(ph_dic, by = c("site", "Date" = "sampledate")) |>
  mutate(
    site_type = case_when(
      startsWith(geo_site, "P") ~ "pelagic",
      biomass < 80              ~ "littoral",
      TRUE                      ~ "dense"),
    site_type = factor(site_type, levels = type_levels)) |>
  filter(season %in% seasons) |>
  drop_na(site_type, co2_mean, ph) |>
  mutate(season = factor(season, levels = seasons),
         site = factor(site),
         date = factor(Date))

print(distinct(plot_df, site, geo_site, biomass, site_type) |> arrange(biomass))

# Fixed fraction of each response's overall range, so CO2 and pH letters sit equally far above their boxes
co2_range <- diff(range(plot_df$co2_mean))
ph_range  <- diff(range(plot_df$ph))

# One mixed model per response across all seasons:
# response ~ season + season:site_type + (1|site) + (1|date)
# Random site and date intercepts account for repeated sampling of the same 8 sites and for
# conditions shared across sites on a sampling date. Satterthwaite df via lmerTest.
# Within each season: joint F test of site type; if p < 0.05, all pairwise site-type contrasts
# (Holm-corrected within season) are converted to compact letters.
#
# Arguments: s = season name ("Spring", "Summer", "Fall"); response = column to model
# ("co2_mean" or "ph"); range_ = that response's overall range, used to place letters.
# Returns a data frame of letters and y-positions for the plot, or NULL if site type
# has no significant effect in that season.
season_letters <- function(s, response, range_) {

  # --- 1. Fit the mixed model ---------------------------------------------------------
  # "season + season:site_type" (no site_type main effect) gives every season its own
  # set of site-type coefficients. With pelagic as the reference level, the fixed effects are:
  #   seasonSpring, seasonSummer, seasonFall            = mean of pelagic sites in each season
  #   seasonSummer:site_typelittoral, ...:site_typedense = difference from pelagic in that season
  # (1|site) and (1|date) are random intercepts: each site, and each sampling date, gets its
  # own offset, so repeat visits to a site and sites sampled on the same day are not treated
  # as independent observations.
  
  f <- as.formula(paste(response, "~ season + season:site_type + (1|site) + (1|date)"))
  m <- lmer(f, data = plot_df)

  # --- 2. Joint test: does site type matter at all in season s? ------------------------
  # Find the two coefficients for this season (e.g. "seasonSummer:site_typelittoral" and
  # "seasonSummer:site_typedense") and build a contrast matrix L with one row per
  # coefficient, each row a 1 in that coefficient's column and 0 elsewhere. contest() with
  # joint = TRUE then tests whether both are zero at once, i.e. whether all three site
  # types have the same mean in season s (an F test, Satterthwaite df).
  idx <- grep(paste0("^season", s, ":site_type"), names(fixef(m)))
  L <- matrix(0, length(idx), length(fixef(m)))
  L[cbind(seq_along(idx), idx)] <- 1
  overall_p <- contest(m, L, joint = TRUE)[["Pr(>F)"]]

  # --- 3. Pairwise comparisons between site types in season s --------------------------
  # combn() lists the three pairs: pelagic-littoral, pelagic-dense, littoral-dense.
  # The model only reports differences from its reference level, so for each pair we refit
  # with the first type of the pair as reference (relevel). The coefficient
  # "season<s>:site_type<second type>" is then that pair's difference in season s, with its
  # t-test p-value. Refitting changes only the coding, not the fitted model.
  
  pairs <- combn(type_levels, 2)
  est <- p <- numeric(ncol(pairs))
  for (j in seq_len(ncol(pairs))) {
    m_ref <- lmer(f, data = mutate(plot_df, site_type = relevel(site_type, pairs[1, j])))
    cf <- coef(summary(m_ref))[paste0("season", s, ":site_type", pairs[2, j]), ]
    est[j] <- cf[["Estimate"]]   # mean of second type minus mean of first type
    p[j] <- cf[["Pr(>|t|)"]]
  }

  # Holm correction for the three comparisons within this season. Names must be
  # "typeA-typeB" because multcompLetters() reads the pair from them.
  names(p) <- paste(pairs[1, ], pairs[2, ], sep = "-")
  p_adj <- p.adjust(p, "holm")

  # Print the results to the console (for reporting in the text)
  cat(sprintf("\n%s, %s: joint site-type test p = %.4f\n", s, response, overall_p))
  print(data.frame(contrast = paste(pairs[2, ], "minus", pairs[1, ]), estimate = round(est, 3),
                   p = round(p, 4), p_holm = round(p_adj, 4), row.names = NULL))

  # --- 4. Compact letter display for the plot ------------------------------------------
  # No letters if the joint test is not significant (same rule as an ANOVA before Tukey).
  if (overall_p >= 0.05) return(NULL)

  # multcompLetters() takes TRUE/FALSE for each pair (TRUE = significantly different) and
  # gives each site type letters such that types sharing a letter are not different.
  letters <- multcompLetters(p_adj < 0.05)$Letters

  # Put each letter just above the tallest observation of its box (8% of the response's
  # overall range) so letters sit the same distance above the boxes in both panels.
  tibble(site_type = factor(names(letters), levels = type_levels), Letters = letters, season = s) |>
    left_join(
      plot_df |> filter(season == s) |> group_by(site_type) |>
        summarise(y = max(.data[[response]]) + 0.08 * range_),
      by = "site_type")
}

co2_letters <- map(seasons, season_letters, response = "co2_mean", range_ = co2_range) |> bind_rows()
ph_letters  <- map(seasons, season_letters, response = "ph",       range_ = ph_range)  |> bind_rows()

# Boxplot of `response` by season, site types dodged side-by-side, letters overlaid
dodge <- position_dodge(width = 0.8)
fills <- c(pelagic = "#e5f5e0", littoral = "#74c476", dense = "#006d2c")

make_plot <- function(df, letters_df, response, ylab) {
  p <- ggplot(df, aes(season, .data[[response]], fill = site_type)) +
    geom_boxplot(position = dodge, linewidth = 0.2, outlier.size = 0.2) +
    scale_fill_manual(values = fills, labels = type_labels, name = NULL) +
    xlab("") + ylab(ylab) +
    theme_bw(base_size = 9)

  if (nrow(letters_df) > 0) {
    p <- p + geom_text(
      data = letters_df,
      aes(x = season, y = y, label = Letters, group = site_type),
      position = dodge, inherit.aes = FALSE, size = 2)
  }
  p
}

co2_plot <- make_plot(plot_df, co2_letters, "co2_mean",
                      expression(paste("C", O[2], " Flux (mmol ", " ", m^-2, d^-1, ")"))) +
  geom_hline(yintercept = 0, linetype = "dashed")

ph_plot <- make_plot(plot_df, ph_letters, "ph", "pH")

combined_plot <- (co2_plot | ph_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(plot.tag = element_text(size = 8),
        legend.position = "bottom",
        legend.box.spacing = unit(0, "pt"),
        legend.margin = margin(2, 0, 0, 0),
        legend.box.margin = margin(-4, 0, 0, 0))

combined_plot

ggsave(filename = 'figures/Figure4.png', plot = combined_plot, width = 6, height = 2.5, units = 'in', dpi = 500)
