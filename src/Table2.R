# Table 2: lake-wide CO2 flux (g C m-2 y-1) by season, ice melt, and annual totals.
# Values are mean ± half the max-min range from the upscaling scripts (mean ± SD scenarios).
# Negative = CO2 uptake by the lake.
library(tidyverse)

# Run each upscaling script in its own environment (both scripts create objects called df)
open_env <- new.env()
sys.source("src/1_Upscaling.R", envir = open_env)
ice_env <- new.env()
sys.source("src/1_UpscalingWinter.R", envir = ice_env)

# mean ± half-range from a mean and its max/min scenarios
pm <- function(mean, max, min) tibble(mean = mean, pm = abs(max - min) / 2)

# Open-water seasons (summer, fall, spring)
open_seasons <- open_env$co2_season_total_flux %>%
  mutate(season = as.character(season)) %>%
  { bind_cols(select(., season), pm(.$wingra_co2_gC_yr, .$wingra_co2_gC_yr_max, .$wingra_co2_gC_yr_min)) }

# Ice melt: release of CO2 stored under ice. Linear scenario (main value) and plateau scenario
ice_linear  <- with(ice_env$winter_co2, pm(wingra_co2_gC_m2_yr, wingra_co2_gC_m2_yr_max, wingra_co2_gC_m2_yr_min))
ice_plateau <- with(ice_env$winter_co2_plateau, pm(wingra_co2_gC_m2_yr, wingra_co2_gC_m2_yr_max, wingra_co2_gC_m2_yr_min))

# Annual = open-water seasons + ice melt (linear); uncertainties combined in quadrature
annual_parts <- bind_rows(open_seasons, ice_linear)
annual <- tibble(mean = sum(annual_parts$mean), pm = sqrt(sum(annual_parts$pm^2)))

# Annual with no macrophytes: the pelagic site with no macrophytes (P1, 0 g) applied to the whole
# lake. P1 had no under-ice storage release (ice-melt efflux = 0), so this is open water only.
p1 <- filter(open_env$co2_annual, geo_site == "P1")
annual_no_macro <- pm(p1$co2_mmol_m2_year, p1$co2_mmol_m2_year_max, p1$co2_mmol_m2_year_min) %>%
  mutate(across(everything(), ~ .x * 12.01 * 0.001))  # mmol m-2 y-1 to g C m-2 y-1
p1_ice <- filter(ice_env$co2_winter_calc_gC, geo_site == "P1")$flux_gC_m2_year_site
stopifnot(p1_ice == 0)

# Ice-covered days: 21-27 Nov 2022 and 18 Dec 2022-19 Mar 2023 (Magnuson et al. 2023)
ice_days <- 7 + 92
stopifnot(sum(open_env$season_days) + ice_days == 365)

fmt <- function(d) sprintf("%s ± %s", signif(d$mean, 2), signif(d$pm, 2))

table2 <- tibble(
  Season = c("Summer", "Fall", "Ice melt*", "Spring", "Annual", "Annual - no macrophytes"),
  Days = c(open_env$season_days[c("Summer", "Fall")], ice_days, open_env$season_days["Spring"], 365, 365),
  `CO2 (g C m-2)` = c(
    fmt(filter(open_seasons, season == "Summer")),
    fmt(filter(open_seasons, season == "Fall")),
    sprintf("%s (%s)", fmt(ice_linear), fmt(ice_plateau)),
    fmt(filter(open_seasons, season == "Spring")),
    fmt(annual),
    fmt(annual_no_macro)))

table2
# * Ice melt: CO2 stored under ice released at ice-off (ice-covered days shown); linear accumulation
#   to ice-off, plateau scenario in parentheses

# Ice-melt contribution to the annual budget (Abstract and Discussion), linear and plateau scenarios.
#   share of gross flux  = |ice melt| / sum of |seasonal fluxes| (definition behind the original 22%)
#   share of efflux      = ice melt / (ice melt + spring), the seasons with net CO2 release
ice_share <- function(ice) {
  seasonal <- c(open_seasons$mean, ice$mean)
  tibble(share_gross_pct = 100 * abs(ice$mean) / sum(abs(seasonal)),
         share_efflux_pct = 100 * ice$mean / sum(seasonal[seasonal > 0]))
}
bind_rows(linear = ice_share(ice_linear), plateau = ice_share(ice_plateau), .id = "scenario")
