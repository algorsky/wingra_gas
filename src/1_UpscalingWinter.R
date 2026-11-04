library(tidyverse)
library(broom)

macrophyte<- read_csv("data/map/crosswalk_biomass.csv")
co2_dissolved<- read_csv("data/co2_dissolved_use.csv")|>
  left_join(macrophyte, by = c("site"))
polygon_sa_depth<- read_csv("data/polygon_sa_depth.csv")


# 13 days of freeze in December (13 + DOY = Days since Ice-On)
co2_winter<- co2_dissolved%>%
  filter(season == "ice" | date == as.Date("2023-03-23"))%>%
  mutate(doy = yday(date))%>%
  mutate(days_ice = 13 + doy)%>%
  mutate(mM = CO2_mean/1000)%>%
  mutate(CO2_max = CO2_mean + CO2_sd)

co2_iceoff<- co2_winter%>%
  filter(date == as.Date("2023-03-23"))%>%
  dplyr::select(geo_site, CO2_mean, CO2_sd)%>%
  rename(CO2_mean_off = CO2_mean, CO2_sd_off = CO2_sd)

co2_plateau<- co2_winter%>%
  filter(date == as.Date("2023-02-17"))%>%
  dplyr::select(geo_site, CO2_mean, CO2_sd)%>%
  rename(CO2_mean_plateau = CO2_mean, CO2_sd_plateau = CO2_sd)


#regression
df<- co2_winter%>%
  filter(season == "ice")
# Create an empty list to store the results
results <- list()
# Loop through each site and run linear regression
for (site_id in unique(df$geo_site)) {
  # Filter data for the current site
  site_data <- df %>% filter(geo_site == site_id)
  # Run the linear regression on days_ice and CO2_mean
  model <- lm(CO2_mean ~ days_ice, data = site_data)
  # Get the summary statistics of the model
  model_summary <- tidy(model)  # tidying the model results
  # Extract slope, intercept, and p-value
  slope <- model_summary$estimate[2]  # The slope is the second coefficient
  intercept <- model_summary$estimate[1]  # The intercept is the first coefficient
  r_squared <- summary(model)$r.squared# R2
  
  # Store the results in a dataframe
  results[[site_id]] <- data.frame(
    geo_site = site_id,
    slope = slope,
    intercept = intercept,
    r_squared = r_squared)
}

# Combine the results into one final dataframe
co2_winter_regression <- bind_rows(results)

co2_winter_flux<- co2_winter_regression%>%
  mutate(co2_under_ice = intercept + slope *91)%>%
  left_join(co2_iceoff, by = "geo_site")%>%
  rename(co2_after_ice = CO2_mean_off)%>%
  left_join(co2_plateau, by = "geo_site")

co2_winter_calc<- co2_winter_flux%>%
  dplyr::select(geo_site, co2_under_ice, co2_after_ice, CO2_sd_off)

co2_winter_calc_plateau<- co2_winter_flux%>%
  dplyr::select(geo_site, CO2_mean_plateau, CO2_sd_plateau, co2_after_ice, CO2_sd_off)

#### CO2 released at ice-off, by polygon ####
# Total lake area, as in 1_Upscaling.R (lake-wide rate = total gC / lake area)
surfacearea <- sum(polygon_sa_depth$area)

# Under-ice CO2 extrapolated to ice-off (day 91) from a per-site regression on days since ice-on
extrapolate_ice_off <- function(data, y) {
  data %>%
    group_by(geo_site) %>%
    summarize(co2_under_ice = predict(lm(.data[[y]] ~ days_ice), newdata = tibble(days_ice = 91)))
}

# For each polygon (L.W and L.SW share one): CO2 stored at ice-off minus CO2 after ice-off,
# times water volume. Negative values (more CO2 after ice-off than at ice-off) are set to 0.
# Input: geo_site, co2_under_ice and co2_after_ice (µmol/L), one row per site.
storage_flux <- function(d) {
  d %>%
    mutate(geo_site = ifelse(geo_site == "L.SW" | geo_site == "L.W", "L.W", geo_site)) %>%
    group_by(geo_site) %>%
    summarize(
      co2_under_ice = mean(co2_under_ice),  # Average under-ice CO₂
      co2_after_ice = mean(co2_after_ice)) %>%  # Average after-ice CO₂
    ungroup() %>%
    left_join(polygon_sa_depth, by = "geo_site") %>%
    rename(area_m2 = surface_area_m2, depth_m = depth) %>%
    dplyr::select(-cluster) %>%
    mutate(
      volume_L = area_m2 * depth_m * 1000,  # 1 m³ = 1000 L
      co2_storage_ice_off_mol = (co2_under_ice * volume_L) / 1e6,  # Convert μmol to mol
      co2_storage_post_ice_mol = (co2_after_ice * volume_L) / 1e6,
      total_efflux_mol_site = co2_storage_ice_off_mol - co2_storage_post_ice_mol,
      total_efflux_mol_site_filter = ifelse(total_efflux_mol_site < 0, 0, total_efflux_mol_site),
      total_efflux_gC = total_efflux_mol_site_filter * 12.01,
      flux_gC_m2_year_site = total_efflux_gC/area_m2)  # gC per m² of that polygon
}

# Lake-wide winter efflux (gC per m² of lake) = sum of polygon gC / total lake area
lake_flux <- function(d) sum(d$total_efflux_gC) / surfacearea

# Linear scenario: CO2 keeps rising at the under-ice rate until ice-off.
# Max/min as in 1_Upscaling.R (mean ± SD): regression on CO2 ± SD, after-ice CO2 ∓ SD.
co2_winter_calc_gC <- storage_flux(co2_winter_calc)
sites_max <- extrapolate_ice_off(df, "CO2_max") %>%
  left_join(co2_iceoff, by = "geo_site") %>%
  mutate(co2_after_ice = CO2_mean_off - CO2_sd_off) %>%
  storage_flux()
sites_min <- extrapolate_ice_off(mutate(df, CO2_min = CO2_mean - CO2_sd), "CO2_min") %>%
  left_join(co2_iceoff, by = "geo_site") %>%
  mutate(co2_after_ice = CO2_mean_off + CO2_sd_off) %>%
  storage_flux()

# Plateau scenario: CO2 stays at the last under-ice measurement (2023-02-17) until ice-off
sites_plateau <- co2_winter_calc_plateau %>%
  rename(co2_under_ice = CO2_mean_plateau) %>%
  storage_flux()
sites_plateau_max <- co2_winter_calc_plateau %>%
  mutate(co2_under_ice = CO2_mean_plateau + CO2_sd_plateau, co2_after_ice = co2_after_ice - CO2_sd_off) %>%
  storage_flux()
sites_plateau_min <- co2_winter_calc_plateau %>%
  mutate(co2_under_ice = CO2_mean_plateau - CO2_sd_plateau, co2_after_ice = co2_after_ice + CO2_sd_off) %>%
  storage_flux()

winter_co2 <- tibble(
  season = "winter",
  wingra_co2_gC_m2_yr = lake_flux(co2_winter_calc_gC),
  wingra_co2_gC_m2_yr_max = lake_flux(sites_max),
  wingra_co2_gC_m2_yr_min = lake_flux(sites_min))

winter_co2_plateau <- tibble(
  season = "winter (plateau)",
  wingra_co2_gC_m2_yr = lake_flux(sites_plateau),
  wingra_co2_gC_m2_yr_max = lake_flux(sites_plateau_max),
  wingra_co2_gC_m2_yr_min = lake_flux(sites_plateau_min))

bind_rows(winter_co2, winter_co2_plateau)
