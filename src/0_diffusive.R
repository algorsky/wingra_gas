library(tidyverse)

df <- read_csv("data/diffusive.csv") |>
  mutate(season = case_when(
    month(Date) %in% 3:5   ~ "Spring", # March-May
    month(Date) %in% 6:8   ~ "Summer", # June-August
    month(Date) %in% 9:10  ~ "Fall",   # September-October
    month(Date) %in% 1:2   ~ "Winter"  # January-February, under ice
  )) %>%
  # filter(Date != as.Date("2022-06-28")) %>%
  mutate(Site_num = gsub("_", " ", Site)) %>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring", "Winter")))

#crosswalk
crosswalk <- read_csv("data/map/crosswalk_biomass.csv")

df_figure <- df %>%
  left_join(crosswalk, by = "Site") %>%
  mutate(season_n = case_when(
    season == "Summer" ~ "Summer (n = 10)",
    season == "Fall"   ~ "Fall (n = 4)",
    season == "Spring" ~ "Spring (n = 5)",
    season == "Winter" ~ "Winter (under ice)"
  )) %>%
  mutate(season_n = factor(season_n, levels = c("Summer (n = 10)", "Fall (n = 4)", "Spring (n = 5)", "Winter (under ice)"))) %>%
  mutate(co2_mmol.m2.d = co2_mean*1000*24)%>%
  mutate(co2_sd_units = sd_co2*1000*24)
