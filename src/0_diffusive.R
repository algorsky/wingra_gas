library(tidyverse)

df<-read_csv("data/diffusive.csv")|>
  mutate(season = ifelse(month(Date) < 6, "Spring", 
                         ifelse(month(Date) > 8, "Fall", "Summer")))%>%
  # filter(Date != as.Date("2022-06-28"))%>%
  mutate(Site_num = gsub("_", " ", Site))%>%
  mutate(season = factor(season, levels = c("Summer", "Fall", "Spring")))

#crosswalk
crosswalk<- read_csv("data/map/crosswalk_biomass.csv")

df_figure<- df%>%
  left_join(crosswalk, by = "Site")%>%
  mutate(season_n = ifelse(season == "Summer", "Summer (n = 8)",
                           ifelse(season == "Fall", "Fall (n = 4)", "Spring (n = 4)")))%>%
  mutate(season_n = factor(season_n, levels = c("Summer (n = 8)", "Fall (n = 4)", "Spring (n = 4)")))%>%
  mutate(co2_mmol.m2.d = co2_mean*1000*24)%>%
  mutate(co2_sd_units = sd_co2*1000*24)
