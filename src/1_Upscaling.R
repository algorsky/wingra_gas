# Load required packages
library(tidyverse)
library(lubridate)
library(tidyverse)

#Data
################################# co2 data #################################
source('src/0_diffusive.R')

################################# polygons #################################
polygon_sa_depth<- read_csv("data/polygon_sa_depth.csv")

#Total lake area
cluster_area_total<- polygon_sa_depth%>%
  summarize(area = sum(area))
surfacearea<- as.numeric(cluster_area_total$area)
  
#Calculate mean and sd of co2 per season (Summer: June-Aug)
#combine Site 2 and 3 for upscaling as they share a cluster
co2_data_season<- df_figure%>%
  mutate(geo_site = ifelse(geo_site == "L.SW" | geo_site == "L.W", "L.W", geo_site))%>%
  group_by(geo_site, season)%>%
  summarize(co2_mean = mean(co2_mmol.m2.d), 
            co2_sd = mean(co2_sd_units),  # mean replicate SD (measurement variability)
            n = n())

co2_data_sd<- co2_data_season%>%
  mutate(co2_max = co2_mean + co2_sd)%>%
  mutate(co2_min = co2_mean - co2_sd)

#Multiple seasonal average by the number of days in the season
# Summer: 1 Jun-31 Aug. Fall: 1 Sep to ice-on (18 Dec), minus 7 days of ice cover 21-27 Nov.
# Spring: ice-off (20 Mar) to 31 May. Ice-covered: 21-27 Nov + 18 Dec-19 Mar = 99 days.
season_days <- c(Summer = 92, Fall = 101, Spring = 73)
co2_season_sum<- co2_data_season%>%
  mutate(co2_max = co2_mean + co2_sd)%>%
  mutate(co2_min = co2_mean - co2_sd)%>%
  mutate(days = season_days[as.character(season)],
         annual_mmol_m2_year = co2_mean * days,
         annual_mmol_m2_year_max = co2_max * days,
         annual_mmol_m2_year_min = co2_min * days)

co2_season_flux <- co2_season_sum%>%
  left_join(polygon_sa_depth, by = "geo_site")%>%
  mutate(annual_gC_year = (annual_mmol_m2_year*area*12.01*0.001))%>%
  # mutate(annual_gC_m2_year = annual_gC_year/as.numeric(area))%>%
  mutate(annual_gC_year_max = (annual_mmol_m2_year_max*area*12.01*0.001))%>%
  mutate(annual_gC_year_min = (annual_mmol_m2_year_min*area*12.01*0.001))


#Total co2 (mmol/m2/yr) at each site
co2_annual<-co2_season_sum%>%
  group_by(geo_site)%>%
  summarize(co2_mmol_m2_year = sum(annual_mmol_m2_year),
            co2_mmol_m2_year_max = sum(annual_mmol_m2_year_max),
            co2_mmol_m2_year_min = sum(annual_mmol_m2_year_min))%>%
  left_join(polygon_sa_depth, by = "geo_site")


#mmol/m2/yr to gC/yr
co2_annual_open<- co2_annual%>%
  mutate(annual_mmol_year = (co2_mmol_m2_year * area))%>%
  mutate(annual_gC_year = (annual_mmol_year *12.01 * 0.001))%>%
  # mutate(annual_gC_m2_year = annual_gC_year/as.numeric(area))%>%
  mutate(annual_gC_year_max = ((co2_mmol_m2_year_max*area)*12.01*0.001))%>%
  mutate(annual_gC_year_min = ((co2_mmol_m2_year_min*area)*12.01*0.001))

#Ice-free wingra
wingra_co2_open <-co2_annual_open%>%
  summarize(wingra_co2_gC_m2_yr = sum(annual_gC_year)/surfacearea,
            wingra_co2_gC_m2_yr_max = sum(annual_gC_year_max)/surfacearea,
            wingra_co2_gC_m2_yr_min = sum(annual_gC_year_min)/surfacearea)%>%
  mutate(season = "total")

co2_season_total_flux<-co2_season_flux%>%
  group_by(season)%>%
  summarize(wingra_co2_gC_yr = sum(annual_gC_year)/surfacearea,
            wingra_co2_gC_yr_max = sum(annual_gC_year_max)/surfacearea,
            wingra_co2_gC_yr_min = sum(annual_gC_year_min)/surfacearea)

#Ice-free wingra pelagic only
co2_annual_open_pelagic<- co2_annual_open%>%
  filter(geo_site == "P1")%>%
  summarize(wingra_co2_gC_m2_yr = sum(annual_gC_year)/surfacearea,
            wingra_co2_gC_m2_yr_max = sum(annual_gC_year_max)/surfacearea,
            wingra_co2_gC_m2_yr_min = sum(annual_gC_year_min)/surfacearea)%>%
  mutate(season = "total2")

