library(tidyverse)
library(sf)

#Load in data
#wingra shapefile
wingra = st_read('data/map/yld_study_lakes.shp')|>
  filter(LAKEID == "WI")
#Use the dnr depth from macrophyte survey to transform tprs data
dnr_depth <- read_csv('data/map/depth_dnr.csv')|>
  mutate(depth_m = depth_ft *  0.304)
depth_sf = st_as_sf(dnr_depth, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")|>
  mutate(depth_m = depth_ft *  0.304)%>%
  mutate(Depth_m_fill = if_else(is.na(depth_m), 100, depth_m))
tprs_csv<- read_csv("data/map/bathymetry.csv")
tprs_sf <- st_as_sf(tprs_csv, coords = c("x", "y"), crs = st_crs(depth_sf)$proj4string)
tprs_sf_longlat <- st_transform(tprs_sf, crs = 4326)
# Extract lat/lon coordinates
tprs_df_longlat <- st_coordinates(tprs_sf_longlat)

# Add lat and lon to the original data frame
tprs_sf$lat <- tprs_df_longlat[, 2]
tprs_sf$lon <- tprs_df_longlat[, 1]

tprs_sf <- st_set_crs(tprs_sf, st_crs(wingra))

## Sites
sites = read_csv('data/map/sites.csv') 
crosswalk<- read_csv("data/map/crosswalk_biomass.csv")
sites_biomass<- sites%>%
  left_join(crosswalk, by = c("Site" = "site"))
sites_sf = st_as_sf(sites_biomass, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

#Rake Years
post_22<- read_csv('data/map/macrophyte_22_rake.csv')
#post_22_sampled<- post_22%>%
  #filter(rake2 < 10)
#Sf
post_22.sf<- st_as_sf(post_22, coords = c("longitude", "latitude"), crs = 4326)

points_sf <- st_transform(post_22.sf, st_crs(wingra))
points_within_lake <- st_intersection(wingra, points_sf)


