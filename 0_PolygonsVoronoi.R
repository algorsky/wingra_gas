# Load packages
library(sf)
library(terra)
library(ggplot2)
library(cluster)
library(tidyverse)
library(concaveman)  # For concave hulls

# Load Lake Wingra shapefile
lake_outline = st_read('data/map/yld_study_lakes.shp') %>% 
  filter(LAKEID == "WI")

# Load sites 
sites = read_csv('data/map/crosswalk_biomass.csv') 
sites_sf = st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Read DNR data sheet 
dnr = read_csv('data/map/DNRsurvey.csv')
# Rake rating (0 to 3; 10 means it wasn't sampled)
post_22 <- read_csv('data/map/macrophyte_22_rake.csv') # DNR data with rake ratings adjusted
# Convert data to spatial
post_22_sf = st_as_sf(post_22, coords = c("longitude", "latitude"), crs = 4326)
macrophytes_sf <- st_as_sf(dnr, coords = c("longitude", "latitude"), crs = 4326)
dnr_depth <- read_csv('data/map/depth_dnr.csv')|>
  mutate(depth_m = depth_ft *  0.304)
depth_sf = st_as_sf(dnr_depth, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")|>
  mutate(depth_m = depth_ft *  0.304)%>%
  mutate(Depth_m_fill = if_else(is.na(depth_m), 0, depth_m))

# Join DNR data
macrophytes_sf = macrophytes_sf %>% st_join(post_22_sf) %>% st_join(depth_sf)%>%
  filter(rake <= 3) %>% 
  mutate(Depth_ft_fill = if_else(is.na(Depth_ft), 0, Depth_ft)) %>% # fill in missing depths with zero
  # The following are "weird" - QA/QC
  mutate(Depth_ft_fill = if_else(Site == 277, 10, Depth_ft_fill)) %>% 
  mutate(rake = if_else(Site == 301, 1, rake)) %>% 
  mutate(rake = if_else(Site == 62, 3, rake))

st_crs(macrophytes_sf)  # Check CRS of macrophyte data
st_crs(sites_sf)  # Check CRS of sampling sites
st_crs(lake_outline)  # Check CRS of lake outline

# Transform everything to match the lake outline
sites_sf <- st_transform(sites_sf, st_crs(lake_outline))
macrophytes_sf <- st_transform(macrophytes_sf, st_crs(lake_outline)) %>%
  mutate(Depth_m_fill = if_else(is.na(depth_m), 0, depth_m))
post_22_sf = st_transform(post_22_sf, st_crs(lake_outline)) 

# Plot data 
ggplot() +
  geom_sf(data = lake_outline, fill = "lightblue", color = "black") +
 # geom_sf(data = macrophytes_sf, alpha = 0.5, aes(color = as.factor(rake))) +
  geom_sf(data = macrophytes_sf, alpha = 0.5, aes(color = as.factor(Depth_ft_fill))) +
  # geom_sf(data = macrophytes_sf, alpha = 0.5, aes(color = TotalSpecies)) +
  # geom_sf_text(data = macrophytes_sf, alpha = 0.5, aes(label = Site)) +
  geom_sf(data = sites_sf) +
  theme_minimal()

# lake bounding box
lake_bbox <- st_bbox(lake_outline) %>%
  st_as_sfc() %>%
  st_buffer(500)  # Add some buffer to extend beyond the lake

##### K means clustering #####
coords <- st_coordinates(macrophytes_sf)  # Extracts x and y coordinates
rake_values <- macrophytes_sf$rake  # Extract the 'rake' column
depth_values = macrophytes_sf$Depth_ft_fill # Extract depth
species_values = macrophytes_sf$TotalSpecies # Extract total species

# Combine coordinates and data into one data frame
clustering_data <- data.frame(coords, rake = rake_values, 
                              depth = depth_values, species = species_values)
clustering_data_scaled <- scale(clustering_data) # scale data 
# Increase 'importance' of location so multiple scale lat/long by 2
clustering_data_scaled[,1:2] = clustering_data_scaled[,1:2]*2 
# kmeans with 8 clusters
kmeans_result <- stats::kmeans(clustering_data_scaled, centers = 8, nstart = 20)
# add cluster to dataframe 
macrophytes_sf$cluster <- kmeans_result$cluster

# plot clusters
ggplot(macrophytes_sf) +
  geom_sf(aes(color = as.factor(cluster)), size = 2) +
  geom_sf(data = sites_sf, shape = 'X') +
  theme_minimal() +
  ggtitle("Rake Value")

#### Convert clusters to polygons ####
concave_hull <- concaveman(macrophytes_sf) # Compute concave hull
buffered_hull <- st_buffer(concave_hull, dist = 20)  # Buffer distance in map units
plot(st_geometry(buffered_hull), col = "lightblue", border = "blue", lwd = 2)
plot(st_geometry(macrophytes_sf), col = "red", add = TRUE)

# Compute Voronoi polygons
# Create a bounding box larger than the lake to ensure complete Voronoi coverage
lake_bbox <- st_bbox(buffered_hull) %>%
  st_as_sfc()   # Add some buffer to extend beyond the lake
voronoi <- st_voronoi(st_union(macrophytes_sf %>% dplyr::select(cluster)), 
                      envelope = lake_bbox) %>%
  st_collection_extract("POLYGON")
# Convert to sf object
voronoi_sf <- st_sf(geometry = voronoi)
lake_polygons <- st_intersection(voronoi_sf, buffered_hull)

# Spatial join to assign points to hexagons
points_hex <- st_join(lake_polygons, macrophytes_sf, join = st_intersects)
cluster_polygons = points_hex %>% 
  group_by(cluster) %>% 
  summarize(geometry = st_union(geometry))
# Plot results
ggplot() +
  geom_sf(data = lake_outline, fill = "lightblue", color = "black") +
  geom_sf(data = cluster_polygons, aes(fill = as.factor(cluster))) +
  geom_sf(data = sites_sf, alpha = 0.5) +
  theme_minimal()

# Join clusters in bottom right. These change numbers every time so 
# need to look up cluster numbers by sites that are in them. 
joinclusters = points_hex %>% 
  filter(Site %in% c(465,443)) %>% 
  arrange(cluster) %>% 
  pull(cluster) 
# Select two features to union (replace 1 and 2 with the feature indices you want)
union_result <- st_union(cluster_polygons[joinclusters[1], ], cluster_polygons[joinclusters[2], ]) %>% 
  dplyr::select(cluster)
cluster_polygons[joinclusters[1], ] <- union_result  # Replace cluster with union results
cluster_polygons <- cluster_polygons[-joinclusters[2], ]  # Remove second cluster

ggplot() +
  geom_sf(data = lake_outline, fill = "lightblue", color = "black") +
  geom_sf(data = cluster_polygons, aes(fill = as.factor(cluster))) +
  geom_sf(data = sites_sf, alpha = 0.5) +
  theme_minimal()

lake_macrophytes <- st_intersection(lake_outline, macrophytes_sf)


#Average depth and surface area of each polygon
polygons<- cluster_polygons%>%
  mutate(site = c(5, 6, 1, 3, 8, 7, 4))

dnr_depth <- read_csv('data/map/depth_dnr.csv')|>
  mutate(depth_m = depth_ft *  0.304)

depth_sf = st_as_sf(dnr_depth, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")|>
  mutate(depth_m = depth_ft *  0.304)

# Ensure both datasets have the same CRS
depth_sf <- st_transform(depth_sf, st_crs(polygons))

# Spatial join: Assign each depth point to the corresponding polygon
depth_with_polygons <- st_join(depth_sf, polygons, left = FALSE)  # left = FALSE removes unmatched points

# Compute average depth for each polygon cluster
polygon_avg_depth <- depth_with_polygons %>%
  mutate(Depth_m_fill = if_else(is.na(depth_m), 0.5, depth_m)) %>% # fill in missing depths with zero
  group_by(site) %>%  # Adjust if the polygon ID is different
  # summarize(avg_depth_m = mean(depth_m, na.rm = TRUE))%>%
  summarize(avg_depth_m = mean(Depth_m_fill, na.rm = TRUE))

depth<- polygon_avg_depth$avg_depth_m

polygon_sa_depth <- polygons %>%
  mutate(surface_area_m2 = st_area(.))%>%
  dplyr::select(cluster, site, surface_area_m2)%>%
  mutate(area = as.numeric(surface_area_m2))%>%
  cbind(depth)%>%
  left_join(sites, by = "site")

#write_csv(polygon_sa_depth, "data/polygon_sa_depth.csv")

