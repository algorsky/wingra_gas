library(sf)       # For handling spatial data
library(terra)    # For raster data
# library(tmap)     # For map visualization
library(ggspatial)
library(tidyverse)

# Must run 0_GISprocess first

# Load watershed
wingraWS = st_read('data/gis/Wingra/Wingra_Basin.shp')
lakewingra = st_read('data/gis/YaharaLakes/YaharaLakes_DaneCty.shp') %>% 
  filter(NAME == 'Lake Wingra')
bathy = st_read('data/gis/Wingra/wingra-contours-all.shp')
bathy <- st_set_crs(bathy, 3071)

# Ran this and saved output 
# library(FedData)
# # # Load NCLD data using FedData (this is just for legend colors)
# nlcd <- get_nlcd(template = wingraWS,
#                  label = "Wingra", year = 2019, dataset = "landcover")
# # Plot with terra::plot
# terra::plot(nlcd)
# # Save the raster to a GeoTIFF
# writeRaster(nlcd, "data/gis/nlcd_export.tif", overwrite = TRUE)
# 
# nlcd.2023 = terra::rast('data/gis/NCLD/Annual_NLCD_LndCov_2023_CU_C1V0_5yBUfLryZkDCeOYocVzN.tiff')
# terra::plot(nlcd.2023)
nlcd <- rast("data/gis/nlcd_export.tif")

# Transform CRS
wingraWS = st_transform(wingraWS, st_crs(nlcd.2023))
lakewingra = st_transform(lakewingra, st_crs(nlcd.2023))
bathy = st_transform(bathy, st_crs(nlcd.2023))

# Crop raster and mask to watershed 
cropped_landuse <- crop(nlcd.2023, wingraWS)
masked_landuse <- mask(cropped_landuse, wingraWS)

# Convert SpatRaster to Dataframe
landuse_df <- as.data.frame(masked_landuse, xy = TRUE)  # Include x, y coordinates
colnames(landuse_df) <- c("x", "y", "landuse")  # Rename columns for clarity

landuse_background <- as.data.frame(cropped_landuse, xy = TRUE)  # Include x, y coordinates
colnames(landuse_background) <- c("x", "y", "landuse")  # Rename columns for clarity

# extract colors
landuse_classes <- cats(nlcd)[[1]]  # Get category names
landuse_colors <- terra::coltab(nlcd)  # Get associated colors

# Merge some classes in the palette
landuse_classes = landuse_classes %>% 
  mutate(Class_merge = recode(Class, 
                              "Pasture/Hay" = "Pasture/Crops", 
                              "Cultivated Crops" = "Pasture/Crops",
                              "Shrub/Scrub" = "Shrub/Grassland", 
                              "Grassland/Herbaceous" = "Shrub/Grassland",
                              "Woody Wetlands" = "Wetlands", 
                              "Barren Land (Rock/Sand/Clay)" = "Barren Land",
                              "Emergent Herbaceous Wetlands" = "Wetlands",
                              "Evergreen Forest" = "Forest", 
                              "Deciduous Forest" = "Forest",
                              "Mixed Forest" = "Forest"))
# Convert to a named vector for ggplot2
palette_OG <- setNames(landuse_classes$Color, landuse_classes$Class) 
palette_merge <- setNames(landuse_classes$Color, landuse_classes$Class_merge) 
palette_merge["Barren Land"] = '#524a30'
palette_merge["Shrub/Grassland"] = '#a6c7a5'

# Merge landuse
landuse_merge = landuse_df %>% 
  left_join(landuse_classes %>% select(-Description), by = c('landuse' = 'ID')) %>% 
  mutate(landuse = Class_merge)
  # mutate(landuse = recode(landuse, 
  #        "Pasture/Hay" = "Pasture/Crops", 
  #        "Cultivated Crops" = "Pasture/Crops",
  #        "Shrub/Scrub" = "Shrub/Grassland", 
  #        "Grassland/Herbaceous" = "Shrub/Grassland",
  #        "Barren Land (Rock/Sand/Clay)" = "Barren Land",
  #        "Woody Wetlands" = "Wetlands", 
  #        "Emergent Herbaceous Wetlands" = "Wetlands",
  #        "Evergreen Forest" = "Forest", 
  #        "Deciduous Forest" = "Forest",
  #        "Mixed Forest" = "Forest")) %>% 
  # group_by(landuse) 

landuse_background = landuse_background %>% 
  left_join(landuse_classes %>% select(-Description), by = c('landuse' = 'ID')) %>% 
  mutate(landuse = Class_merge)

# Met station location
metstation = st_as_sf(data.frame(lon = -89.482, lat = 43.060), coords = c("lon", "lat"), crs = 4326)


ggplot() +
  geom_raster(data = landuse_background, 
              aes(x = x, y = y, fill = factor(landuse)), alpha = 0.1) +
  geom_raster(data = landuse_merge, 
              aes(x = x, y = y, fill = factor(landuse))) +
  geom_sf(data = wingraWS, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = lakewingra, fill = '#5475A1', linewidth = 0.5, color = 'black') +
  
  # geom_sf(data = bathy, fill = NA, col = 'white') +
  
  annotate('text',x = 532700, y = 2247150, label = "Lake Wingra", 
           size = 2.5, angle = 22, color = 'white', fontface = 3) +
  scale_fill_manual(values = palette_merge, name = "Land Use") +
  coord_sf(expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25, line_width = 0.5) +  # Adds scale bar
  theme_bw(base_size = 10) +
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        legend.key.height = unit(0.4,'cm'),
        # legend.position = 'bottom',
        axis.title = element_blank())

ggsave('figures/Figure_map.png', width = 6, height = 2.5, dpi = 500)


# Land use stats 
landuse_merge %>% ungroup() %>% 
  mutate(group = str_detect(landuse, "Developed")) %>% 
  mutate(tot = n()) %>% 
  group_by(group) %>% 
  summarise(n = n()/first(tot))

# 74.1% Developed land use vs other
# 54.7% "low-high intensity developed" 
landuse_merge %>% ungroup() %>% 
  mutate(group = str_detect(landuse, "Open Space")) %>% 
  mutate(tot = n()) %>% 
  group_by(group) %>% 
  summarise(n = n()/first(tot))
