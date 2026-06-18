# ============================================================
# IDW Interpolation of FLAME Data - Lake Wingra
# Adapted from Italian Lakes workflow (Iseo/Maggiore) - Luke Loken
# ============================================================

# --- Libraries -----------------------------------------------
library(sf)
library(sp)
library(raster)
library(terra)
library(gstat)
library(tidyverse)
library(viridis)
library(lubridate)
library(stringr)

# ============================================================
# 1. LOAD AND PREPARE LAKE OUTLINE
# ============================================================

# Load the Yahara lakes shapefile and filter to Lake Wingra (LAKEID == "WI")
# NOTE: update path to wherever your yahara shapefile lives
yahara <- read_sf("data/FLAMe/yld_study_lakes.shp")

CL <- yahara %>%
  filter(LAKEID == "WI")

# Project to UTM Zone 16N (EPSG:32616) for metric units
outline <- st_transform(CL, crs = 32616)

ggplot() +
  geom_sf(data = outline)


# ============================================================
# 2. BUILD BATHYMETRY RASTER
# ============================================================

wingra_bathy <- st_read("data/FLAMe/wingra_bathy.shp")
st_crs(wingra_bathy) <- 3071  # assign known CRS if missing

# Reproject to UTM Zone 16N
bathymetry_proj <- st_transform(wingra_bathy, crs = 32616)

ggplot() +
  geom_sf(data = bathymetry_proj)

# Rasterize bathymetry contours at 10 m resolution
bathymetry_proj$ID <- as.numeric(bathymetry_proj$ID)
bathymetry_vect  <- vect(bathymetry_proj)

raster_res  <- 10
r           <- rast(bathymetry_vect, resolution = raster_res)
depth_raster <- rasterize(bathymetry_vect, r, field = "ID", fun = mean)

print(depth_raster)
plot(depth_raster, col = rev(terrain.colors(100)))

# writeRaster(depth_raster, "wingra_bathy_rast.tif", overwrite = TRUE)
# depth_raster <- rast("wingra_bathy_rast.tif")
# ============================================================
# 3. PREPARE PREDICTION GRID
# ============================================================

raster_predict  <- depth_raster
outline_predict <- st_cast(outline, "POLYGON")

# Mask raster to lake outline
water_raster_cropped <- crop(raster_predict, outline_predict)
water_raster_masked  <- mask(water_raster_cropped, outline_predict)

plot(water_raster_masked, col = terrain.colors(100))

# Convert masked raster to spatial points for IDW prediction target
raster_layer     <- raster(water_raster_masked)          # terra -> raster for gstat
watergrid_predict <- rasterToPoints(raster_layer, spatial = TRUE)

# Ensure CRS matches outline
watergrid_predict <- spTransform(
  watergrid_predict,
  CRSobj = CRS(st_crs(outline_predict)$proj4string)
)


# ============================================================
# 4. LOAD FLAME DATA
# ============================================================

data <- readRDS("data/FLAMe/ProcessedData/2022-06-29_LakeWingra_09_geoclean.rds")

# Reproject FLAME data to match lake outline
data <- spTransform(data, CRSobj = CRS(st_crs(outline)$proj4string))


# ============================================================
# 5. DEFINE VARIABLES AND LABELS
# ============================================================

variables <- c(
  "ODO_percent",    "ODO_percent_tau",
  "ODO_mgL",        "ODO_mgL_tau",
  "CH4Sat",         "CH4Sat_tau",
  "CH4uM",          "CH4uM_tau",
  "CO2Sat",         "CO2Sat_tau",
  "CO2uM",          "CO2uM_tau",
  "pH"
)

# Keep only variables that actually exist in the dataset
variables <- intersect(variables, names(data))

variable_labels <- list(
  expression(paste("Dissolved oxygen-raw (%)")),
  expression(paste("Dissolved oxygen-tau (%)")),
  expression(paste("Dissolved oxygen-raw (mg L"^"-1", ")")),
  expression(paste("Dissolved oxygen-tau (mg L"^"-1", ")")),
  expression(paste("Methane-raw (%)")),
  expression(paste("Methane-tau (%)")),
  expression(paste("Methane-raw (", mu, "M)")),
  expression(paste("Methane-tau (", mu, "M)")),
  expression(paste("Carbon dioxide-raw (%)")),
  expression(paste("Carbon dioxide-tau (%)")),
  expression(paste("Carbon dioxide-raw (", mu, "M)")),
  expression(paste("Carbon dioxide-tau (", mu, "M)")),
  expression(paste("pH"))
)


# ============================================================
# 6. COLOR RAMP
# ============================================================

color.palette.flame <- colorRampPalette(
  c(viridis(6, begin = .1, end = .98),
    rev(magma(5, begin = .5, end = .98))),
  bias = 1
)


# ============================================================
# 7. IDW INTERPOLATION LOOP
# ============================================================

# Initialize output grid (copy structure from prediction grid)
watergrid_predict_data <- watergrid_predict

for (var_number in seq_along(variables)) {
  
  var <- variables[var_number]
  cat("Interpolating:", var, "\n")
  
  # Drop NAs for this variable
  col_idx  <- which(names(data) == var)
  data_idw <- data[!is.na(data@data[[var]]), ]
  
  if (nrow(data_idw) == 0) {
    cat("  No data for", var, "- skipping.\n")
    next
  }
  
  # Build IDW formula dynamically
  formula_idw <- as.formula(paste(var, "~ 1"))
  
  # Run IDW (idp = 2 is standard inverse-square weighting)
  predict_idw <- gstat::idw(
    formula  = formula_idw,
    locations = data_idw,
    newdata  = watergrid_predict,
    idp      = 2
  )
  
  # Store predictions in output grid
  watergrid_predict_data@data[[var]] <- signif(predict_idw$var1.pred, 4)
  
  cat("  Done:", var, "\n")
}


# ============================================================
# 8. CONVERT TO SF AND PLOT
# ============================================================

watergrid_sf        <- st_as_sf(watergrid_predict_data)
st_crs(watergrid_sf) <- 32616

for (var_number in seq_along(variables)) {
  
  var     <- variables[var_number]
  label_i <- variable_labels[[var_number]]
  
  df_i <- watergrid_sf %>% filter(!is.na(.data[[var]]))
  if (nrow(df_i) == 0) next
  
  g1 <- ggplot(df_i) +
    theme_bw() +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "gray")
    ) +
    geom_sf(aes(color = .data[[var]]), shape = 15, size = 2) +
    geom_sf(data = outline_predict, fill = NA, color = "black") +
    labs(color = label_i) +
    scale_color_gradientn(colors = color.palette.flame(11)) +
    theme(
      legend.position        = c(0.01, 0.99),
      legend.justification   = c(0, 1),
      legend.box.background  = element_blank(),
      legend.background      = element_blank(),
      legend.text            = element_text(size = 10),
      legend.title           = element_text(size = 10)
    ) +
    guides(color = guide_colorbar(
      direction       = "horizontal",
      title.position  = "top",
      label.position  = "bottom",
      title.hjust     = 0.5,
      barwidth        = unit(1.8, "in"),
      barheight       = unit(0.2, "in"),
      frame.colour    = "black"
    )) +
    scale_x_continuous(expand = c(0.01, 0.01)) +
    scale_y_continuous(expand = c(0.01, 0.01))
  
  print(g1)
  
  ggsave(
    filename = file.path("figures/FLAMe_Maps_idw", paste0(var, "_gg.png")),
    plot     = g1,
    dpi      = 400,
    width    = 6,
    height   = 6,
    units    = "in"
  )
}
