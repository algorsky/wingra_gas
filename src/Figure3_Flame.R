# ============================================================
# 2D GAM (Thin Plate Spline) - CO2 FLAMe - Lake Wingra
# ============================================================

library(sf)
library(sp)
library(raster)
library(terra)
library(mgcv)          # gam() + thin plate spline
library(tidyverse)
library(khroma)
library(cowplot)

# ============================================================
# 1. LAKE OUTLINE
# ============================================================

yahara  <- read_sf("data/gis/YaharaLakes/yld_study_lakes.shp")
CL      <- yahara %>% filter(LAKEID == "WI")
outline <- st_transform(CL, crs = 32616)

# ============================================================
# 2. PREDICTION GRID (from outline — no bathymetry gaps)
# ============================================================

outline_predict <- st_cast(outline, "POLYGON")
outline_vect    <- vect(outline_predict)

raster_res         <- 10
r_template         <- rast(outline_vect, resolution = raster_res, crs = "EPSG:32616")
values(r_template) <- 1
r_template         <- mask(r_template, outline_vect)

raster_layer      <- raster(r_template)
watergrid_predict <- rasterToPoints(raster_layer, spatial = TRUE)
watergrid_predict <- spTransform(
  watergrid_predict,
  CRSobj = CRS(st_crs(outline_predict)$proj4string)
)

# Grid as data frame with centred coordinates
grid_coords     <- as.data.frame(coordinates(watergrid_predict))
names(grid_coords) <- c("x", "y")

# ============================================================
# 3. FLAME DATA
# ============================================================

data <- readRDS("data/FLAMe/2022-06-29_LakeWingra_09_geoclean.rds")
# Remove first 400 rows
data <- spTransform(data, CRSobj = CRS(st_crs(outline)$proj4string)) [-(1:400), ]

data_sf = st_as_sf(data)
ggplot(data_sf) +
  geom_sf(aes(col = CO2Sat)) +
  scale_color_viridis_c()

# Very start has very high CO2 sat. Delete first 400 points 

# Convert to data frame + append coordinates
data_df        <- as.data.frame(data)
data_coords    <- as.data.frame(coordinates(data))
names(data_coords) <- c("x", "y")
data_df        <- cbind(data_df, data_coords)

# ============================================================
# 4. CO2 VARIABLES & LABELS
# ============================================================

co2_vars <- intersect(
  c("CO2Sat", "CO2Sat_tau", "CO2uM", "CO2uM_tau", "pH", "pH_tau"),
  names(data_df)
)

co2_labels <- list(
  CO2Sat     = expression(paste("CO"[2], " saturation-raw (%)")),
  CO2Sat_tau = expression(paste("CO"[2], " saturation-tau (%)")),
  CO2uM      = expression(paste("CO"[2], "(", mu, "M)")),
  CO2uM_tau  = expression(paste("CO"[2], "-tau(", mu, "M)")),
  pH = "pH",
  pH_tau = "pH"
)

# ============================================================
# 5. COLOR PALETTE
# ============================================================

color.palette.flame <- colorRampPalette(
  khroma::colour("sunset")(11),
  bias = 1
)

# ============================================================
# 6. GAM INTERPOLATION LOOP
# ============================================================
# k = 300 basis functions — generous for a lake this size.
# REML automatically penalises over-fitting; increasing k will
# not over-smooth, but will slow fitting slightly.

gam_rasters <- list()

for (var in co2_vars) {

  cat("GAM fitting:", var, "\n")

  df_var <- data_df[!is.na(data_df[[var]]), c("x", "y", var)]
  names(df_var)[3] <- "z"

  if (nrow(df_var) < 10) { cat("  Too few points — skipping.\n"); next }

  # Remove near-duplicate locations (within 0.1 m)
  dup_idx <- duplicated(round(df_var[, c("x","y")], 1))
  df_var  <- df_var[!dup_idx, ]

  # Centre coordinates for numerical stability
  x0 <- mean(df_var$x); y0 <- mean(df_var$y)
  df_var$xc   <- df_var$x - x0
  df_var$yc   <- df_var$y - y0
  grid_df      <- grid_coords
  grid_df$xc  <- grid_df$x - x0
  grid_df$yc  <- grid_df$y - y0

  # Fit 2D thin plate spline GAM
  fit <- tryCatch(
    mgcv::gam(
      z ~ s(xc, yc, bs = "tp", k = 300),
      data   = df_var,
      method = "REML"
    ),
    error = function(e) { cat("  GAM failed:", e$message, "\n"); NULL }
  )
  if (is.null(fit)) next

  cat("  Smoothing parameter (sp):", round(fit$sp, 4), "\n")
  cat("  EDF:", round(sum(fit$edf), 1), "\n")

  # Predict onto full grid
  preds <- predict(fit, newdata = grid_df, type = "response")

  # Back-fill into raster template and mask
  # Use values() round-trip — safer than [<- when preds has named attributes
  r_out  <- r_template
  vals   <- values(r_out)
  vals[!is.na(vals)] <- as.numeric(signif(preds, 4))
  values(r_out) <- vals
  r_out  <- mask(r_out, outline_vect)

  gam_rasters[[var]] <- r_out
    cat("  Done:", var, "\n")
}

# ============================================================
# 7. PLOT AND SAVE ALL FIGURES
# ============================================================
# 
# dir.create("figures/FLAMe_Maps_gam", showWarnings = FALSE)
# 
# for (var in co2_vars) {
# 
#   r_plot <- gam_rasters[[var]]
#   if (is.null(r_plot)) next
# 
#   label_i <- co2_labels[[var]]
# 
#   plot_df        <- as.data.frame(r_plot, xy = TRUE)
#   names(plot_df) <- c("x", "y", "value")
#   plot_df        <- plot_df[!is.na(plot_df$value), ]
# 
#   g <- ggplot(plot_df) +
#     theme_bw() +
#     theme(
#       panel.grid       = element_blank(),
#       panel.background = element_rect(fill = "gray85"),
#       axis.title       = element_blank()
#     ) +
#     geom_raster(aes(x = x, y = y, fill = value)) +
#     geom_sf(data = outline_predict, fill = NA, color = "black",
#             linewidth = 0.6, inherit.aes = FALSE) +
#     coord_sf(crs = 32616, expand = FALSE) +
#     labs(fill = label_i) +
#     scale_fill_gradientn(colors = color.palette.flame(11)) +
#     theme(
#       legend.position       = c(0.01, 0.99),
#       legend.justification  = c(0, 1),
#       legend.box.background = element_blank(),
#       legend.background     = element_blank(),
#       legend.text           = element_text(size = 10),
#       legend.title          = element_text(size = 10)
#     ) +
#     guides(fill = guide_colorbar(
#       direction      = "horizontal",
#       title.position = "top",
#       label.position = "bottom",
#       title.hjust    = 0.5,
#       barwidth       = unit(1.8, "in"),
#       barheight      = unit(0.2, "in"),
#       frame.colour   = "black"
#     ))
# 
#   print(g)
# 
#   ggsave(
#     filename = file.path("figures/FLAMe_Maps_gam", paste0(var, "_gam.png")),
#     plot     = g, dpi = 400, width = 6, height = 6, units = "in"
#   )
#   cat("Saved:", var, "\n")
# }

# ============================================================
# 7. PLOT AND SAVE MANUSCRIPT FIGURE
# ============================================================
site.numbers = read_csv('data/map/sites.csv') 
crosswalk <- read_csv("data/map/crosswalk_biomass.csv")
sites_biomass <- site.numbers %>%
  left_join(crosswalk |> dplyr::select(-latitude, -longitude, -biomass), by = c("Site" = "site"))
site.numbers_sf = st_as_sf(sites_biomass, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")


manuscript.plots = list()
for (var in c('pH', 'CO2uM')) {

  r_plot <- gam_rasters[[var]]
  label_i <- co2_labels[[var]]

  plot_df        <- as.data.frame(r_plot, xy = TRUE)
  names(plot_df) <- c("x", "y", "value")
  plot_df        <- plot_df[!is.na(plot_df$value), ]

  manuscript.plots[[var]] <- ggplot(plot_df) +
    theme_bw(base_size = 8) +
    theme(
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "gray85"),
      axis.title       = element_blank()
    ) +
    geom_raster(aes(x = x, y = y, fill = value)) +
    geom_sf(data = outline_predict, fill = NA, color = "black",
            linewidth = 0.6, inherit.aes = FALSE) +
    # geom_sf(data = data_sf, size = 0.01, shape = 20, alpha = 0.4, col = 'grey50') +
    geom_text(data = site.numbers_sf, aes(st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2], 
                                   label = rating), color = "black", size = 2) +
    coord_sf(crs = 32616, expand = FALSE) +
    labs(fill = label_i) +
    scale_fill_gradientn(colors = color.palette.flame(11)) +
    theme(
      legend.position       = 'bottom',
      legend.justification  = c(0, 1),
      legend.box.background = element_blank(),
      legend.background     = element_blank(),
      legend.text           = element_text(size = 8),
      legend.title          = element_text(size = 8)
    ) +
    guides(fill = guide_colorbar(
      direction      = "horizontal",
      label.position = "bottom",
      title.hjust    = 0.5,
      barwidth       = unit(1.6, "in"),
      barheight      = unit(0.1, "in")
    ))
  
}

plot_grid(
  manuscript.plots[[1]] +
    theme(plot.tag = element_text(size = 8)),
  manuscript.plots[[2]] +
    theme(plot.tag = element_text(size = 8)),
  labels = c("(a)", "(b)"),
  label_size = 8,
  label_fontface = "plain",
  ncol = 2
)
ggsave(filename = 'figures/Figure3_flame.png', 
  width = 6, height = 2.5, units = 'in', dpi = 500, bg = 'white')

