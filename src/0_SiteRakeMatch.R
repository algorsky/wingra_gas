source('src/0_PolygonsVoronoi.R')

# For each of the 8 sample sites, list the DNR rake rating (0-3) of the 9 nearest macrophyte survey points
dist_matrix <- st_distance(sites_sf, lake_macrophytes)
sites_df <- st_drop_geometry(sites_sf)

site_rake_match <- purrr::map_dfr(seq_len(nrow(sites_sf)), function(i) {
  idx <- order(dist_matrix[i, ])[1:7]
  tibble::tibble(
    site = sites_df$site[i],
    rating_site = sites_df$rating_site[i],
    neighbor = seq_along(idx),
    rake = lake_macrophytes$rake[idx],
    distance_m = as.numeric(dist_matrix[i, idx])
  )
})

site_rake_mean <- sites_sf |>
  st_drop_geometry() |>
  dplyr::select(site, rating_site) |>
  dplyr::mutate(
    mean_rake_nearest = purrr::map_dbl(seq_len(nrow(sites_sf)), function(i) {
      idx <- order(dist_matrix[i, ])[1:9]
      mean(lake_macrophytes$rake[idx], na.rm = TRUE)
    })
  )


print(site_rake_mean, n = Inf)
