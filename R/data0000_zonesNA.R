#' Zones for NA values (this could definitely be improved...)
#'
#' @export

get_zonesNA <- function() {
  #   x <- mapedit::editMap()
  #   x <- mapview(x) %>% mapedit::editMap()
  #   st_coordinates(x[[1]])

  # Global parameters
  global_parameters()
  
  # Study area
  data(aoi)
  data(grid1p)
  
  # Simplify aoi 
  aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))

  # Zones NA 
  zonesNA <- list()

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Saguenay
  coords <- rbind(c(-69.72744, 48.12389),
                  c(-69.71440, 48.14965),
                  c(-69.81974, 48.17628),
                  c(-69.91734, 48.29655),
                  c(-70.15595, 48.30741),
                  c(-70.50999, 48.45983),
                  c(-71.07434, 48.50930),
                  c(-71.09313, 48.31592),
                  c(-69.93126, 48.11431),
                  c(-69.72744, 48.12389))

  zonesNA[["saguenay"]] <- list(coords) %>%
                          st_polygon() %>%
                          st_sfc(crs = 4326) %>%
                          st_transform(crs = global_param$crs) %>%
                          st_intersection(aoi, .) %>%
                          mutate(name = "saguenay")


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fluvial pêche 
  coords <- rbind(c(-69.49415, 46.99082),
                  c(-70.74165, 47.76227),
                  c(-74.30856, 45.63569),
                  c(-73.51700, 45.05656),
                  c(-69.49415, 46.99082))

  zonesNA[["fluvial_peche"]] <- list(coords) %>%
                          st_polygon() %>%
                          st_sfc(crs = 4326) %>%
                          st_transform(crs = global_param$crs) %>%
                          st_intersection(aoi, .) %>%
                          mutate(name = "fluvial_peche")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Intégrité des berges (Bernier et al. limits)
  coords <- rbind(c(-70.74674, 47.08735),
                  c(-70.77840, 46.90255),
                  c(-70.39636, 46.80487),
                  c(-65.88445, 49.13390),
                  c(-68.12657, 50.36001),
                  c(-71.31287, 48.37184),
                  c(-70.74674, 47.08735))
                  
  zonesNA[["berge"]] <- list(coords) %>%
                          st_polygon() %>%
                          st_sfc(crs = 4326) %>%
                          st_transform(crs = global_param$crs) %>%
                          st_intersection(aoi, .) %>%
                          mutate(name = "berge_fluvial")


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sites alevinage (MFFP 2020)
  coords <- rbind(c(-70.81542, 47.08104),
                  c(-70.65560, 46.90436),
                  c(-66.07052, 49.04776),
                  c(-67.70377, 49.51784),
                  c(-71.33224, 48.44207),
                  c(-70.81542, 47.08104))

  zonesNA[["alevinage"]] <- list(coords) %>%
                          st_polygon() %>%
                          st_sfc(crs = 4326) %>%
                          st_transform(crs = global_param$crs) %>%
                          st_intersection(aoi, .) %>%
                          mutate(name = "alevinage")
 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Frayères (MFFP 2020)
  coords <- rbind(c(-69.63638, 48.27378),
                  c(-69.20995, 47.88120),
                  c(-65.89774, 49.02596),
                  c(-68.33866, 50.16443),
                  c(-69.63638, 48.27378))

  zonesNA[["frayere"]] <- list(coords) %>%
                            st_polygon() %>%
                            st_sfc(crs = 4326) %>%
                            st_transform(crs = global_param$crs) %>%
                            st_intersection(aoi, .) %>%
                            mutate(name = "frayere")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  zonesNA <- bind_rows(zonesNA) 
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = zonesNA,
           dsn = "./data/data-basemap/zonesNA.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #



  # _____________________________________________________________________________ #
}
