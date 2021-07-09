#' Navigation
#'
#' Couche de données transformées pour la navigation dans le Saint-Laurent
#'
#' @keywords navigation
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_navigation <- function() {
  # Load vessel type dataset and shipping tracks
  ceanav_load_data("data0020")
  ceanav_load_data("data0021")

  # ---------
  vessel_type <- unique(data0020$NTYPE)

  # Add vessel type to ais data
  ais <- data0021 %>%
         left_join(data0020, 'MMSI')

  # Divide vessel data by type
  vessels <- list()
  for(i in 1:length(vessel_type)) {
    uid <- ais$NTYPE == vessel_type[i]
    vessels[[i]] <- ais[uid, ]
  }
  names(vessels) <- vessel_type
  # _____________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Evaluate intensity
  # -------------------------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Study grid
  data(grid1p)

  system.time({
  grd <- list()
  # Intensity for each vessel type (~15 minutes to run on my laptop)
  for(i in 1:length(vessels)) {
    grd[[i]] <- st_intersects(grid1p, vessels[[i]]) %>%
           lapply(length) %>%
           unlist() %>%
           ifelse(. == 0, NA, .) %>%
           mutate(grid1p, intensity = .)
  }
  })

  # In matrix
  df <- matrix(NA, nrow = nrow(grid1p), ncol = length(vessel_type), dimnames = list(c(), vessel_type))
  for(i in 1:length(grd)) df[,i] <- grd[[i]]$intensity

  # As single sf object
  navigation <- cbind(grid1p, df)


  # # Vessel type viz
  # vesselID <- gsub(' ', '.', vessel_type) %>% gsub('/','.',.) %>% gsub('-','.',.)
  # mapview::mapview(nav, border = 'transparent', zcol = vesselID)

  # mapview::mapview(ais[[1]], color = '#50d7c3', alpha = 0.1)


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export
  # ------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  st_write(obj = navigation,
           dsn = "./data/data-integrated/st_navigation.geojson",
           delete_dsn = TRUE)
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}
