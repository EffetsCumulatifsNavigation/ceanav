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
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Divide by boat type
  # -------------------------
  #
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load vessel type dataset
  df <- read.csv(paste0(folder, '/STATIC_DATA_2015_to_2019_forULAVAL.csv'))

  # Unique vessel types
  vessel_type <- unique(df$NTYPE)

  # Add vessel type to ais data
  ais <- ais %>%
         left_join(df, 'MMSI')

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
  save(navigation, file = './data/str_navigation.RData')
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
}
