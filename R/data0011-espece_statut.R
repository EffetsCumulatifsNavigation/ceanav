#' Data 0011 : Espèces à statut
#'
#' Zones d'importance pour les espèces à statut dans le Saint-Laurent fluvial
#'
#' @keywords espèce à statut
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source Marc Mingelbier, Ministère des Forêts, de la Faune et des Parcs
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0011 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0011-espece_statut/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # Unzip
  unzip(zipfile = paste0(folder, 'DonneesMFFP_PourPASL.gdb.zip'), exdir = folder)

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0011 <- st_read(paste0(folder, 'DonneesMFFP_PourPASL.gdb'),
                      layer = 'CDPNQ_CEGRIM_29_04_2020_3')

  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0011,
           dsn = "./data/data-format/data0011-espece_statut.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0011, file = "./data/data0011.RData")
  # _________________________________________________________________________ #
}
