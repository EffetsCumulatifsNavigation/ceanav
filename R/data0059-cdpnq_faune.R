#' Data 0059 : Espèces fauniques à statut
#'
#' Présence d'espèces fauniques menacées ou vulnérables ou susceptibles d'être ainsi désignées ou rares
#'
#' @keywords faune
#' @keywords espèce à statut
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0059 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0059-cdpnq_faune/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # Unzip
  unzip(zipfile = paste0(folder, 'CDPNQ_Faune_FleuveFjord_dec21.zip'), exdir = folder)

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0059 <- st_read(paste0(folder, 'CDPNQ_Faune_FleuveFjord_dec21.shp'), quiet = TRUE) %>%
              mutate(LASTOBSyear = as.numeric(substr(LASTOBS, 1, 4))) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0059,
           dsn = "./data/data-format/data0059-cdpnq_faune.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
