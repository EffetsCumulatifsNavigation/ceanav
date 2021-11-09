#' Data 0060 : Espèces floristiques à statut
#'
#' Présence d'espèces floristiques menacées ou vulnérables ou susceptibles d'être ainsi désignées ou rares
#'
#' @keywords flore
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

get_data0060 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0060-cdpnq_flore/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # Unzip
  unzip(zipfile = paste0(folder, 'CDPNQ_Flore_fleuve400m.zip'), exdir = folder)

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0060 <- st_read(paste0(folder, 'CDPNQ_Flore_fleuve400m.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0060,
           dsn = "./data/data-format/data0060-cdpnq_flore.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
