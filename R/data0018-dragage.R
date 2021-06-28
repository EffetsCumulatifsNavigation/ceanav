#' Data 0018 : Chenal de navigation et dragage
#'
#' Chenal de navigation entretenu par des activités de dragage au sein du Saint-Laurent et localisation des sites de dépôts utilisés
#'
#' @keywords dragage
#' @keywords chenal de navigation
#' @keywords stresseurs
#'
#' @source Garde côtière canadienne
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0018 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data

  # Output folder
  output <- "data0018-dragage/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import
  # ----------------------------------------
  data0018 <- st_read(paste0(folder, 'secteurs.shp'))

  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0018,
           dsn = "./data/data-format/data0018-dragage.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
