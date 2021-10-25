#' Data 0055 : Nation Huronne-Wendat - Sites d'intérêt
#'
#' Localisation des sites Sites d’intérêt culturels, patrimoniaux et archéologiques pour la Nation Huronne-Wendat
#'
#' @keywords Wendake
#' @keywords Nation Huronne-Wendat
#' @keywords site d'importance
#' @keywords composante valorisée
#'
#' @source  Nation Huronne-Wendat (2021) Localisation des sites Sites d’intérêt culturels, patrimoniaux et archéologiques pour la Nation Huronne-Wendat
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0055 <- function() {

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être détruites au terme du projet. L'équipe de recherche s'engage à envoyer une attestation de destruction de données. ")


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0055-wendake/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Import data
  data0055 <- st_read(paste0(folder, "20211020KVD.PPO.SAISIE.shp"), quiet = TRUE)

  # Transform projection
  data0055 <- st_transform(data0055, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0055,
           dsn = "./data/data-format/data0055-wendake.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
