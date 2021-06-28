#' Data 0025 : Communauté Wolastoqiyik Wahsipekuk pêche homard
#'
#' Localisation des sites d'importance pour la pêche aux homards dans l'estuaire du Saint-Laurent pour l'année 2019
#'
#' @keywords homard Américain
#' @keywords Wolastoqiyik Wahsipekuk
#' @keywords sites d'importance
#' @keywords composante valorisée
#'
#' @source Première Nation Wolastoqiyik Wahsipekuk (2021) Localisation des sites d'importance pour la pêche aux homards dans l'estuaire du Saint-Laurent pour l'année 2019.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0025 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être supprimées au terme du projet")

  # Output folder
  output <- "data0025-wolastoqiyik_wahsipekuk/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # WARNING: Retirer information sur les débarquements et uniquement utiliser la
  #          délimitation des sites pêchés
  data0025 <- st_read(paste0(folder, "Debarquement_homard.shp")) %>%
              select(-Debar.lbs.)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0025,
           dsn = "./data/data-format/data0025-wolastoqiyik_wahsipekuk.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
