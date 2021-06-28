#' Data 0026 : Communauté Wolastoqiyik Wahsipekuk pêche oursin vert
#'
#' Localisation des sites d'importance pour la pêche à l'oursin vert dans l'estuaire du Saint-Laurent.
#'
#' @keywords oursin vert
#' @keywords Wolastoqiyik Wahsipekuk
#' @keywords sites d'importance
#' @keywords composante valorisée
#'
#' @source Première Nation Wolastoqiyik Wahsipekuk (2021) Localisation des sites d'importance pour la pêche à l'oursin vert dans l'estuaire du Saint-Laurent
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0026 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données et doivent être supprimées au terme du projet")

  # Output folder
  output <- "data0026-wolastoqiyik_wahsipekuk/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0026 <- list()
  data0026[[1]] <- st_read(paste0(folder, "Oursins_zones_exploitation_potentielleA.shp"))
  data0026[[2]] <- st_read(paste0(folder, "Oursins_zones_exploitation_potentielleB.shp"))
  data0026[[3]] <- st_read(paste0(folder, "Oursins_zones_exploitation_potentielleC.shp"))

  # Add missing id in dataset
  data0026[[1]]$id <- "A"
  data0026[[2]]$id <- "B"
  data0026[[3]]$id <- "C"

  # Single dataset
  data0026 <- bind_rows(data0026)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0026,
           dsn = "./data/data-format/data0026-wolastoqiyik_wahsipekuk.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
