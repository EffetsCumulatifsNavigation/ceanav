#' Data 0028 : Observation en mer de mammifères marins
#'
#' Activité d'observation en mer de mammifères marins sur des embarcations avec permis de classe 1 en 2017
#'
#' @keywords observation en mer
#' @keywords navigation
#' @keywords stresseurs
#'
#' @source Turgeon, S., 2019. Portrait de la navigation dans le parc marin du Saguenay–Saint-Laurent - 2017. Parcs Canada, 59 pages + annexes
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0028 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0028-navigation_observation/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Unzip dataset
  unzip(zipfile = paste0(folder, "grille_classe1.zip"),
        exdir = paste0(folder, "grille_classe1"))

  data0028 <- st_read(paste0(folder, "grille_classe1/grille_500m_classe1.shp"))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0028,
           dsn = "./data/data-format/data0028-navigation_observation.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0028, file = "./data/data0028.RData")
  # _________________________________________________________________________ #
}
