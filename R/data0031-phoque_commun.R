#' Data 0031 : Phoque commun fjord du Saguenay
#'
#' Données du suivi du phoque commun dans le fjord du Saguenay entre 2007 et 2020 exprimées en pourcentage d'individus observés selon la grille d'étude du projet d'évaluation des effets cumulatifs des activités maritime dans le Saint-Laurent et le Saguenay
#'
#' @keywords mesure de conservation
#'
#' @source Agence Parcs Canada (2021) Données du suivi du phoque commun dans le fjord du Saguenay entre 2007 et 2020 exprimées en pourcentage d'individus observés selon la grille d'étude du projet d'évaluation des effets cumulatifs des activités maritime dans le Saint-Laurent et le Saguenay
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0031 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Les données brutes sont soumis à une entente de partage de données")

  # Output folder
  output <- "data0031-phoque_commun/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  # Unzip dataset
  unzip(zipfile = paste0(folder, "phoques_Saguenay.zip"), exdir = folder)
  unzip(zipfile = paste0(folder, "zone_etude_phoque.zip"), exdir = folder)

  # Import data
  data0031 <- st_read(paste0(folder, "zone_etude_phoque.shp"), quiet = TRUE)

  # Remove NAs
  uid <- is.na(data0031$perc)
  data0031 <- data0031[!uid, ]
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0031,
           dsn = "./data/data-format/data0031-phoque_commun.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
