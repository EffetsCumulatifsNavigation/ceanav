#' Data 0013 : Zones inondables MRC
#'
#' Grille de présence de zone inondable identifiée par les MRC
#'
#' @keywords zone inondable
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://www.donneesquebec.ca/recherche/fr/dataset/0d9de0d6-9873-4a8c-adc7-0e94d51b3fa0
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0013 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  # > Cette grille est une représentation spatiale des secteurs où une
  #   cartographie a été produite par les municipalités régionales de comté
  #   (MRC) ou les villes à compétence de MRC. Elle indique qu’une cartographie
  #   des zones inondables a été intégrée dans le schéma d’aménagement et de
  #   développement (SAD) ou dans un règlement de contrôle intérimaire (RCI)
  #   en vigueur dans la MRC.
  #
  # > Cette information est fournie à titre indicatif et n’a aucune valeur
  #   légale. L'utilisateur est invité à communiquer avec les municipalités ou
  #   les MRC afin de connaitre les limites exactes des zones inondables
  #   cartographiées et la réglementation en vigueur applicable sur leur
  #   territoire.

  # Output folder
  output <- "data0013-zone_inondable/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'grillepresencezoneinondable.geojson'))) {
    # URL
    zone_inondable_mrc <- c('https://donneesouvertes.affmunqc.net/zones_inondables/grillepresencezoneinondable.json')

    # Download
    download.file(zone_inondable_mrc[1], destfile = paste0(folder, 'grillepresencezoneinondable.geojson'))
  }

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0013 <- st_read(paste0(folder, 'grillepresencezoneinondable.geojson'))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0013,
           dsn = "./data/data-format/data0013-zone_inondable.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0013, file = "./data/data0013.RData")
  # _________________________________________________________________________ #
}
