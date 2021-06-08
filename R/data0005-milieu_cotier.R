#' Data 0005 : Milieux côtiers Saint-Laurent
#'
#' Atlas des milieux côtiers d’intérêt pour la conservation dans l’estuaire et # le golfe du Saint-Laurent
#'
#' @keywords milieu humide
#' @keywords marais côtier
#' @keywords côte sableuse
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://catalogue.ogsl.ca/fr/dataset/0a232214-05cc-438a-b914-6a8b53ac184e
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0005 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0005-milieu_cotier/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))) {
    # URL
    milieu_humide_stl <- c('https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/atlas_estuairegolfe_rapport_final_fr.pdf',
                           'https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/milieuxhum_publ_janv2019.zip')

    # Download
    download.file(milieu_humide_stl[1], destfile = paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))
    download.file(milieu_humide_stl[2], destfile = paste0(folder, 'milieuxhum_publ_janv2019.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'milieuxhum_publ_janv2019.zip'), exdir = folder)
  }  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  # Milieux humides lac Saint-Pierre
  data0005 <- st_read(paste0(folder, 'MilieuxHum_Publ_janv2019/MilieuxHum_Publ_janv2019.shp'))

  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0005,
           dsn = "./data/data-format/data0005-milieu_humide.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0005, file = "./data/data0005.RData")
  # _________________________________________________________________________ #
}
