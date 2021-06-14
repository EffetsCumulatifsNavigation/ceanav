#' Data 0007 : Milieux humides Saint-Laurent
#'
#' Atlas des milieux côtiers d’intérêt pour la conservation dans l’estuaire et le golfe du Saint-Laurent
#'
#' @keywords marais côtier
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://catalogue.ogsl.ca/fr/dataset/0a232214-05cc-438a-b914-6a8b53ac184e
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0007 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0007-marais_cotier/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))) {
    # URL
    milieu_cotier <- c('https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/atlas_estuairegolfe_rapport_final_fr.pdf',
                       'https://catalogue.ogsl.ca/data/pasl/0a232214-05cc-438a-b914-6a8b53ac184e/maraiscotiers_publ_janv2019.zip')

    # Download
    download.file(milieu_cotier[1], destfile = paste0(folder, 'atlas_estuairegolfe_rapport_final_fr.pdf'))
    download.file(milieu_cotier[2], destfile = paste0(folder, 'maraiscotiers_publ_janv2019.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'maraiscotiers_publ_janv2019.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0007 <- st_read(paste0(folder, 'MaraisCotiers_Publ_janv2019/MaraisCotiers_Publ_janv2019.shp'))
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0007,
           dsn = "./data/data-format/data0007-marais_cotier.geojson",
           delete_dsn = TRUE)

  # RData
  save(data0007, file = "./data/data0007.RData")
  # _________________________________________________________________________ #
}
