#' Data 0003 : Zostère Mitis
#'
#' Données d'un projet de restauration expérimentale d’une zostéraie dans la Baie de Mitis
#'
#' @keywords zostère
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://catalogue.ogsl.ca/fr/dataset/93fd20e8-c80b-4304-9eb4-80ac13a2d365
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0003 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0003-zostere/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'analyse_eau.zip'))) {
    # URL
    zostere_mitis <- c('https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/analyse_eau.zip',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/zone_de_plantation.zip',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/projet-zostere-profil-plage.xlsx',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/description_champs_profil_plage.xlsx',
                       'https://zipsud.org/wp-content/uploads/2014/07/Caracterisation-Mitis-Zostere-28-05-14.pdf',
                       'https://zipsud.org/wp-content/uploads/2016/11/Rapport-de-restauration-Zosterere-ZIPSE-05-10-15.pdf',
                       'https://catalogue.ogsl.ca/data/zip-sud/93fd20e8-c80b-4304-9eb4-80ac13a2d365/cote_mitis2012.zip')

    # Download
    download.file(zostere_mitis[1], destfile = paste0(folder, 'analyse_eau.zip'))
    download.file(zostere_mitis[2], destfile = paste0(folder, 'zone_de_plantation.zip'))
    download.file(zostere_mitis[3], destfile = paste0(folder, 'projet-zostere-profil-plage.xlsx'))
    download.file(zostere_mitis[4], destfile = paste0(folder, 'description_champs_profil_plage.xlsx'))
    download.file(zostere_mitis[5], destfile = paste0(folder, 'Caracterisation-Mitis-Zostere-28-05-14.pdf'))
    download.file(zostere_mitis[6], destfile = paste0(folder, 'Rapport-de-restauration-Zosterere-ZIPSE-05-10-15.pdf'))
    download.file(zostere_mitis[7], destfile = paste0(folder, 'cote_mitis2012.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'analyse_eau.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'zone_de_plantation.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'cote_mitis2012.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  # List to store results
  data0003 <- list()

  # 2012
  data0003[[1]] <- st_read(paste0(folder, 'Plantation_12_juin.shp')) %>%
                   mutate(Annee = 2012)

  # 2013
  data0003[[2]] <- st_read(paste0(folder, 'Plantation_13_juin.shp')) %>%
                   mutate(Annee = 2013)

  # 2014
  data0003[[3]] <- st_read(paste0(folder, 'Plantation_14_juin.shp')) %>%
                   mutate(Annee = 2014)

  # Single object
  data0003 <- bind_rows(data0003)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ----------------------------------------
  # Nothing to format
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0003,
           dsn = "./data/data-format/data0003-zostere.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
