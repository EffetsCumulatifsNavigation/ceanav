#' Data 0002 : Zostère Pointe au père
#'
#' Données d'un projet de restauration de l'herbier de zostère marine dans l'anse de Point-au-Père
#'
#' @keywords zostère
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://catalogue.ogsl.ca/fr/dataset/3d0c057b-ab39-4a7b-8809-4367d5028c11
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0002 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0002-zostere/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'herbier-zostere_continu_2017-18.zip'))) {
    # URL
    zostere_pap <- c('https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/herbier-zostere_continu_2017-18.zip',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/zones_plantation_zostere_2017-18.zip',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/parcelles_suivi_2017-18.zip',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/depliant_zip_zostere.pdf',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/rapport_suivis_pointe-au-pere_2017-18.pdf',
                     'https://catalogue.ogsl.ca/data/zip-sud/3d0c057b-ab39-4a7b-8809-4367d5028c11/descriptionchamps_pointe-au-pere.xlsx')

    # Download
    download.file(zostere_pap[1], destfile = paste0(folder, 'herbier-zostere_continu_2017-18.zip'))
    download.file(zostere_pap[2], destfile = paste0(folder, 'zones_plantation_zostere_2017-2018.zip'))
    download.file(zostere_pap[3], destfile = paste0(folder, 'parcelles_suivi_2017-18.zip'))
    download.file(zostere_pap[4], destfile = paste0(folder, 'depliant_zip_zostere.pdf'))
    download.file(zostere_pap[5], destfile = paste0(folder, 'rapport_suivis_pointe-au-pere_2017-18.pdf'))
    download.file(zostere_pap[6], destfile = paste0(folder, 'descriptionchamps_pointe-au-pere.xlsx'))

    # Unzip
    unzip(zipfile = paste0(folder, 'herbier-zostere_continu_2017-18.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'zones_plantation_zostere_2017-2018.zip'), exdir = folder)
    unzip(zipfile = paste0(folder, 'parcelles_suivi_2017-18.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  zostere_pap <- st_read(paste0(folder, 'herbier-zostere_continu.shp')) %>%
                 mutate(Type = "Releve")

  zostere_pap_plantation <- st_read(paste0(folder, 'zones_plantation_zostere.shp')) %>%
                            mutate(Type = "Plantation")


  data0002 <- bind_rows(zostere_pap, zostere_pap_plantation)
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
  st_write(obj = data0002,
           dsn = "./data/data-format/data0002-zostere.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
