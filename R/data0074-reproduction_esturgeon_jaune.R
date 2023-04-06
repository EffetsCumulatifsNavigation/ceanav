#' Data 0074 : Aires de reproduction, d'alimentation et de concentration de l'esturgeon jaune dans le fleuve et l'estuaire du Saint-Laurent
#'
#' Couche regroupant l’information connue des aires de reproduction, d'alimentation et de concentration de l'esturgeon jaune dans le fleuve et l'estuaire du Saint-Laurent selon une revue de littérature de documents réalisés entre 1976 et 2002.
#'
#' @keywords frayères
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/0db67020-7521-4356-bd53-3132cb7804a3
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0074 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0074-reproduction_esturgeon_jaune/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'LakeSturgeon_EsturgeonJaune_ImportantSites.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/LakeSturgeon_EsturgeonJaune_ImportantSites/DataDictionary_DictionnaireDonnees.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/LakeSturgeon_EsturgeonJaune_ImportantSites/LakeSturgeon_EsturgeonJaune_ImportantSites.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(dat[2], destfile = paste0(folder, 'LakeSturgeon_EsturgeonJaune_ImportantSites.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'LakeSturgeon_EsturgeonJaune_ImportantSites.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0074 <- st_read(paste0(folder, 'LakeSturgeon_EsturgeonJaune_ImportantSites.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0074,
           dsn = "./data/data-format/data0074-reproduction_esturgeon_jaune.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
