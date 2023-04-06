#' Data 0075 : Aires de reproduction, d'alimentation et de concentration de l'esturgeon noir dans le fleuve et l'estuaire du Saint-Laurent
#'
#' Couche regroupant l’information connue des aires de reproduction, d'alimentation et de concentration de l'esturgeon noir dans le fleuve et l'estuaire du Saint-Laurent selon une revue de littérature de documents réalisés entre 1993 et 2003.
#'
#' @keywords frayères
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/bfa19b1c-a248-4dc6-b497-327a8937a07b
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0075 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0075-reproduction_esturgeon_noir/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'AtlanticSturgeon_EsturgeonNoir_ImportantSites.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/AtlanticSturgeon_EsturgeonNoir_ImportantSites/DataDictionary_DictionnaireDonnees.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/AtlanticSturgeon_EsturgeonNoir_ImportantSites/AtlanticSturgeon_EsturgeonNoir_ImportantSites.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(dat[2], destfile = paste0(folder, 'AtlanticSturgeon_EsturgeonNoir_ImportantSites.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'AtlanticSturgeon_EsturgeonNoir_ImportantSites.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0075 <- st_read(paste0(folder, 'AtlanticSturgeon_EsturgeonNoir_ImportantSites.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0075,
           dsn = "./data/data-format/data0075-reproduction_esturgeon_noir.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
