#' Data 0073 : Aires de reproduction de l'alose savoureuse dans le fleuve et l'estuaire du Saint-Laurent
#'
#' Couche regroupant l’information connue des aires de reproduction de l'alose savoureuse dans le fleuve et l'estuaire du Saint-Laurent selon une revue de littérature de documents réalisés entre 1976 et 1997.
#'
#' @keywords frayères
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/a0817db6-42cb-4fa2-a601-48246c812e94
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0073 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0073-reproduction_alose_savoureuse/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'AmericanShad_AloseSavoureuse_BreedingArea.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/AmericanShad_AloseSavoureuse_BreedingArea/DataDictionary_DictionnaireDonnees.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/AmericanShad_AloseSavoureuse_BreedingArea/AmericanShad_AloseSavoureuse_BreedingArea.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(dat[2], destfile = paste0(folder, 'AmericanShad_AloseSavoureuse_BreedingArea.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'AmericanShad_AloseSavoureuse_BreedingArea.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0073 <- st_read(paste0(folder, 'AmericanShad_AloseSavoureuse_BreedingArea.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0073,
           dsn = "./data/data-format/data0073-reproduction_alose_savoureuse.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
