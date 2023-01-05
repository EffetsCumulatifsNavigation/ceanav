#' Data 0076 : Aires de reproduction et d'alimentation de l'éperlan-arc-en-ciel dans la Rivière Saguenay, le fleuve et l'estuaire du Saint-Laurent
#'
#' Couche regroupant l’information connue des aires de reproduction et d'alimentation de l'éperlan-arc-en-ciel, dans la Rivière Saguenay, le fleuve et l'estuaire du Saint-Laurent selon une revue de littérature de documents réalisés entre 1977 et 2000.
#'
#' @keywords frayères
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/9ddbf6ff-4314-47d8-ac0f-8d2f7278edcf
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0076 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0076-reproduction_eperlan_arc_en_ciel/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'RainbowSmelt_Eperlan_BreedingAndFeedingArea.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/RainbowSmelt_Eperlan_BreedingAndFeedingArea/DataDictionary_DictionnaireDonnees.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/RainbowSmelt_Eperlan_BreedingAndFeedingArea/RainbowSmelt_Eperlan_BreedingAndFeedingArea.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(dat[2], destfile = paste0(folder, 'RainbowSmelt_Eperlan_BreedingAndFeedingArea.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'RainbowSmelt_Eperlan_BreedingAndFeedingArea.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0076 <- st_read(paste0(folder, 'RainbowSmelt_Eperlan_BreedingAndFeedingArea.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0076,
           dsn = "./data/data-format/data0076-reproduction_eperlan_arc_en_ciel.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
