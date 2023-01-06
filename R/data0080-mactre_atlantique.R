#' Data 0080 : Aires de concentration connues de la Mactre de l'Atlantique dans le golfe du Saint-Laurent
#'
#' La couche représente des aires de concentration connues de la Mactre de l'Atlantique (Spisula solidissima) exploitées (communément appelées « gisements ») ou non exploitées présentes dans le golfe du Saint-Laurent, autour des Îles-de-la-Madeleine. Elle a été conçue pour le Centre national des urgences environnementales (CNUE) pour la préparation et l’intervention en cas de déversements d’hydrocarbures. Les aires de concentration ont été délimitées grâce à des données d'inventaire de Pêches et Océans Canada (MPO) de 2000 à 2020, des données de pêches commerciales (journaux de bord et récépissés d'achat de 2002 à 2020 et observations en mer de 2005 à 2017), des données de pêches exploratoires ainsi que des entrevues.
#'
#' @keywords gisements coquilliers
#'
#' @source https://open.canada.ca/data/fr/dataset/ef06ae4e-151c-439c-be88-ef3b6d3b39a6
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0080 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0080-mactre_atlantique/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, '5-MactreAtlantique_AireConcentration.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/5-MactreAtlantique_AireConcentration.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv'))
    download.file(dat[2], destfile = paste0(folder, '5-MactreAtlantique_AireConcentration.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, '5-MactreAtlantique_AireConcentration.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0080 <- st_read(
    paste0(folder, 'MactreAtlantique_AireConcentration/MactreAtlantique_AireConcentration.shp'), 
    quiet = TRUE
  ) %>%
  st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0080,
           dsn = "./data/data-format/data0080-mactre_atlantique.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
