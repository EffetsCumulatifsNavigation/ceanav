#' Data 0081 : Aires de concentration connues de la Mye commune dans la zone intertidale de l’estuaire et du golfe du Saint-Laurent
#'
#' La couche représente des aires de concentration de la Mye commune (Mya arenaria) exploitées (communément appelées « gisements ») ou non exploitées présentes dans la zone intertidale de l’estuaire et du golfe du Saint-Laurent de la région du Québec. Elle a été conçue pour le Centre national des urgences environnementales (CNUE) pour la préparation et l’intervention en cas de déversement pétrolier. Les aires de concentration ont été délimitées grâce à des inventaires réalisés par le Pêches et Océans Canada (MPO) entre les années 2000 et 2020. À noter que cette couche est tributaire des inventaires effectués et ne représente ainsi que les aires de Mye commune connues. Par exemple, pour la Haute-Côte-Nord, les inventaires ont été limités aux secteurs ouverts à la cueillette (à l'exception de 4 secteurs), mais il est connu que la Mye commune est présente aussi à l'extérieur de ces secteurs. De plus, peu d'informations étaient disponibles pour la Moyenne et la Basse-Côte-Nord.
#'
#' @keywords gisements coquilliers
#'
#' @source https://open.canada.ca/data/fr/dataset/83fc6c4f-8a13-4e42-8d22-4dbacb5fa8c3
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0081 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0081-mye_commune/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, '7-MyeCommune_AireConcentration.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/7-MyeCommune_AireConcentration.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv'))
    download.file(dat[2], destfile = paste0(folder, '7-MyeCommune_AireConcentration.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, '7-MyeCommune_AireConcentration.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0081 <- st_read(
    paste0(folder, 'MyeCommune_AireConcentration/MyeCommune_AireConcentration.shp'), 
    quiet = TRUE
  ) %>%
  st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0081,
           dsn = "./data/data-format/data0081-mye_commune.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
