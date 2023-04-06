#' Data 0079 : Aires de concentration connues du Couteau de l'Atlantique dans l’estuaire et le golfe du Saint-Laurent
#'
#' La couche représente des aires de concentration du Couteau de l'Atlantique (Ensis leei) exploitées (communément appelées « gisements ») ou non exploitées de l’estuaire et du golfe du Saint-Laurent dans la région du Québec. Elle a été conçue pour le Centre national des urgences environnementales (CNUE) pour la préparation et l’intervention en cas de déversements d’hydrocarbures. Les aires de concentration ont été délimitées grâce à des inventaires et divers projets de recherche réalisés par Pêches et Océans Canada (MPO) et de données de pêches commerciales.
#'
#' @keywords gisements coquilliers
#'
#' @source https://open.canada.ca/data/fr/dataset/a0039b2e-7847-4d48-8fd8-638677760f8b
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0079 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0079-couteau_atlantique/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, '4-CouteauAtlantique_AireConcentration.zip'))) {
    # URL
    dat <- c(
      'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv',
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/4-CouteauAtlantique_AireConcentration.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv'))
    download.file(dat[2], destfile = paste0(folder, '4-CouteauAtlantique_AireConcentration.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, '4-CouteauAtlantique_AireConcentration.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0079 <- st_read(
    paste0(folder, 'CouteauAtlantique_AireConcentration/CouteauAtlantique_AireConcentration.shp'), 
    quiet = TRUE
  ) %>%
  st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0079,
           dsn = "./data/data-format/data0079-couteau_atlantique.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
