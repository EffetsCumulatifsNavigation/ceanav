#' Data 0078 : Aires de concentration connues de la Clovisse arctique dans l’estuaire et le golfe du Saint-Laurent
#'
#' La couche représente des aires de concentration connues de la Clovisse arctique (Mesodesma arctatum) exploitées (communément appelées 'gisements') ou non exploitées de l'estuaire et du golfe du Saint-Laurent, région du Québec. Elle a été conçue pour le Centre national des urgences environnementales (CNUE) pour la préparation et l'intervention en cas de déversements pétroliers. Les aires de concentration ont été délimitées grâce à des inventaires réalisés par Pêches et Océans Canada (MPO) entre les années 2000 et 2020 et des données de divers projets de recherche du MPO. Pour plus d’information sur la façon dont la couche de données a été construite, consultez les métadonnées incluses dans son fichier de forme (.shp), particulièrement la section 'Lineage'.
#'
#' @keywords gisements coquilliers
#'
#' @source https://open.canada.ca/data/fr/dataset/b0633101-18e6-483a-b668-4e1f76789696
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0078 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0078-clovisse_arctique/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, '3-ClovisseArctique_AireConcentration.zip'))) {
    # URL
    dat <- c(
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv",
      "https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Biological_Sensitivity_Mapping_Oil_Spill_Planning_Response_Quebec_Region/3-ClovisseArctique_AireConcentration.zip"
    )

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees_CouchesPIEI2022.csv'))
    download.file(dat[2], destfile = paste0(folder, '3-ClovisseArctique_AireConcentration.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, '3-ClovisseArctique_AireConcentration.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0078 <- st_read(
    paste0(folder, 'ClovisseArctique_AireConcentration/ClovisseArctique_AireConcentration.shp'), 
    quiet = TRUE
  ) %>%
  st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0078,
           dsn = "./data/data-format/data0078-clovisse_arctique.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
