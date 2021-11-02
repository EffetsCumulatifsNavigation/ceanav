#' Data 0056 : Gisements connus et exploités de pétoncles dans l'estuaire et le golfe du Saint-Laurent
#'
#' Le pétoncle d'Islande (Chlamys islandica) et le pétoncle géant (Placopecten magellanicus) représentent une ressource biologique importante pour les pêcheries commerciales et l'écologie côtière. Les gisements de cette couche sont décrits comme étant connus et commercialement exploités historiquement et/ou actuellement et ont subi un traitement de validation et de délimitation avec un bon degré de précision. Ils sont situés dans l'estuaire et le golfe du Saint-Laurent, sur la Côte-Nord, Anticosti, la rive Nord de la Gaspésie et les Îles-de-la-Madeleine. Les gisements de cette couche sont considérés parmi les gisements les plus abondants en pétoncles et utilisés pour la pêche commerciale (à noter: les fortes densités et la pêche ne s'en tiennent pas qu'à ces gisements). Ils sont reconnus vulnérables principalement durant la période de croissance larvaire d'août à novembre pour la survie des larves, mais sont vulnérables aussi le reste de l'année étant des espèces sessiles. La couche de données contient les informations sur l'aire totale des gisements, la zone et le fond de pêche (étape de validation), le nom des gisements, les périodes de reproduction par secteur et les périodes de pêche (valide en 2015).
#'
#' @keywords pétoncle
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/82e082be-8f04-4273-830a-8aea63319f07
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0056 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0056-gisement_petoncle/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'ScallopBeds_GisementsPetoncle.zip'))) {
    # URL
    dat <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Scallop_Petoncle/ScallopBeds_GisementsPetoncle.zip',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Scallop_Petoncle/DataDictionary_DictionnaireDonnees.csv',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Scallop_Petoncle/ScallopBeds_GisementsPetoncle_DonneesData.csv')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'ScallopBeds_GisementsPetoncle.zip'))
    download.file(dat[2], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))
    download.file(dat[3], destfile = paste0(folder, 'ScallopBeds_GisementsPetoncle_DonneesData.csv'))

    # Unzip file
    unzip(zipfile = paste0(folder, 'ScallopBeds_GisementsPetoncle.zip'), exdir = folder)

  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0056 <- st_read(paste0(folder, 'ScallopBeds_GisementsPetoncle.shp'),
                      quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0056,
           dsn = "./data/data-format/data0056-gisement_petoncle.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #

}

# Les pétoncles géant et d'Islande se retrouvent principalement à de faibles profondeurs en milieu côtier (de 20 à 80 m selon les secteurs) sur des fonds de gravier, coquillages ou de roches. Les gisements sont habituellement associés à la présence de zones de rétention des larves, et sont vulnérables durant la période de croissance des larves (5 semaines) entre août et novembre.
#
# Les données représentent les gisements importants et abondants en pétoncles et exploités historiquement. Ils ne représentent pas la distribution générale de l'espèce, ne tiennent pas compte des gisements importants non-exploités de pétoncles ni des gisements exploités dont le travail de délimitation n'a pas encore été précisé. Cette couche représente les données les plus précises connues à ce jour.
