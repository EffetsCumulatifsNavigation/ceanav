#' Data 0057 : Gisements connus et exploités de mactre de Stimpson des eaux côtières du Québec
#'
#' La mactre de Stimpson est visée par des mesures de protection et de conservation ayant pour objectif de protéger son potentiel reproducteur et l'intégrité des population (gisements) dans la région du Québec. Les gisements de cette couche sont décrits comme étant connus et commercialement exploités historiquement et/ou actuellement et ont subi un traitement de validation et de délimitation avec un bon degré de précision. Ils sont situés dans l'estuaire et le Golfe du Saint-Laurent, sur la Côte-Nord et les Îles-de-la-Madeleine. Les gisements de cette couche sont considérés parmi les gisements les plus abondants en mactre de Stimpson et sont utilisés pour la pêche commerciale (à noter: les fortes densités ne s'en tiennent pas qu'à ces gisements). Ils sont principalement reconnus vulnérables pendant la saison de reproduction de juin à juillet. Il pourrait y avoir une ponte secondaire à l'automne également. La couche de données contient les informations sur l'aire totale des gisements, les zones et noms des gisements, les secteurs concernés et les périodes de reproduction et de pêcheries.
#'
#' @keywords mactre de Stimpson
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/8bb29ee0-6cd8-4dd4-abe0-afe8682a69d9
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0057 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0057-gisement_mactre/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'SurfClamBeds_GisementsMactre.zip'))) {
    # URL
    dat <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/SurfClamBeds_GisementsMactre/SurfClamBeds_GisementsMactre.zip',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/SurfClamBeds_GisementsMactre/SurfClamBeds_GisementMactre_DonneesData.csv',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/SurfClamBeds_GisementsMactre/DataDictionary_DictionnaireDonnees.csv')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'SurfClamBeds_GisementsMactre.zip'))
    download.file(dat[2], destfile = paste0(folder, 'SurfClamBeds_GisementMactre_DonneesData.csv'))
    download.file(dat[3], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))

    # Unzip file
    unzip(zipfile = paste0(folder, 'SurfClamBeds_GisementsMactre.zip'), exdir = folder)

  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0057 <- st_read(paste0(folder, 'SurfClamBeds_GisementsMactre.shp'),
                      quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0057,
           dsn = "./data/data-format/data0057-gisement_mactre.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #

}
