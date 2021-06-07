#' Data 0001 : Inventaire zostère
#'
#' Inventaire de la zostère marine dans la Baie James, la Baie des Chaleurs, l'estuaire et le golfe du Saint-Laurent
#'
#' @keywords zostère
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/c9ab948f-5009-4dbc-9129-2f6e373f17f6
#'
#' @export
#'
#' @details Cette fonction télécharge les données et les préparent pour les analyses
#'

data0001_zostere <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0001-zostere/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'Zostera_Zostere.zip'))) {
    # URL
    zostere_inv <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/Zostera_Zostere.zip',
                     'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Zostera_Zostere/DataDictionary_DictionnaireDonnees.csv')

    # Download
    download.file(zostere_inv[1], destfile = paste0(folder, 'Zostera_Zostere.zip'))
    download.file(zostere_inv[2], destfile = paste0(folder, 'DataDictionary_DictionnaireDonnees.csv'))

    # Unzip
    unzip(zipfile = paste0(folder, 'Zostera_Zostere.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0001 <- st_read(paste0(folder, 'Zostera_Zostere.shp'))
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ----------------------------------------
  # Nothing to format
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  folder <- paste0("./data/data-format/", output)
  if (!file.exists(folder)) dir.create(folder)
  st_write(data0001, dsn = paste0(folder, "data0001-zostere.geojson"))

  # RData
  save(data0001, file = "./data/data0001.RData")
  # _________________________________________________________________________ #
}
