#' Data 0012 : Classification rivages côtiers
#'
#' Classification des Rivages du Québec - Fleuve Saint-Laurent
#'
#' @keywords milieu côtier
#' @keywords rivage
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/ba580518-59e8-4d1c-b3ef-41d2658e6965
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0012 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0012-classification_cote/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf'))) {
    # URL
    cote_classification <- c('http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/quebec-saint-lawrence-river-shoreline-classification/How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf',
                             'http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/quebec-saint-lawrence-river-shoreline-classification/ShorelineClassification_QC_OpenDataCatalogue.gdb.zip')

    # Download
    download.file(cote_classification[1], destfile = paste0(folder, 'How_To_Guide_-_Navigating_Through_Open_Data_Portals_-_for_ECCC_Shoreline_Data.pdf'))
    download.file(cote_classification[2], destfile = paste0(folder, 'ShorelineClassification_QC_OpenDataCatalogue.gdb.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'ShorelineClassification_QC_OpenDataCatalogue.gdb.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0012 <- st_read(paste0(folder, 'ShorelineClassification_QC_OpenDataCatalogue.gdb'),
                      layer = "O14Oceans_ShorelineClass_QC")

  # Transform projection
  data0012 <- st_transform(data0012, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0012,
           dsn = "./data/data-format/data0012-classification_cote.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
