#' Data 0040 : Oceans Act Marine Protected Areas
#'
#' Fisheries and Oceans Canada has a number of Marine Protected Areas designated under the Oceans Act and Areas of Interest for new Marine Protected Areas at various stages of progress towards designation. These areas are ecologically significant, with species and/or properties that require special management consideration.
#'
#' @keywords aires protégées
#'
#' @source https://open.canada.ca/data/en/dataset/a1e18963-25dd-4219-a33f-1a38c4971250
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0040 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0040-aire_protegee/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'DFO_MPA_MPO_ZPM_SHP.zip'))) {
    # URL
    dat <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Oceans_Act_Marine_Protected_Areas/DFO_MPA_MPO_ZPM_SHP.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DFO_MPA_MPO_ZPM_SHP.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'DFO_MPA_MPO_ZPM_SHP.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0040 <- st_read(paste0(folder, 'DFO_MPA_MPO_ZPM_SHP/DFO_MPA_MPO_ZPM.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0040,
           dsn = "./data/data-format/data0040-aire_protegee.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
