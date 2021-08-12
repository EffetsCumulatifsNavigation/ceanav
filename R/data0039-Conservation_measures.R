#' Data 0039 : Other Effective Area-Based Conservation Measures
#'
#' This dataset contains area-based management measures that constitute 'other effective area-based conservation measures' ('other measures') according to DFO's Operational Guidance for Identifying ‘Other Effective Area-Based Conservation Measures’ in Canada’s Marine Environment.
#'
#' @keywords aires protégées
#'
#' @source https://open.canada.ca/data/en/dataset/44769543-7a23-4991-a53f-c2cf7c7a946f
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0039 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0039-aire_protegee/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'DFO_OEABCM_MPO_AMCEZ_SHP.zip'))) {
    # URL
    dat <- c('https://pacgis01.dfo-mpo.gc.ca/FGPPublic/Oceans_Act_Marine_Protected_Areas/DFO_OEABCM_MPO_AMCEZ_SHP.zip')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'DFO_OEABCM_MPO_AMCEZ_SHP.zip'))

    # Unzip
    unzip(zipfile = paste0(folder, 'DFO_OEABCM_MPO_AMCEZ_SHP.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0039 <- st_read(paste0(folder, 'DFO_OEABCM_MPO_AMCEZ_SHP/DFO_OEABCM_MPO_AMCEZ.shp'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0039,
           dsn = "./data/data-format/data0039-aire_protegee.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
