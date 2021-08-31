#' Data 0051 : Canada's designated alternate ballast water exchange areas
#'
#' This data is intended to identify Canadian Alternate Exchange Areas described in https://tc.canada.ca/en/marine-transportation/marine-safety/list-canada-s-designated-alternate-ballast-water-exchange-area-fresh-waters-tp-13617e-2021.
#'
#' @keywords ballast water exchange area
#'
#' @source https://open.canada.ca/data/en/dataset/23d26c61-b119-42c0-aa41-bd06cd96a973
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0051 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0051-ballast_water/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'CanadaAlternateBallastWaterExchangeAreas.gdb.zip'))) {
    # URL
    dat <- c('https://publications.gc.ca/collections/collection_2016/mpo-dfo/Fs70-5-2015-037-eng.pdf',
             'https://waves-vagues.dfo-mpo.gc.ca/Library/316461.pdf',
             'https://pacgis01.dfo-mpo.gc.ca/FGPPublic/CanadaAlternateBallastWaterExchangeAreas/CanadaAlternateBallastWaterExchangeAreas.gdb.zip',
             'https://tc.canada.ca/sites/default/files/2021-06/tp13617e.pdf')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'Fs70-5-2015-037-eng.pdf'))
    download.file(dat[2], destfile = paste0(folder, '316461.pdf'))
    download.file(dat[3], destfile = paste0(folder, 'CanadaAlternateBallastWaterExchangeAreas.gdb.zip'))
    download.file(dat[4], destfile = paste0(folder, 'tp13617e.pdf'))

    # Unzip file
    unzip(zipfile = paste0(folder, 'CanadaAlternateBallastWaterExchangeAreas.gdb.zip'), exdir = folder)

  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0051 <- st_read(paste0(folder, 'CanadaBallastExchangeAreas.gdb'),
                      layer = "ExchangeAreas_Zones_du_renouvellement",
                      quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0051,
           dsn = "./data/data-format/data0051-ballast_water.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #

}
