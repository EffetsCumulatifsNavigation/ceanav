#' Data 0041 : Installations portuaires
#'
#' Localisation des terminaux portuaires
#'
#' @keywords ports
#'
#' @source https://ouvert.canada.ca/data/fr/dataset/dcf460ae-c27f-4721-bec4-6e4467a5a3b6
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0041 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0041-port/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'port.pdf'))) {
    # URL
    dat <- c('https://ws.mapserver.transports.gouv.qc.ca/swtq?service=wfs&version=2.0.0&request=getfeature&typename=ms:port_installation&outputformat=csv',
             'https://ws.mapserver.transports.gouv.qc.ca/swtq?service=wfs&version=2.0.0&request=getfeature&typename=ms:port_installation&outputformat=geojson&srsname=EPSG:4326',
             'https://www.donneesquebec.ca/recherche/dataset/dcf460ae-c27f-4721-bec4-6e4467a5a3b6/resource/a37a2d1d-4b3c-4282-96e5-08c25c9199f1/download/port.pdf')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'port_installation.csv'))
    download.file(dat[2], destfile = paste0(folder, 'port_installation.geojson'))
    download.file(dat[3], destfile = paste0(folder, 'port_installation.pdf'))
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0041 <- st_read(paste0(folder, 'port_installation.geojson'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0041,
           dsn = "./data/data-format/data0041-port.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
