#' Data 0047 : Délimitation des zones industrialo-portuaires
#'
#' Données géolocalisées et cartes illustrant la délimitation des zones industrialo-portuaires
#'
#' @keywords ports
#'
#' @source https://www.donneesquebec.ca/recherche/dataset/delimitation-des-zones-ip-industrialo-portuaires
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0047 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0047-zone_portuaire/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'zoneindustrialoportuaire.json'))) {
    # URL
    dat <- c('https://donneesouvertes.affmunqc.net/zones_ip/zoneindustrialoportuaire.json')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'zoneindustrialoportuaire.json'))
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0047 <- st_read(paste0(folder, 'zoneindustrialoportuaire.json'), quiet = TRUE) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0047,
           dsn = "./data/data-format/data0047-zone_portuaire.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
