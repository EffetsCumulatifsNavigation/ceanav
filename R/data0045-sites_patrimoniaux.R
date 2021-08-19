#' Data 0045 : Sites patrimoniaux
#'
#' Sites patrimoniaux cités par les municipalités et les communautés autochtones
#'
#' @keywords sites patrimoniaux
#' @keywords composantes valorisées
#'
#' @source https://www.donneesquebec.ca/recherche/fr/dataset/34ca1a49-a020-4f60-986b-69fb4cfa852a
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0045 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0045-sites_patrimoniaux/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'donneesouvertesmccspciv3.csv'))) {
    # URL
    dat <- c('https://www.donneesquebec.ca/recherche/dataset/34ca1a49-a020-4f60-986b-69fb4cfa852a/resource/c49cec03-f16c-4cb1-a65a-10f12d81950b/download/donneesouvertesmccspciv3.csv')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'donneesouvertesmccspciv3.csv'))
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0045 <- read.csv(paste0(folder, 'donneesouvertesmccspciv3.csv')) %>%
              filter(!is.na(latitude)) %>%
              st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
              st_transform(crs = global_parameters()$crs) %>%
              st_buffer(500)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0045,
           dsn = "./data/data-format/data0045-sites_patrimoniaux.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
