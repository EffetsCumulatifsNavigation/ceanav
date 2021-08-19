#' Data 0044 : Sites patrimoniaux
#'
#' Sites patrimoniaux classés par le ministre de la Culture et des Communications
#'
#' @keywords sites patrimoniaux
#' @keywords composantes valorisées
#'
#' @source https://www.donneesquebec.ca/recherche/fr/dataset/578d182d-4897-4c11-aa7f-ee0cfed222a0
#'
#' @export
#'
#' @details This function loads and formats the data
#'

get_data0044 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0044-sites_patrimoniaux/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'donneesouvertesmccspclv7.csv'))) {
    # URL
    dat <- c('https://www.donneesquebec.ca/recherche/dataset/578d182d-4897-4c11-aa7f-ee0cfed222a0/resource/e5992bab-543a-4703-bbab-916f4f1126d8/download/donneesouvertesmccspclv7.csv')

    # Download
    download.file(dat[1], destfile = paste0(folder, 'donneesouvertesmccspclv7.csv'))
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  data0044 <- read.csv(paste0(folder, 'donneesouvertesmccspclv7.csv')) %>%
              filter(!is.na(latitude)) %>%
              st_as_sf(coords = c("longitude","latitude"), crs = 4326) %>%
              st_transform(crs = global_parameters()$crs) %>%
              st_buffer(500)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0044,
           dsn = "./data/data-format/data0044-sites_patrimoniaux.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #

}
