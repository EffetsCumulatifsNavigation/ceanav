#' Data 0027 :
#'
#'
#'
#' @keywords mammifères marins
#' @keywords composante valorisée
#'
#' @source Le WWF-Canada et le Réseau d’observation de mammifères marins. Données cartographiques : Navires et baleines de l’Atlantique Nord-Ouest : Guide à l’intention de l’industrie maritime, 2e édition. 2021. Données disponibles sur le site https://www.navigationbaleines.ca/fr/accueil/
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0027 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # WARNING:
  message("Ces données doivent être accompagnées d'une mise en garde sur leur utilisation. À établir avec le ROMM et WWF Canada")

  # Output folder
  output <- "data0027-mammiferes_marins/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently
  # Proceed only if data is not already loaded
  if (!file.exists(paste0(folder, 'CetaceanHeatmapRasters/'))) {
    # Unzip
    unzip(zipfile = paste0(folder, 'CetaceanHeatmapRasters.zip'), exdir = folder)
  }
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  files <- dir(paste0(folder, 'CetaceanHeatmapRasters'), pattern = ".tif$", full.names = TRUE)


  # Single dataset
  # data0027 <- bind_rows(data0027)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # # Output
  # st_write(obj = data0027,
  #          dsn = "./data/data-format/data0027-mammiferes_marins.geojson",
  #          delete_dsn = TRUE)
  #
  # # RData
  # save(data0027, file = "./data/data0027.RData")
  # _________________________________________________________________________ #
}
