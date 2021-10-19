#' Data 0054 : Observation mammifères marins
#'
#'
#'
#' @keywords mammifères marins
#' @keywords composante valorisée
#'
#' @source https://ogsl.ca/fr/biodiversite-mammiferes-marins-romm-a-propos/
#'
#' @export
#'
#' @details Cette fonction télécharge et formatte les données
#'

get_data0054 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0054-mammiferes_marins_romm/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0054 <- read.csv(paste0(folder, "export.csv")) %>%
              st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0054,
           dsn = "./data/data-format/data0054-mammiferes_marins_romm.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
