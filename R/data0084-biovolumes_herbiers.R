#' Data 0084 : Biovolumes herbiers
#'
#' Biovolumes d'herbiers aquatiques à partir de données sonar en 2021.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0084 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0084-biovolumes_herbiers/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  data0084 <- sf::st_read(
    paste0(folder, "Herbier2021Plus_0_1DeBiovolume.shp"), 
    quiet = TRUE
  )
  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0084,
           dsn = "./data/data-format/data0084-biovolumes_herbiers.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
