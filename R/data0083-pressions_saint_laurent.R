#' Data 0083 : Cartographie des principales pressions exercées sur le Saint-Laurent en 2012
#'
#' Cartographie de six pressions susceptibles d’avoir des impacts sur les poissons dans le fleuve Saint-Laurent: agriculture, artificialisation des berges, navigation commerciale, pêche commerciale, régularisation du débit, industrialisation/urbanisation. Un indice a été calculé pour chacune d’elles et aux fins de comparaison, les valeurs ont été remplacées par les cotes 1, 3 ou 5 correspondant aux niveaux faible, moyen ou fort. Pour cartographier les pressions, ces calculs ont été effectués dans chaque segment du fleuve, correspondant à des unités de 5 km de long partant des rives nord ou sud et allant jusqu’à la voie navigable.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0083 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0083-pressions_saint_laurent/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  dat <- read.csv(paste0(folder,"Pressions_fleuve_MM2012.csv"))
  grd <- sf::st_read(
    paste0(folder, "PressionsSL_polygones5km.shp"), 
    quiet = TRUE
  )

  data0083 <- dplyr::left_join(grd, dat, by = c("ID" = "SEGMENT"))  |>
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0083,
           dsn = "./data/data-format/data0083-pressions_saint_laurent.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
