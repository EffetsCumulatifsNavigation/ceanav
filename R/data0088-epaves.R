#' Data 0088 : Épaves
#'
#' 
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0088 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0088-epaves/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  shp <- dir(paste0(folder,"Donnees_client_210325"), pattern = ".shp$", full.names = TRUE)
  nm <- tools::file_path_sans_ext(basename(shp))
  dat <- lapply(shp, sf::st_read)
  names(dat) <- nm

  # -----
  data0088 <- dplyr::bind_rows(dat) |>
              sf::st_transform(data0088, crs = global_parameters()$crs) 

  # # Number of sites
  # data(aoi)
  # aoi <- sf::st_buffer(aoi, 750)
  # ns <- length(sf::st_intersects(aoi, data0088)[[1]])
  # # 428
  
  # Decrease resolution
  data0088 <- sf::st_buffer(data0088, 1000) |>
              sf::st_union()

  # Intersect with study area
  data(aoi)
  data0088 <- sf::st_intersection(data0088, aoi)
  
  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0088,
           dsn = "./data/data-format/data0088-epaves.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
