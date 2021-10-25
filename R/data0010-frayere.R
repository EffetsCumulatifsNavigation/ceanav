#' Data 0010 : Frayères
#'
#' Identification des frayères dans le Saint-Laurent fluvial
#'
#' @keywords frayère
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source Marc Mingelbier, Ministère des Forêts, de la Faune et des Parcs
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0010 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0010-frayere/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # WARNING: Data transfered physically, no cloud access currently

  # Unzip
  unzip(zipfile = paste0(folder, 'DonneesMFFP_PourPASL.gdb.zip'), exdir = folder)

  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data
  # ----------------------------------------
  # -----
  s <- st_read(paste0(folder, 'DonneesMFFP_PourPASL.gdb'),
                      layer = 'Frayere_s_CEGRIM_25_02_2020',
                      quiet = TRUE) %>%
       st_cast("MULTIPOLYGON")

  # -----
  p <- st_read(paste0(folder, 'DonneesMFFP_PourPASL.gdb'),
                      layer = 'Frayere_p_CEGRIM__2_3_2020',
                      quiet = TRUE) %>%
       st_buffer(100)

  # -----
  data0010 <- bind_rows(s, p)

  # Transform projection
  data0010 <- st_transform(data0010, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0010,
           dsn = "./data/data-format/data0010-frayere.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
