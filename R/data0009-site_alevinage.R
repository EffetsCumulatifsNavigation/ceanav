#' Data 0009 : Sites d'alevinage
#'
#' Identification des sites d'alevinage dans le Saint-Laurent fluvial
#'
#' @keywords site alevinage
#' @keywords habitat
#' @keywords composante valorisée
#'
#' @source Marc Mingelbier, Ministère des Forêts, de la Faune et des Parcs
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0009 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0009-site_alevinage/"
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
                      layer = 'Alevinage_DEFA_s_CEGRIM',
                      quiet = TRUE) %>%
       st_cast("MULTIPOLYGON") %>%
       rename(Source = Alevinage_Union_Source)


  # -----
  p <- st_read(paste0(folder, 'DonneesMFFP_PourPASL.gdb'),
                      layer = 'Alevinage_DEFA_p_CEGRIM',
                      quiet = TRUE) %>%
       st_buffer(100)

  # -----
  data0009 <- bind_rows(s, p)

  # Transform projection
  data0009 <- st_transform(data0009, crs = global_parameters()$crs)
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0009,
           dsn = "./data/data-format/data0009-site_alevinage.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
