#' Data 0086 : Sites d'alevinage
#'
#' Identification des sites d'alevinage dans le Saint-Laurent fluvial
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0086 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0086-site_alevinage/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  s <- st_read(paste0(folder, 'Alevinage2023.gdb'),
                      layer = 'Alevinage_s_17Janv2023',
                      quiet = TRUE) %>%
       st_cast("MULTIPOLYGON") 


  # -----
  p <- st_read(paste0(folder, 'Alevinage2023.gdb'),
                      layer = 'Alevinage_p_17Janv2023',
                      quiet = TRUE) %>%
       st_buffer(100)
       
  # -----
  data0086 <- bind_rows(s, p)

  # Transform projection
  data0086 <- st_transform(data0086, crs = global_parameters()$crs)

  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0086,
           dsn = "./data/data-format/data0086-site_alevinage.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
