#' Data 0087 : Frayères
#'
#' Identification des frayères dans le Saint-Laurent fluvial
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0087 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Download data
  # ----------------------------------------
  # Output folder
  output <- "data0087-frayere/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)
  # WARNING: Data transfered physically, no cloud access currently
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import and format data
  # ----------------------------------------
  s <- st_read(paste0(folder, 'Frayere2023.gdb'),
                      layer = 'Frayere_s_Janv2023',
                      quiet = TRUE) %>%
       st_cast("MULTIPOLYGON") 

  # -----
  p <- st_read(paste0(folder, 'Frayere2023.gdb'),
                      layer = 'Frayer_p_Janvier2023',
                      quiet = TRUE) %>%
       st_buffer(100)
       
  # -----
  l <- st_read(paste0(folder, 'Frayere2023.gdb'),
                      layer = 'Frayere_l_Janv2023',
                      quiet = TRUE) %>%
       st_buffer(100)
       
  # -----
  data0087 <- bind_rows(s, p, l) |>
              sf::st_make_valid()

  # Transform projection
  data0087 <- st_transform(data0087, crs = global_parameters()$crs)

  # _________________________________________________________________________ #    

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0087,
           dsn = "./data/data-format/data0087-frayere.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
