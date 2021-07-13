#' Ancrages
#'
#' Couche de données transformées pour les sites d'ancrages dans le Saint-Laurent
#'
#' @keywords ancrage
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_ancrage <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # WARNING: Method has to be revisited
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/6
  #
  # For now, I will do something simple:
  #   1. 5km buffer around anchorages (totally arbitrary)
  #   2. Intersect with grid
  #   3. Number of buffers intersecting grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data
  load_format("data0015")

  # Load grid
  data(grid1p)

  # Identify grid cells with zostera
  ancrage <- st_buffer(ancrage, 5000) %>%
             st_intersects(grid1p,.) %>%
             lapply(., length) %>%
             unlist()

  # Add info to grid
  ancrage <- grid1p %>% mutate(ancrage = ancrage)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  st_write(obj = ancrage,
           dsn = "./data/data-integrated/st_ancrage.geojson",
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #

}
