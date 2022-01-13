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
  #   1. 2km buffer around anchorages - arbitrary
  #   2. Intersect with grid
  #   3. Number of buffers intersecting grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data & metadata
  load_format("data0015")
  meta <- read_yaml("./data/data-metadata/int_st_ancrage.yml")
  meta$rawData <-  c("0015")

  # Load grid
  data(grid1p)

  # -----
  ancrage <- st_buffer(data0015, 2000) %>%
             st_intersects(grid1p,.) %>%
             lapply(., length) %>%
             unlist()

  # -----
  ancrage <- grid1p %>% mutate(ancrage = ancrage)

  # -------
  # Identify cells in the aoi that are only terrestrial and hence shouldn't be included.
  data(aoi)
  uid <- st_intersects(aoi, ancrage) %>% unlist()
  nid <- !1:nrow(ancrage) %in% uid
  ancrage$ancrage[nid] <- 0
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta$dataDescription$spatial$extent <- st_bbox(data0015)

  # -----
  meta$dataDescription$categories$accronyme <-  "ancrage"
  meta$dataDescription$categories$francais <-  "Sites d'ancrage de navires"
  meta$dataDescription$categories$source <-  meta$rawData
  meta$dataDescription$categories$description <- "Distribution des sites d'ancrage de navires" 

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  # -----
  meta$dataDescription$observations$total <-  st_intersects(data0015, aoi) %>%
                                              unlist() %>%
                                              length()
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_ancrage.yml")

  # -----
  st_write(obj = ancrage,
           dsn = "./data/data-integrated/st_ancrage.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
