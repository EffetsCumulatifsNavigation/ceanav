#' Naufrages
#'
#' Couche de données présentant les sites de naufrages dans la zone d'étude
#'
#' @keywords naufrage
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_naufrage <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # WARNING: Method has to be revisited
  # https://github.com/EffetsCumulatifsNavigation/ceanav/issues/6
  #
  # For now, I will do something simple:
  # Gaussian kernal around ship position, with 2km radius
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data & metadata
  load_format("data0062")
  meta <- read_yaml("./data/data-metadata/int_st_naufrage.yml")
  meta$rawData <-  c("0062")

  # Load grid
  data(grid1p)

  # -----
  coords <- st_centroid(grid1p) %>%
            st_coordinates() %>%
            as.data.frame() %>%
            rename(x = X, y = Y)

  # -----
  dat <- st_coordinates(data0062) %>%
         as.data.frame() %>%
         select(x = X, y = Y) %>%
         # mutate(naufrage = 1)
         mutate(naufrage = runif(nrow(.), 1,100))

  # -----
  naufrage <- btb::kernelSmoothing(dfObservations = dat,
                              sEPSG = "32198",
                              iCellSize = 1000,
                              iBandwidth = 5000,
                              # iBandwidth = 5000,
                              vQuantiles = NULL,
                              dfCentroids = round(coords,0))
  mapview(naufrage[,'naufrage'])
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta$dataDescription$spatial$extent <- st_bbox(data0062)

  # -----
  meta$dataDescription$categories$accronyme <-  "naufrage"
  meta$dataDescription$categories$francais <-  "Sites de naufrages répertoriés"
  meta$dataDescription$categories$source <-  meta$rawData

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  # -----
  meta$dataDescription$observations$total <-  st_intersects(data0062, aoi) %>%
                                              unlist() %>%
                                              length()
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_naufrage.yml")

  # -----
  st_write(obj = naufrage,
           dsn = "./data/data-integrated/st_naufrage.geojson",
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
