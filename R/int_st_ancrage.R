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
  # After discussions with Corinne Berthold, the plan is to: 
  #   - Take all of the GCC dataset (0070)
  #   - Take the SHC dataset (0071) beyond the GCC dataset, i.e. east of Les Rasades
  #
  # For now, I will do something simple:
  #   1. 2km buffer around anchorages - arbitrary
  #   2. Intersect with grid
  #   3. Number of buffers intersecting grid cell
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data & metadata
  load_format("data0070")
  load_format("data0071")
  meta <- read_yaml("./data/data-metadata/int_st_ancrage.yml")
  meta$rawData <-  c("0070","0071")

  # Load grid
  data(grid1p)
  
  # -----
  # Select data overlapping with aoi for both datasets 
  # data0070
  uid <- st_intersects(grid1p, data0070) %>% unlist() %>% unique() %>% sort()
  data0070 <- data0070[uid, ]
  
  # data0071
  uid <- st_intersects(grid1p, data0071) %>% unlist() %>% unique() %>% sort()
  data0071 <- data0071[uid, ]

  # -----
  # Data east of Les Rasades for data0071 
  xy <- st_centroid(data0070) %>% st_coordinates() 
  maxLong <- max(xy[,"X"])
  xy <- st_centroid(data0071) %>% st_coordinates()
  uid <- xy[,"X"] > maxLong
  data0071 <- data0071[uid,]
  
  # -----
  ancrage <- bind_rows(data0070, data0071) %>%
             st_buffer(2000) %>%
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
  meta$dataDescription$spatial$extent <- st_bbox(bind_rows(data0070,data0071))

  # -----
  meta$dataDescription$categories$accronyme <-  "ancrage"
  meta$dataDescription$categories$francais <-  "Sites d'ancrage commerciaux"
  meta$dataDescription$categories$english <-  "Commercial anchorage sites"
  meta$dataDescription$categories$source <-  paste(meta$rawData, collapse = ",")
  meta$dataDescription$categories$description <- "Distribution des sites d'ancrage pour la navigation commerciale" 
  meta$dataDescription$categories$description_en <- "Distribution of anchorage sites for commercial shipping" 

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)

  # -----
  meta$dataDescription$observations$total <-  bind_rows(data0070,data0071) %>%
                                              st_intersects(aoi) %>%
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
