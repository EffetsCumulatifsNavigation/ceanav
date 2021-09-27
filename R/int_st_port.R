#' Influence de la navigation en zone portuaire
#'
#' Couche de données transformées pour l'influence de la navigation en zone portuaire dans le Saint-Laurent et le Saguenay
#'
#' @keywords navigation portuaire
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_port <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load  data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  load_format("data0021")
  load_format("data0041")
  load_format("data0047")

  # -----
  data(grid1p)
  data(aoi)

  # -----
  meta <- load_metadata("int_st_port")
  meta$rawData <- c("0021", "0041", "0047")
  # ------------------------------------------------------------------------- #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Combine port data and add buffer
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING:
  # Retrait de la zone marine pour les données de zones industrialo-portuaires
  # Ceci est fait surtout pour Bécancour, puisque la zone s'étend jusqu'au milieu
  # du chenal de navigation. On ajoute ensuite un buffer pour identifier la zone d'influence marine
  data0047 <- st_difference(data0047, aoi)

  # -----
  port <- bind_rows(data0041, data0047) %>%
          st_buffer(200) %>%
          st_union() %>%
          st_intersection(aoi) %>%
          st_cast("POLYGON") %>%
          st_sf()

  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # AIS segments
  # -------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  ais <- ais_segment(data0021)
  # _____________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Number of transits per port area
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  uid <- st_intersects(port, ais)

  # -----
  ntransit <- list()
  for(i in 1:length(uid)) ntransit[[i]] <- length(uid[[i]])

  # -----
  port$port <- unlist(ntransit)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate to study grid
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  grid1p$id <- 1:nrow(grid1p)

  # -----
  uid <- st_intersects(port, grid1p)

  # -----
  dat <- list()
  for(i in 1:length(uid)) {
    dat[[i]] <- data.frame(id = uid[[i]], port = port$port[i])
  }
  dat <- bind_rows(dat)

  # -----
  dat <- dat %>%
         group_by(id) %>%
         summarise(port = sum(port))

  # -----
  port <- left_join(dat, by = 'id') %>%
          select(-id)

  # -----
  port <- cbind(grid1p, port) %>%
          select(-id)
  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_port.yml")

  # -----
  st_write(obj = port,
           dsn = "./data/data-integrated/st_port.geojson",
           delete_dsn = TRUE, quiet = TRUE)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
