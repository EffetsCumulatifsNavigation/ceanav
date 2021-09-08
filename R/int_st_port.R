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
  load_format("data0041")
  load_format("data0047")

  # -----
  meta <- load_metadata("int_st_port")
  meta$rawData <- c("0041", "0047")
  # ------------------------------------------------------------------------- #



  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format and combine data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING: For visualization purposes, simply a 5 km buffer for now
  data0041 <- st_buffer(data0041, 2000)
  data0047 <- st_buffer(data0047, 4000)

  # -----
  # port <- bind_rows(data0041, data0047)

  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Integrate to study grid
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  data(grid1p)

  # Ports
  # -----
  uid <- st_intersects(data0041, grid1p) %>%
         unlist() %>%
         unique()

  # -----
  port <- grid1p %>%
          mutate(port = 0)
  port$port[uid] <- 1

  # Zones industrialo-portuaires
  # -----
  uid <- st_intersects(data0047, grid1p) %>%
         unlist() %>%
         unique()

  # -----
  port <- port %>%
          mutate(zone_portuaire = 0)
  port$zone_portuaire[uid] <- 1

  # ------------------------------------------------------------------------- #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING: Déterminer comment établir l'intensité de cette influence en
  #          fonction du traffic qui transite par les ports (utiliser
  #          données AIS)
  message("WARNING: Déterminer comment établir l'intensité de cette influence en fonction du traffic qui transite par les ports (utiliser données AIS)")

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
