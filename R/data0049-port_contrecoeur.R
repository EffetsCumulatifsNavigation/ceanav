#' Data 0049 : Dragage prévu Port de Montréal à Contrecoeur
#'
#' Identification des activités de dragage prévus au Port de Montréal à Contrecoeur
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source SNC Lavalin (2017) Agrandissement du terminal portuaire de Contrecoeur. Étude d’impact environnemental - Volume 1 – Rapport principal. Rapport final - F00. Ref. Interne 639223. 889p
#' @source SNC Lavalin (2017) Contrecoeur Maritime Terminal Expansion Project. Environmental Impact Assessment – Summary. Final Report > Rev. F02. Internal ref. 639223. 109p.
#' @source Impact Assessment Agency of Canada (2021) Contrecoeur Port Terminal Expansion Project. Environmental Assessment Report. Catalog Number: En106-236/2021E-PD. ISBN: 978-0-660-37365-2. 336p. + xiii.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0049 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  #
  # --------------------
  #
  #
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data & general parameters
  # ----------------------------------------
  # Output folder
  output <- "data0049-port_contrecoeur/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Données
  dat <- data.frame(municipalite = "Montréal",
                    name = "Port de Montréal à Contrecoeur",
                    annee = c(2025),
                    volume = c(839000))

  # Dragage
  coords <- rbind(
    c(-73.29935, 45.82361),
    c(-73.30792, 45.81704),
    c(-73.30696, 45.81635),
    c(-73.30581, 45.81614),
    c(-73.29803, 45.82150),
    c(-73.29935, 45.82361)
  )

  # -----
  data0049 <- list(coords) %>%
              st_polygon() %>%
              st_sfc(crs = 4326) %>%
              st_sf(dat, row.names = "") %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0049,
           dsn = "./data/data-format/data0049-port_contrecoeur.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
