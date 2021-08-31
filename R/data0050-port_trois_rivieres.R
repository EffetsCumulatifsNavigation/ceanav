#' Data 0050 : Dragage prévu Port de Trois-Rivières
#'
#' Identification des activités de dragage prévus au Port de Trois-Rivières
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source Stantec (2015) Agrandissement des installations portuaires du Port de Trois-Rivières. Description de projet désigné en vertu de la Loi canadienne sur l’évaluation environnementale (2012). Rapport Principal. Administration portuaire de Trois-Rivières. 131-P-0007772-0-01-100-EN-R-0002-01. 91p.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0050 <- function() {
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
  output <- "data0050-port_trois_rivieres/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Données
  message("Data 0050: Les travaux prévus citent entre 170000 m^3 et 65000 m^3 de sédiments dragués selon le scénario sélectioné. Puisque nous n'avons pas trouvé d'informations supplémentaires indiquant le scénario prévilégié, nous utilisons la valeur citée de 170000 m^3 de sédiments dragués par principe de précaution.")
  dat <- data.frame(municipalite = "Trois-Rivières",
                    name = "Port de Trois-Rivières",
                    annee = c(2030),
                    volume = c(170000))

  # Dragage
  coords <- rbind(
    c(-72.55357, 46.32223),
    c(-72.54910, 46.32746),
    c(-72.54944, 46.32759),
    c(-72.55082, 46.32719),
    c(-72.55096, 46.32702),
    c(-72.55130, 46.32601),
    c(-72.55166, 46.32594),
    c(-72.55211, 46.32575),
    c(-72.55251, 46.32550),
   c(-72.55308, 46.32506),
   c(-72.55321, 46.32495),
   c(-72.55333, 46.32496),
   c(-72.55376, 46.32462),
   c(-72.55424, 46.32367),
   c(-72.55425, 46.32350),
   c(-72.55434, 46.32321),
   c(-72.55448, 46.32316),
   c(-72.55513, 46.32322),
   c(-72.55554, 46.32299),
   c(-72.55357, 46.32223)
  )

  # -----
  data0050 <- list(coords) %>%
              st_polygon() %>%
              st_sfc(crs = 4326) %>%
              st_sf(dat, row.names = "") %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0050,
           dsn = "./data/data-format/data0050-port_trois_rivieres.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
