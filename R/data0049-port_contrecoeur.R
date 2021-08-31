#' Data 0049 : Dragage prévu Port de Montréal à Contrecoeur
#'
#' Identification des activités de dragage prévus au Port de Montréal à Contrecoeur
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source Ministère de l'Environnement et Lutte Contre les Changement Climatiques (2019) Rapport d’analyse environnementale Programme décennal de dragage d’entretien des installations portuaires de Port-Alfred sur le territoire de la ville de Saguenay. Direction générale de l'évaluation environnementale et stratégique. Direction de l'évaluation environnementale des projets hydriques et industriels. Dossier 3211-02-299. 29p. + viii
#' @source Englobe (2016) Programme décennal de dragage d’entretien et réfection des quais – Installations portuaires Port-Alfred, La Baie, Québec. Étude d’impact sur l’environnement. Rapport final 045-P-0008779-0-01-291-EN-R-0200-00. 132p. + ix
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
