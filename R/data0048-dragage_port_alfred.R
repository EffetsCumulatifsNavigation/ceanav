#' Data 0048 : Dragage Port Alfred
#'
#' Identification des sites de dragage au Port Alfred
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source Ministère de l'Environnement et Lutte Contre les Changement Climatiques (2019) Rapport d’analyse environnementale Programme décennal de dragage d’entretien des installations portuaires de Port-Alfred sur le territoire de la ville de Saguenay. Direction générale de l'évaluation environnementale et stratégique. Direction de l'évaluation environnementale des projets hydriques et industriels. Dossier 3211-02-299. 29p. + viii
#' @source Englobe (2016) Programme décennal de dragage d’entretien et réfection des quais – Installations portuaires Port-Alfred, La Baie, Québec. Étude d’impact sur l’environnement. Rapport final 045-P-0008779-0-01-291-EN-R-0200-00. 132p. + ix
#' @source Englobe (2017) Programme décennal de dragage d’entretien et réfection des quais – Installations portuaires Port-Alfred, La Baie, Québec. Résumé de l’étude d’impact sur l’environnement et du projet. 24p.
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0048 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # WARNING:
  #   Les sites de dragage du port Alfred sont adjacent aux 2 quais. Toutefois,
  #   compte tenu l'échelle de notre analyse, un seul polygone regroupant les
  #   différents sites de dragage a été tracé.
  message("Les sites de dragage du port Alfred sont adjacent aux 2 quais. Toutefois, compte tenu l'échelle de notre analyse, un seul polygone regroupant les différents sites de dragage a été tracé.")
  #
  # RESOURCES:
  #   Degrees minutes seconds to degrees decimals converter:
  #   https://www.rapidtables.com/convert/number/degrees-minutes-seconds-to-degrees.html
  #
  #   Code to visualize point, draw polygon and extract coordinates
  #   x <- st_point(matrix(c(-67.2848, 49.4214), nrow = 1)) %>%
  #        st_sfc(crs = 4326)
  #   x <- mapview(x) %>% mapedit::editMap()
  #   st_coordinates(x[[1]])
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data & general parameters
  # ----------------------------------------
  # Output folder
  output <- "data0048-dragage_port_alfred/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # Données
  dat <- data.frame(municipalite = "Saguenay",
                    name = "Port Alfred",
                    annee = c(2010,2014,2018),
                    volume = c(1300,900,2886))

  # Dragage
  coords <- rbind(
    c(-70.87397, 48.33494),
    c(-70.86860, 48.33564),
    c(-70.86820, 48.33344),
    c(-70.87339, 48.33296),
    c(-70.87397, 48.33494)
  )

  data0048 <- list(coords) %>%
              st_polygon() %>%
              st_sfc(crs = 4326) %>%
              st_sf(dat) %>%
              st_transform(crs = global_parameters()$crs)
  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Output
  st_write(obj = data0048,
           dsn = "./data/data-format/data0048-dragage_port_alfred.geojson",
           delete_dsn = TRUE)
  # _________________________________________________________________________ #
}
