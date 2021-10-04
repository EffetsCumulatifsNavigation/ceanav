#' Data 0052 : Dragage d'entretien Port de Québec
#'
#' Activités de dragage d'entretien au Port de Québec
#'
#' @keywords dragage
#' @keywords stresseurs
#'
#' @source Communication personnelle Marie-Ève Lemieux (marie-eve.lemieux@portquebec.ca)
#'
#' @export
#'
#' @details Cette fonction formatte les données
#'

get_data0052 <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  #
  # --------------------
  message("Data 0052: Les informations ont été fournies par courriel par Marie-Ève Lemieux du Port de Québec. Voir commentaires dans le script `data0052-port_quebec.R` pour les détails de la communication.")
  #
  # Communication avec Marie-Ève Lemieux (marie-eve.lemieux@portquebec.ca)
  # NOTE: Uniquement les portions de communications pertinentes sont rapportées.
  #
  # 2021-09-09
  # > Pour ce qui est des dragages d’entretien, les volumes sont minimes, soit de
  #   l’ordre de 300 m3 aux 2 ans environ, dans les secteurs de Beauport et de
  #   l’Anse au Foulon.
  #
  # 2021-09-16
  # > On parle bien des 2 secteurs pour le 300 m³ et cela comprend le dragage d’entretien et
  #   les projets d’approfondissement, car s’il était strictement question de dragage d’entretien
  #   le volume serait davantage de l’ordre de 100m³ pour les deux secteurs.
  #
  # Nour utilisons donc un dragage de 50m3 par secteur à tous les deux ans entre 2001 et 2020
  # pour les secteur Beauport et de l'Anse au Foulons. Cette période est sélectionnée pour
  # bien fonctionner avec les autres données de dragage utilisées pour la caractérisation
  # de la zone d'étude.
  # _________________________________________________________________________ #


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Import data & general parameters
  # ----------------------------------------
  # Output folder
  output <- "data0052-port_quebec/"
  folder <- paste0("./data/data-raw/", output)
  if (!file.exists(folder)) dir.create(folder)

  # data
  volume <- 50
  annee <- seq(2001,2020,by = 2)

  # Anse au Foulon
  dat <- data.frame(municipalite = "Québec",
                    name = "Port de Québec - Secteur Anse au Foulon",
                    annee = annee,
                    volume = volume)

  # Dragage
  coords <- rbind(
    c(-71.22605, 46.79086),
    c(-71.22656, 46.79231),
    c(-71.21436, 46.79824),
    c(-71.21418, 46.79794),
    c(-71.22599, 46.79225),
    c(-71.22558, 46.79090),
    c(-71.22605, 46.79086)
  )

  # -----
  foulon <- list(coords) %>%
              st_polygon() %>%
              st_sfc(crs = 4326) %>%
              st_sf(dat, row.names = "")


  # Beauport
  dat <- data.frame(municipalite = "Québec",
                    name = "Port de Québec - Secteur Beauport",
                    annee = annee,
                    volume = volume)

  # Dragage
  coords <- rbind(
    c(-71.20541, 46.82676),
    c(-71.19317, 46.83213),
    c(-71.19290, 46.83172),
    c(-71.20514, 46.82637),
    c(-71.20541, 46.82676)
  )

  # -----
  beauport <- list(coords) %>%
              st_polygon() %>%
              st_sfc(crs = 4326) %>%
              st_sf(dat, row.names = "")

  # -----
  data0052 <- rbind(foulon, beauport) %>%
              st_transform(crs = global_parameters()$crs)


  # _________________________________________________________________________ #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ----------------------------------------
  # Also output as raw data
  st_write(obj = data0052,
           dsn = "./data/data-raw/data0052-port_quebec/port_quebec.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)


  # Output
  st_write(obj = data0052,
           dsn = "./data/data-format/data0052-port_quebec.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # _________________________________________________________________________ #
}
