#' Pollution maritime
#'
#' Couche de données transformées pour la pollution maritime dans le Saint-Laurent
#'
#' @keywords ancrage
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction importe et formatte les données pour l'analyse d'effets cumulatifs
#'

st_pollution_maritime <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Format data
  # ------------------------------------
  #
  # Approach:
  # ---------
  # La caractérisation de la pollution maritime correspond au risque de pollution associé aux activités de navigation et de la pollution qui en résulte dans la zone d'étude via des rejets opérationnels, des fuites de gaz, la décharge de déchets, etc. En l'absence de données exhaustive sur les rejets opérationnels et de la difficulté d'établir un lien direct entre la qualité de l'eau et les activités maritimes cumulées, l'indice de pollution maritime nous sert de proxy visant à identifier les milieux les plus à risque de pollution issues des activités maritimes.
  #
  # L'indice de pollution maritime est obtenu en combinant l'intensité relative de tous les stresseurs associés directement à une activités de navigation, *i.e.* les sites d'ancrage, les activités de dragage, la navigation et la pêche commerciale.

  # Cette évaluation correspond à l'exposition cumulée normalisée pour les sources de stress sélectionnées (voir section X)

  # $$\sum_{S} \sum_{s \in S} \frac{1}{|S|} I_{s,x}$$

  # <!--TODO: ajouter nom de section -->

  # Les déversements accidentels n'y sont pas inclus puisqu'ils décrivent déjà explicitement des problématiques de déversements toxiques dans l'environnement plutôt qu'un stresseur relié à une activités de navigation.
  #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Load data & metadata
  load_integrated("ancrage")
  load_integrated("dragage")
  load_integrated("navigation")
  load_integrated("peche_commerciale")
  data(grid1p)

  # Remove geometries
  ancrage <- st_drop_geometry(ancrage)
  dragage <- st_drop_geometry(dragage)
  navigation <- st_drop_geometry(navigation)
  peche_commerciale <- st_drop_geometry(peche_commerciale)

  # Normaliser
  ancrage <- apply(ancrage, 2, FUN = function(x) x / max(x, na.rm = TRUE))
  dragage <- apply(dragage, 2, FUN = function(x) x / max(x, na.rm = TRUE))
  navigation <- apply(navigation, 2, FUN = function(x) x / max(x, na.rm = TRUE))
  peche_commerciale <- apply(peche_commerciale, 2, FUN = function(x) x / max(x, na.rm = TRUE))

  # Cumul stresseurs individuels
  ancrage <- cumulativeFootprint(ancrage, normaliser = TRUE)
  dragage <- cumulativeFootprint(dragage, normaliser = TRUE)
  navigation <- cumulativeFootprint(navigation, normaliser = TRUE)
  peche_commerciale <- cumulativeFootprint(peche_commerciale, normaliser = TRUE)

  # -----
  pollution_maritime <- dragage + ancrage + navigation + peche_commerciale

  # -----
  pollution_maritime <- grid1p %>%
                      mutate(pollution_maritime = pollution_maritime)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Update metadata
  # ----------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  meta <- read_yaml("./data/data-metadata/int_st_pollution_maritime.yml")
  meta$dataDescription$spatial$extent <- st_bbox(pollution_maritime)

  # ----
  anc <- read_yaml("./data/data-metadata/int_st_ancrage.yml")
  dra <- read_yaml("./data/data-metadata/int_st_dragage.yml")
  nav <- read_yaml("./data/data-metadata/int_st_navigation.yml")
  pec <- read_yaml("./data/data-metadata/int_st_peche_commerciale.yml")
  meta$rawData <- sort(unique(c(anc$rawData, dra$rawData, nav$rawData, pec$rawData)))
  # -----
  meta$dataDescription$categories$accronyme <-  "pollution_maritime"
  meta$dataDescription$categories$francais <-  "Pollution maritime"
  meta$dataDescription$categories$source <-  meta$rawData
  meta$dataDescription$categories$description <- "Pollution reliée aux activités de navigation tels la navigation commerciale et la pêche. Les stresseurs associés peuvent être des rejets opérationnels, des déchets, des débris, de l'eau de ballast, etc."

  # --- For proper referencing in markdown syntax
  meta$dataDescription$categories$mdref <- modif_md(meta$dataDescription$categories$accronyme)
  # ------------------------------------------------------------------------- #

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  write_yaml(meta, "./data/data-metadata/int_st_pollution_maritime.yml")

  # -----
  st_write(obj = pollution_maritime,
           dsn = "./data/data-integrated/st_pollution_maritime.geojson",
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
