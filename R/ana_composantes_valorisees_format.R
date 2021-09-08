#' Transformation des données sur les composantes valorisées
#'
#' Transformation des données sur les composantes valorisées
#'
#' @keywords transformation
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue transforme les données sur les composantes valorisées pour les préparer pour l'évaluation des effets cumualatifs
#'

ana_composantes_valorisees_format <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # En théorie, les composantes valorisées sont déjà formatées puisqu'elles sont
  # majoritairement des variables binaires. Par contre, les données de mammifères
  # marins sont des variables continues, alors nous les normalisons entre 0 et 1
  # pour les anlayses.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  load_output("composantes_valorisees_raw")
  data(grid1p)

  # -----
  nm <- colnames(composantes_valorisees_raw) %>%
        str_detect("mammiferes_marins") %>%
        which()

  # -----
  cv <- st_drop_geometry(composantes_valorisees_raw) %>%
        .[, nm] %>%
        apply(2, quantNorm)

  # -----
  composantes_valorisees_format <- composantes_valorisees_raw
  composantes_valorisees_format[, nm] <- cv


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = composantes_valorisees_format,
           dsn = "./data/data-output/composantes_valorisees_format.geojson",
           delete_dsn = TRUE,
           quiet = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
