#' Empreinte cumulée des stressurs environnementaux
#'
#' Évaluation de l'empreinte cumulée des stresseurs environnementaux considérés
#'
#' @keywords empreinte cumulée
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_stressors <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # L'évaluation de l'empreinte cumulée permet d'identifier les sites qui sont
  # le plus exposés aux effets potentiels des stresseurs environnementaux
  # considérés pour l'évaluation des effets cumulatifs. Elle ne fournit pas
  # d'évaluation des effets, puisqu'elle ne considère que les stresseurs,
  # leur intensité et leur distribution. Elle permet toutefois d'obtenir une
  # évaluation des milieux qui sont le plus propices d'être affectés par les
  # stresseurs dans notre zone d'étude.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # -----
  load_output("stresseurs_format")
  dr <- st_drop_geometry(stresseurs_format)

  # -----
  stressor_cumul <- function(x, stress, normaliser = FALSE) {
    uid <- str_detect(colnames(x), stress)
    dat <- cumulativeFootprint(x[,uid, drop = FALSE], normaliser)
    dat
  }

  # -----
  ancrage <- stressor_cumul(dr, "ancrage", normaliser = TRUE)
  deversement <- stressor_cumul(dr, "deversement", normaliser = TRUE)
  dragage <- stressor_cumul(dr, "dragage", normaliser = TRUE)
  navigation <- stressor_cumul(dr, "navigation", normaliser = TRUE)
  peche_commerciale <- stressor_cumul(dr, "peche_commerciale", normaliser = TRUE)
  port <- stressor_cumul(dr, "port", normaliser = TRUE)

  # -----
  cumulative_footprint <- cumulativeFootprint(dr)

  # -----
  cumulative_footprint_norm <- ancrage + deversement + dragage + navigation + peche_commerciale + port

  # -----
  cumulative_footprint <- cbind(
    grid1p,
    cumulative_footprint,
    cumulative_footprint_norm,
    ancrage,
    deversement,
    dragage,
    navigation,
    peche_commerciale,
    port
   )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = cumulative_footprint,
           dsn = "./data/data-output/cumulative_footprint.geojson",
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
