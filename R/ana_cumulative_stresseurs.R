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

ana_cumulative_stresseurs <- function() {
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
  pollution_maritime <- stressor_cumul(dr, "pollution_maritime", normaliser = TRUE)

  # -----
  cumulative_st <- cumulativeFootprint(dr)

  # -----
  cumulative_st_norm <- ancrage + deversement + dragage + navigation + peche_commerciale + pollution_maritime

  # -----
  cumulative_st_ancrage <- stressor_cumul(dr, "ancrage")
  cumulative_st_deversement <- stressor_cumul(dr, "deversement")
  cumulative_st_dragage <- stressor_cumul(dr, "dragage")
  cumulative_st_navigation <- stressor_cumul(dr, "navigation")
  cumulative_st_peche_commerciale <- stressor_cumul(dr, "peche_commerciale")
  cumulative_st_pollution_maritime <- stressor_cumul(dr, "pollution_maritime")

  # -----
  cumulative_stresseurs <- cbind(
    grid1p,
    cumulative_st,
    cumulative_st_norm,
    cumulative_st_ancrage,
    cumulative_st_deversement,
    cumulative_st_dragage,
    cumulative_st_navigation,
    cumulative_st_peche_commerciale,
    cumulative_st_pollution_maritime
   )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = cumulative_stresseurs,
           dsn = "./data/data-output/cumulative_stresseurs.geojson",
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
