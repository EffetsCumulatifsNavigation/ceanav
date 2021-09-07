#' Empreinte cumulée
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

ana_cumulative_footprint <- function() {
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
  load_output("stresseurs_format")

  # -----
  cumulative_footprint <- cumul_foot(stresseurs_format)
  


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = cumulative_footprint,
           dsn = "./data/data-output/cumulative_footprint.geojson",
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
