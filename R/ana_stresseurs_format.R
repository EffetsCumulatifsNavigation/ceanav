#' Transformation des données sur les stresseurs environnementaux
#'
#' Transformation des données sur les stresseurs environnementaux
#'
#' @keywords transformation
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction effectue transforme les données sur les stresseurs environnementaux pour les préparer pour l'évaluation des effets cumualatifs
#'

ana_data_transformation <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # Pour combiner les données de stresseurs et obtenir une évaluation des effets
  # cumulatifs relatives, il est nécessaire de normaliser leur intensité entre
  # 0 et 1 pour s'assurer que les données soient comparables d'une stresseur
  # à l'autre malgré des unités brutes différentes.
  #
  # On débute par une tranformation logarithmique pour les données dont la
  # distribution n'est pas normale, puis on normalise les données entre 0 et 1
  # en utilisant le 99e percentile comme valeur maximale pour la normalisation.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  load_integrated("stresseurs")

  
  # -----
  # Log transformation
  # Cette étape nécessite une visualisation initiale des données afin d'identifier
  # quelles nécessitent une transformation logarithmique


  # -----
  # Normalisation


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = stresseurs_format,
           dsn = "./data/data-output/stresseurs_format.geojson",
           delete_dsn = TRUE)
  # ------------------------------------------------------------------------- #}

  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Clean global environment
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  clean()
  # ------------------------------------------------------------------------- #}
}
