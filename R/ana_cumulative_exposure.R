#' Exposition cumulée
#'
#' Évaluation de l'exposition cumulée des composantes valorisées aux stresseurs environnementaux
#'
#' @keywords exposition cumulée
#' @keywords stresseurs
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_exposure <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # L'évaluation de l'exposition cumulée fournit une évaluation des milieux
  # où il y a un chevauchement plus important entre les composantes valorisées
  # et les stresseurs environnementaux. Bien que cette étape n'établisse pas une
  # prédiction de l'effets des stresseurs sur les composantes valorisées, qui
  # qui nécessiterait une évaluation de la sensibilité des composantes valorisées
  # aux stresseurs, elle permet tout de même d'identifier les milieux où les
  # composantes valorisées sont le plus susceptibles d'être soumises aux effets
  # des stresseurs environnementaux considérés.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # -----
  load_output("cumulative_stresseurs")
  load_output("cumulative_composantes_valorisees")
  st <- st_drop_geometry(cumulative_stresseurs)
  cv <- st_drop_geometry(cumulative_composantes_valorisees)

  # -----
  cumulative_exposure <- st$cumulative_st * cv$cumulative_cv
  cumulative_exposure_norm <- st$cumulative_st_norm * cv$cumulative_cv_norm
  cumulative_exposure_berge <- st$cumulative_st_norm * cv$cumulative_cv_berge
  cumulative_exposure_habitat <- st$cumulative_st_norm * cv$cumulative_cv_habitat
  cumulative_exposure_mammiferes_marins <- st$cumulative_st_norm * cv$cumulative_cv_mammiferes_marins
  cumulative_exposure_site <- st$cumulative_st_norm * cv$cumulative_cv_site

  # -----
  cumulative_exposure <- cbind(
    grid1p,
    cumulative_exposure,
    cumulative_exposure_norm,
    cumulative_exposure_berge,
    cumulative_exposure_habitat,
    cumulative_exposure_mammiferes_marins,
    cumulative_exposure_site
  )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = cumulative_exposure,
           dsn = "./data/data-output/cumulative_exposure.geojson",
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
