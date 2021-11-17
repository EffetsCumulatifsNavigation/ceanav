#' Stresseurs
#'
#' Base de données avec l'ensemble des stresseurs environnementaux
#'
#' @keywords empreinte cumulée
#' @keywords stresseurs
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_stresseurs_raw <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  data(grid1p)

  # -----
  load_integrated("ancrage")
  load_integrated("deversement")
  load_integrated("dragage")
  load_integrated("navigation")
  load_integrated("peche_commerciale")
  load_integrated("pollution_maritime")

  # -----
  change_colnames <- function(x, suffix) {
    x <- st_drop_geometry(x)
    colnames(x) <- glue("{suffix}_{colnames(x)}")
    x
  }

  # -----
  ancrage <- change_colnames(ancrage, "ancrage")
  deversement <- change_colnames(deversement, "deversement")
  dragage <- change_colnames(dragage, "dragage")
  peche_commerciale <- change_colnames(peche_commerciale, "peche_commerciale")
  navigation <- change_colnames(navigation, "navigation")
  pollution_maritime <- change_colnames(pollution_maritime, "pollution_maritime")

  # -----
  stresseurs_raw <- cbind(
    grid1p,
    ancrage,
    deversement,
    dragage,
    peche_commerciale,
    navigation,
    pollution_maritime
  )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = stresseurs_raw,
           dsn = "./data/data-output/stresseurs_raw.geojson",
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
