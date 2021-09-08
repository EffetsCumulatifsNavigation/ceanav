#' Composantes valorisées
#'
#' Base de données avec l'ensemble des composantes valorisées brutes
#'
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_composantes_valorisees_raw <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  data(grid1p)

  # -----
  load_integrated("berge")
  load_integrated("habitat")
  load_integrated("mammiferes_marins")
  load_integrated("site")


  # -----
  change_colnames <- function(x, suffix) {
    x <- st_drop_geometry(x)
    colnames(x) <- glue("{suffix}_{colnames(x)}")
    x
  }

  # -----
  berge <- change_colnames(berge, "berge")
  habitat <- change_colnames(habitat, "habitat")
  mammiferes_marins <- change_colnames(mammiferes_marins, "mammiferes_marins")
  site <- change_colnames(site, "site")


  # -----
  composantes_valorisees_raw <- cbind(
    grid1p,
    berge,
    habitat,
    mammiferes_marins,
    site
  )


  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Export data
  # ------------------------------------
  #
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # -----
  st_write(obj = composantes_valorisees_raw,
           dsn = "./data/data-output/composantes_valorisees_raw.geojson",
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
