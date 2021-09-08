#' Empreinte cumulée des composantes valorisées
#'
#' Évaluation de l'empreinte cumulée des composantes valorisées considérés
#'
#' @keywords empreinte cumulée
#' @keywords composantes valorisées
#'
#' @export
#'
#' @details Cette fonction effectue une partie des analyses du projet d'évaluation des effets cumulatifs
#'

ana_cumulative_composantes_valorisees <- function() {
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
  # Notes
  # ------------------------------------
  #
  # L'évaluation de l'empreinte cumulée permet d'identifier les sites qui sont
  # le plus importants en terme de présentes de composantes valorisées. Elle ne
  # fournit pas d'évaluation des effets, puisqu'elle ne considère que les
  # composantes valorisées. Elle permet toutefois d'obtenir une évaluation des
  # milieux qui sont les plus importants pour les composantes valorisées dans
  # la zone d'étude.
  # =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

  # -----
  data(grid1p)

  # -----
  load_output("composantes_valorisees_format")
  cv <- st_drop_geometry(composantes_valorisees_format)

  # -----
  cv_cumul <- function(x, cv, normaliser = FALSE) {
    uid <- str_detect(colnames(x), cv)
    dat <- cumulativeFootprint(x[,uid, drop = FALSE], normaliser)
    dat
  }

  # -----
  berge <- cv_cumul(cv, "berge", normaliser = TRUE)
  habitat <- cv_cumul(cv, "habitat", normaliser = TRUE)
  mammiferes_marins <- cv_cumul(cv, "mammiferes_marins", normaliser = TRUE)
  site <- cv_cumul(cv, "site", normaliser = TRUE)

  # -----
  cumulative_composantes_valorisees <- cumulativeFootprint(cv)

  # -----
  cumulative_composantes_valorisees_norm <- berge + habitat + mammiferes_marins + site

  # -----
  cumulative_composantes_valorisees <- cbind(
    grid1p,
    cumulative_composantes_valorisees,
    cumulative_composantes_valorisees_norm,
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
  st_write(obj = cumulative_composantes_valorisees,
           dsn = "./data/data-output/cumulative_composantes_valorisees.geojson",
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
