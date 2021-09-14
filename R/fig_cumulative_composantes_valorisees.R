#' Figures composantes valorisées cumulés
#'
#' Fonctions pour générer des figures pour les composantes_valorisees cumulés
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' fig_cumulative_composantes_valorisees()



fig_cumulative_composantes_valorisees <- function() {
  # -----
  data_id <- "cumulative_composantes_valorisees"
  load_output("cumulative_composantes_valorisees")
  dat <- cumulative_composantes_valorisees

  # -----
  png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_ceanav(
    dat[, data_id],
    main = "Composantes valorisées cumulés",
    subtitle = "Somme des composantes valorisées individuelles",
    unit_data = ""
  )
  dev.off()

  # -----
  data_id <- "cumulative_composantes_valorisees_norm"
  png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_ceanav(
    dat[, data_id],
    main = "Composantes valorisées cumulées normalisées",
    subtitle = "Somme des composantes valorisées individuelles divisée\npar le nombre de catégories",
    unit_data = ""
  )
  dev.off()
}
