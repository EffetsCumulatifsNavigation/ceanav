#' Figures exposition cumul/e
#'
#' Fonctions pour générer des figures pour l'exposition cumulée
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' fig_cumulative_exposure()



fig_cumulative_exposure <- function() {
  # -----
  data_id <- "cumulative_exposure"
  load_output("cumulative_exposure")
  dat <- cumulative_exposure

  # -----
  png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_ceanav(
    dat[, data_id],
    main = "Exposition cumulée",
    subtitle = "Stresseurs * composantes valorisées cumulés",
    unit_data = ""
  )
  dev.off()

  # -----
  data_id <- "cumulative_exposure_norm"
  png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_ceanav(
    dat[, data_id],
    main = "Exposition cumulée normalisée",
    subtitle = "Stresseurs * composantes valorisées normalisés cumulés",
    unit_data = ""
  )
  dev.off()
}
