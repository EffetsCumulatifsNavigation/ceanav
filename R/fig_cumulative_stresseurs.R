#' Figures stresseurs cumulés
#'
#' Fonctions pour générer des figures pour les stresseurs cumulés
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' fig_cumulative_stresseurs()



fig_cumulative_stresseurs <- function() {
  # -----
  data_id <- "cumulative_stresseurs"
  load_output("cumulative_stresseurs")
  dat <- cumulative_stresseurs

  # -----
  png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_ceanav(
    dat[, data_id],
    main = "Stresseurs cumulés",
    subtitle = "Somme des stresseurs individuels",
    unit_data = ""
  )
  dev.off()

  # -----
  data_id <- "cumulative_stresseurs_norm"
  png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_ceanav(
    dat[, data_id],
    main = "Stresseurs cumulés normalisés",
    subtitle = "Somme des stresseurs individuels divisé par le nombre de catégories",
    unit_data = ""
  )
  dev.off()




}
