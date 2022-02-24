#' Figures hotspots cumulés
#'
#' Fonctions pour générer des figures pour les hotspots cumulés
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' fig_cumulative_hotspots()



fig_cumulative_hotspots <- function() {

  # Function to cycle through elements to plot
  temp <- function(dat, data_id, main = "", subtitle = "") {
    png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_ceanav(
      dat[, data_id],
      main = main,
      subtitle = subtitle,
      unit_data = "Nombre de stresseurs"
    )
    dev.off()
  }

  # -----
  load_output("cumulative_hotspots")
  dat <- cumulative_hotspots

  # -----
  data_id <- "cumulative_hotspots"

  # -----
  main <- "Hotspots cumulés"

  # -----
  subtitle <- "80e quantile"

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])
}
