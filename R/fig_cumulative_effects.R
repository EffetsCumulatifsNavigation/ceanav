#' Figures effets cumulatifs0
#'
#' Fonctions pour générer des figures pour les effets cumulatifs
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' fig_cumulative_effects()



fig_cumulative_effects <- function() {

  # Function to cycle through elements to plot
  temp <- function(dat, data_id, main = "", subtitle = "") {
    png(glue('./figures/figures-output/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_ceanav(
      dat[, data_id],
      main = main,
      subtitle = subtitle,
      unit_data = ""
    )
    dev.off()
  }

  # -----
  load_output("cumulative_effects")
  dat <- cumulative_effects

  # -----
  data_id <- c(
    "cumulative_effects"
  )

  # -----
  main <- c(
    "Effets cumulatifs"
  )

  # -----
  subtitle <- c(
    "Stresseurs * composantes valorisées cumulés * vulnérabilité"
  )

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])
}
