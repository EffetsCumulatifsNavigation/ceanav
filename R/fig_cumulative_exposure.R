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
  load_output("cumulative_exposure")
  dat <- cumulative_exposure

  # -----
  data_id <- c(
    "cumulative_exposure",
    "cumulative_exposure_norm",
    "cumulative_exposure_berge",
    "cumulative_exposure_habitat",
    "cumulative_exposure_mammiferes_marins",
    "cumulative_exposure_site"
  )

  # -----
  main <- c(
    "Exposition cumulée",
    "Exposition cumulée normalisée",
    "Exposition cumulée des berges",
    "Exposition cumulée des habitats",
    "Exposition cumulés des mammifères marins",
    "Exposition cumulés des sites d'intérêt"
  )

  # -----
  subtitle <- c(
    "Stresseurs * composantes valorisées cumulés",
    "Stresseurs * composantes valorisées normalisés cumulés",
    "Stresseurs * berges cumulés",
    "Stresseurs * habitats cumulés",
    "Stresseurs * mammifères marins cumulés",
    "Stresseurs * sites d'intérêt cumulés"
  )

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])
}
