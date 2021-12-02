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
  load_output("cumulative_stresseurs")
  dat <- cumulative_stresseurs

  # -----
  data_id <- c(
    "cumulative_st",
    "cumulative_st_norm",
    "cumulative_st_ancrage",
    "cumulative_st_deversement",
    "cumulative_st_dragage",
    "cumulative_st_naufrage",
    "cumulative_st_navigation",
    "cumulative_st_peche_commerciale",
    "cumulative_st_pollution_maritime"
  )

  # -----
  main <- c(
    "Stresseurs cumulés",
    "Stresseurs cumulés normalisés",
    "Ancrages cumulés",
    "Déversements accidentels cumulés",
    "Dragage cumulés",
    "Naufrage cumulés"
    "Navigation cumulée",
    "Pêche commerciale cumulée",
    "Pollution maritime cumulée"
  )

  # -----
  subtitle <- c(
    "Somme des stresseurs individuels",
    "Somme des stresseurs individuels divisée\npar le nombre de catégories",
    "Somme des catégories",
    "Somme des catégories",
    "Somme des catégories",
    "Somme des catégories",
    "Somme des catégories",
    "Somme des catégories",
    "Somme des catégories"
  )

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])
}
