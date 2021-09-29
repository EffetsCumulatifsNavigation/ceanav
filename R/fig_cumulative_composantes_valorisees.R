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
  load_output("cumulative_composantes_valorisees")
  dat <- cumulative_composantes_valorisees


  # -----
  data_id <- c("cumulative_cv","cumulative_cv_norm","cumulative_cv_berge","cumulative_cv_habitat",
               "cumulative_cv_mammiferes_marins","cumulative_cv_site")

  # -----
  main <- c("Composantes valorisées cumulées",
            "Composantes valorisées cumulées normalisées",
            "Intégrité des berges cumulée",
            "Habitats cumulés",
            "Mammifères marins cumulés",
            "Sites d'intérêt cumulés")

  # -----
  subtitle <- c(
    "Somme des composantes valorisées individuelles",
    "Somme des composantes valorisées individuelles divisée\npar le nombre de catégories",
    "Somme des catégories de berges",
    "Somme  des types d'habitats",
    "Somme de la présence de mammifères marins",
    "Somme des sites d'intérêt"
  )

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])
}
