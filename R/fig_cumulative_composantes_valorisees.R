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
  # ------------------
  global_parameters()

  # Function to cycle through elements to plot
  temp <- function(dat, data_id, main = "", subtitle = "") {
    png(glue('./figures/figures-output/{data_id}.png'), res = global_param$figures$resolution, width = global_param$figures$width, height = global_param$figures$height, units = "mm", pointsize = global_param$figures$pointsize)
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

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read("figures/figures-output/cumulative_cv_berge.png")
  i2 <- magick::image_read("figures/figures-output/cumulative_cv_habitat.png")
  i3 <- magick::image_read("figures/figures-output/cumulative_cv_mammiferes_marins.png")
  i4 <- magick::image_read("figures/figures-output/cumulative_cv_site.png")

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))

  img <- image_append(c(l1,l2), stack = TRUE)
  magick::image_write(img, path = "./figures/figures-output/cumulative_cv_panel.png", format = "png")

}
