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



fig_cumulative_composantes_valorisees <- function(lang = "fr") {
  # ------------------
  global_parameters()

  # output
  if (lang == "fr") {
    output <- glue('./figures/figures-output/')
  } else if (lang == "en") {
    output <- glue('./figures_en/figures-output/')
  }
  
  # Function to cycle through elements to plot
  temp <- function(dat, data_id, main = "", subtitle = "") {
    png(glue("{output}{data_id}.png"), res = global_param$figures$resolution, width = global_param$figures$width, height = global_param$figures$height, units = "mm", pointsize = global_param$figures$pointsize)
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
  if (lang == "fr") {
    main <- c("Composantes valorisées cumulées",
              "Composantes valorisées cumulées normalisées",
              "Intégrité des berges cumulée",
              "Habitats cumulés",
              "Mammifères marins cumulés",
              "Sites d'intérêt cumulés")    
  } else if (lang == "en") {
    main <- c("Cumulative valued components",
              "Cumulative valued components normalized",
              "Cumulative bank integrity",
              "Cumulative habitats",
              "Cumulative marine mammals",
              "Cumulative areas of interest")
  }

  # -----
  if (lang == "fr") {
    subtitle <- c(
      "Somme des composantes valorisées individuelles",
      "Somme des composantes valorisées individuelles divisée\npar le nombre de catégories",
      "Somme des catégories de berges",
      "Somme  des types d'habitats",
      "Somme de la présence de mammifères marins",
      "Somme des sites d'intérêt"
    )
  } else if (lang == "en") {
    subtitle <- c(
      "Sum of individual valued components",
      "Sum of individual valued components divided\nby the number of categories",
      "Sum of bank integrity categories",
      "Sum of habitat categories",
      "Sum of marine mammals presence",
      "Sum of areas of interest categories"
    )
  }
  

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])

  # -----
  if (lang == "fr") {
    # Stack individual figures using magick package
    i1 <- magick::image_read("figures/figures-output/cumulative_cv_berge.png")
    i2 <- magick::image_read("figures/figures-output/cumulative_cv_habitat.png")
    i3 <- magick::image_read("figures/figures-output/cumulative_cv_mammiferes_marins.png")
    i4 <- magick::image_read("figures/figures-output/cumulative_cv_site.png")

    l1 <- image_append(c(i1,i2))
    l2 <- image_append(c(i3,i4))

    img <- image_append(c(l1,l2), stack = TRUE)
    magick::image_write(img, path = "./figures/figures-output/cumulative_cv_panel.png", format = "png")
  } else if (lang == "en") {
    # Stack individual figures using magick package
    i1 <- magick::image_read("figures_en/figures-output/cumulative_cv_berge.png")
    i2 <- magick::image_read("figures_en/figures-output/cumulative_cv_habitat.png")
    i3 <- magick::image_read("figures_en/figures-output/cumulative_cv_mammiferes_marins.png")
    i4 <- magick::image_read("figures_en/figures-output/cumulative_cv_site.png")

    l1 <- image_append(c(i1,i2))
    l2 <- image_append(c(i3,i4))

    img <- image_append(c(l1,l2), stack = TRUE)
    magick::image_write(img, path = "./figures_en/figures-output/cumulative_cv_panel.png", format = "png")
  }
}
