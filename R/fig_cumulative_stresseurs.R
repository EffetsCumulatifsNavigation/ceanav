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
    "Naufrage cumulés",
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

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read("figures/figures-output/cumulative_st_ancrage.png")
  i2 <- magick::image_read("figures/figures-output/cumulative_st_deversement.png")
  i3 <- magick::image_read("figures/figures-output/cumulative_st_dragage.png")
  i4 <- magick::image_read("figures/figures-output/cumulative_st_naufrage.png")
  i5 <- magick::image_read("figures/figures-output/cumulative_st_navigation.png")
  i6 <- magick::image_read("figures/figures-output/cumulative_st_peche_commerciale.png")
  i7 <- magick::image_read("figures/figures-output/cumulative_st_pollution_maritime.png")

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))
  l3 <- image_append(c(i5,i6))
  l4 <- i7

  img <- image_append(c(l1,l2,l3,l4), stack = TRUE)
  magick::image_write(img, path = "./figures/figures-output/cumulative_st_panel.png", format = "png")

}
