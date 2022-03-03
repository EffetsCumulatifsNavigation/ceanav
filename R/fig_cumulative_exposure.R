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

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read("figures/figures-output/cumulative_exposure_berge.png")
  i2 <- magick::image_read("figures/figures-output/cumulative_exposure_habitat.png")
  i3 <- magick::image_read("figures/figures-output/cumulative_exposure_mammiferes_marins.png")
  i4 <- magick::image_read("figures/figures-output/cumulative_exposure_site.png")

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))

  img <- image_append(c(l1,l2), stack = TRUE)
  magick::image_write(img, path = "./figures/figures-output/cumulative_exposure_panel.png", format = "png")

}
