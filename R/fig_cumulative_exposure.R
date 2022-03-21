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



fig_cumulative_exposure <- function(lang = "fr") {
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
  if (lang == "fr") {
    main <- c(
      "Exposition cumulée",
      "Exposition cumulée normalisée",
      "Exposition cumulée des berges",
      "Exposition cumulée des habitats",
      "Exposition cumulée des mammifères marins",
      "Exposition cumulée des sites d'intérêt"
    )
  } else if (lang == "en") {
    main <- c(
      "Cumulative exposure",
      "Cumulative exposure normalized",
      "Cumulative exposure of banks",
      "Cumulative exposure of habitats",
      "Cumulative exposure of marine mammals",
      "Cumulative exposure of areas of interest"
    )
  }
  
  # -----
  if (lang == "fr") {
    subtitle <- c(
      "Stresseurs * composantes valorisées cumulés",
      "Stresseurs * composantes valorisées normalisés cumulés",
      "Stresseurs * berges cumulés",
      "Stresseurs * habitats cumulés",
      "Stresseurs * mammifères marins cumulés",
      "Stresseurs * sites d'intérêt cumulés"
    )
  } else if (lang == "en") {
    subtitle <- c(
      "Stressors * cumulative valued components",
      "Stressors * cumulative valued components normalized",
      "Stressors * cumulative bank",
      "Stressors * cumulative habitats",
      "Stressors * cumulative marine mammals",
      "Stressors * cumulative areas of interest"
    )  
  }

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read(glue("{output}cumulative_exposure_berge.png"))
  i2 <- magick::image_read(glue("{output}cumulative_exposure_habitat.png"))
  i3 <- magick::image_read(glue("{output}cumulative_exposure_mammiferes_marins.png"))
  i4 <- magick::image_read(glue("{output}cumulative_exposure_site.png"))

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))

  img <- image_append(c(l1,l2), stack = TRUE)
  magick::image_write(img, path = glue("{output}cumulative_exposure_panel.png"), format = "png")

}
