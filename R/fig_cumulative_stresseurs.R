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



fig_cumulative_stresseurs <- function(lang = "fr") {
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
  if (lang == "fr") {
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
  } else if (lang == "en") {
    main <- c(
      "Cumulative stressors",
      "Cumulative stressors normalized",
      "Cumulative anchorages",
      "Cumulative accidental spills",
      "Cumulative dredging",
      "Cumulative shipwreck",
      "Cumulative shipping",
      "Cumulative commercial fishing",
      "Cumulative marine pollution"
    )
  }

  # -----
  if (lang == "fr") {
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
  } else if (lang == "en") {
    subtitle <- c(
      "Sum of individual stressors",
      "Sum of individual stressors divided\nby the number of categories",
      "Sum of categories",
      "Sum of categories",
      "Sum of categories",
      "Sum of categories",
      "Sum of categories",
      "Sum of categories",
      "Sum of categories"
    )
  }
  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read(glue("{output}cumulative_st_ancrage.png"))
  i2 <- magick::image_read(glue("{output}cumulative_st_deversement.png"))
  i3 <- magick::image_read(glue("{output}cumulative_st_dragage.png"))
  i4 <- magick::image_read(glue("{output}cumulative_st_naufrage.png"))
  i5 <- magick::image_read(glue("{output}cumulative_st_navigation.png"))
  i6 <- magick::image_read(glue("{output}cumulative_st_peche_commerciale.png"))
  i7 <- magick::image_read(glue("{output}cumulative_st_pollution_maritime.png"))

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))
  l3 <- image_append(c(i5,i6))
  l4 <- i7

  img <- image_append(c(l1,l2,l3,l4), stack = TRUE)
  magick::image_write(img, path = glue("{output}cumulative_st_panel.png"), format = "png")

}
