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



fig_cumulative_effects <- function(lang = "fr") {
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
    png(glue('{output}{data_id}.png'), res = global_param$figures$resolution, width = global_param$figures$width, height = global_param$figures$height, units = "mm", pointsize = global_param$figures$pointsize)
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

  data_id <- c(
    "cumulative_effects",
    "cumulative_effects_berge",
    "cumulative_effects_habitat",
    "cumulative_effects_mammiferes_marins",
    "cumulative_effects_site"
  )

  # -----
  if (lang == "fr") {
    main <- c(
      "Effets cumulatifs",
      "Effets cumulatifs des berges",
      "Effets cumulatifs des habitats",
      "Effets cumulatifs des mammifères marins",
      "Effets cumulatifs des sites d'intérêt"
    )
  } else if (lang == "en") {
    main <- c(
      "Cumulative effects",
      "Cumulative effects on banks",
      "Cumulative effects on habitats",
      "Cumulative effects on marine mammals",
      "Cumulative effects on sites of interest"
    )
  }  

  # -----
  if (lang == "fr") {
    subtitle <- c(
      "Stresseurs * composantes valorisées * vulnérabilité",
      "Stresseurs * berges * vulnérabilité",
      "Stresseurs * habitats * vulnérabilité",
      "Stresseurs * mammifères marins * vulnérabilité",
      "Stresseurs * sites d'intérêt * vulnérabilité"
    )
  } else if (lang == "en") {
    subtitle <- c(
      "Stressors * valued components * vulnerability",
      "Stressors * banks * vulnerability",
      "Stressors * habitats * vulnerability",
      "Stressors * marine mammals marins * vulnerability",
      "Stressors * sites of interest * vulnerability"
    )
  }

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i])

  # -----
  # Stack individual figures using magick package
  i1 <- magick::image_read(glue("{output}cumulative_effects_berge.png"))
  i2 <- magick::image_read(glue("{output}cumulative_effects_habitat.png"))
  i3 <- magick::image_read(glue("{output}cumulative_effects_mammiferes_marins.png"))
  i4 <- magick::image_read(glue("{output}cumulative_effects_site.png"))

  l1 <- image_append(c(i1,i2))
  l2 <- image_append(c(i3,i4))

  img <- image_append(c(l1,l2), stack = TRUE)
  magick::image_write(img, path = glue("{output}cumulative_effects_panel.png"), format = "png")

  # -----
  # Simple plot
  png(glue('./figures/figures-output/cumulative_effects_simple.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
  plot_simple(dat[, "cumulative_effects"])
  dev.off()


}
