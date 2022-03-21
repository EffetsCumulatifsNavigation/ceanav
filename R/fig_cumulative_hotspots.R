#' Figures hotspots cumulés
#'
#' Fonctions pour générer des figures pour les hotspots cumulés
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' fig_cumulative_hotspots()



fig_cumulative_hotspots <- function(lang = "fr") {
  # ------------------
  global_parameters()

  # output
  if (lang == "fr") {
    output <- glue('./figures/figures-output/')
  } else if (lang == "en") {
    output <- glue('./figures_en/figures-output/')
  }

  # Function to cycle through elements to plot
  temp <- function(dat, data_id, main = "", subtitle = "", un = "") {
    png(glue('{output}{data_id}.png'), res = global_param$figures$resolution, width = global_param$figures$width, height = global_param$figures$height, units = "mm", pointsize = global_param$figures$pointsize)
    plot_ceanav(
      dat[, data_id],
      main = main,
      subtitle = subtitle,
      unit_data = un
    )
    dev.off()
  }

  # -----
  load_output("cumulative_hotspots")
  dat <- cumulative_hotspots

  # -----
  data_id <- "cumulative_hotspots"

  # -----
  if (lang == "fr") {
    main <- "Hotspots cumulés"
  } else if (lang == "en") {
    main <- "Cumulative hotspots"
  }
  # -----
  if (lang == "fr") {
    subtitle <- "80e quantile"
  } else if (lang == "en") {
    subtitle <- "80th quantile"
  }
  
  # -----
  if (lang == "fr") {
    unt <- "Nombre de stresseurs"
  } else if (lang == "en") {
    unt <- "Number of stressors"
  }

  # -----
  for(i in 1:length(data_id)) temp(dat, data_id[i], main[i], subtitle[i], un = unt)
}
