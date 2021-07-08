#' Figures données formatées
#'
#' Fonctions pour générer des figures pour les données formatées
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' rep_data_figures(data_id = "data0001")
#'
#' # Render all figures for available data
#' rep_data_figures_all()
#'
#' # Update figures based on available data
#' rep_data_figures_update()


rep_data_figures <- function(data_id) {
  # Data and libraries
  ceanav_load_data(data_id)
  dat <- get(data_id)

  # -----------------
  if (class(dat)[1] == "sf") {
    dat <- st_transform(dat, global_parameters()$crs)

    # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
    png(glue('./figures/figures-format/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    ceanav_plot(dat)
    dev.off()
  }

  # -----------------
  else if (class(dat) == "stars") {
    png(glue('./figures/figures-format/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    ceanav_plot(dat)
    dev.off()
  }
}


#' @rdname rep_data_figures
#' @aliases rep_data_figures_all
#' @export
rep_data_figures_all <- function() {
  dataname <- dir("./data/data-format/") %>% substring(1,8)
  for(i in dataname) rep_data_figures(data_id = i)
}


#' @rdname rep_data_figures
#' @aliases rep_data_figures_update
#' @export
rep_data_figures_update <- function() {
  fig_done <- dir("./figures/figures-format/") %>% substring(1,8)
  data_available <- dir("./data/data-format/") %>% substring(1,8)
  dataname <- data_available[!data_available %in% fig_done]
  for(i in dataname) rep_data_figures(data_id = i)
}
