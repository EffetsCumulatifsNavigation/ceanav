#' Figures données formatées
#'
#' Fonctions pour générer des figures pour les données formatées
#'
#' @keywords figure
#'
#' @rdname fig_format
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_format(data_id = "data0001")
#'
#' # Render all figures for available data
#' fig_format_all()
#'
#' # Update figures based on available data
#' fig_format_update()


fig_format <- function(data_id) {
  # Data and libraries
  load_format(data_id)
  dat <- get(data_id)

  # -----------------
  if (class(dat)[1] == "sf") {
    dat <- st_transform(dat, global_parameters()$crs)

    # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
    png(glue('./figures/figures-format/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_format(dat)
    dev.off()
  }

  # -----------------
  else if (class(dat) == "stars") {
    png(glue('./figures/figures-format/{data_id}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_format(dat)
    dev.off()
  }
}


#' @rdname fig_format
#' @aliases fig_format_all
#' @export
fig_format_all <- function() {
  dataname <- dir("./data/data-format/") %>% substring(1,8)
  for(i in dataname) fig_format(data_id = i)
}


#' @rdname fig_format
#' @aliases fig_format_update
#' @export
fig_format_update <- function() {
  fig_done <- dir("./figures/figures-format/") %>% substring(1,8)
  data_available <- dir("./data/data-format/") %>% substring(1,8)
  dataname <- data_available[!data_available %in% fig_done]
  for(i in dataname) fig_format(data_id = i)
}
