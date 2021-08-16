#' Figures données intgrées
#'
#' Fonctions pour générer des figures pour les données intégrées
#'
#' @keywords figure
#'
#' @rdname rep_portrait_data_figures_int
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' rep_portrait_data_figures_int(data_id = "mammiferes_marins")
#'
#' # Render all figures for available data
#' rep_portrait_data_figures_int_all()
#'
#' # Update figures based on available data
#' rep_portrait_data_figures_int_update()


rep_portrait_data_figures_int <- function(data_id) {
  # Data and libraries
  load_integrated(data_id)
  dat <- get(data_id)
  nm <- colnames(dat)[-ncol(dat)] # remove geometry column name

  for(i in nm) {
    # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
    png(glue('./figures/figures-integrated/{data_id}-{i}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_integrated(dat[, i], main = data_id, subtitle = i)
    dev.off()
  }
}


#' @rdname rep_portrait_data_figures_int
#' @aliases rep_portrait_data_figures_int_all
#' @export
rep_portrait_data_figures_int_all <- function() {
  dataname <- dir("./data/data-integrated/") %>%
              gsub("cv_","",.) %>%
              gsub("st_","",.) %>%
              gsub(".geojson","",.)
  for(i in dataname) rep_portrait_data_figures_int(data_id = i)
}
