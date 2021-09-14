#' Figures données intégrées
#'
#' Fonctions pour générer des figures pour les données intégrées
#'
#' @keywords figure
#'
#' @rdname fig_integrated
#'
#' @export
#'
#' @examples
#' # Figure for specific dataset
#' fig_integrated(data_id = "mammiferes_marins")
#'
#' # Render all figures for available data
#' fig_integrated_all()
#'
#' # Update figures based on available data
#' fig_integrated_update()


fig_integrated <- function(data_id) {
  # Integrated data
  load_integrated(data_id)
  dat <- get(data_id)
  nm <- colnames(dat)[-ncol(dat)] # remove geometry column name

  # Metadata
  # meta <- load_metadata(data_id)

  for(i in nm) {
    # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
    png(glue('./figures/figures-integrated/{data_id}-{i}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_ceanav(
      dat[, i],
      main = data_id,
      subtitle = i,
      unit_data = "essai"
    )
    dev.off()
  }
}


#' @rdname fig_integrated
#' @aliases fig_integrated_all
#' @export
fig_integrated_all <- function() {
  dataname <- dir("./data/data-integrated/") %>%
              gsub("cv_","",.) %>%
              gsub("st_","",.) %>%
              gsub(".geojson","",.)
  for(i in dataname) fig_integrated(data_id = i)
}
