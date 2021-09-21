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

  # Metadata
  meta_id <- data_id
  st <- c("ancrage","deversement","dragage","naufrage","navigation","peche_commerciale","rejet")
  cv <- c("berge","habitat","mammiferes_marins","site")
  if (meta_id %in% st) meta_id <- glue("int_st_{meta_id}")
  if (meta_id %in% cv) meta_id <- glue("int_cv_{meta_id}")
  meta <- load_metadata(meta_id)

  # Titles
  main <- meta$title
  nm <- meta$dataDescription$categories$accronyme
  subt <- meta$dataDescription$categories$francais
  ref <- meta$dataDescription$categories$source
  un <- meta$dataDescription$units

  for(i in 1:length(nm)) {
    # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
    png(glue('./figures/figures-integrated/{data_id}-{nm[i]}.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)
    plot_ceanav(
      dat[, nm[i]],
      main = main,
      subtitle = subt[i],
      unit_data = un,
      references = ref[i]
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
