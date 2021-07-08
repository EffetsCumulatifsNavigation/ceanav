#' Figures données formatées
#'
#' Fonctions pour générer des figures pour les données formatées
#'
#' @keywords figure
#'
#' @export
#'
#' @examples
#' rep_data_figures(data_id = "data0001")

rep_data_summary <- function(data_id) {
  # Data and libraries
  ceanav_load_data(data_id)
  dat <- get(data_id) %>%
         st_transform(global_parameters()$crs)
  data(aoi)


  # Simple plot
  pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
  par(mar = c(0, 0, 0, 0), family = 'serif')
  # plot0(x = c(-1.3,1.5), y = c(-1,1))
  plot(st_geometry(aoi), lwd = .5, border = global_parameters()$col$coastline)
  plot(
    st_geometry(dat), lwd = 1.25, add = TRUE,
    col = global_parameters()$col$palette[1],
    border = global_parameters()$col$palette[1]
  )
  dev.off()
}
