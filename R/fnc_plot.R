#' plot ceanav data
#'
#' base plot functions for ceanav project
#'
#' @param dat object of class sf, stars
#' @param ... further specifications, see \link{plot} and details.
#'
#'
#' @export

ceanav_plot <- function(dat, ...) {
  UseMethod("ceanav_plot", dat)
}

#' @method ceanav_plot sf
#' @name ceanav_plot
#'
#' @export
ceanav_plot.sf <- function(dat, ...) {
  # ------------------
  data(aoi)
  bbox <- global_parameters()$bbox$base

  # ------------------
  par(family = 'serif', mar = c(0, 0, 0, 0))

  # ------------------
  plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))

  # ------------------
  plot(
    st_geometry(aoi),
    lwd = .5,
    border = global_parameters()$col$coastline,
    add = TRUE
  )

  # ------------------
  plot(
    st_geometry(dat),
    lwd = 1.25,
    add = TRUE,
    pch = 20,
    cex = .25,
    col = global_parameters()$col$palette[1],
    border = global_parameters()$col$palette[1]
  )
}
