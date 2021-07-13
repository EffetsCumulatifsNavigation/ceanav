#' plot ceanav formatted data
#'
#' base plot functions for ceanav project
#'
#' @param dat object of class sf, stars
#' @param ... further specifications, see \link{plot} and details.
#'
#'
#' @export

plot_format <- function(dat, ...) {
  UseMethod("plot_format", dat)
}

#' @method plot_format sf
#' @name plot_format
#' @export
plot_format.sf <- function(dat, ...) {
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

#' @method plot_format stars
#' @name plot_format
#' @export
plot_format.stars <- function(dat, ...) {
  # WARNING: Make this better at some point
  # ------------------
  plot(dat)
}
