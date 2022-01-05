#' plot ceanav simple data maps with only outline of study area
#'
#' base plot functions for ceanav project
#'
#' @param dat object of class sf
#' @param ... further specifications, see \link{plot} and details.
#'
#' @examples
#'  load_integrated("navigation")
#'  plot_simple(navigation[,10])
#'
#' @export

plot_simple <- function(dat, ...) {
  UseMethod("plot_simple", dat)
}

#' @method plot_simple sf
#' @name plot_simple
#' @export
plot_simple.sf <- function(dat, ...) {

  # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
  # png(glue('./figures/delete.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)

  # ------------------
  uid <- !is.na(dat[, 1, drop = TRUE]) &
         dat[, 1, drop = TRUE] > 0
  dat <- dat[uid, ]

  # ------------------
  data(aoi)
  aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))

  # ------------------
  global_parameters()

  # ------------------
  plotDat <- function(d = dat, a = aoi) {
    # -----
    plot(
      st_geometry(d),
      lwd = .25,
      add = TRUE,
      pch = 20,
      cex = .25,
      col = cols,
      border = cols
    )

    # ------------------
    plot(
      st_geometry(a),
      lwd = .5,
      border = global_parameters()$col$integrated$coastline,
      add = TRUE
    )
  }

  # ------------------
  pal <- colorRampPalette(viridis::viridis(100))
  # pal <- colorRampPalette(global_param$col$integrated$palette)

  # Colors
  bin <- dat[,1,drop = TRUE] %>%
         table() %>%
         names()

  if (length(bin) == 2 | length(bin) == 1) {
    cols <- global_param$col$integrated$palette[4]
  } else {
    maxDat <- max(dat[,1,drop = TRUE], na.rm = TRUE)
    cols <- pal(101)[((dat[,1,drop = TRUE] / maxDat)*100)+1]
  }

  # ------------------------------------------------------------------------
  # Graph principal

  # ------------------
  par(mar = c(.5, .5, .5, .5), bg = "transparent")

  # ------------------
  bbox <- global_param$bbox$base

  # ------------------
  # Basemap
  plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))

  # ------------------
  # Data
  plotDat()

  # dev.off()
}
