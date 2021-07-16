#' plot ceanav integrated data
#'
#' base plot functions for ceanav project
#'
#' @param dat object of class sf
#' @param main main title
#' @param subtitle subtitle
#' @param ... further specifications, see \link{plot} and details.
#'
#'
#' @examples
#'  load_integrated("navigation")
#'  plot_integrated(navigation[,10], "Navigation", "Recherche gouvernementale")
#'
#' @export

plot_integrated <- function(dat, ...) {
  UseMethod("plot_integrated", dat)
}

#' @method plot_integrated sf
#' @name plot_integrated
#' @export
plot_integrated.sf <- function(dat, main = NULL, subtitle = NULL, ...) {

  # ------------------
  dat <- st_transform(dat, global_parameters()$crs)

  # pdf(glue('./figures/figures-format/{data_id}.pdf'), width = 7, height = 5, pointsize = 12)
  # png(glue('./figures/delete.png'), res = 300, width = 100, height = 70, units = "mm", pointsize = 12)

  # ------------------
  global_parameters()

  # ------------------
  par(family = 'serif', mar = c(0, 0, 0, 0))

  # ------------------
  bbox <- global_param$bbox$base
  plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))

  # ------------------
  if (!is.null(main)) {
    text(
      x = bbox$xmin + 1000,
      y = bbox$ymax - 10000,
      labels = main,
      font = 2,
      adj = c(0,.5)
    )
  }

  # ------------------
  if (!is.null(subtitle)) {
    text(
      x = bbox$xmin + 1000,
      y = bbox$ymax - 40000,
      labels = subtitle,
      adj = c(0,.5),
      font = 3,
      cex = .75
    )
  }


  # # ------------------
  # basemap("quebec")
  # plot(
  #   st_geometry(quebec),
  #   col = glue("{global_param$col$integrated$offset}44"),
  #   border = global_param$col$integrated$offset,
  #   lwd = .5,
  #   add = TRUE
  # )
  #
  # # ------------------
  # basemap("usa")
  # plot(
  #   st_geometry(usa),
  #   col = glue("{global_param$col$integrated$offset}44"),
  #   border = global_param$col$integrated$offset,
  #   lwd = .5,
  #   add = TRUE
  # )
  #


  # ------------------
  # pal <- colorRampPalette(global_param$col$palette)
  pal <- colorRampPalette(viridis::viridis(100))
  cols <- pal(101)[((dat[,1,drop = TRUE] / max(dat[,1,drop = TRUE], na.rm = T))*100)+1]
  plot(
    st_geometry(dat),
    lwd = .25,
    add = TRUE,
    pch = 20,
    cex = .25,
    col = cols,
    border = cols
  )

  # ------------------
  data(aoi)
  aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))
  plot(
    st_geometry(aoi),
    lwd = .5,
    border = global_parameters()$col$integrated$coastline,
    add = TRUE
  )

  # dev.off()
}
