#' plot ceanav data maps
#'
#' base plot functions for ceanav project
#'
#' @param dat object of class sf
#' @param main main title
#' @param subtitle subtitle
#' @param unit_data units of data
#' @param references data citation used for integrated data
#' @param ... further specifications, see \link{plot} and details.
#'
#' @examples
#'  load_integrated("navigation")
#'  plot_ceanav(navigation[,10], "Navigation", "Recherche gouvernementale")
#'
#' @export

plot_ceanav <- function(dat, ...) {
  UseMethod("plot_ceanav", dat)
}

#' @method plot_ceanav sf
#' @name plot_ceanav
#' @export
plot_ceanav.sf <- function(dat, main = NULL, subtitle = NULL, unit_data = NULL, references = NULL, ...) {

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

  # ------------------------------------------------------------------------
  # Graph principal

  # ------------------
  par(family = 'serif', mar = c(.5, .5, .5, .5))

  # ------------------
  bbox <- global_param$bbox$base
  fluvial <- global_param$bbox$fluvial
  quebec <- global_param$bbox$quebec
  montreal <- global_param$bbox$montreal
  lacstpierre <- global_param$bbox$lacstpierre

  # ------------------
  pal <- colorRampPalette(viridis::viridis(100))
  # pal <- colorRampPalette(global_param$col$integrated$palette)

  # ------------------
  # Basemap
  plot0(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax))
  box()


  # ------------------
  # Inserts location
  # rect(fluvial[1], fluvial[3], fluvial[2], fluvial[4], lty = 2, border = "#00000088")
  rect(montreal[1], montreal[3], montreal[2], montreal[4], lty = 2,
       border = paste0(global_param$col$integrated$palette[1], "88"))
  rect(lacstpierre[1], lacstpierre[3], lacstpierre[2], lacstpierre[4], lty = 2,
       border = paste0(global_param$col$integrated$palette[3], "88"))
  rect(quebec[1], quebec[3], quebec[2], quebec[4], lty = 2,
       border = paste0(global_param$col$integrated$palette[5], "88"))

  # ------------------
  # Legend
  bin <- dat[,1,drop = TRUE] %>%
         table() %>%
         names()

  if (length(bin) == 2 | length(bin) == 1) {
    cols <- global_param$col$integrated$palette[4]
    plot_legend_bin(
      col = cols,
      subTitle = "Présence",
      cexSub = .5
    )
  } else {
    maxDat <- max(dat[,1,drop = TRUE], na.rm = TRUE)
    cols <- pal(101)[((dat[,1,drop = TRUE] / maxDat)*100)+1]
    plot_legend_cont(
      range = range(dat[,1,drop = TRUE], na.rm = TRUE),
      pal = pal,
      subTitle = unit_data,
      cexSub = .5
    )
  }

  # ------------------
  # Text
  if (!is.null(main)) {
    text(x = bbox$xmin + 1000,
         y = bbox$ymax - 10000,
         labels = main,
         font = 2,
         adj = c(0,.5)
       )
  }

  if (!is.null(subtitle)) {
    text(
      x = bbox$xmin + 1000,
      y = bbox$ymax - 40000,
      labels = subtitle,
      adj = c(0,.5),
      font = 3,
      cex = .65
    )
  }

  # ------------------
  # Data
  plotDat()

  # ------------------------------------------------------------------------
  # Inserts
  # Place name
  name <- function(nm) {
    xmin <- par("usr")[1]
    xmax <- par("usr")[2]
    ymin <- par("usr")[3]
    ymax <- par("usr")[4]
    x <- xmin + 2500
    y <- ymax - 4000
    text(x, y, nm, adj = c(0,.5), cex = .5, col = global_param$col$integrated$textOff)
  }

  # ---------------------------
  # Montreal
  par(new = TRUE)
  par(fig = c(.525,.68,.05,.3), mar = c(0,0,0,0))
  # par(fig = c(.545,.745,.05,.25), mar = c(0,0,0,0))
  plot0(x = c(montreal$xmin, montreal$xmax), y = c(montreal$ymin+5000, montreal$ymax))
  box(col = paste0(global_param$col$integrated$palette[1], "88"))
  plotDat()
  name("Montréal")

  # ---------------------------
  # Lac St-Pierre
  par(new = TRUE)
  par(fig = c(.7,.965,.05,.3), mar = c(0,0,0,0))
  # par(fig = c(.765,.965,.05,.25), mar = c(0,0,0,0))
  plot0(x = c(lacstpierre$xmin, lacstpierre$xmax), y = c(lacstpierre$ymin, lacstpierre$ymax))
  box(col = paste0(global_param$col$integrated$palette[3], "88"))
  plotDat()
  name("Lac St-Pierre")


  # ---------------------------
  # Québec
  par(new = TRUE)
  par(fig = c(.7,.965,.325,.575), mar = c(0,0,0,0))
  # par(fig = c(.765,.965,.275,.475), mar = c(0,0,0,0))
  plot0(x = c(quebec$xmin, quebec$xmax), y = c(quebec$ymin, quebec$ymax))
  box(col = paste0(global_param$col$integrated$palette[5], "88"))
  plotDat()
  name("Québec")


  # dev.off()
}
