#' Figure of ballast exchange waters - data0051
#'
#' @export

fig_data0051_ballast <- function() {
  # ------------------
  data(aoi)
  aoi <- suppressWarnings(st_simplify(aoi, dTolerance = 100, preserveTopology = F))
  basemap("quebec")
  basemap("canada")
  basemap("usa")
  load_format("data0051")

  # ------------------
  global_parameters()

  # ------------------------------------------------------------------------
  # Graph principal
  png(glue('./figures/data0051-ballast.png'), res = 300, width = 70, height = 70, units = "mm", pointsize = 12)

  # ------------------
  par(family = 'serif', mar = c(.5, .5, 1, .5))

  # ------------------
  bbox <- global_param$bbox$base
  qc <- st_bbox(quebec)

  # ------------------
  pal <- colorRampPalette(viridis::viridis(100))
  # pal <- colorRampPalette(global_param$col$integrated$palette)

  # ------------------
  # Basemap
  plot0(x = c(qc$xmin, qc$xmax), y = c(qc$ymin, qc$ymax))
  box()


  # ------------------
  # Text
  mtext(side = 3,
       text = "Zones désignées de renouvellement d’eau de ballast",
       font = 2,
       adj = 0,
       cex = .6
     )

  mtext(text = glue("Donnée(s) brute(s) : 0051"),
        side = 1,
        font = 3,
        adj = .98,
        cex = .4,
        line = -.45)

  # ---------------------------
  # Québec
  # ------------------
  plot(
    st_geometry(quebec),
    lwd = .5,
    border = "#6b6b6b",
    add = TRUE
  )
  plot(
    st_geometry(canada),
    lwd = .5,
    border = "#6b6b6b",
    add = TRUE
  )
  plot(
    st_geometry(usa),
    lwd = .5,
    border = "#6b6b6b",
    add = TRUE
  )
  # ------------------
  plot(
    st_geometry(aoi),
    lwd = .5,
    border = global_param$col$integrated$focus,
    col = global_param$col$integrated$focus,
    add = TRUE
  )

  # ------------------
  plot(
    st_geometry(data0051),
    lwd = .5,
    border = global_param$col$integrated$offset,
    col = global_param$col$integrated$offset,
    add = TRUE
  )

  # ---------------------------
  # Legend
  xmin <- par("usr")[1]
  xmax <- par("usr")[2]
  ymin <- par("usr")[3]
  ymax <- par("usr")[4]
  text(x = xmax-480000, y = ymax-100000, labels = "Zone d'étude", cex = .5, adj = c(0,.5), family = "serif", font = 3)
  text(x = xmax-480000, y = ymax-225000, labels = "Zones de", cex = .5, adj = c(0,.5), family = "serif", font = 3)
  text(x = xmax-480000, y = ymax-300000, labels = "renouvellement", cex = .5, adj = c(0,.5), family = "serif", font = 3)

  polygon(x = c(rep(xmax-520000,2),rep(xmax-600000,2)),
          y = c(ymax-130000, ymax-70000, ymax-70000, ymax-130000),
          border = "#000000",
          col = global_param$col$integrated$focus,
          lwd = 1)

  polygon(x = c(rep(xmax-520000,2),rep(xmax-600000,2)),
          y = c(ymax-292500, ymax-232500, ymax-232500, ymax-292500),
          border = "#000000",
          col = global_param$col$integrated$offset,
          lwd = 1)


  # ---------------------------
  dev.off()
}
